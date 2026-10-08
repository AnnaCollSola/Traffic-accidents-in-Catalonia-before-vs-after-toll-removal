#===============================================================================
# 1. SETUP AND LIBRARIES
#===============================================================================

# Install packages if missing
install.packages(c("ggcorrplot", "lsr", "gridExtra", "fastDummies", "caret",
                   "kernlab", "VIM", "glmnet", "tidyverse", "writexl",
                   "rpart", "rpart.plot", "vcd", "factoextra", "corrplot","ClustOfVar"))

suppressPackageStartupMessages({
  library(tidyverse)    # Data manipulation and visualization
  library(GGally)       # Extension to ggplot2
  library(ggcorrplot)   # Correlation plots
  library(lubridate)    # Date handling
  library(stringr)      # String manipulation
  library(scales)       # Formatting scales
  library(writexl)      # Excel export
  library(forcats)      # Factor handling
  library(fastDummies)  # One-hot encoding
  library(caret)        # Machine Learning workflow
  library(kernlab)      # SVM
  library(gridExtra)    # Grid graphics
  library(VIM)          # Missing value visualization/imputation
  library(glmnet)       # Lasso/Ridge regression
  library(rpart)
  library(rpart.plot)
  library(vcd)          # For Cramer's V (Categorical association)
  library(factoextra)   # For beautiful dendrograms
  library(corrplot)     # For correlation visualization
  library(ClustOfVar)
  library(stringr)
  library(pROC)
  library(ggplot2)
})

#===============================================================================
# 2. HELPER FUNCTIONS
#===============================================================================

# Generate summary statistics for all columns
get_column_summary <- function(df) {
  df %>%
    select(everything()) %>%
    imap_dfr(~ {
      vals <- .x
      tibble(
        column   = .y,
        type     = class(vals)[1],
        n_unique = n_distinct(vals),
        n_na     = sum(is.na(vals)),
        uniques  = paste(sort(unique(vals)), collapse = " | ")
      )
    })
}

# Analyze missing values grouped by a specific column
get_na_summary <- function(df, group_col = "toll_period", before_value = "before", after_value = "after") {
  n_before <- sum(df[[group_col]] == before_value, na.rm = TRUE)
  n_after  <- sum(df[[group_col]] == after_value, na.rm = TRUE)
  
  # Row-wise summary
  row_na_summary <- df %>%
    mutate(n_NA = rowSums(is.na(.))) %>%
    group_by(n_NA) %>%
    summarise(
      before_count = sum(.data[[group_col]] == before_value, na.rm = TRUE),
      after_count  = sum(.data[[group_col]] == after_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      before_percent = before_count / n_before * 100,
      after_percent  = after_count / n_after * 100
    ) %>%
    arrange(desc(n_NA))
  
  # Column-wise summary
  col_na_summary <- df %>%
    group_by(.data[[group_col]]) %>%
    summarise(across(everything(), ~ sum(is.na(.))), .groups = "drop") %>%
    pivot_longer(cols = -all_of(group_col), names_to = "column", values_to = "n_NA") %>%
    pivot_wider(names_from = all_of(group_col), values_from = n_NA, values_fill = 0) %>%
    rename(before = all_of(before_value), after = all_of(after_value)) %>%
    mutate(
      before_percent = before / n_before * 100,
      after_percent  = after  / n_after  * 100
    ) %>%
    arrange(desc(before))
  
  list(rows = row_na_summary, cols = col_na_summary)
}

# Clean variable names (remove quotes and spaces)
clean_var_names <- function(df) {
  df$Variable <- gsub("['\"`]", "", df$Variable)
  df$Variable <- trimws(df$Variable)
  return(df)
}

# Extract coefficients from Lasso model
extract_lasso_coefs <- function(model) {
  best_model  <- model$finalModel
  best_lambda <- model$bestTune$lambda
  
  coefs <- coef(best_model, s = best_lambda)
  coefs_df <- as.data.frame(as.matrix(coefs))
  colnames(coefs_df) <- "Value"
  coefs_df$Variable <- rownames(coefs_df)
  
  coefs_df %>%
    filter(Variable != "(Intercept)", Value != 0) %>%
    clean_var_names() %>%
    arrange(desc(abs(Value)))
}

# Extract importance scores from Caret models (SVM/RF)
extract_caret_importance <- function(model) {
  imp_obj <- varImp(model, scale = TRUE)
  imp <- imp_obj$importance
  if (ncol(imp) > 0) colnames(imp)[1] <- "Value"
  
  imp %>%
    rownames_to_column(var = "Variable") %>%
    clean_var_names() %>%
    arrange(desc(Value))
}

# Generic plotting function for feature importance
create_imp_plot <- function(df, model_name, color_hex, show_direction = FALSE) {
  df_top <- head(df, 15)
  
  p <- ggplot(df_top, aes(x = reorder(Variable, abs(Value)), y = Value)) +
    coord_flip() +
    theme_minimal() +
    labs(title = model_name, x = "", y = "Importance / Coeff")
  
  if (show_direction) {
    p <- p + geom_col(aes(fill = Value > 0)) +
      scale_fill_manual(values = c("FALSE" = "#377EB8", "TRUE" = "#E41A1C"),
                        labels = c("Before", "After"),
                        name = "Direction")
  } else {
    p <- p + geom_col(fill = color_hex)
  }
  return(p)
}

# Prepare importance dataframes for consensus calculation
prep_for_consensus <- function(df, model_name) {
  df %>%
    mutate(Value = abs(Value)) %>%
    rename(!!model_name := Value)
}

# Extract performance metrics (ROC, Sens, Spec)
extract_metrics <- function(model, run_label, algo_label) {
  perf <- getTrainPerf(model)
  tibble(
    Run         = run_label,
    Algorithm   = algo_label,
    ROC         = round(perf$TrainROC, 4),
    Sens        = round(perf$TrainSens, 4),
    Spec        = round(perf$TrainSpec, 4)
  )
}

# Helper to identify pure dummy columns (only 0, 1, or NA)
is_binary_col <- function(x) {
  all(unique(x) %in% c(0, 1, NA))
}

# Helper to abbreviate names for clustered variables
create_smart_abbr <- function(var_name) {
  # 1. Split at underscore
  parts <- str_split(var_name, "_", simplify = TRUE)[1, ]
  parts <- parts[parts != ""] # Remove empty parts
  n <- length(parts)
  
  # If the name is short (only 1 or 2 parts), leave it whole
  if (n <= 2) {
    return(var_name)
  }
  
  # 2. Apply logic
  # Everything BEFORE the last 2 words -> shorten to 2 letters
  idx_abbr <- 1:(n - 2)
  abbr_parts <- substr(parts[idx_abbr], 1, 2)
  
  # The last 2 words -> write out fully
  idx_keep <- (n - 1):n
  keep_parts <- parts[idx_keep]
  
  # 3. Reassemble
  prefix <- paste(abbr_parts, collapse = "")
  suffix <- paste(keep_parts, collapse = "_")
  
  return(paste0(prefix, "_", suffix))
}

#===============================================================================
# 3. DATA LOADING AND INITIAL TRANSFORMATION
#===============================================================================

# Setup directory
# NOTE: Assuming file is in working directory for this script
accidents <- read.csv("data/accidents_transformed.csv", check.names = FALSE)
cat("Initial columns:", ncol(accidents), "\n")

accidents <- accidents %>%
  mutate(
    # Date conversion and pandemic flag
    Date = dmy(Date),
    
    # Clean Road variable ('SE' -> NA)
    Road = na_if(Road, "SE"),
    
    # Clean Speed limit (invalid values -> NA)
    `Speed limit` = if_else(`Speed limit` > 121, NA_real_, `Speed limit`),
    
    # Clean Kilometer marker
    `Kilometer marker` = str_replace(`Kilometer marker`, ",", "."),
    `Kilometer marker` = ifelse(str_detect(`Kilometer marker`, "9999"), NA, `Kilometer marker`),
    `Kilometer marker` = as.numeric(`Kilometer marker`),
    
    # Define Toll Period (Target Variable Proxy)
    toll_period = if_else(Date >= as.Date("2021-09-01"), "after", "before"),
    toll_period = factor(toll_period, levels = c("before", "after")),
    
    # Define Toll Corridor (Specific roads where tolls were removed)
    toll_corridor = if_else(Road %in% c("AP-7", "AP-2", "C-32", "C-33"), "toll_removed", "other"),
    toll_corridor = factor(toll_corridor, levels = c("other", "toll_removed")),
    
    # Group roads into families
    road_family = case_when(
      str_starts(Road, "AP-") ~ "AP",
      str_starts(Road, "A-")  ~ "A",
      str_starts(Road, "N-")  ~ "N",
      str_starts(Road, "B-")  ~ "B",
      is.na(Road)             ~ "missing",
      TRUE                    ~ "other"
    ),
    road_family = factor(road_family),
    
    # Fix Time of accident format
    `Time of accident` = ifelse(grepl(",", `Time of accident`), `Time of accident`, paste0(`Time of accident`, ",00")),
    `Time of accident` = str_replace(`Time of accident`, ",", ":"),
    
    # Replace "Unspecified" text with true NA in all character columns
    across(where(is.character), ~ ifelse(.x == "Unspecified", NA, .x))
  )

cat("Columns after transformation:", ncol(accidents), "\n")

#===============================================================================
# 4. MISSING VALUE ANALYSIS AND HANDLING
#===============================================================================

# Analyze missing values
na_summary <- get_na_summary(accidents)$cols
na_summary_filtered <- na_summary %>% filter(before_percent > 20, after_percent > 20)

# Visualizing missing values
p1 <- ggplot(na_summary_filtered, aes(x = column, y = before_percent, fill = column)) +
  geom_col() + labs(title = "NA % Before") + theme(legend.position = "none") + coord_flip()
p2 <- ggplot(na_summary_filtered, aes(x = column, y = after_percent, fill = column)) +
  geom_col() + labs(title = "NA % After") + theme(legend.position = "none") + coord_flip()
grid.arrange(p1, p2, ncol = 2)

# Drop columns with excessive missing values and time related columns
cols_to_drop <- na_summary_filtered$column
accidents <- accidents %>% select(-all_of(cols_to_drop))
accidents <- accidents %>% select(-`Time of accident`)
accidents <- accidents %>% select(-`Date`)
accidents <- accidents %>% select(-`Year`)

cat("Columns after dropping high-NA variables:", ncol(accidents), "\n")

#===============================================================================
# 5. PRE-PROCESSING: LEVELS & IMPUTATION
#===============================================================================

threshold_levels <- 10

plot_data <- accidents %>%
  select(where(is.character)) %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_unique") %>%
  filter(n_unique >= threshold_levels - 3) %>%
  mutate(variable = fct_reorder(variable, n_unique))

# Visualize levels
ggplot(plot_data, aes(x = variable, y = n_unique, fill = n_unique > threshold_levels)) +
  geom_col() +
  geom_hline(yintercept = threshold_levels, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  coord_flip() + theme_minimal() +
  labs(title = "Levels per Variable", subtitle = paste("Cut-off:", threshold_levels))

# Drop columns exceeding the threshold
cols_high_levels <- plot_data %>% filter(n_unique > threshold_levels) %>% pull(variable)

accidents <- accidents %>% select(-all_of(as.character(cols_high_levels)))

cat("Dropped columns with >", threshold_levels, "levels:", paste(cols_high_levels, collapse = ", "), "\n")

# Imputation & One-Hot Encoding

# Fill NAs
accidents_prep <- accidents %>%
  mutate(
    # Numeric: Fill with Median
    across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))),
    
    # Categorical: Fill with Mode + Keep top 15 levels
    across(where(is.character) | where(is.factor), ~ {
      # Calc mode
      tbl <- table(.)
      mode_val <- names(tbl)[which.max(tbl)]
      if(is.null(mode_val)) mode_val <- "Unknown" # Fallback
      
      filled <- replace_na(as.character(.), mode_val)
      fct_lump_n(factor(filled), n = 15, other_level = "Other")
    }),
    
    # Keep target as factor
    toll_period = factor(toll_period, levels = c("before", "after"))
  ) %>%
  # Create dummies (One-Hot)
  dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE)

cat("Dimensions after One-Hot Encoding:", paste(dim(accidents_prep), collapse = " x "), "\n")

# A) Isolate the Dummies (These will be clustered)
dummies_df <- accidents_prep %>%
  select(-matches("toll_period|toll_corridor")) %>% # Exclude target and toll corridor
  select(where(is.numeric)) %>%
  select(where(is_binary_col))

# B) Park the "Real" Numeric Variables (Speed, Vehicles, etc.)
numeric_original_df <- accidents_prep %>%
  select(-matches("toll_period")) %>%
  select(where(is.numeric)) %>%
  select(-all_of(names(dummies_df)))

cat("-> Dummies to cluster:", ncol(dummies_df), "\n")
cat("-> Numeric vars preserved:", ncol(numeric_original_df), "\n")

#===============================================================================
# 6. CLUSTERING: CORRELATION & CONSENSUS
#===============================================================================

# Calculate Correlation Matrix (Efficient Input for both algorithms)
cor_mat <- cor(dummies_df, use = "pairwise.complete.obs")

# Define inputs based on correlation
dist_mat <- as.dist(1 - abs(cor_mat))
# Input for K-Means
X_clustering <- 1 - abs(cor_mat)

target_k <- 35 # Reduce number of encoded categorical variables by half

# Run 1: Hierarchical Clustering
hc <- hclust(dist_mat, method = "complete")
cluster_h <- cutree(hc, k = target_k)

# Run 2: K-Means Clustering
set.seed(42)
km <- kmeans(X_clustering, centers = target_k, nstart = 25)
cluster_k <- km$cluster

# Consensus Logic: Match variables only if BOTH algorithms agree
consensus_df <- data.frame(
  Variable = names(cluster_h),
  H_Cluster = cluster_h,
  K_Cluster = cluster_k
) %>%
  group_by(H_Cluster, K_Cluster) %>%
  mutate(Final_Group_ID = cur_group_id()) %>%
  ungroup()

# Visualization: Dendrogram
# Long branches = distinct groups. Short branches = high similarity.
fviz_dend(
  hc,
  k = target_k,
  cex = 0.5,
  k_colors = "jco",
  rect = TRUE,
  rect_border = "jco",
  rect_fill = TRUE,
  horiz = TRUE,
  main = "Hierarchical Clustering of Variables"
) +
  theme(plot.margin = margin(1, 4, 1, 1, "cm")) # Fix for cut-off labels

# Consensus Check
disagreements <- consensus_df %>%
  group_by(Final_Group_ID) %>%
  filter(n_distinct(H_Cluster) > 1 | n_distinct(K_Cluster) > 1)
agreements <- consensus_df %>%
  group_by(Final_Group_ID) %>%
  filter(n_distinct(H_Cluster) == 1 & n_distinct(K_Cluster) == 1) %>%
  ungroup()

head(agreements)
if(nrow(disagreements) > 0) {
  cat("Note: The algorithms disagreed on", nrow(disagreements), "variables.\n")
  cat("These will be separated into stricter sub-groups to ensure safety.\n")
} else {
  cat("Perfect Consensus! Both algorithms agree on all groupings.\n")
}

#===============================================================================
# 7. AGGREGATION: CREATING DUMMY GROUPS
#===============================================================================

n_final_groups <- max(consensus_df$Final_Group_ID)
dummies_processed <- data.frame(row.names = 1:nrow(accidents_prep))

for(i in 1:n_final_groups) {
  # Get variables of the group
  vars_in_group <- consensus_df %>% filter(Final_Group_ID == i) %>% pull(Variable)
  
  # We take maximum top 3 so the name doesn't explode
  top_vars <- head(vars_in_group, 3)
  
  # 1. Apply your abbreviation function to each variable
  short_names <- sapply(top_vars, create_smart_abbr)
  
  # 2. Connect the variables
  combined_name <- paste(short_names, collapse = "_&_")
  
  # Optional: If there were more than 3, indicate with "..."
  if(length(vars_in_group) > 3) {
    combined_name <- paste0(combined_name, "_etc")
  }
  
  # Final column name
  new_col_name <- paste0(i, "_", combined_name)
  
  # --- ASSIGNMENT ---
  if(length(vars_in_group) == 1) {
    dummies_processed[[new_col_name]] <- dummies_df[[vars_in_group]]
  } else {
    dummies_processed[[new_col_name]] <- rowMeans(dummies_df[, vars_in_group], na.rm = TRUE)
  }
}

# --- CHECK RESULT ---
print(names(dummies_processed))

# Create overview
group_overview <- consensus_df %>%
  group_by(Final_Group_ID) %>%
  summarise(
    Count_Vars = n(),
    Variable_List = paste(Variable, collapse = ", ")
  )

write.csv(group_overview, "cluster_groups_overview.csv", row.names = FALSE)

# Show table (n=Inf shows all rows)
print(group_overview, n = Inf)

#===============================================================================
# 8. RE-ASSEMBLY: FINAL DATASET CREATION
#===============================================================================

# Combine: [Aggregated Dummies] + [Original Numerics] + [Target]
accidents_processed <- bind_cols(dummies_processed, numeric_original_df) %>%
  bind_cols(select(accidents_prep, toll_period_after))

print("--- Feature Engineering Complete ---")
cat("Final structure:", ncol(accidents_processed), "columns.\n")

#===============================================================================
# 9. MODELING SETUP: SPLIT & CONTROLS
#===============================================================================

target_col <- "toll_period_after"

# Final data selection
model_data <- accidents_processed %>%
  mutate(toll_period_after = factor(toll_period_after, levels = c(0, 1), labels = c("before", "after"))) %>%
  select(-matches("Number of unknown units involved"))

# Verification
if (sum(is.na(model_data)) == 0) cat("✓ Data check passed: No missing values.\n")

# Train/Test Split
set.seed(42)
train_index <- createDataPartition(model_data[[target_col]], p = 0.8, list = FALSE)
train_data  <- model_data[train_index, ]
test_data   <- model_data[-train_index, ]

# Training Control
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "down",
  verboseIter = FALSE
)

# Remove Zero-Variance Variables
nzv <- nearZeroVar(train_data, saveMetrics = TRUE, freqCut = 99/1)
# Important: Check if columns exist before exempting them
vars_to_keep <- intersect(c("toll_corridor_toll_removed", "road_family_AP"), colnames(train_data))
nzv[rownames(nzv) %in% vars_to_keep, "nzv"] <- FALSE

if (any(nzv$nzv)) {
  cols_remove_nzv <- rownames(nzv)[nzv$nzv]
  train_data <- train_data[, !nzv$nzv]
  cat("Removed Zero-Variance vars:", paste(cols_remove_nzv, collapse = ", "), "\n")
}
cat("Remaining columns count:", ncol(train_data), "\n")

#===============================================================================
# 10. RUN 1: FULL DATASET
#===============================================================================

print("--- Starting Run 1: Full Dataset ---")

# Lasso / Elastic Net
fit_glmnet <- train(as.formula(paste(target_col, "~ .")), data = train_data, method = "glmnet",
                    trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 10)

# SVM Radial
fit_svm <- train(as.formula(paste(target_col, "~ .")), data = train_data, method = "svmRadial",
                 trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 5)

# Random Forest
fit_rf <- train(as.formula(paste(target_col, "~ .")), data = train_data, method = "ranger",
                trControl = control, metric = "ROC", importance = "impurity")

# Visualization Run 1
df_lasso_v1 <- extract_lasso_coefs(fit_glmnet) %>% mutate(Variable = str_trunc(Variable, 50))
df_svm_v1   <- extract_caret_importance(fit_svm) %>% mutate(Variable = str_trunc(Variable, 50))
df_rf_v1    <- extract_caret_importance(fit_rf) %>% mutate(Variable = str_trunc(Variable, 50))

p1 <- create_imp_plot(df_lasso_v1, "Run 1: Lasso", NA, show_direction = TRUE)
p2 <- create_imp_plot(df_svm_v1,   "Run 1: SVM",   "#377EB8")
p3 <- create_imp_plot(df_rf_v1,    "Run 1: RF",    "#4DAF4A")

print(p1)
print(p2)
print(p3)

# Consensus Calculation
consensus_v1 <- prep_for_consensus(df_lasso_v1, "Lasso") %>%
  full_join(prep_for_consensus(df_svm_v1, "SVM"), by = "Variable") %>%
  full_join(prep_for_consensus(df_rf_v1, "RandomForest"), by = "Variable") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(Lasso = scales::rescale(Lasso, to = c(0, 100))) %>%
  rowwise() %>%
  mutate(Mean_Importance = mean(c(Lasso, SVM, RandomForest), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Mean_Importance))

# --- 1. Top 20 (Most Important Variables) ---
top_20 <- consensus_v1 %>%
  arrange(desc(Mean_Importance)) %>%
  head(20)

p_top <- ggplot(top_20 %>% mutate(Variable = str_trunc(Variable, 50)), 
                aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
  geom_col(fill = "darkslateblue", width = 0.7) +
  geom_text(aes(label = round(Mean_Importance, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 20 Most Important Variables (Consensus)",
    subtitle = "High influence on classifying before vs. after",
    x = "",
    y = "Mean Importance Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# --- 2. Bottom 20 (Least Important Variables) ---
bottom_20 <- consensus_v1 %>%
  arrange(Mean_Importance) %>%
  head(20)

p_bottom <- ggplot(bottom_20 %>% mutate(Variable = str_trunc(Variable, 50)), 
                   aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
  geom_col(fill = "firebrick", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Mean_Importance, 2)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Bottom 20 Least Important Variables",
    subtitle = "Candidates for Feature Elimination (Run 2)",
    x = "",
    y = "Mean Importance Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_top)
print(p_bottom)

#===============================================================================
# 11. RUN 2: REDUCED DATASET (FEATURE SELECTION)
#===============================================================================

# Identify bottom 30 variables
bottom_30_df <- consensus_v1 %>% arrange(Mean_Importance) %>% head(30)
vars_to_drop_r1 <- bottom_30_df %>% pull(Variable)

vars_to_drop_r1 <- setdiff(vars_to_drop_r1, target_col)

# Create Reduced Dataset
train_data_run2 <- train_data %>% select(-any_of(vars_to_drop_r1))
cat("Reduced variables from", ncol(train_data), "to", ncol(train_data_run2), "\n")

print("--- Starting Run 2: Reduced Dataset ---")

fit_glmnet_v2 <- train(as.formula(paste(target_col, "~ .")), data = train_data_run2, method = "glmnet",
                       trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 10)

fit_svm_v2 <- train(as.formula(paste(target_col, "~ .")), data = train_data_run2, method = "svmRadial",
                    trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 5)

fit_rf_v2 <- train(as.formula(paste(target_col, "~ .")), data = train_data_run2, method = "ranger",
                   trControl = control, metric = "ROC", importance = "impurity")

# Visualization Run 2
df_lasso_v2 <- extract_lasso_coefs(fit_glmnet_v2) %>% mutate(Variable = str_trunc(Variable, 50))
df_svm_v2   <- extract_caret_importance(fit_svm_v2) %>% mutate(Variable = str_trunc(Variable, 50))
df_rf_v2    <- extract_caret_importance(fit_rf_v2) %>% mutate(Variable = str_trunc(Variable, 50))

p1_v2 <- create_imp_plot(df_lasso_v2, "Run 2: Lasso", NA, show_direction = TRUE)
p2_v2 <- create_imp_plot(df_svm_v2,   "Run 2: SVM",   "#377EB8")
p3_v2 <- create_imp_plot(df_rf_v2,    "Run 2: RF",    "#4DAF4A")

print(p1_v2)
print(p2_v2)
print(p3_v2)

# Visualization: Top 20 Variables (Run 2)
consensus_v2 <- prep_for_consensus(df_lasso_v2, "Lasso") %>%
  full_join(prep_for_consensus(df_svm_v2, "SVM"), by = "Variable") %>%
  full_join(prep_for_consensus(df_rf_v2, "RandomForest"), by = "Variable") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(Lasso = scales::rescale(Lasso, to = c(0, 100))) %>%
  rowwise() %>%
  mutate(Mean_Importance = mean(c(Lasso, SVM, RandomForest), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Mean_Importance))

top_20_v2 <- consensus_v2 %>%
  arrange(desc(Mean_Importance)) %>%
  head(20)

p_top_v2 <- ggplot(top_20_v2 %>% mutate(Variable = str_trunc(Variable, 50)), 
                   aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
  geom_col(fill = "darkslateblue", width = 0.7) +
  geom_text(aes(label = round(Mean_Importance, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 20 Most Important Variables (Run 2)",
    subtitle = "Consensus on Reduced Feature Set",
    x = "",
    y = "Mean Importance Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_top_v2)

# Compare Performance (Run 1 vs Run 2)
results_comparison <- resamples(list(
  Lasso_V1 = fit_glmnet, Lasso_V2 = fit_glmnet_v2,
  SVM_V1   = fit_svm,    SVM_V2   = fit_svm_v2,
  RF_V1    = fit_rf,     RF_V2    = fit_rf_v2
))
bwplot(results_comparison, main = "Performance: Full vs. Reduced")

# 1. ROC-List 
roc_list_clean <- list(
  # Run 1
  "R1: Lasso" = roc(test_data[[target_col]], probs_r1_lasso),
  "R1: SVM"   = roc(test_data[[target_col]], probs_r1_svm),
  "R1: RF"    = roc(test_data[[target_col]], probs_r1_rf),
  
  # Run 2
  "R2: Lasso" = roc(test_data[[target_col]], probs_r2_lasso),
  "R2: SVM"   = roc(test_data[[target_col]], probs_r2_svm),
  "R2: RF"    = roc(test_data[[target_col]], probs_r2_rf)
)

# 2. Plot (Only Run 1 & 2)
g_clean <- ggroc(roc_list_clean, legacy.axes = TRUE, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") +
  labs(
    title = "ROC Curve Comparison (Runs 1 & 2)",
    subtitle = "Comparing Full Data (R1) vs. Reduced Feature Set (R2)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model Configuration"
  ) +
  theme_minimal() +
  # Colors only for R1 and R2
  scale_color_manual(values = c(
    "R1: Lasso"="#A6CEE3", "R1: SVM"="#1F78B4", "R1: RF"="#B2DF8A",
    "R2: Lasso"="#33A02C", "R2: SVM"="#FB9A99", "R2: RF"="#E31A1C"
  )) +
  theme(legend.position = "right")

print(g_clean)

#===============================================================================
# 12. RUN 3: TOLL ROADS SUBSET (DEEP DIVE)
#===============================================================================

print("--- Starting Run 3: Toll Roads Only ---")

# Filter for toll roads only
toll_data <- model_data %>%
  filter(toll_corridor_toll_removed == 1) %>%
  select(-toll_corridor_toll_removed)

# Create specific partition
set.seed(42)
train_index_toll <- createDataPartition(toll_data[[target_col]], p = 0.8, list = FALSE)
train_data_toll  <- toll_data[train_index_toll, ]

# Zero-Variance check for subset
# Calculate variance metrics
nzv_metrics <- nearZeroVar(train_data_toll, saveMetrics = TRUE, freqCut = 99/1)

# Only proceed if zero-variance variables exist
if (any(nzv_metrics$nzv)) {
  
  # 1. Capture names and metrics of variables to remove
  vars_to_remove <- rownames(nzv_metrics)[nzv_metrics$nzv]
  removal_table  <- nzv_metrics[vars_to_remove, c("freqRatio", "percentUnique")]
  
  # 2. Print table to console
  cat("\n--- Removing Zero-Variance Variables (Run 3) ---\n")
  print(removal_table)
  
  # 3. Visualization: Why are they being removed?
  # We convert row names to a column for plotting
  nzv_plot_data <- removal_table %>%
    rownames_to_column(var = "Variable") %>%
    arrange(desc(freqRatio))
  
  p_nzv <- ggplot(nzv_plot_data %>% mutate(Variable = str_trunc(Variable, 50)), 
                  aes(x = reorder(Variable, freqRatio), y = freqRatio)) +
    geom_col(fill = "firebrick", alpha = 0.8, width = 0.6) +
    coord_flip() +
    # Mark the threshold of 99
    geom_hline(yintercept = 99, linetype = "dashed", color = "gray30") +
    labs(
      title = "Variables Removed: Zero Variance (Run 3)",
      subtitle = "High Frequency Ratio indicates extreme imbalance (or constant values)",
      x = "",
      y = "Frequency Ratio (Majority : Minority)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 12)
    )
  
  print(p_nzv)
  
  # 4. Remove variables from dataset
  train_data_toll <- train_data_toll[, !nzv_metrics$nzv]
  cat("Action: Removed", length(vars_to_remove), "variables from train_data_toll.\n")
  
} else {
  cat("✓ No Zero-Variance variables found in Run 3.\n")
}
# Train Models (Subset)
fit_glmnet_toll <- train(as.formula(paste(target_col, "~ .")), data = train_data_toll, method = "glmnet",
                         trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 10)

fit_svm_toll <- train(as.formula(paste(target_col, "~ .")), data = train_data_toll, method = "svmRadial",
                      trControl = control, metric = "ROC", preProcess = c("center", "scale"), tuneLength = 5)

fit_rf_toll <- train(as.formula(paste(target_col, "~ .")), data = train_data_toll, method = "ranger",
                     trControl = control, metric = "ROC", importance = "impurity")

# Visualization Run 3
df_lasso_v3 <- extract_lasso_coefs(fit_glmnet_toll) %>% mutate(Variable = str_trunc(Variable, 50))
df_svm_v3   <- extract_caret_importance(fit_svm_toll) %>% mutate(Variable = str_trunc(Variable, 50))
df_rf_v3    <- extract_caret_importance(fit_rf_toll) %>% mutate(Variable = str_trunc(Variable, 50))

p1_v3 <- create_imp_plot(df_lasso_v3, "Run 3: Lasso (Toll Only)", NA, show_direction = TRUE)
p2_v3 <- create_imp_plot(df_svm_v3,   "Run 3: SVM (Toll Only)",   "#377EB8")
p3_v3 <- create_imp_plot(df_rf_v3,    "Run 3: RF (Toll Only)",    "#4DAF4A")

print(p1_v3)
print(p2_v3)
print(p3_v3)

# Consensus Calculation Run 3
consensus_v3 <- prep_for_consensus(df_lasso_v3, "Lasso") %>%
  full_join(prep_for_consensus(df_svm_v3, "SVM"), by = "Variable") %>%
  full_join(prep_for_consensus(df_rf_v3, "RandomForest"), by = "Variable") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(Lasso = scales::rescale(Lasso, to = c(0, 100))) %>%
  rowwise() %>%
  mutate(Mean_Importance = mean(c(Lasso, SVM, RandomForest), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Mean_Importance))

# Visualization: Top 20 Variables (Run 3)
top_20_v3 <- consensus_v3 %>%
  arrange(desc(Mean_Importance)) %>%
  head(20)

p_top_v3 <- ggplot(top_20_v3 %>% mutate(Variable = str_trunc(Variable, 50)), 
                   aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
  geom_col(fill = "darkslateblue", width = 0.7) +
  geom_text(aes(label = round(Mean_Importance, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 20 Most Important Variables (Run 3)",
    subtitle = "Consensus on Toll Roads Only",
    x = "",
    y = "Mean Importance Score (0-100)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_top_v3)

# DATA QUALITY CHECK: Road Type Consistency on Toll Roads

# Motivation:
# 'Road type_Motorway' was identified as a very strong predictor in Run 3.
# Since the physical road did not change, we suspect a labeling inconsistency
# between 'before' and 'after' periods in the raw data.

cat("--- Analyzing Road Type Distribution on Toll Roads (AP-7, AP-2, C-32, C-33) ---\n")

# 1. Filter for toll roads and calculate the percentage of each road type per period
road_type_analysis <- accidents %>%
  filter(toll_corridor == "toll_removed") %>%   # Focus only on the relevant highways
  group_by(toll_period, `Road type`) %>%        # Group by time period and raw road label
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percent = round(n / sum(n) * 100, 1))  # Calculate share in %

# Print the table to console
print(road_type_analysis)

# 2. Visualize the shift in labeling
p_quality_check <- ggplot(road_type_analysis, aes(x = toll_period, y = percent, fill = `Road type`)) +
  geom_col(position = "stack") +
  # Add percentage labels for segments > 5%
  geom_text(aes(label = ifelse(percent > 5, paste0(percent, "%"), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Data Quality Check: Road Type Labeling",
    subtitle = "Did the classification of the exact same roads change over time?",
    y = "Percentage of Accidents",
    x = "Period"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") # Distinct colors

print(p_quality_check)


#===============================================================================
# 13. FINAL EVALUATION AND MASTER TABLE
#===============================================================================

# Compile all results
master_metrics <- bind_rows(
  # Run 1
  extract_metrics(fit_glmnet, "Run 1 (Full)", "Lasso"),
  extract_metrics(fit_svm,    "Run 1 (Full)", "SVM"),
  extract_metrics(fit_rf,     "Run 1 (Full)", "Random Forest"),
  # Run 2
  extract_metrics(fit_glmnet_v2, "Run 2 (Reduced)", "Lasso"),
  extract_metrics(fit_svm_v2,    "Run 2 (Reduced)", "SVM"),
  extract_metrics(fit_rf_v2,     "Run 2 (Reduced)", "Random Forest"),
  # Run 3
  extract_metrics(fit_glmnet_toll, "Run 3 (Toll Only)", "Lasso"),
  extract_metrics(fit_svm_toll,    "Run 3 (Toll Only)", "SVM"),
  extract_metrics(fit_rf_toll,     "Run 3 (Toll Only)", "Random Forest")
)

print("--- Master Performance Summary ---")
print(master_metrics %>% arrange(Run, desc(ROC)))

# Dotplot Visualization
ggplot(master_metrics, aes(x = ROC, y = reorder(interaction(Run, Algorithm), ROC), color = Run)) +
  geom_point(size = 4) +
  geom_text(aes(label = round(ROC, 3)), vjust = -0.8, size = 3, color = "black") +
  labs(title = "Model Comparison: ROC Performance", x = "ROC (Area Under Curve)", y = "Model Configuration") +
  theme_minimal() +
  theme(legend.position = "right")

#===============================================================================
# 14. EXPORT PERFORMANCE TABLE (Run, Model, ROC-AUC)
#===============================================================================

performance_table <- bind_rows(
  # --- Run 1 (Full) ---
  tibble(Run = "Run 1 (Full)", Model = "SVM",           `ROC-AUC` = getTrainPerf(fit_svm)$TrainROC),
  tibble(Run = "Run 1 (Full)", Model = "Random Forest", `ROC-AUC` = getTrainPerf(fit_rf)$TrainROC),
  tibble(Run = "Run 1 (Full)", Model = "Lasso",         `ROC-AUC` = getTrainPerf(fit_glmnet)$TrainROC),
  
  # --- Run 2 (Reduced) ---
  tibble(Run = "Run 2 (Reduced)", Model = "SVM",           `ROC-AUC` = getTrainPerf(fit_svm_v2)$TrainROC),
  tibble(Run = "Run 2 (Reduced)", Model = "Lasso",         `ROC-AUC` = getTrainPerf(fit_glmnet_v2)$TrainROC),
  tibble(Run = "Run 2 (Reduced)", Model = "Random Forest", `ROC-AUC` = getTrainPerf(fit_rf_v2)$TrainROC),
  
  # --- Run 3 (Toll only) ---
  tibble(Run = "Run 3 (Toll only)", Model = "SVM",           `ROC-AUC` = getTrainPerf(fit_svm_toll)$TrainROC),
  tibble(Run = "Run 3 (Toll only)", Model = "Random Forest", `ROC-AUC` = getTrainPerf(fit_rf_toll)$TrainROC),
  tibble(Run = "Run 3 (Toll only)", Model = "Lasso",         `ROC-AUC` = getTrainPerf(fit_glmnet_toll)$TrainROC)
) %>%
  mutate(`ROC-AUC` = round(`ROC-AUC`, 3)) # Runden auf 3 Nachkommastellen wie im Bild

print(performance_table)

# save as CSV
write.csv(performance_table, "performance_metrics.csv", row.names = FALSE)
