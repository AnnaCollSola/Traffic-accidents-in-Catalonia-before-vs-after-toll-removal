#def version exploratory clean
#=========================================================
#---- 0) Libraries & data loading ------------------------
#=========================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, readr, stringr, tidyr, purrr...
  library(lubridate)   # date handling
  library(stringr)     # extra string tools (if needed)
  library(scales)      # pretty scales (percent, etc.)
})

accidents <- read_csv("data/project_data.csv",
                      show_col_types = FALSE)

#=========================================================
#---- 1) Basic feature engineering on main data ----------
#      - date
#      - clean 'via'
#      - toll_period
#      - toll_corridor
#      - road_family
#=========================================================

accidents <- accidents |>
  mutate(
    # date to Date
    dat = dmy(dat),
    
    # 'SE' in via -> NA (sense especificar)
    via = na_if(via, "SE"),
    
    # speed limit 999 -> NA (code for unknown / invalid)
    C_VELOCITAT_VIA = if_else(C_VELOCITAT_VIA > 121,
                              NA_real_, C_VELOCITAT_VIA),
    
    # before/after toll removal
    toll_period = if_else(dat >= as.Date("2021-09-01"), "after", "before"),
    toll_period = factor(toll_period, levels = c("before", "after")),
    
    # roads where tolls were removed (AP-7, AP-2, C-32, C-33)
    toll_corridor = if_else(
      via %in% c("AP-7", "AP-2", "C-32", "C-33"),
      "toll_removed",
      "other"
    ),
    toll_corridor = factor(toll_corridor,
                           levels = c("other", "toll_removed")),
    
    # simple road family (AP / A / N / C / other / missing)
    road_family = case_when(
      str_starts(via, "AP-") ~ "AP",
      str_starts(via, "A-")  ~ "A",
      str_starts(via, "N-")  ~ "N",
      str_starts(via, "C-")  ~ "C",
      is.na(via)             ~ "missing",
      TRUE                   ~ "other"
    ),
    road_family = factor(road_family)
  )

#=========================================================
#---- 2) Create working dataset for EDA / modelling ------
#      - drop pure labels / not needed columns
#      - drop 'via' (773 levels) from model data
#=========================================================

accidents_work <- accidents |>
  select(
    -Any,
    -nomMun,
    -nomCom,
    -nomDem,
    -pk,
    -hor,
    -via
  )

#=========================================================
#---- 3) Convert text "Sense especificar" etc. to NA -----
#=========================================================

accidents_work <- accidents_work |>
  mutate(
    across(
      where(is.character),
      ~ .x |>
        na_if("Sense especificar") |>
        na_if("Sense Especificar") |>
        na_if("NA") |>
        na_if("")
    )
  )

#=========================================================
#---- 4) Convert character variables to factors ----------
#=========================================================

accidents_work <- accidents_work |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )
#=========================================================
#---- 5) Factor level summaries (console output) ---------
#      For each factor variable, print the count of
#      observations per level (sorted by frequency).
#=========================================================

factor_vars <- accidents_work |>
  select(where(is.factor)) |>
  names()

for (v in factor_vars) {
  cat("\n-----------", v, "-----------\n")
  print(
    accidents_work |>
      count(.data[[v]], sort = TRUE)
  )
}

#=========================================================
#---- 6) PLOTS -------------------------------------------
#=========================================================
#---------------------------------------------------------
# 6.1 Normalized cumulative accidents by road type
#     - Each line shows the cumulative proportion of
#       accidents over calendar time for:
#         * toll_removed corridors (AP-7, AP-2, C-32, C-33)
#         * other roads
#     - The dashed line marks the toll removal date.
#     - Steeper segments = periods with more accidents.
#---------------------------------------------------------

accidents_work |>
  filter(!is.na(toll_corridor)) |>
  arrange(dat) |>
  group_by(toll_corridor) |>
  mutate(
    acc_id   = row_number(),                 # cumulative accident index
    acc_prop = acc_id / max(acc_id)          # normalized (0–1) per group
  ) |>
  ungroup() |>
  ggplot(aes(x = dat, y = acc_prop, color = toll_corridor)) +
  geom_line(linewidth = 0.7) +
  geom_vline(
    xintercept = as.Date("2021-09-01"),
    linetype   = "dashed"
  ) +
  annotate(
    "text",
    x = as.Date("2021-09-01"),
    y = 0.88,
    label = "TOLL REMOVED",
    angle = 90,
    vjust = -0.5,
    hjust = 0
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Normalized cumulative accidents by road type",
    x = "Date",
    y = "Cumulative proportion of accidents",
    color = "Road type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#---------------------------------------------------------
# 6.2 Cumulative within period (toll_removed only)
#     - We restrict to toll_removed corridors.
#     - Time is reset at the start of each period
#       (before vs after toll removal).
#     - Both curves go from 0 to 1, so their shapes
#       are directly comparable:
#         * A steeper "after" line means accidents
#           accumulate faster after toll removal.
#---------------------------------------------------------

accidents_work |>
  filter(toll_corridor == "toll_removed") |>
  group_by(toll_period) |>
  arrange(dat, .by_group = TRUE) |>
  mutate(
    days_since_start = as.numeric(dat - first(dat)),
    acc_id          = row_number(),
    acc_prop        = acc_id / max(acc_id)  # 0–1 per period
  ) |>
  ungroup() |>
  ggplot(aes(x = days_since_start, y = acc_prop, color = toll_period)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Normalized cumulative accidents on toll-removed corridors",
    subtitle = "Time reset at start of each period (before vs after toll removal)",
    x = "Days since period start",
    y = "Cumulative proportion of accidents",
    color = "Period"
  ) +
  theme_bw()

#---------------------------------------------------------
# 6.3 Severity mix before/after by road type
#     - For each road type (other / toll_removed) we look
#       at the composition of:
#         * Accident greu
#         * Accident mortal
#       before and after toll removal.
#     - Bars are stacked to 100% within each road type and
#       period, so we compare *proportions*, not counts.
#---------------------------------------------------------

accidents_work |>
  count(toll_period, toll_corridor, D_GRAVETAT) |>
  group_by(toll_period, toll_corridor) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = toll_period, y = prop, fill = D_GRAVETAT)) +
  geom_col(position = "fill") +
  facet_wrap(~ toll_corridor) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Severity mix before/after by road type",
    x = "Period",
    y = "Proportion within road type",
    fill = "Severity"
  ) +
  theme_bw()


