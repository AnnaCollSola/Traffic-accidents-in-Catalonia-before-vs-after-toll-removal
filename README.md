# Traffic-accidents-in-Catalonia-before-vs-after-toll-removal

Goal:

Assess whether severe/fatal traffic accidents in Catalonia changed after tolls were removed on 2021-09-01 for major corridors (AP-7, AP-2 and parts of C-32, C-33), and build a classifier to predict before vs after and explain the drivers.


Scope and interpretation:

The analysis does not aim to establish causal effects of the toll removal. Instead, it examines whether accident characteristics allow a statistical separation between accidents occurring before and after 1 September 2021. All results should therefore be interpreted as evidence of distributional differences coinciding with the policy change, not as proof of causality.


Data:

Official accident-level dataset (Catalonia), processed into a modelling-ready table. We engineer: (i) toll_period (before/after 2021-09-01), (ii) toll_corridor (toll-removed corridors vs other roads), (iii) road_family (AP/A/N/C/other/missing).


Methodology. The pipeline includes:

• Translating: Translating and harmonizing labels to English.

• Cleaning: Removing variables with > 20% missingness and categorical variables with > 10 levels. Imputation via Median (numeric) and Mode (categorical).

• Feature Engineering: Variable Clustering (Hierarchical + K-Means) to group collinear binary variables into 35 representative “consensus” features.

• Modeling: Training Lasso, SVM, and Random Forest models using 5-fold Cross-Validation with downsampling to handle class imbalance. Modelling strategy (3 runs).

• Run 1 (Full): Uses the full clustered dataset.

• Run 2 (Reduced): Uses a Reduced Feature Set, removing the bottom 30 least important variables identified in Run 1 to test efficiency.

• Run 3 (Toll corridors only): Focuses on AP-7, AP-2, and parts of C-32, C-33, applying a specific Zero-Variance filter.


Headline result:

Model separation is strongest on toll corridors (Run 3), with ROC-AUC up to 0.699 (Lasso), compared with about 0.646–0.653 in Run 1 and 0.624–0.64 in Run 2.


Interpretation:

Run 2 driver plots (Lasso) reveal that specific environmental contexts, most notably fog presence (Cluster 8) and terrain characteristics, strongly differentiate pre/post periods. In contrast, Run 3 feature importance on toll corridors is dominated by structural variables like road type (Motorway) and zone type. A critical distribution shift is observed: while fog presence is the top predictor in Run 2, it is explicitly removed by the zero-variance filter in the toll-only subset (Run 3). This implies that while weather conditions on the toll corridor remained stable (triggering the zero-variance filter), the primary change was structural: the Data Quality Check reveals a massive shift in accident composition from ‘Motorway’ to ‘Conventional road’ in the post-removal period. This suggests that the model’s separation ability is driven by a redistribution of accidents towards conventional roads within the corridor, rather than by meteorological variability.


Run in the following order:

translation.R

exp_analysis

Models
