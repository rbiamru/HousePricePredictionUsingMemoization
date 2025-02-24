# HousePricePredictionUsingMemoization
Trying subset selection in R using Memoization instead of the regular Forward Pass(Greedy Algorithm) for choosing accurate subset


Inference 1: # Using Forward pass Subset selection(Greedy Algorithm)

Inference 2: # Using Memoization
Both inferences show improvements in RMSE by reducing unnecessary predictors, which in turn improves generalization:

Inference 1 – Model Simplification:

By removing redundant or non‐informative predictors (for example, dropping predictors like I(area^3) and using a binary indicator for furnishing status), the final model (UpdatedModel2) shows a cross-validated RMSE of about 0.213 (on the log scale).
This simplification reduces overfitting, leading to more stable predictions on new data.
Inference 2 – Dynamic Programming (DP) with Memoization:

The DP-based forward subset selection method systematically evaluates all combinations of predictors to find the subset that minimizes the RMSE on a held-out test set.
Memoization is used to cache RMSE values for feature subsets that have already been computed. This not only speeds up the search by avoiding redundant calculations but also ensures that the algorithm reliably identifies the best-performing subset.
In this example, the DP approach returned a best subset (comprising “bathrooms”, “stories”, “parking”, “airconditioning”, “mainroad”, “prefarea”, and “unfurnished”) with an RMSE of approximately 1,236,242 on the original price scale. Although this RMSE is reported in the original scale (after exponentiation), it reflects a more efficient model that better generalizes to unseen data.
Summary of RMSE Improvement:

Model Simplification (Inference 1):
Reducing the number of predictors while keeping those with significant contributions decreased overfitting and improved RMSE as seen by cross-validation.

DP-based Subset Selection with Memoization (Inference 2):
A forward pass approach with memoization explores the feature space efficiently and identifies the subset that minimizes the RMSE. This systematic reduction in model complexity further improves predictive accuracy by selecting only the most relevant predictors.

In both cases, a more parsimonious model leads to better prediction performance, as evidenced by lower RMSE, while memoization in the DP method ensures an optimal feature set is identified efficiently.
