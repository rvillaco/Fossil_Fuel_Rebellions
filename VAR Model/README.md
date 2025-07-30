README – VAR/VECM Estimation and Cholesky Ordering Analysis

This part of the project continues the analysis after imputing the protest series. It includes the construction and estimation of a VAR/VECM model to evaluate the effect of fuel prices on protest activity in Ecuador, followed by an in-depth analysis of variable ordering for the orthogonalization of impulse response functions (IRFs).

________________________________________


3. Var_Vecm.R
Goal: Estimate a VAR or VECM model using the imputed protest series and fuel price variables (Diesel, Regular, Premium) and assess model assumptions.

________________________________________

Input Datasets:

• Precios_Final.csv: National-level daily fuel prices (Diesel, Regular, Premium).
• Precios_USA_diario_act.csv: International oil prices (WTI).
• personas_imputada_final.csv: Final version of the imputed protest series, averaged from ARIMAX and LSTM results.
These datasets are merged into a single time series dataset named df_var.

________________________________________

Key Procedures in this Script:

1. Stationarity Testing:
 * Conducted using Augmented Dickey-Fuller, Phillips-Perron, and KPSS tests.
 * The series Premium is found to be non-stationary and is differenced accordingly.

2. Optimal Lag Selection:
 * The optimal number of lags is determined using information criteria: AIC, BIC, and HQC.

3. Model Estimation:
 * A VAR model is estimated using the differenced or stationary series.
 * VECM is also estimated for comparison purposes.

4. Model Diagnostics:
 * Normality of residuals (Jarque-Bera tests).
 * Homoscedasticity (ARCH test).
 * Autocorrelation (Portmanteau test).
 * Stability (roots inside unit circle and CUSUM test).

5. Model Selection:
 * Information criteria (AIC, BIC) are used to compare the fit of the VAR and VECM models.
 * The best fitting specification is retained for impulse response analysis.

________________________________________

4. Var_ordered_exog.R
Goal: Determine the most appropriate ordering of fuel price variables for the Cholesky decomposition used in orthogonalized IRFs.

________________________________________
Input Dataset:
• df_var: The time series dataframe constructed in Var_Vecm.R, containing:
  o Diesel_Gasolinera
  o Eco_Extra_Gasolinera (Regular)
  o Super_Gasolinera (Premium)
  o final_imputation (Protests)

________________________________________

Key Analytical Steps:
1. Estimate Baseline VAR (Unrestricted):
 * A baseline VAR is estimated without any orthogonalization.

2. Pearson Correlation of Residuals:
 * Used to explore contemporaneous correlations among the residuals of the VAR model.
 * Helps suggest plausible variable ordering for the Cholesky decomposition.

3. Granger Causality Tests:
 * Evaluates causal precedence among fuel price variables and protests.
 * Supports empirical justification for ordering choices.

4.	Sensitivity Analysis of IRFs:
 * All possible 24 permutations (4 variables) of ordering are tested.
 * For each ordering, orthogonalized IRFs are generated.

5. Area Under the Curve (AUC):
 * For each IRF generated in each permutation, the AUC is calculated to quantify the strength and persistence of responses.
 * AUC values are used to compare different orderings.


6. Robustness Check with Exogenous Instrument:
 * A VAR is re-estimated including WTI surprises as an exogenous instrument for fuel price shocks.
 * Confirms whether the baseline IRFs are robust to an alternative identification strategy.

7. Final Ordering Selection:
 * Based on correlation, causality, IRF sensitivity, AUC, and robustness, the following ordering was selected: Diesel → Regular → Premium → Protesters

8. Final IRFs:
 * Orthogonalized impulse response functions (IRFs) are computed and rescaled by the standard deviation of the response variable for interpretability.

________________________________________

Summary
* Var_Vecm.R: Estimate VAR/VECM model and check assumptions. Validated model with stationarity, lag order, and diagnostics.
* Var_orderes_exog.R: Analyze and justify variable ordering for Cholesky decomposition. Final ordering Diesel → Regular → Premium → Protesters and orthogonalized IRFs.

