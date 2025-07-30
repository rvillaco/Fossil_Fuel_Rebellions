This project documents the imputation of missing values in the time series of protests in Ecuador and the subsequent estimation of a VAR model to analyze the effects of fuel prices on protest dynamics.

Project Structure
The full process is organized into four main scripts. The first two are described below and correspond to the ARIMAX-based imputation of the protest series.

0. 0-Correlation_Media-Indices_Protests.ipynb
Goal: Evaluates the correlation between the series of protests and international oil prices and fuel subsidies. It also compares the relationship between protests and other indicators of social pressure. These resuts are presented in the online appendix to the main paper. 
Datasets used:
• protestas_original.csv
• RSU_index.csv
• acled_final_new.csv: 


1. arimax_2019_2020.R
Goal: Impute missing values for the second segment of the protest time series (2019-10-14 to 2021-02-03) using an ARIMAX model.
Datasets used:
• protestas_original.csv
• RSU_index.csv
• acled_final_new.csv: 
Output: The best ARIMAX model for imputing the second segment of the protest series, using GDELT and ACLED indices as exogenous variables. This dataset includes indicators developed from the Armed Conflict Location & Event Data Project (ACLED), specifically from its records of protest and political violence events. The construction of these indicators follows the methodological approach detailed by Barrett et al. (2022), allowing for consistent and structured quantification of contentious events over time, such as demonstrations, riots, and violent confrontations.

2. LTSM-Sequence2point.ipynb
Goal: Impute missing values for the second segment of the protest time series (2019-10-14 to 2021-02-03) using an LSTM model. Uses Pytorch to define the model arquitecture and define data structures.
Support Files:
• lstm_classes.py: Defines the LSTM arquitecture, and Datasets used to train, validate and predict with the model.
• support_functions.py: Functions used plot training results and forecast using trained model
Datasets used:
• protestas_original.csv
• RSU_index.csv
• acled_final_new.csv
• WTI_final_act.csv
Output: The best LSTM model for the first segment, using GDELT and ACLED indices and WTI prices as explanatory variables. The script determines the best model weights, using early stopping and different hyperparameter configurations, based on its performance on the validation set. The best model weights are saved to the file: LSTM-Results\lstm_model_checkpoint_57-23.pth.


3. arimax_2018_2019.R
Goal: Impute the first segment of the protest series (2018-01-01 to 2019-10-01) using a new ARIMAX model. This model uses a version of the protest series that incorporates imputation results from the second segment (averaged between ARIMAX and LSTM).
Datasets used:
• protestas_imputadas_personas.csv: Protest series constructed by averaging ARIMAX and LSTM imputed values for the second segment and combining them with the original protest series.
• RSU_index.csv and acled_final_new.csv: Used again as exogenous regressors.
Output: The best ARIMAX model for the first segment, using GDELT and ACLED indices as explanatory variables.

Additional Notes on Input Data
• RSU_index.csv: Contains risk and instability indicators derived from the GDELT database.
• acled_final_new.csv: Contains indicators based on ACLED data (event frequency, intensity, and type).
These variables were included as external regressors in the ARIMAX models for both segments.

Imputation Workflow
1. The original protest series was split into two segments:
   • Segment 1: 2018-01-01 to 2019-10-01
   • Segment 2: 2019-10-14 to 2021-02-03
2. The second segment was imputed using ARIMAX (arimax_2019_2020.R), incorporating GDELT and ACLED indices as predictors.
3. In parallel, the same segment was also imputed using an LSTM model (LTSM-Sequence2point.ipynb),  incorporating GDELT and ACLED indices, and WTI prices as predictors.
4. The ARIMAX and LSTM imputations were averaged to construct the file protestas_imputadas_personas.csv.
5. This new series was then used to impute the first segment using ARIMAX (arimax_2018_2019.R).
