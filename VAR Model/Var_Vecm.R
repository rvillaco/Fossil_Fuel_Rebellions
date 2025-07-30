# Este código explora la relación causal entre las protestas y el subsidio de diferentes tipos de gasolina

#==============================================================================#
####                   Installing and loading packages                      ####
#==============================================================================#

rm(list = ls(all = TRUE))

library(tseries)
library(urca)
library(forecast)
library(ggplot2)
library(corrplot)
library(lmtest)
library(readr)
library(ggcorrplot)
library(reshape2)
library(tseries)  
library(urca)     
library(xts)      
library(vars)
library(tidyr)
library(dplyr)
library(xlsx)

#==============================================================================#
####                             Database loading                           ####
#==============================================================================#

# Load fuel price data
combustible_gasolinera <- read_csv("Datos/Gasoline pump prices/Precios_Final.csv", col_types = cols(Fecha = col_date(format = "%d-%b-%y")))
combustible_gasolinera <- select(combustible_gasolinera, Fecha, matches("Gasolinera"))

combustible_internacional <- read_csv("Datos/Gasoline pump prices/Precios_USA_diario_act.csv", col_types = cols(date = col_date())) %>% 
  select(-semana, -'...1') %>% 
  rename(gas_diesel = Diesel, gas_midgrade = Gasoline_midgrade, gas_premium = Gasoline_premium)

combustible_gasolinera <- inner_join(combustible_gasolinera, combustible_internacional, by = c("Fecha" = "date"))

# Calculate subsidies
combustible_gasolinera <- combustible_gasolinera %>% 
  mutate(
    subsidio_diesel = gas_diesel - Diesel_Gasolinera,
    subsidio_extra = gas_midgrade - Eco_Extra_Gasolinera,
    subsidio_super = gas_premium - Super_Gasolinera
  )

# Load protest data
protests_data <- read_csv("Datos/Protest/personas_imputada_final.csv", col_types = cols(date = col_date()))

# Combine gasoline data and protests
compiledDF <- inner_join(protests_data %>% select(date, num_eventos, final_imputation),
                         combustible_gasolinera %>% select(Fecha, matches("subsidio|Gasolinera")), 
                         by = c("date" = "Fecha")) %>%
  select(-num_eventos)

# Define imputed sections
faltantes1_inicio <- as.Date("2018-01-13")
faltantes1_fin    <- as.Date("2019-10-01")
faltantes2_inicio <- as.Date("2019-10-14")
faltantes2_fin    <- as.Date("2021-02-03")

# Classify the imputed sections
protests_data <- protests_data %>%
  mutate(tramo = case_when(
    date >= faltantes1_inicio & date <= faltantes1_fin ~ "Imputation 1",
    date >= faltantes2_inicio & date <= faltantes2_fin ~ "Imputation 2",
    TRUE ~ "Original"
  ))

# Calculate the midpoints of the imputed sections to add labels
faltantes1_medio <- faltantes1_inicio + (faltantes1_fin - faltantes1_inicio) / 2
faltantes2_medio <- faltantes2_inicio + (faltantes2_fin - faltantes2_inicio) / 2
y_max <- max(protests_data$personas, na.rm = TRUE) * 0.5

ggplot(protests_data, aes(x = date, y = final_imputation, color = tramo, group = 1)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Original" = "deepskyblue3", 
                                "Imputation 1" = "firebrick", 
                                "Imputation 2" = "firebrick")) +
  annotate("rect", xmin = faltantes1_inicio, xmax = faltantes1_fin, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  annotate("rect", xmin = faltantes2_inicio, xmax = faltantes2_fin, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  annotate("text", x = faltantes1_medio, y = y_max, label = "Imputation 1", color = "black", size = 5, fontface = "bold") +
  annotate("text", x = faltantes2_medio, y = y_max, label = "Imputation 2", color = "black", size = 5, fontface = "bold") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Definir el nombre del archivo y las dimensiones
ggsave("Datos/Imagen/grafico_protestas_imputada.png", width = 10, height = 4, dpi = 300)

#==============================================================================#
####                           Stationarity tests                           ####
#==============================================================================#

datos_xts <- xts(compiledDF[, 2:5], order.by = compiledDF$date)  # Eliminar columna de fecha

# Función para calcular AIC y BIC manualmente
calcular_AIC_BIC <- function(modelo, n) {
  rss <- sum(residuals(modelo)^2)  # Suma de los residuos al cuadrado
  k <- length(coef(modelo))  # Número de parámetros estimados
  logL <- - (n / 2) * log(rss / n)  # Log-verosimilitud aproximada
  
  AIC <- -2 * logL + 2 * k
  BIC <- -2 * logL + k * log(n)
  
  return(list(AIC = AIC, BIC = BIC))
}

# Función para realizar pruebas de estacionariedad y guardar resultados de cada lag
pruebas_estacionariedad_trend <- function(datos_xts, variables) {
  resultados <- data.frame(Variable = character(),
                           Lag = integer(),
                           ADF_pvalor = numeric(),
                           ADF_Estacionaria = character(),
                           ADF_AIC = numeric(),
                           ADF_BIC = numeric(),
                           KPSS_pvalor = numeric(),
                           KPSS_Estacionaria = character(),
                           stringsAsFactors = FALSE)
  
  for (var in variables) {
    if (!(var %in% colnames(datos_xts))) {  
      warning(paste("Variable no encontrada en datos_xts:", var))
      next  # Salta si la variable no está en datos_xts
    }
    
    serie <- datos_xts[, var] 
    n <- length(serie)  # Sample size
    
    for (l in 1:15) {  # Iterate over lags from 1 to 15
      # Test ADF with trend
      adf_test <- ur.df(serie, type = "trend", lags = l)
      adf_pvalor <- adf_test@testreg$coefficients["z.lag.1", "Pr(>|t|)"]
      adf_estacionaria <- ifelse(adf_pvalor < 0.05, "Si", "No")
      adf_aic_bic <- calcular_AIC_BIC(adf_test@testreg, n)  
      
      # KPSS test with trend
      kpss_test <- ur.kpss(serie, type = "tau", use.lag = l)
      kpss_pvalor <- kpss_test@teststat  
      kpss_critico <- kpss_test@cval[1,"5pct"] 
      kpss_estacionaria <- ifelse(kpss_pvalor < kpss_critico, "Si", "No")
      
      # Saving results
      resultados <- rbind(resultados, data.frame(Variable = var,
                                                 Lag = l,
                                                 ADF_pvalor = adf_pvalor,
                                                 ADF_Estacionaria = adf_estacionaria,
                                                 ADF_AIC = adf_aic_bic$AIC,
                                                 ADF_BIC = adf_aic_bic$BIC,
                                                 KPSS_pvalor = kpss_pvalor,
                                                 KPSS_Estacionaria = kpss_estacionaria
                                                 ))
    }
  }
  
  return(resultados)
}

pruebas_estacionariedad_drift <- function(datos_xts, variables) {
  resultados <- data.frame(Variable = character(),
                           Lag = integer(),
                           ADF_pvalor = numeric(),
                           ADF_Estacionaria = character(),
                           ADF_AIC = numeric(),
                           ADF_BIC = numeric(),
                           KPSS_pvalor = numeric(),
                           KPSS_Estacionaria = character(),
                           stringsAsFactors = FALSE)
  
  for (var in variables) {
    if (!(var %in% colnames(datos_xts))) {  
      warning(paste("Variable no encontrada en datos_xts:", var))
      next  
    }
    
    serie <- datos_xts[, var] 
    n <- length(serie)  
    
    for (l in 1:15) {  # Iterate over lags from 1 to 15 
      # Test ADF with drift
      adf_test <- ur.df(serie, type = "drift", lags = l)
      adf_pvalor <- adf_test@testreg$coefficients["z.lag.1", "Pr(>|t|)"]
      adf_estacionaria <- ifelse(adf_pvalor < 0.05, "Si", "No")
      adf_aic_bic <- calcular_AIC_BIC(adf_test@testreg, n)    
      
      # KPSS test with drift
      kpss_test <- ur.kpss(serie, type = "mu", use.lag = l)
      kpss_pvalor <- kpss_test@teststat  
      kpss_critico <- kpss_test@cval[1,"5pct"] 
      kpss_estacionaria <- ifelse(kpss_pvalor < kpss_critico, "Si", "No")
      
      # Saving results
      resultados <- rbind(resultados, data.frame(Variable = var,
                                                 Lag = l,
                                                 ADF_pvalor = adf_pvalor,
                                                 ADF_Estacionaria = adf_estacionaria,
                                                 ADF_AIC = adf_aic_bic$AIC,
                                                 ADF_BIC = adf_aic_bic$BIC,
                                                 KPSS_pvalor = kpss_pvalor,
                                                 KPSS_Estacionaria = kpss_estacionaria
      ))
    }
  }
  
  return(resultados)
}


# Apply stationarity tests
variables <- c("final_imputation", "Diesel_Gasolinera", "Eco_Extra_Gasolinera", "Super_Gasolinera")

print(colnames(datos_xts))
print(variables)

# Run the tests
resultados_estacionariedad_trend <- pruebas_estacionariedad_trend(datos_xts, variables)
resultados_estacionariedad_drift <- pruebas_estacionariedad_drift(datos_xts, variables)

# Show results
print(resultados_estacionariedad_trend)
print(resultados_estacionariedad_drift)

#write.xlsx(resultados_estacionariedad_drift, file = "C:/Luis/Paper/Combustible/Redacción/Imagenes/pruebas_estacionarias_drift.xlsx")

# DIFFERENTIATE THE SERIES

prices_diff <- data.frame(diff(as.matrix(compiledDF[,3:5]), differences = 1))
prices_diff <- na.omit(prices_diff)

names_ur <- names(prices_diff)

# Run the tests
resultados_estacionariedad_diff_drift <- pruebas_estacionariedad_drift(prices_diff, names_ur)

# Show results
print(resultados_estacionariedad_diff_drift)

df_var <- data.frame(compiledDF[-1,2], prices_diff)
names(df_var)

#==============================================================================#
####          Selecting the optimal VAR order - Information criteria        ####
#==============================================================================#

var_order <- VARselect(df_var, lag.max=15, type="const")
print(var_order)

criterios <- data.frame(t(var_order$criteria))
criterios[,1:3] <- round(criterios[,1:3],3)
criterios[,4] <- round(criterios[,4],6)
criterios$Lag <- seq(1:15)

criterios <- criterios[,c(5,1:4)]

names(criterios)[2] <- "AIC"
names(criterios)[3] <- "HQ"
names(criterios)[4] <- "BIC"
names(criterios)[5] <- "FPE"

criterios <- criterios[,c(1,2,4,3,5)]

write.xlsx(df_var, file = "Datos/VAR data/df_var.xlsx")
write.xlsx(criterios, file = "Table/criterias_information.xlsx")

library(flextable)
criterios %>% head(15) %>% regulartable()

df_var
names(df_var)[1] <- "Protesters"
names(df_var)[2] <- "Diesel Pump Price"
names(df_var)[3] <- "Regular Pump Price"
names(df_var)[4] <- "Premium Pump Price"

# Fitting the VAR model with the optimal ordering based on AIC
opt_lag <- which.min(var_order$criteria[1,])
var_model <- VAR(df_var, p=opt_lag, type="const")

#==============================================================================#
####                            Pearson Correlation                         ####
#==============================================================================#

# Join the datasets ensuring the date column is well defined
to_graph <- inner_join(
  protests_data %>% select(date, num_eventos, final_imputation), 
  combustible_gasolinera %>% select(Fecha, matches("Gasolinera")), 
  by = c("date" = "Fecha")  
) %>%
  select(-num_eventos)

to_graph <- to_graph %>%
  rename(Protesters = final_imputation,
         `Regular Pump Price` = Eco_Extra_Gasolinera,
         `Diesel Pump Price` = Diesel_Gasolinera,
         `Premium Pump Price` = Super_Gasolinera)

to_graph <- to_graph[,-1]

# Calculate the Pearson correlation matrix
pearsoncorr <- cor(to_graph, method = "pearson")

png(height=1800, width=1800, file="Imagen/Appendix/FigA3_Pearson_correlations_variables.png", res = 300, type = "cairo")
# Display the correlation matrix
pearson <- corrplot(pearsoncorr, method = "color", 
                    type = "upper", 
                    addCoef.col = "black", 
                    order = "AOE", number.cex=0.95,
                    tl.col = "black", 
                    tl.srt = 90,
                    # Combine with significance
                    #p.mat = p.mat, sig.level = 0.01, insig = "blank",
                    # hide correlation coefficient on the principal diagonal
                    diag=FALSE)

dev.off()

# Evaluate contemporary correlation between residues
residuals_var <- residuals(var_model)
cor_residuals <- cor(residuals_var)
# Rename variables in the correlation matrix
colnames(cor_residuals) <- c("Protesters", "Diesel Pump Price", "Regular Pump Price", "Premium Pum Price")
rownames(cor_residuals) <- c("Protesters", "Diesel Pump Price", "Regular Pump Price", "Premium Pum Price")

print(cor_residuals)  

png(height=1800, width=1800, file="Imagen/Appendix/FigA4_Pearson_cor_residual_VAR.png", type = "cairo")

pearson_res <- corrplot(cor_residuals, method = "color", 
                    type = "upper", 
                    addCoef.col = "black", # Add coefficient of correlation
                    order = "AOE", number.cex=0.95,
                    tl.col = "black", 
                    tl.srt = 90,
                    # Combine with significance
                    #p.mat = p.mat, sig.level = 0.01, insig = "blank",
                    # hide correlation coefficient on the principal diagonal
                    diag=FALSE)

dev.off()

# The best suggested order, according to statistical and theoretical evidence, 
# of the variables for the Cholesky decomposition is: Diesel, Regular, Premium 
# and Protesters.

var_model_ordered <- VAR(df_var[,c(2:4,1)], p=opt_lag, type="const")

#==============================================================================#
####                                Assumptions                             ####
#==============================================================================#

# Test for serial autocorrelation in the residuals

for (i in 1:12) {
  serial_corr <- serial.test(var_model_ordered, lags.bg = i, type = "BG")
  print(serial_corr)
}

# H0: The residuals are not correlated p>0.05
# H1: The residuals are correlated p<0.05
# Since the estimated p statistic is p-value < 2.2e-16, then I reject H0,
# therefore the residuals are correlated.

# Normality test of residuals

norm <- normality.test(var_model_ordered)
norm$jb.mul

# H0: The residuals are normally distributed p>0.05
# H1: The residuals are not normally distributed p<0.05
# Since the estimated statistical p is p-value < 2.2e-16, I reject H0,
# therefore the residuals are not normally distributed.


# Homoscedasticity test of the residuals

het_arch <- arch.test(var_model_ordered, lags.multi = opt_lag)
het_arch$arch.mul

# H0: The variance of the residuals is constant if p>0.05
# H1: The variance of the residuals is not constant if p<0.05
# Since the estimated statistical p is p-value = 0.917, then I do not reject H0,
# therefore the variance of the residuals is constant.

robust_errors <- vcovHC(var_model, type = "HC0")

# Structural changes in waste

Stability <- stability(var_model_ordered, type = "OLS-CUSUM")

png("Imagen/Fig5_Structural_changes_VAR.png", width = 4500, height = 3500, res = 300)
plot(Stability)  
dev.off()  


#==============================================================================#
####                            Granger Causality                           ####
#==============================================================================#

# Granger Causality Tests
variables <- c("Protesters", 
               "Diesel.Pump.Price", "Regular.Pump.Price", "Premium.Pump.Price")

variables <- colnames(var_model$y)
target_variable <- "Protesters"  

for (var in variables) {
  if (var != target_variable) {
    granger_test <- causality(var_model, cause=var)
    print(paste("Causalidad de", var, "hacia", target_variable, ":"))
    print(granger_test$Granger)
  }
}

#==============================================================================#
####                              Cointegration                             ####
#==============================================================================#

df_vecm <- compiledDF[,2:5]

johansen_test <- ca.jo(df_vecm[, c("final_imputation", "Diesel_Gasolinera", 
                                  "Eco_Extra_Gasolinera", "Super_Gasolinera")], 
                       type = "trace", K = opt_lag, ecdet = "const")
summary(johansen_test)

# r=0 → 251.62 > 53.12 (We reject Ho: there is at least one cointegrating relationship).
# r<=1 → 72.49 > 34.91 (We reject Ho: there are at least two cointegrating relationships).

# Conclusion: There are two cointegrating relationships in the variables, which indicates
# that there is a long-run relationship between the series. Therefore, a VAR model
# is not the best option; instead, you should use a VECM (Vector Error Correction Model)
# to model both the short- and long-run relationships.

#==============================================================================#
####                      Vector Error Correction (VECM)                    ####
#==============================================================================#

library(tsDyn)

# We specify the VECM with 2 cointegration relations (r = 2) and the optimal lags
# (use the optimal value from the lag selection)
vecm_model <- VECM(df_vecm, 
                   lag = opt_lag-1, r = 2, include = "const", estim = "ML")

summary(vecm_model)

library(vars)


# Convert the VECM model to VAR
vecm_to_var <- vec2var(johansen_test)

# Diagnostic tests
serial.test(vecm_to_var, lags.pt = opt_lag, type = "PT.asymptotic")
normality.test(vecm_to_var)
arch.test(vecm_to_var)

AIC_var <- AIC(var_model)
BIC_var <- BIC(var_model)

print(AIC_var)
print(BIC_var)

AIC_vecm <- AIC(vecm_to_var)
BIC_vecm <- BIC(vecm_to_var)

print(AIC_vecm)
print(BIC_vecm)

comparison <- data.frame(
  Model = c("VAR", "VECM_to_VAR"),
  AIC = c(AIC_var, AIC_vecm),
  BIC = c(BIC_var, BIC_vecm)
)

print(comparison)
