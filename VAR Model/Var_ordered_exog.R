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
library(openxlsx)
library(gridExtra)
library(ggplot2)
library(flextable)
library(officer)

#==============================================================================#
####                             Database loading                           ####
#==============================================================================#

df_var <- read.xlsx("Datos/VAR data/df_var.xlsx")

variable.names(df_var)

names(df_var)[1] <- "Protesters"
names(df_var)[2] <- "Diesel"
names(df_var)[3] <- "Regular"
names(df_var)[4] <- "Premium"

variable.names(df_var)

# Fitting the VAR model with the optimal order based on AIC
# Selecting the optimal VAR order
var_order <- VARselect(df_var, lag.max=15, type="const")
print(var_order)

opt_lag <- which.min(var_order$criteria[1,])  # Selecting the lag based on AIC, BIC
var_model <- VAR(df_var, p=opt_lag, type="const")


#==============================================================================#
####                            Order Permutations                          ####
#==============================================================================#

# Variables in original order
var_names <- c("Protesters", "Diesel", "Regular", "Premium")

# All possible permutations
ordenes <- gtools::permutations(n = length(var_names), r = length(var_names), v = var_names)

#==============================================================================#
####                Diesel's Response to Protesters: All Orders             ####
####                        IRFs sensitivity analysis                       ####           
#==============================================================================#

library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

# Target and impulse variable for the IRF
impulse_var <- "Diesel"
response_var <- "Protesters"

# Save the IRFs for each order
irf_list <- list()

# Save the scaled IRFs for each order
irf_scale_list <- list()

for (i in 1:nrow(ordenes)) {
  orden_actual <- ordenes[i, ]
  
  # Reorder the dataframe
  df_ordenado <- df_var[, match(orden_actual, names(df_var))]
  
  # Estimating the VAR model
  modelo <- VAR(df_ordenado, p = opt_lag, type = "const")
  
  # Rescaling of the orthogonalized IRF based on the units of the impulse variables
  sigma2 <- summary(modelo)$covres
  
  # Obtain the standard deviation of the impulse variable to rescale
  sd <- sqrt(diag(sigma2)[impulse_var])
  
  # Only continue if impulse and response are present
  if (impulse_var %in% orden_actual && response_var %in% orden_actual) {
    irf_result <- irf(modelo,
                      impulse = impulse_var,
                      response = response_var,
                      n.ahead = 20,
                      ortho = TRUE,
                      boot = TRUE)
    
    irf_scale_result <- extract_varirf(irf_result)
    
    irf_scale_result[,2:4] <- irf_scale_result[,2:4] / sd
    
    irf_list[[paste(orden_actual, collapse = "_")]] <- irf_result
    
    irf_scale_list[[paste(orden_actual, collapse = "_")]] <- irf_scale_result
    
  }
}

# Comparison of IRFs of the target variable to the shock of a key variable (e.g., Diesel Pump Price)

plot_compare_irf <- function(irf_list, impulse_var, response_var) {
  irf_df_list <- list()
  
  for (name in names(irf_list)) {
    irf_obj <- irf_list[[name]]
    if (!is.null(irf_obj$irf[[impulse_var]])) {
      temp <- data.frame(
        horizon = 1:nrow(irf_obj$irf[[impulse_var]]),
        response = irf_obj$irf[[impulse_var]][, response_var],
        lower = irf_obj$Lower[[impulse_var]][, response_var],
        upper = irf_obj$Upper[[impulse_var]][, response_var],
        orden = name
      )
      irf_df_list[[name]] <- temp
    }
  }
  
  all_irf <- do.call(rbind, irf_df_list)
  
  ggplot(all_irf, aes(x = horizon, y = response, color = orden)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = orden), alpha = 0.2, color = NA) +
    labs(title = paste("IRF de", response_var, "ante un shock en", impulse_var),
         y = "Response", x = "Periods") +
    theme_minimal()
}

plot_compare_irf_scaled <- function(irf_scale_list, response_var) {
  
  all_irfs <- bind_rows(irf_scale_list, .id = "orden")
  
  colnames(all_irfs) <- c("orden", "horizon", "response", "lower", "upper")
  
  ggplot(all_irfs, aes(x = horizon, y = response, color = orden)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = orden), alpha = 0.07, color = NA) +
    labs(#title = paste("IRFs escaladas: impacto de Diesel.Pump.Price sobre", response_var),
         x = "Periods",
         y = "Rescaled response") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0),
          plot.title = element_text(face = "bold")) +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL)) +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
}

# Protesters' Response to Shock at Diesel Pump Price
plot_compare_irf(irf_list, impulse_var = impulse_var, response_var = response_var)

plot_compare_irf_scaled(irf_scale_list, response_var = "Protesters")

ggsave("Imagen/Appendix/FigA5_Sensity_IRFs_VAR_ordering.png", width = 15, height = 10, dpi = 300)

# Build summary table of sensitivity analysis

resumen_irf <- data.frame(
  orden = character(),
  max_impact = numeric(),
  duracion_significativa = numeric(),
  error_std_promedio = numeric(),
  auc = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(irf_list)) {
  irf_obj <- irf_list[[name]]
  
  if (!is.null(irf_obj$irf[[impulse_var]])) {
    respuesta <- irf_obj$irf[[impulse_var]][, response_var]
    lower <- irf_obj$Lower[[impulse_var]][, response_var]
    upper <- irf_obj$Upper[[impulse_var]][, response_var]
    
    # Significant duration: periods where the IC does not contain 0
    duracion_sig <- sum(lower * upper > 0)
    
    # Standard error of mean
    error_std <- mean((upper - lower) / 2, na.rm = TRUE)
    
    # Maximum absolute impact
    max_impacto <- max(abs(respuesta), na.rm = TRUE)
    
    # Area under the curve (accumulated absolute or positive responses)
    auc <- sum(respuesta, na.rm = TRUE) 
    
    resumen_irf <- rbind(resumen_irf, data.frame(
      orden = name,
      max_impact = max_impacto,
      duracion_significativa = duracion_sig,
      error_std_promedio = error_std,
      auc = auc
    ))
  }
}

# Sort by descending AUC
resumen_irf <- resumen_irf[order(-resumen_irf$auc), ]

print(resumen_irf)


# Asegura el orden de los factores según el AUC (para que se ordene correctamente en el gráfico)
resumen_irf$orden <- factor(resumen_irf$orden, levels = resumen_irf$orden[order(-resumen_irf$auc)])

resumen_irf <- resumen_irf[c(3,1,2,4:24),]

# Crear gráfico
p <- ggplot(resumen_irf, aes(x = orden, y = auc, fill = auc)) +
  geom_bar(stat = "identity") +
  labs(
    #title = "Figure A-6. Sensitivity of IRF Results to Cholesky Ordering",
    #subtitle = "Area Under the Curve (AUC) of the IRF from Diesel to Protesters",
    x = "Cholesky Ordering of Variables",
    y = "AUC (Cumulative Impact)"
  ) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_minimal(base_size = 16) +  # Tamaño base aumentado
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16)
  )

# Saving image in high resolution (300 dpi)
ggsave("Imagen/Appendix/FigA6_AUC.png", plot = p,
       width = 15, height = 10, dpi = 300, units = "in")

resumen_irf_scale <- data.frame(
  orden = character(),
  max_impact = numeric(),
  duracion_significativa = numeric(),
  error_std_promedio = numeric(),
  auc = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(irf_scale_list)) {
  irf_df <- irf_scale_list[[name]]
  
  respuesta <- irf_df[,2]
  lower <- irf_df[,3]
  upper <- irf_df[,4]
  
  duracion_sig <- sum(lower * upper > 0, na.rm = TRUE)
  
  error_std <- mean((upper - lower) / 2, na.rm = TRUE)
  
  max_impacto <- max(abs(respuesta), na.rm = TRUE)
  
  # Area under the curve (rescaled cumulative)
  auc <- sum(abs(respuesta), na.rm = TRUE)
  
  resumen_irf_scale <- rbind(resumen_irf_scale, data.frame(
    orden = name,
    max_impact = max_impacto,
    duracion_significativa = duracion_sig,
    error_std_promedio = error_std,
    auc = auc
  ))
}

# Sort by descending AUC
resumen_irf_scale <- resumen_irf_scale %>%
  arrange(desc(auc))

# Get top 10 orders with highest AUC
top10_ordenes <- resumen_irf_scale %>%
  arrange(desc(auc)) %>%
  slice(1:10) %>%
  pull(orden)

# Filter scaled IRFs only for those orders
irf_top10 <- irf_scale_list[names(irf_scale_list) %in% top10_ordenes]

# Graphing
plot_compare_irf_scaled(irf_top10, response_var)

# The best suggested order, according to statistical and theoretical evidence, 
# of the variables for the Cholesky decomposition is: Diesel, Regular, Premium 
# and Protesters.

#==============================================================================#
####           Baseline model (Order for Cholesky decomposition)            ####
#==============================================================================#

var_ordered <- VAR(df_var[,c(2,3,4,1)], p=opt_lag, type="const")

# Rescaling of the orthogonalized IRF based on the units of the impulse variables
sigma2 <- summary(var_ordered)$covres

# Obtain the standard deviation of the impulse variable to rescale
sd <- sqrt(diag(sigma2)[impulse_var])

irf_base <- irf(var_ordered, impulse = impulse_var, response = response_var,
                n.ahead = 20, ortho = TRUE, boot = TRUE)

irf_base_scale <- extract_varirf(irf_base)

# Rescale the IRF by dividing by the standard deviation
irf_base_scale[,2:4] <- irf_base_scale[,2:4] / sd

names(irf_base_scale) <- c("horizon", "response", "lower", "upper")


ggplot(irf_base_scale, aes(x = horizon, y = response)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  labs(title = "IRF reescalada (VAR base)",
       x = "Periods",
       y = "Rescaled response") +
  theme_minimal()

irf_base_scale$Model <- "Baseline VAR (Cholesky-ordered)"

# AUC as the sum of the absolute value of the response
auc_base <- sum(abs(irf_base_scale$response), na.rm = TRUE)

#==============================================================================#
####                            Granger Causality                           ####
#==============================================================================#

names(df_var)
print(colnames(var_model$y))
print(colnames(var_model_ordered$y))

variables <- colnames(var_ordered$y)
target_variable <- "Protesters"  

granger_results <- data.frame(
  Cause = character(),
  F_Statistic = numeric(),
  df1 = numeric(),
  df2 = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  if (var != target_variable) {
    granger_test <- causality(var_ordered, cause = var)
    granger_summary <- granger_test$Granger
    
    granger_results <- rbind(granger_results, data.frame(
      Cause = var,
      F_Statistic = round(granger_summary$statistic, 3),
      df1 = granger_summary$parameter[1],
      df2 = granger_summary$parameter[2],
      P_Value = round(granger_summary$p.value, 4)
    ))
  }
}

granger_ft <- flextable(granger_results) %>%
  set_header_labels(
    Cause = "Causing Variable",
    F_Statistic = "F Statistic",
    df1 = "DF (Num)",
    df2 = "DF (Den)",
    P_Value = "p-value"
  ) %>%
  autofit() %>%
  theme_booktabs()

doc <- read_docx() %>%
  body_add_par("Appendix A-5. Granger Causality Tests for Protesters", style = "heading 1") %>%
  body_add_par("Table A-5 presents the results of Granger causality tests evaluating whether fuel prices help predict protest activity. The null hypothesis is that the specified variable does not Granger-cause the number of protesters. Results are based on the estimated VAR with ordering Diesel → Regular → Premium → Protesters.", style = "Normal") %>%
  body_add_flextable(granger_ft) %>%
  body_add_par("Note: Significance is assessed at the 5% level.", style = "Normal")

print(doc, target = "Table/Appendix/TableA5_Granger_Causality_Protesters.docx")

#==============================================================================#
####                            Estimating WTI shocks                       ####
#==============================================================================#

# -------------------------------------------
# 1. Download daily WTI spot price (FRED)
# -------------------------------------------
library(quantmod)
library(tidyverse)
library(timetk)

start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2024-12-31")

fechas_completas <- tibble(date = seq(start_date, end_date, by = "day"))

getSymbols("DCOILWTICO", src = "FRED",
           from = start_date, to = end_date)

wti <- DCOILWTICO                               # xts object

wti_tbl <- wti %>%
  tk_tbl(rename_index = "date") %>%
  rename(wti_price = DCOILWTICO)

wti_full <- fechas_completas %>%
  left_join(wti_tbl, by = "date") %>%
  arrange(date)

wti_full <- wti_full[-1,]

wti_full <- wti_full %>%
  mutate(wti_price = na.locf(wti_price, fromLast = FALSE))

wti_full <- wti_full[12:2168,]

sum(is.na(wti_full$wti_price))

wti_full <- wti_full %>%
  mutate(wti_price = ifelse(wti_price <= 0, 1, wti_price))

# -------------------------------------------
# 2. Stationarity transform: ∆ln WTI
# -------------------------------------------

log_diff <- diff(log(wti_full$wti_price))

sum(is.na(log_diff))

wti_df <- wti_full[-1, ] %>%
  mutate(dln_wti = log_diff)

sum(is.na(wti_df$dln_wti))

# -------------------------------------------
# 3. AR(p) to obtain WTI shocks
# -------------------------------------------
library(forecast)
p <- 10
ar_fit <- Arima(wti_df$dln_wti, order = c(p, 0, 0))
wti_df <- wti_df %>%
  mutate(oil_shock = residuals(ar_fit))   # exogenous series

sum(is.na(wti_df$oil_shock))

# -------------------------------------------
# 4. Save or merge with VAR dataset
# -------------------------------------------
#write.csv(wti_df[, c("date", "oil_shock")],
#          "C:/Luis/Paper/Combustible/Datos/Precio del Combustible/Nuevos Precios/wti_oil_shocks.csv", row.names=FALSE)

oil_shocks <- wti_df[,4]

sum(is.na(oil_shocks))

#==============================================================================#
####                      Exogenous model: Price surprises                  ####
#==============================================================================#

modelo_iv <- VAR(df_var[,c(4,2,3,1)], p=opt_lag, type ="const", exogen = oil_shocks)

# Rescaling of the orthogonalized IRF based on the units of the impulse variables
sigma2_iv <- summary(modelo_iv)$covres

# Obtain the standard deviation of the impulse variable to rescale
sd_iv <- sqrt(diag(sigma2_iv)[impulse_var])

irf_iv <- irf(modelo_iv, impulse = impulse_var, response = response_var,
                n.ahead = 20, ortho = TRUE, boot = TRUE)

irf_iv_scale <- extract_varirf(irf_iv)

# Rescale the IRF by dividing by the standard deviation
irf_iv_scale[,2:4] <- irf_iv_scale[,2:4] / sd_iv

names(irf_iv_scale) <- c("horizon", "response", "lower", "upper")

ggplot(irf_iv_scale, aes(x = horizon, y = response)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  labs(title = "Rescaled IRF (VAR with Instrument)",
       x = "Periods",
       y = "Rescaled response") +
  theme_minimal()

irf_iv_scale$Model <- "Instrumented VAR (Proxy-SVAR)"

# We join both
irf_comb <- rbind(irf_base_scale, irf_iv_scale)

ggplot(irf_comb, aes(x = horizon, y = response, color = Model)) +
  geom_line(aes(y = response), size = 1) +  
  geom_line(aes(y = lower), linetype = "dotdash", size = 0.75) +  
  geom_line(aes(y = upper), linetype = "dotdash", size = 0.75) +  
  labs(
    x = "Periods",
    y = "Rescaled response"
  ) +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = c(
    "Baseline VAR (Cholesky-ordered)" = "darkgrey",
    "Instrumented VAR (Proxy-SVAR)" = "black"
  )) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(face = "bold")
  )

ggsave("Imagen/Appendix/FigA7_Comparison_IRFs_Cholesky_IV_VAR.png", width = 18, height = 10, units = "cm", dpi = 600)


#==============================================================================#
####                Impulse Response - Cholesky decomposition               ####
#==============================================================================#

var_ordered <- VAR(df_var[,c(2,3,4,1)], p=opt_lag, type="const")

sigma2 <- summary(var_ordered)$covres

sd <- sqrt(diag(sigma2)[impulse_var])

irf_adj <- function(var_model, var_impulse, var_response, horizont, shock_scale, ortho = TRUE, cumulative = FALSE) {
  
  # Calculate the IRF
  irf_model <- irf(var_model, impulse = var_impulse, response = var_response, 
                   n.ahead = horizont, ortho = ortho, cumulative = cumulative)
  
  # Adjust the IRF response to the desired shock
  irf_model$irf[[var_impulse]] <- irf_model$irf[[var_impulse]]*shock_scale
  irf_model$Lower[[var_impulse]] <- irf_model$Lower[[var_impulse]]*shock_scale
  irf_model$Upper[[var_impulse]] <- irf_model$Upper[[var_impulse]]*shock_scale
  
  return(irf_model)
}

# Variables and forecast horizon

horizont <- 20  # Number of periods to project
shock_scale <- 1

#var_impulse <- c("Premium", "Diesel", "Regular")
#var_impulse <- c("Diesel", "Premium", "Regular")
var_impulse <- c("Diesel", "Regular", "Premium")
var_labels <- c("D", "R", "P")  
target_variable <- "Protesters"

####           Non-cumulative impulse response - orthogonalized             ####
#==============================================================================#

library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

# Iterate over each impulse variable
for (i in seq_along(var_impulse)) {
  var <- var_impulse[i]
  var_label <- var_labels[i]
  
  irf_adjusted <- irf_adj(var_ordered, var, target_variable, horizont, shock_scale, ortho = TRUE, cumulative = FALSE)
  single_varirf <- extract_varirf(irf_adjusted)
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  # Save the graphic in high resolution (300 dpi)
  ggsave(filename = file.path("Imagen/IRF_ortho/", paste0("IRF_", var, "_ortho_ordered", ".png")), 
         plot = p, width = 9, height = 3, dpi = 300)
  
  # Obtain the standard deviation of the impulse variable to rescale
  sd_impulse <- sqrt(diag(sigma2)[var])
  
  # Rescale the IRF by dividing by the standard deviation
  single_varirf[,2:4] <- single_varirf[,2:4] / sd_impulse
  
  # Create the chart
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma,  
                       breaks = seq(-10000, 70000, by = 10000), 
                       limits = c(-13000, 75200),
                       expand = expansion(mult = c(0.05, 0.05))) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  # Save the graphic in high resolution (300 dpi)
  ggsave(filename = file.path("Imagen/IRF_ortho_scaled/", paste0("IRF_", var, "_ortho_ordered_scale", ".png")), 
         plot = p, width = 9, height = 3, dpi = 300)
}


library(magick)


img1_ordered <- image_read("Imgen/IRF_ortho/IRF_Diesel_ortho_ordered.png")
img2_ordered <- image_read("Imgen/IRF_ortho/IRF_Regular_ortho_ordered.png")
img3_ordered <- image_read("Imgen/IRF_ortho/IRF_Premium_ortho_ordered.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "C:/Luis/Paper/Combustible/Redacción/Imagenes/IRF_ortho/IRF_prices_ortho_ordered.png")


img1_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_Diesel_ortho_ordered_scale.png")
img2_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_Regular_ortho_ordered_scale.png")
img3_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_Premium_ortho_ordered_scale.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "Imagen/Fig6_IRFs_ortho_scaled_gasoline_protest.png")


####            Cumulative response impulses - orthogonalized               ####
#==============================================================================#

for (i in seq_along(var_impulse)) {
  var <- var_impulse[i]
  var_label <- var_labels[i]  
  
  irf_adjusted <- irf_adj(var_ordered, var, target_variable, horizont, shock_scale, ortho = TRUE, cumulative = TRUE)
  single_varirf <- extract_varirf(irf_adjusted)
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  # Evita notacion cientifica en el eje Y
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  # Ajuste dinámico en la ecuación
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  ggsave(filename = file.path("Imagen/IRF_ortho/", paste0("IRF_cum_", var, "_ortho_ordered", ".png")), 
         plot = p, width = 9, height = 3, dpi = 300)
  
  sd_impulse <- sqrt(diag(sigma2)[var])
  
  single_varirf[,2:4] <- single_varirf[,2:4] / sd_impulse
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma,  
                       breaks = seq(-50000, 500000, by = 100000), 
                       limits = c(-50000, 510000),
                       expand = expansion(mult = c(0.05, 0.05))) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  
  ggsave(filename = file.path("Imagen/IRF_ortho_scaled/", paste0("IRF_cum_", var, "_ortho_ordered_scale", ".png")), 
         plot = p, width = 9, height = 3, dpi = 300)
}


img1_ordered <- image_read("Imagen/IRF_ortho/IRF_cum_Diesel_ortho_ordered.png")
img2_ordered <- image_read("Imagen/IRF_ortho/IRF_cum_Regular_ortho_ordered.png")
img3_ordered <- image_read("Imagen/IRF_ortho/IRF_cum_Premium_ortho_ordered.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "Imagen/IRF_ortho/IRF_cum_prices_ortho_ordered.png")



img1_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_cum_Diesel_ortho_ordered_scale.png")
img2_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_cum_Regular_ortho_ordered_scale.png")
img3_ordered <- image_read("Imagen/IRF_ortho_scaled/IRF_cum_Premium_ortho_ordered_scale.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "Imagen/Fig7_Cum_IRFs_ortho_scaled_gasoline_protest.png")


####                           Instrumented model                           ####
####           Non-cumulative impulse response - orthogonalized             ####
#==============================================================================#

modelo_iv <- VAR(df_var[,c(2,3,4,1)], p=opt_lag, type ="const", exogen = oil_shocks)


sigma2_iv <- summary(modelo_iv)$covres

# Variables y horizonte de pronostico
#var_impulse <- c("Premium", "Diesel", "Regular")
#var_impulse <- c("Diesel", "Premium", "Regular")
var_impulse <- c("Diesel", "Regular", "Premium")
#var_labels <- c("P", "D", "R")  
#var_labels <- c("D", "P", "R")
var_labels <- c("D", "R", "P")
target_variable <- "Protesters"
horizont <- 20  
shock_scale <- 1



for (i in seq_along(var_impulse)) {
  var <- var_impulse[i]
  var_label <- var_labels[i]  
  
  irf_adjusted <- irf_adj(modelo_iv, var, target_variable, horizont, shock_scale, ortho = TRUE, cumulative = FALSE)
  single_varirf <- extract_varirf(irf_adjusted)
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  ggsave(filename = file.path("Imagen/IRF_ortho/Instrumented model/", paste0("IRF_", var, "_IV_ortho_ordered", ".png")), 
         plot = p, width = 8, height = 2, dpi = 300)
  
  sd_impulse <- sqrt(diag(sigma2_iv)[var])
  
  single_varirf[,2:4] <- single_varirf[,2:4] / sd_impulse
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  ggsave(filename = file.path("Imagen/IRF_ortho_scaled/Instrumented model", paste0("IRF_", var, "_IV_ortho_ordered_scale", ".png")), 
         plot = p, width = 8, height = 2, dpi = 300)
}



img1_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_Diesel_IV_ortho_ordered.png")
img2_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_Regular_IV_ortho_ordered.png")
img3_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_Premium_IV_ortho_ordered.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "Imagen/IRF_ortho/Instrumented model/IRF_prices_IV_ortho_ordered.png")


img1_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_Diesel_IV_ortho_ordered_scale.png")
img2_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_Regular_IV_ortho_ordered_scale.png")
img3_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_Premium_IV_ortho_ordered_scale.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical


image_write(img_final_ordered, "Imagen/IRF_ortho_scaled/Instrumented model/IRF_prices_IV_ortho_ordered_scaled.png")


####            Cumulative response impulses - orthogonalized               ####
#==============================================================================#

for (i in seq_along(var_impulse)) {
  var <- var_impulse[i]
  var_label <- var_labels[i]  
  
  irf_adjusted <- irf_adj(modelo_iv, var, target_variable, horizont, shock_scale, ortho = TRUE, cumulative = TRUE)
  single_varirf <- extract_varirf(irf_adjusted)
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  
  ggsave(filename = file.path("Imagen/IRF_ortho/", paste0("IRF_cum_", var, "_IV_ortho_ordered", ".png")), 
         plot = p, width = 8, height = 2, dpi = 300)
  
  
  sd_impulse <- sqrt(diag(sigma2_iv)[var])
  
  
  single_varirf[,2:4] <- single_varirf[,2:4] / sd_impulse
  
  
  p <- ggplot(single_varirf, aes(x = period, y = single_varirf[,2], ymin = single_varirf[,3], ymax = single_varirf[,4])) +
    geom_hline(yintercept = 0, color = "red") +
    geom_ribbon(fill = "grey", alpha = .2, color = "grey50", linetype = "dashed") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +  
    theme_light() +
    ggtitle(bquote(Delta * p[t]^.(var_label) %->% y[t])) +  
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          axis.title.y = element_text(size = 11))
  
  
  ggsave(filename = file.path("Imagen/IRF_ortho_scaled/Instrumented model/", paste0("IRF_cum_", var, "_IV_ortho_ordered_scale", ".png")), 
         plot = p, width = 8, height = 2, dpi = 300)
}


img1_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_cum_Diesel_IV_ortho_ordered.png")
img2_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_cum_Regular_IV_ortho_ordered.png")
img3_ordered <- image_read("Imagen/IRF_ortho/Instrumented model/IRF_cum_Premium_IV_ortho_ordered.png")


img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical

image_write(img_final_ordered, "Imagen/IRF_ortho/Instrumented model/IRF_cum_prices_IV_ortho_ordered.png")

img1_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_cum_Diesel_IV_ortho_ordered_scale.png")
img2_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_cum_Regular_IV_ortho_ordered_scale.png")
img3_ordered <- image_read("Imagen/IRF_ortho_scaled/Instrumented model/IRF_cum_Premium_IV_ortho_ordered_scale.png")

img_final_ordered <- image_append(c(img1_ordered, img2_ordered, img3_ordered), stack = TRUE)  # stack=TRUE une en vertical

image_write(img_final_ordered, "Imagen/IRF_ortho_scaled/Instrumented model/IRF_cum_prices_IV_ortho_ordered_scale.png")


#==============================================================================#
####              Coefficients Baseline VAR and Instrumented VAR            ####
#==============================================================================#

# Extract coefficients from the base VAR and the instrumented model
coef_base <- coef(var_ordered)  # model_var: your VAR model with Premium-Diesel-Regular-Protesters order
coef_iv   <- coef(modelo_iv)    # VAR model with exogenous variable: oil_shocks   

library(tidyverse)

# Function to transform coefficients
extract_coefs <- function(coef_list, model_name) {
  map_df(names(coef_list), function(dep_var) {
    coefs <- coef_list[[dep_var]]
    as.data.frame(coefs) %>%
      rownames_to_column("Predictor") %>%
      mutate(Dependent = dep_var,
             Model = model_name)
  })
}

# Apply to both models
df_base <- extract_coefs(coef_base, "VAR Ordered")
df_iv   <- extract_coefs(coef_iv,   "VAR IV")

# Join the results
df_all <- bind_rows(df_base, df_iv)

# Filter only Protesters if you wish
df_protest <- df_all %>%
  filter(Dependent == "Protesters") %>%
  select(Predictor, Estimate, Model)

# Pivot wide
df_wide <- df_protest %>%
  pivot_wider(names_from = Model, values_from = Estimate)

# Create desired order vector
variables_order <- c("Premium", "Diesel", "Regular", "const", "oil_shock")
lags_order <- paste0("l", 1:11)

# Expand all expected predictors
orden_total <- expand.grid(var = variables_order, lag = lags_order) %>%
  mutate(Predictor = ifelse(var %in% c("const", "oil_shock"), var,
                            paste0(var, ".", lag))) %>%
  pull(Predictor)

# Apply custom order
df_wide <- df_wide %>%
  mutate(Predictor = factor(Predictor, levels = orden_total)) %>%
  arrange(Predictor)

# Clean names
names(df_wide) <- c("Predictor", "VAR Ordered", "VAR IV")

library(officer)
library(flextable)
library(broom)

ft <- flextable(df_wide) %>%
  autofit() %>%
  theme_booktabs() %>%
  set_caption("Comparison of estimated coefficients for Protesters (VAR Ordered vs. VAR IV)")

doc <- read_docx() %>%
  body_add_par("Table: Estimated Coefficients for 'Protesters'", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Table/Appendix/TableA6_Coef_Var_Comparison_Protesters.docx")