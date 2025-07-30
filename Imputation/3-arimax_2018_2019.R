#==============================================================================#
####                   Installing and loading packages                      ####
#==============================================================================#

rm(list = ls())

library(dplyr)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(writexl)
library(urca)
library(DescTools)

# Robust Scaling

median_train <- 301
iqr_train <- 538

#==============================================================================#
####                             Database loading                           ####
#==============================================================================#

protestas<-read.csv("Datos/Protest/Protesters Imputation - Tranche 2/protestas_imputadas_personas.csv", sep = ",")
rsui <- read.csv("Datos/GDELT/RSU_index.csv")
acled <- read.csv("Datos/ACLED/acled_final_new.csv")

final_protestas<-read.csv("Datos/Protest/personas_imputada_final.csv", sep = ",")
final_protestas <- final_protestas[640:1118, c(1,9)]
final_protestas$final_imputation <- (final_protestas$final_imputation - median_train)/iqr_train

protestas <- protestas[, c(-1,-4)]
rsui <- rsui[,-1]
acled <- acled[,-1]

protestas <- left_join(protestas, final_protestas, by = "date")
protestas$prot_fin <- ifelse(is.na(protestas$final_imputation), protestas$personas, protestas$final_imputation)

plot(protestas$personas, type = "l", col = "blue", lwd = 1, 
     xlab = "Tiempo", ylab = "Valor", 
     main = "Comparación de Series: Personas vs. Imputación")

lines(protestas$final_imputation, col = "red", lwd = 1)

legend("topright", legend = c("Personas", "Final Imputation"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

protestas <- protestas[,c(1,4)]
names(protestas)[2] <- "personas"

# Join the protest bases, RSUI and ACLED ---------------------------------------

df <- left_join(rsui, protestas, by="date")
df<- left_join(df, acled, by = "date")
date <- data.frame(df$date)

rm(protestas, rsui, acled, final_protestas)

# Revert df --------------------------------------------------------------------

df <- df[nrow(df):1,]
df$date_rev <- rev(df$date)
df$date_rev <- as.Date(df$date_rev)
df$day <- yday(df$date_rev)
df$year <- format(df$date_rev, "%Y")

inicio <- min(which(!is.na(df$personas)))# Row where protest data begins
fin <- max(which(!is.na(df$personas)))   # Row where updated protest data ends

df1 <- df[inicio:fin,]                    # Database that considers protest data

plot(df1$personas, type ="l")

# Bases for training and validation for the model ------------------------------

cut_point <- 591                        
train <- df1[1:cut_point,]              # Training base
test <- df1[(cut_point+1):nrow(df1),]   # Validation base

plot(train$personas, type="l")
plot(test$personas, type="l")

personas <- df$personas
personas_ts <- train$personas
personas_ts1 <- test$personas

personas <- ts(personas, start =c(as.numeric(df[1,"year"]),as.numeric(df[1,"day"])), frequency = 365.25)     # Base de entrenamiento
personas_ts <- ts(personas_ts, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.25)     # Base de entrenamiento
personas_ts1 <- ts(personas_ts1, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.25)    # Base de validacion


# EVALUATING STATIONARITY ------------------------------------------------------

df_personas <- df1 %>% dplyr::select(personas)
df_personas <- ts(df_personas, start =c(as.numeric(df1[1,"year"]),as.numeric(df1[1,"day"])), frequency = 365.25)     

adftest <- ur.df(df_personas, 
                 type = c("none"),
                 lags = 3,
                 selectlags = c("Fixed"))
summary(adftest) #Ho: Unit root -> Rejection


kpsstest <- ur.kpss(df_personas, 
                    type = c("mu"), 
                    use.lag = 6)
summary(kpsstest)#Ho: Stationary -> Rejection

# Lagging variables ------------------------------------------------------------

lag_names_y <- paste("personas_lag", 1:12, sep = "")

mse_results <- data.frame(lag_names_y) 
indice_final <- data.frame(lag_names_y)
models_final<- c()


xreg <- train %>% dplyr::select(date_rev,RSUI_A_w, RSUI_B_m, acled_w)
newxreg <- test %>% dplyr::select(date_rev, RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w <- xreg %>% dplyr::select(date_rev, RSUI_A_w)
xrsui_b_m <- xreg %>% dplyr::select(date_rev, RSUI_B_m)
xacled_w <- xreg %>% dplyr::select(date_rev, acled_w)
nxrsui_a_w <- newxreg %>% dplyr::select(date_rev, RSUI_A_w)
nxrsui_b_m <- newxreg %>% dplyr::select(date_rev, RSUI_B_m)
nxacled_w <- newxreg %>% dplyr::select(date_rev, acled_w)

for (i in 1:7) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w <- xrsui_a_w %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date_rev)
    )
  col_name <- paste("RSUI_B_m", i, sep = "") 
  xrsui_b_m <- xrsui_b_m %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date_rev)
    )
  col_name <- paste("acled_w", i, sep = "")  
  xacled_w <- xacled_w %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date_rev)
    )
  
  col_name <- paste("RSUI_A_w", i, sep = "")  
  nxrsui_a_w <- nxrsui_a_w %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date_rev)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  
  nxrsui_b_m <- nxrsui_b_m %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date_rev)
    )
  col_name <- paste("acled_w", i, sep = "")  
  nxacled_w <- nxacled_w %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date_rev)
    )
}

xreg <- xreg %>% dplyr:: select(-date_rev)
xrsui_a_w <- xrsui_a_w %>% dplyr:: select(-date_rev)
xrsui_b_m <- xrsui_b_m %>% dplyr:: select(-date_rev)
xacled_w <- xacled_w %>% dplyr:: select(-date_rev)

xrsui_a_w <- ts(xrsui_a_w, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)         # Decarar df como ts
xrsui_b_m <- ts(xrsui_b_m, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)         # Decarar df como ts
xacled_w <- ts(xacled_w, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)           # Decarar df como ts

newxreg <- newxreg %>% dplyr:: select(-date_rev)
nxrsui_a_w <- nxrsui_a_w %>% dplyr:: select(-date_rev)
nxrsui_b_m <- nxrsui_b_m %>% dplyr:: select(-date_rev)
nxacled_w <- nxacled_w %>% dplyr:: select(-date_rev)

nxrsui_a_w <- ts(nxrsui_a_w, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxrsui_b_m <- ts(nxrsui_b_m, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxacled_w <- ts(nxacled_w, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)           # Decarar df como ts


# Leading variables ------------------------------------------------------------

# Select data for the regressor variables with periods after the last
# data available for the protests to be able to apply the lead to the
# variables

#df_fut <- df[2227:3074,] %>% dplyr::select(RSUI_A_w, RSUI_B_m, acled_w)
df_fut <- df[inicio:(fin+10),] %>% dplyr::select(RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w_fut <- df_fut %>% dplyr::select(RSUI_A_w)
xrsui_b_m_fut <- df_fut %>% dplyr::select(RSUI_B_m)
xacled_w_fut <- df_fut %>% dplyr::select(acled_w)

#ndf_fut <- df[2677:3021,] %>% dplyr::select(RSUI_A_w, RSUI_B_m, acled_w)
ndf_fut <- df[(inicio+cut_point):(fin+7),] %>% dplyr::select(RSUI_A_w, RSUI_B_m, acled_w)

nxrsui_a_w_fut <- ndf_fut %>% dplyr::select(RSUI_A_w)
nxrsui_b_m_fut <- ndf_fut %>% dplyr::select(RSUI_B_m)
nxacled_w_fut <- ndf_fut %>% dplyr::select(acled_w)



for (i in 1:7) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w_fut <- xrsui_a_w_fut %>%
    mutate(
      !!col_name := lead(RSUI_A_w, n = i)
    )  
  col_name <- paste("RSUI_B_m", i, sep = "")  
  xrsui_b_m_fut <- xrsui_b_m_fut %>%
    mutate(
      !!col_name := lead(RSUI_B_m, n = i)
    )  
  col_name <- paste("acled_w", i, sep = "")  
  xacled_w_fut <- xacled_w_fut %>%
    mutate(
      !!col_name := lead(acled_w, n = i)
    )  
  
  col_name <- paste("RSUI_A_w", i, sep = "")  
  nxrsui_a_w_fut <- nxrsui_a_w_fut %>%
    mutate(
      !!col_name := lead(RSUI_A_w, n = i)
    )  
  col_name <- paste("RSUI_B_m", i, sep = "")  
  nxrsui_b_m_fut <- nxrsui_b_m_fut %>%
    mutate(
      !!col_name := lead(RSUI_B_m, n = i)
    )  
  col_name <- paste("acled_w", i, sep = "")  
  nxacled_w_fut <- nxacled_w_fut %>%
    mutate(
      !!col_name := lead(acled_w, n = i)
    )  
}

# We eliminate the extra rows we considered before to build the leads and
# we keep those that represent the lags, to maintain the same number
# of rows in the regressor variables that will be included in a single matrix

xrsui_a_w_fut <- xrsui_a_w_fut[1:cut_point,-1] # the first column which is the original variable is removed
xrsui_b_m_fut <- xrsui_b_m_fut[1:cut_point,-1] # the first column which is the original variable is removed
xacled_w_fut <- xacled_w_fut[1:cut_point,-1]   # the first column which is the original variable is removed

nxrsui_a_w_fut <- nxrsui_a_w_fut[1:dim(x = test)[1],-1] # the first column which is the original variable is removed
nxrsui_b_m_fut <- nxrsui_b_m_fut[1:dim(x = test)[1],-1] # the first column which is the original variable is removed
nxacled_w_fut <- nxacled_w_fut[1:dim(x = test)[1],-1]   # the first column which is the original variable is removed

xrsui_a_w_fut <- ts(xrsui_a_w_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)
xrsui_b_m_fut <- ts(xrsui_b_m_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)
xacled_w_fut <- ts(xacled_w_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)

nxrsui_a_w_fut <- ts(nxrsui_a_w_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)
nxrsui_b_m_fut <- ts(nxrsui_b_m_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)
nxacled_w_fut <- ts(nxacled_w_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)


# LOOP BEST MODEL ------------------------------------------------------------

# Option 1: Model with lags only
for (k in 1:7){ # Loop starting with RSUI_A_w (lags)
  for (l in 1:7){ # Loop starting with RSUI_B_m (lags)
    for (m in 1:7){ # Loop starting with acled_w (lags)
      
      models <- c()
      mse <- c()
      indice <- c()
      
      # Only the lags
      x <- cbind(xrsui_a_w[,1:k], xrsui_b_m[,1:l], xacled_w[,1:m])
      nx <- cbind(nxrsui_a_w[,1:k], nxrsui_b_m[,1:l], nxacled_w[,1:m])
      
      for (j in 1:12){
        model <- arima(personas_ts, order = c(j,0,0), xreg = x)
        p <- predict(model, newxreg = nx)
        result_mse <- mean((test$personas - p$pred)^2, na.rm = TRUE)
        mse <- c(mse, result_mse)
        models <- c(models, model)
        indice <- c(indice, paste("Lag", k, l, m, j, sep = ","))
      }
      
      # Save results
      mse <- as.data.frame(mse)
      indice <- as.data.frame(indice)
      mse_results <- cbind(mse_results, mse)
      models_final <- c(models_final, models)
      indice_final <- as.data.frame(c(indice_final, indice))
      
    }
  }
}

# Option 2: Model with leads only
for (p_lead in 1:3){ # Loop starting with RSUI_A_w (leads)
  for (q_lead in 1:3){ # Loop starting with RSUI_B_m (leads)
    for (r_lead in 1:3){ # Loop starting with acled_w (leads)
      
      models <- c()
      mse <- c()
      indice <- c()
      
      # Solo los leads (leads)
      x <- cbind(xrsui_a_w_fut[,1:p_lead], xrsui_b_m_fut[,1:q_lead], xacled_w_fut[,1:r_lead])
      nx <- cbind(nxrsui_a_w_fut[,1:p_lead], nxrsui_b_m_fut[,1:q_lead], nxacled_w_fut[,1:r_lead])
      
      for (j in 1:12){
        model <- arima(personas_ts, order = c(j,0,0), xreg = x)
        p <- predict(model, newxreg = nx)
        result_mse <- mean((test$personas - p$pred)^2, na.rm = TRUE)
        mse <- c(mse, result_mse)
        models <- c(models, model)
        indice <- c(indice, paste("Lead", p_lead, q_lead, r_lead, j, sep = ","))
      }
      
      # Guardar resultados
      mse <- as.data.frame(mse)
      indice <- as.data.frame(indice)
      mse_results <- cbind(mse_results, mse)
      models_final <- c(models_final, models)
      indice_final <- as.data.frame(c(indice_final, indice))
    }
  }
}

# Option 3: Model with combined lags and leads
for (k in 1:7){ # Loop starting with RSUI_A_w (rezagos)
  for (l in 1:7){ # Loop starting with RSUI_B_m (rezagos)
    for (m in 1:7){ # Loop starting with acled_w (rezagos)
      for (p_lead in 1:3){ # Loop starting with RSUI_A_w (leads)
        for (q_lead in 1:3){ # Loop starting with RSUI_B_m (leads)
          for (r_lead in 1:3){ # Loop starting with acled_w (leads)
            
            models <- c()
            mse <- c()
            indice <- c()
            
            # Combination of lags and leads
            x <- cbind(xrsui_a_w[,1:k], xrsui_b_m[,1:l], xacled_w[,1:m], 
                       xrsui_a_w_fut[,1:p_lead], xrsui_b_m_fut[,1:q_lead], xacled_w_fut[,1:r_lead])
            nx <- cbind(nxrsui_a_w[,1:k], nxrsui_b_m[,1:l], nxacled_w[,1:m],
                        nxrsui_a_w_fut[,1:p_lead], nxrsui_b_m_fut[,1:q_lead], nxacled_w_fut[,1:r_lead])
            
            for (j in 1:12){
              model <- arima(personas_ts, order = c(j,0,0), xreg = x)
              p <- predict(model, newxreg = nx)
              result_mse <- mean((test$personas - p$pred)^2, na.rm = TRUE)
              mse <- c(mse, result_mse)
              models <- c(models, model)
              indice <- c(indice, paste("LagLead", k, l, m, p_lead, q_lead, r_lead, j, sep = ","))
            }
            
            # Save results
            mse <- as.data.frame(mse)
            indice <- as.data.frame(indice)
            mse_results <- cbind(mse_results, mse)
            models_final <- c(models_final, models)
            indice_final <- as.data.frame(c(indice_final, indice))
          }
        }
      }
    }
  }
}


mse_matrix <- as.matrix(mse_results)

# Find the position of the minimum value in the array
min_pos <- arrayInd(which.min(mse_matrix), dim(mse_matrix))

# Display the row and column of the minimum value
fila <- min_pos[1]
columna <- min_pos[2]

cat("El valor mínimo de MSE está en la fila", fila, "y la columna", columna, "\n", ", cuyo modelo
    considera lags para RSUI_A_w, RSUI_B_m, acled_w, Y de:", indice_final[fila, columna])

# Best Model

# The model that won out of all combinations of lag, lead, and lag_lead
# was:
#* xrsui_a_w original, no lags
#* xrsui_b_m original and 1 lag
#* xacled_W original and 1 lag
#* protesters with 1 lag

x_1 <- cbind(xrsui_a_w[,1:1], xrsui_b_m[,1:2], xacled_w[,1:2])
nx_1 <- cbind(nxrsui_a_w[,1:1], nxrsui_b_m[,1:2], nxacled_w[,1:2])

model_best_1 <- arima(personas_ts, order = c(1,0,0), xreg = x_1)
p_1<- predict(model_best_1, newxreg = nx_1)
result_mse_best_1 <- mean((test$personas - p_1$pred)^2, na.rm = T)
coeficientes_1 <- round(model_best_1$coef,3)

autoplot(personas_ts, series="Data") + 
  autolayer(p_1$pred+(mean(p_1$pred, na.rm = T)/4), series="Forecast", na.rm =T) + 
  autolayer(personas_ts1, series="Real")

AIC(model_best_1)

# Extract residuals
residuales <- residuals(model_best_1)
residuales

# Calculate RMSE
rmse <- sqrt(mean(residuales^2, na.rm = TRUE))
rmse

# Extract coefficients and standard errors
coefs <- model_best_1$coef
se <- sqrt(diag(model_best_1$var.coef))

# Calculate t-values and p-values
tval <- coefs / se
pval <- round(2 * (1 - pnorm(abs(tval))),4)

coef_table <- data.frame(
  Variable = names(coefs),
  Coefficient = round(coefs, 4),
  `Std. Error` = round(se, 4),
  `t-value` = round(tval, 3),
  `p-value` = signif(pval, 3)
)

ft <- flextable(coef_table) %>%
  theme_booktabs() %>%
  set_caption("Table A.8.2: Estimated Coefficients of the Best ARIMAX Model for Tranche 1 Protester Data Imputation (2018-01-01 to 2019-10-01)") %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table: Estimated Coefficients of the Best ARIMAX Model for Tranche 1 'Protesters'", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Table/Appendix/TableA6_Coef_ARIMAX2_Protesters.docx")


#==============================================================================#
####        Protester Data Imputation (2018-01-01 to 2019-10-01)            ####
#==============================================================================#

inicio1 <- max(which(!is.na(df$personas))) # Initial date for which I have no data on protests
fin1 <- which(df$date_rev=="2021-10-03")   

df2 <- df[inicio1:fin1,]
xreg2 <- df2 %>% dplyr::select(date_rev, RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w1 <- xreg2 %>% dplyr::select(date_rev, RSUI_A_w)
xrsui_b_m1 <- xreg2 %>% dplyr::select(date_rev, RSUI_B_m)
xacled_w1 <- xreg2 %>% dplyr::select(date_rev, acled_w)

for (i in 1:6) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w1 <- xrsui_a_w1 %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date_rev)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  
  xrsui_b_m1 <- xrsui_b_m1 %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date_rev)
    )
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xacled_w1 <- xacled_w1 %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date_rev)
    )
}

xreg2 <- xreg2 %>% dplyr:: select(-date_rev)
xrsui_a_w1 <- xrsui_a_w1 %>% dplyr:: select(-date_rev)
xrsui_b_m1 <- xrsui_b_m1 %>% dplyr:: select(-date_rev)
xacled_w1 <- xacled_w1 %>% dplyr:: select(-date_rev)

xrsui_a_w1 <- ts(xrsui_a_w1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)         # Decarar df como ts
xrsui_b_m1 <- ts(xrsui_b_m1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)         # Decarar df como ts
xacled_w1 <- ts(xacled_w1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)           # Decarar df como ts

# LEAD VARIABLES ---------------------------------------------------------------

xrsui_a_w_fut1 <- xreg2 %>% dplyr::select(RSUI_A_w)
xrsui_b_m_fut1 <- xreg2 %>% dplyr::select(RSUI_B_m)
xacled_w_fut1 <- xreg2 %>% dplyr::select(acled_w)

for (i in 1:3) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w_fut1 <- xrsui_a_w_fut1 %>%
    mutate(
      !!col_name := lead(RSUI_A_w, n = i)
    )  
  col_name <- paste("RSUI_B_m", i, sep = "")  
  xrsui_b_m_fut1 <- xrsui_b_m_fut1 %>%
    mutate(
      !!col_name := lead(RSUI_B_m, n = i)
    )  
  col_name <- paste("acled_w", i, sep = "")  
  xacled_w_fut1 <- xacled_w_fut1 %>%
    mutate(
      !!col_name := lead(acled_w, n = i)
    )  
}

xrsui_a_w_fut1 <- xrsui_a_w_fut1[,-1]
xrsui_b_m_fut1 <- xrsui_b_m_fut1[,-1]
xacled_w_fut1 <- xacled_w_fut1[,-1]

xrsui_a_w_fut1 <- ts(xrsui_a_w_fut1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)
xrsui_b_m_fut1 <- ts(xrsui_b_m_fut1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)
xacled_w_fut1 <- ts(xacled_w_fut1, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)

x3 <- cbind(xrsui_a_w1[,1:1], xrsui_b_m1[,1:2], xacled_w1[,1:2])

# Predict missing values forward
n_pred <- fin1-inicio1  # number of days to predict


y <- df_personas[length(df_personas)]
y[2:n_pred+1] <- NA
y <- ts(y, start =c(as.numeric(df2[1,"year"]),as.numeric(df2[1,"day"])), frequency = 365.35)     # Declarar df como ts

# Identify the indices of the missing values
na_indices <- which(is.na(y))

# Initialize a list to store the predictions
predictions <- numeric(length(na_indices))

# Predict and store predictions
for (i in (seq_along(na_indices))) {
  idx <- na_indices[i]
  
  # Perform the prediction with the ARIMAX model
  pred <- predict(model_best_1, n.ahead = 1, newxreg = x3[idx, , drop = FALSE])$pred
  
  predictions[i] <- pred
  
  # Impute the predicted value in 'y', subtracting the mean of the predictions
  y[idx] <- pred    # Restar la media de las predicciones
}

autoplot(df_personas, series="Data") + 
  autolayer(y, series="Forecast", na.rm =T) 



y <- y[2:length(y)]

y_final_scaled <- c(df_personas, y)
plot(y_final_scaled, type="l")

# Reverse the Robus Scaling process

y_final <- y_final_scaled*iqr_train + median_train
plot(y_final, type ="l")

protestas_imputadas_2018_act <- data.frame(
  date_rev = df$date_rev[inicio:fin1],
  date = df$date[inicio:fin1],
  personas = y_final_scaled,
  personas_scaled = y_final
)

protestas_imputadas_2018_act <- protestas_imputadas_2018_act[nrow(protestas_imputadas_2018_act):1,]

imputados.ts <- ts(protestas_imputadas_2018_act$personas, start = c(2018,1), frequency = 365.35)
plot(imputados.ts)

# Save the data frame to a CSV and xlsxfile

write.csv(protestas_imputadas_2018_act, quote = F, "Datos/Protest/Protesters Imputation - Tranche 1/protestas_imputadas_2018_act.csv")
