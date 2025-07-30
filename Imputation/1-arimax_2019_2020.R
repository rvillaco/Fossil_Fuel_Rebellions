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

#==============================================================================#
####                             Database loading                           ####
#==============================================================================#

setwd("C:/Luis/Paper/Combustible") 

protestas<-read.csv("Datos/Protest/protestas_original.csv", sep = ",")
rsui <- read.csv("Datos/GDELT/RSU_index.csv")
acled <- read.csv("Datos/ACLED/acled_final_new.csv")

protestas <- protestas[, c(-1, -3, -4, -5)]
rsui <- rsui[,-1]
acled <- acled[,-1]
protestas[2202,2] <- NA

# 2019 Protest Database Seed ---------------------------------------------------

protestas1<- readxl:: read_xlsx(file.path("Datos/Protest/protestas_2019.xlsx"))
protestas1$date <- as.character(protestas1$date)
protestas1 <- protestas1[,1:2]

# Protests2 create the protest variable considering both the 2019 seed data and
# existing data from other years.

protestas2 <- left_join(protestas, protestas1, by="date")
protestas2$personas_sin[!is.na(protestas2$personas)] <- protestas2$personas[!is.na(protestas2$personas)]
protestas2 <- protestas2[,1:2]
names(protestas2)[2] <- "personas"
plot(protestas2$personas, type="l")


# Convert the date variable to Date format if it is not already
protestass <- protestas2 %>%
  mutate(date = as.Date(date))

# Filter the desired date range
protestas_filtradas <- protestass %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2023-12-31"))

# Define missing data sections
faltantes1_inicio <- as.Date("2018-01-01")
faltantes1_fin <- as.Date("2019-10-01")
faltantes2_inicio <- as.Date("2019-10-14")
faltantes2_fin <- as.Date("2021-02-03")

# Calculate the midpoints of the missing sections
faltantes1_medio <- faltantes1_inicio + (faltantes1_fin - faltantes1_inicio) / 2
faltantes2_medio <- faltantes2_inicio + (faltantes2_fin - faltantes2_inicio) / 2

# Define the position on the Y axis to place the labels
y_max <- max(protestas_filtradas$personas, na.rm = TRUE) * 0.5  

# Create the graph
ggplot(protestas_filtradas, aes(x = date, y = personas)) +
  geom_line(color = "deepskyblue3", na.rm = FALSE) +  # Visualize discontinuities
  # Sombrear los tramos faltantes
  annotate("rect", xmin = faltantes1_inicio, xmax = faltantes1_fin, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  annotate("rect", xmin = faltantes2_inicio, xmax = faltantes2_fin, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  annotate("text", x = faltantes1_medio, y = y_max, label = "Tranche 1", color = "black", size = 5, fontface = "bold") +
  annotate("text", x = faltantes2_medio, y = y_max, label = "Tranche 2", color = "black", size = 5, fontface = "bold") +
  labs(x = "",
       y = "") +
  theme_minimal()

ggsave("Redacción/Imagenes/grafico_protestas.png", width = 10, height = 4, dpi = 300)

protestas1$date <- as.Date(protestas1$date)
protestas1$day <- yday(protestas1$date)
protestas1$year <- format(protestas1$date, "%Y")

# Join the protest bases, RSUI and ACLED ---------------------------------------

df <- left_join(rsui, protestas2, by="date")
df<- left_join(df, acled, by = "date")
df$date <- as.Date(df$date)
df$day <- yday(df$date)
df$year <- format(df$date, "%Y")

inicio <- min(which(!is.na(protestas$personas)))# Fila donde inician datos de protestas sin la semilla
fin <- max(which(!is.na(df$personas)))   # Fila donde terminan datos de protestas actualizados

df1 <- df[inicio:fin,]                    # Base que considera datos de protestas

# Bases for training and validation for the model ------------------------------

cut_point <- 450                        # Originalmente era 450
train <- df1[1:cut_point,]              # Base de entrenamiento
test <- df1[(cut_point+1):nrow(df1),]   # Base de validacion

# Robust Scaling

median_train <- median(train$personas, na.rm = T)
iqr_train <- IQR(train$personas, na.rm = T)

median_train_rsui_b_m <- median(train$RSUI_B_m, na.rm = T)
iqr_train_rsui_b_m <- IQR(train$RSUI_B_m, na.rm = T)

median_train_rsui_a_w <- median(train$RSUI_A_w, na.rm = T)
iqr_train_rsui_a_w <- IQR(train$RSUI_A_w, na.rm = T)

df$personas <- (df$personas-median_train)/iqr_train
df1$personas <- (df1$personas-median_train)/iqr_train
train$personas <- (train$personas-median_train)/iqr_train
test$personas <- (test$personas-median_train)/iqr_train
protestas$personas_sin <- (protestas$personas_sin-median_train)/iqr_train
protestas1$personas <- (protestas1$personas-median_train)/iqr_train
protestas2$personas <- (protestas2$personas-median_train)/iqr_train

personas_ts <- train$personas
personas_ts1 <- test$personas

plot(personas_ts, type="l")
plot(personas_ts1, type="l")

personas_ts <- ts(personas_ts, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.25)     # Base de entrenamiento
personas_ts1 <- ts(personas_ts1, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.25)    # Base de validacion

plot(personas_ts, type="l")
plot(personas_ts1, type="l")
plot(protestas2$personas, type="l")

# EVALUATING STATIONARITY ------------------------------------------------------

pruebas_estacionariedad_drift <- function(datos_xts, variables) {
  resultados <- data.frame(Variable = character(),
                           Lag = integer(),
                           ADF_Est = numeric(),
                           ADF_pvalor = numeric(),
                           ADF_Estacionaria = character(),
                           KPSS_Est = numeric(),
                           KPSS_Estacionaria = character(),
                           stringsAsFactors = FALSE)
  
  for (var in variables) {
    if (!(var %in% colnames(datos_xts))) {  
      warning(paste("Variable no encontrada en datos_xts:", var))
      next  # Salta si la variable no está en datos_xts
    }
    
    serie <- datos_xts[, var] 
    n <- length(serie)  # Tamaño de la muestra
    
    for (l in 1:15) {  # Iterar sobre lags de 1 a 15
      # Prueba ADF con drift
      adf_test <- ur.df(serie, type = "drift", lags = l)
      adf_est <- round(adf_test@testreg$coefficients["z.lag.1", "t value"],2)
      adf_pvalor <- adf_test@testreg$coefficients["z.lag.1", "Pr(>|t|)"]
      adf_estacionaria <- ifelse(adf_pvalor < 0.05, "Si", "No")
      
      # Prueba KPSS con drift
      kpss_test <- ur.kpss(serie, type = "mu", use.lag = l)
      kpss_pvalor <- round(kpss_test@teststat,2)  
      kpss_critico <- kpss_test@cval[1,"5pct"] 
      kpss_estacionaria <- ifelse(kpss_pvalor < kpss_critico, "Si", "No")
      
      # Guardar resultados
      resultados <- rbind(resultados, data.frame(Variable = var,
                                                 Lag = l,
                                                 ADF_Est = adf_est,
                                                 ADF_pvalor = adf_pvalor,
                                                 ADF_Estacionaria = adf_estacionaria,
                                                 KPSS_Est = kpss_pvalor,
                                                 KPSS_Estacionaria = kpss_estacionaria
      ))
    }
  }
  
  return(resultados)
}

datos_xts <- xts(df1[,c(13, 7:12, 18:20)], order.by = df1$date)  # Eliminar columna de fecha
datos_xts <- xts(train[,c(13, 7:12, 18:20)], order.by = train$date)  # Eliminar columna de fecha

# Apply stationarity tests
variables <- c("personas", "RSUI_A_d", "RSUI_A_w", "RSUI_A_m", "RSUI_B_d", "RSUI_B_w", "RSUI_B_m", "acled_d",  "acled_w",  "acled_m")

print(colnames(datos_xts))
print(variables)

# Run the tests
resultados_estacionariedad_drift <- pruebas_estacionariedad_drift(datos_xts, variables)
resultados_estacionariedad_drift <- resultados_estacionariedad_drift[,-c(4,5,7)]
resultados_estacionariedad_drift

table_result <- reshape(resultados_estacionariedad_drift, timevar = c("Lag"), idvar = "Variable", direction = "wide")
table_result <- table_result[c(1,3,6,9),c(1,2,4,6,8,10,12,3,5,7,9,11,13)]
table_result$Variable[1] <- c("Protesters")

# Exportar a Excel
library(openxlsx)
write.xlsx(table_result, file = "Table/TableA2_Test_ADF_KPSS_wide.xlsx", overwrite = TRUE)

  
df_personas <- df1 %>% dplyr::select(personas)
df_personas <- ts(df_personas, start =c(as.numeric(df1[1,"year"]),as.numeric(df1[1,"day"])), frequency = 365.25)     
plot(df_personas, type="l")

adftest <- ur.df(df_personas, 
                 type = c("drift"),
                 lags = 3,
                 selectlags = c("Fixed"))
summary(adftest) #Ho: Raiz unitaria -> Rechazo


kpsstest <- ur.kpss(df_personas, 
                    type = c("mu"), 
                    use.lag = 6)
summary(kpsstest)#Ho: Estacionario  -> Rechazo

# Lagging variables ------------------------------------------------------------

lag_names_y <- paste("personas_lag", 1:12, sep = "")

mse_results <- data.frame(lag_names_y) 
indice_final <- data.frame(lag_names_y)
models_final<- c()


xreg <- train %>% dplyr::select(date,RSUI_A_w, RSUI_B_m, acled_w)
newxreg <- test %>% dplyr::select(date, RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w <- xreg %>% dplyr::select(date, RSUI_A_w)
xrsui_b_m <- xreg %>% dplyr::select(date, RSUI_B_m)
xacled_w <- xreg %>% dplyr::select(date, acled_w)
nxrsui_a_w <- newxreg %>% dplyr::select(date, RSUI_A_w)
nxrsui_b_m <- newxreg %>% dplyr::select(date, RSUI_B_m)
nxacled_w <- newxreg %>% dplyr::select(date, acled_w)

for (i in 1:7) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w <- xrsui_a_w %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  # Generar el nombre de la columna
  xrsui_b_m <- xrsui_b_m %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date)
    )
  col_name <- paste("acled_w", i, sep = "")  
  xacled_w <- xacled_w %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date)
    )
  
  col_name <- paste("RSUI_A_w", i, sep = "")  
  nxrsui_a_w <- nxrsui_a_w %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  # Generar el nombre de la columna
  nxrsui_b_m <- nxrsui_b_m %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date)
    )
  col_name <- paste("acled_w", i, sep = "")  
  nxacled_w <- nxacled_w %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date)
    )
}


xreg <- xreg %>% dplyr:: select(-date)
xrsui_a_w <- xrsui_a_w %>% dplyr:: select(-date)
xrsui_b_m <- xrsui_b_m %>% dplyr:: select(-date)
xacled_w <- xacled_w %>% dplyr:: select(-date)

xrsui_a_w <- ts(xrsui_a_w, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)         # Decarar df como ts
xrsui_b_m <- ts(xrsui_b_m, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)         # Decarar df como ts
xacled_w <- ts(xacled_w, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)           # Decarar df como ts

newxreg <- newxreg %>% dplyr:: select(-date)
nxrsui_a_w <- nxrsui_a_w %>% dplyr:: select(-date)
nxrsui_b_m <- nxrsui_b_m %>% dplyr:: select(-date)
nxacled_w <- nxacled_w %>% dplyr:: select(-date)

nxrsui_a_w <- ts(nxrsui_a_w, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxrsui_b_m <- ts(nxrsui_b_m, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxacled_w <- ts(nxacled_w, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)           # Decarar df como ts


# Leading variables ------------------------------------------------------------

# Select data for the regressor variables with periods after the last
# data available for the protests to be able to apply the lead to the
# variables

df_fut <- df[inicio:(fin+10),] %>% dplyr::select(RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w_fut <- df_fut %>% dplyr::select(RSUI_A_w)
xrsui_b_m_fut <- df_fut %>% dplyr::select(RSUI_B_m)
xacled_w_fut <- df_fut %>% dplyr::select(acled_w)

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

xrsui_a_w_fut <- xrsui_a_w_fut[1:cut_point,-1] # se elimina la primera columna que es la variable original
xrsui_b_m_fut <- xrsui_b_m_fut[1:cut_point,-1] # se elimina la primera columna que es la variable original
xacled_w_fut <- xacled_w_fut[1:cut_point,-1]   # se elimina la primera columna que es la variable original

nxrsui_a_w_fut <- nxrsui_a_w_fut[1:dim(x = test)[1],-1] # se elimina la primera columna que es la variable original
nxrsui_b_m_fut <- nxrsui_b_m_fut[1:dim(x = test)[1],-1] # se elimina la primera columna que es la variable original
nxacled_w_fut <- nxacled_w_fut[1:dim(x = test)[1],-1]   # se elimina la primera columna que es la variable original

xrsui_a_w_fut <- ts(xrsui_a_w_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)
xrsui_b_m_fut <- ts(xrsui_b_m_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)
xacled_w_fut <- ts(xacled_w_fut, start =c(as.numeric(train[1,"year"]),as.numeric(train[1,"day"])), frequency = 365.35)

nxrsui_a_w_fut <- ts(nxrsui_a_w_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)
nxrsui_b_m_fut <- ts(nxrsui_b_m_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)
nxacled_w_fut <- ts(nxacled_w_fut, start =c(as.numeric(test[1,"year"]),as.numeric(test[1,"day"])), frequency = 365.35)


# LOOP BEST MODEL --------------------------------------------------------------

# Option 1: Model with lags only
for (k in 1:7){ # Loop que comienza con RSUI_A_w (rezagos)
  for (l in 1:7){ # Loop que comienza con RSUI_B_m (rezagos)
    for (m in 1:7){ # Loop que comienza con acled_w (rezagos)
      
      models <- c()
      mse <- c()
      indice <- c()
      
      # Solo los rezagos (lags)
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
      
      # Guardar resultados
      mse <- as.data.frame(mse)
      indice <- as.data.frame(indice)
      mse_results <- cbind(mse_results, mse)
      models_final <- c(models_final, models)
      indice_final <- as.data.frame(c(indice_final, indice))
      
    }
  }
}

# Option 2: Model with leads only
for (p_lead in 1:3){ # Loop que comienza con RSUI_A_w (leads)
  for (q_lead in 1:3){ # Loop que comienza con RSUI_B_m (leads)
    for (r_lead in 1:3){ # Loop que comienza con acled_w (leads)
      
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
for (k in 1:7){ # Loop que comienza con RSUI_A_w (rezagos)
  for (l in 1:7){ # Loop que comienza con RSUI_B_m (rezagos)
    for (m in 1:7){ # Loop que comienza con acled_w (rezagos)
      for (p_lead in 1:3){ # Loop que comienza con RSUI_A_w (leads)
        for (q_lead in 1:3){ # Loop que comienza con RSUI_B_m (leads)
          for (r_lead in 1:3){ # Loop que comienza con acled_w (leads)
            
            models <- c()
            mse <- c()
            indice <- c()
            
            # Combinación de rezagos y leads
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
            
            # Guardar resultados
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

k=2       *# xrsui_a_w original and 1 lag
l=3       *# xrsui_b_m original and 2 lag
m=3       *# xacled_w original and 2 lag
p_lead=2  *# xrsui_a_w_fut 1 lead
q_lead=2  *# xrsui_b_m_fut 1 lead
r_lead=1  *# xacled_w_fut 1 lead
j=4       *# personas_ts original and 4 lag

x <- cbind(xrsui_a_w[,1:k], xrsui_b_m[,1:l], xacled_w[,1:m], 
           xrsui_a_w_fut[,1:p_lead], xrsui_b_m_fut[,1:q_lead], xacled_w_fut[,1:r_lead])
nx <- cbind(nxrsui_a_w[,1:k], nxrsui_b_m[,1:l], nxacled_w[,1:m],
            nxrsui_a_w_fut[,1:p_lead], nxrsui_b_m_fut[,1:q_lead], nxacled_w_fut[,1:r_lead])

model_best <- arima(personas_ts, order = c(j,0,0), xreg = x)
p<- predict(model_best, newxreg = nx )
result_mse_best <- mean((test$personas - p$pred)^2, na.rm = T)
coeficientes <- round(model_best$coef,3)

autoplot(personas_ts, series="Data") + 
  autolayer(p$pred+2*(mean(p$pred, na.rm = T)), series="Forecast", na.rm =T) + 
  autolayer(personas_ts1, series="Real")

accuracy(model_best)

AIC(model_best)

# Extract residuals
residuales <- residuals(model_best)
residuales

# Compute RMSE
rmse <- sqrt(mean(residuales^2, na.rm = TRUE))
rmse

# Extract coefficients and standard errors
coefs <- model_best$coef
se <- sqrt(diag(model_best$var.coef))

# Calculate t-values and p-values
tval <- coefs / se
pval <- round(2 * (1 - pnorm(abs(tval))),4)

# Build data frame
coef_table <- data.frame(
  Variable = names(coefs),
  Coefficient = round(coefs, 4),
  `Std. Error` = round(se, 4),
  `t-value` = round(tval, 3),
  `p-value` = signif(pval, 3)
)

ft <- flextable(coef_table) %>%
  theme_booktabs() %>%
  set_caption("Table A.8.1: Estimated Coefficients of the Best ARIMAX Model for Tranche 2 Protester Data Imputation (2019-10-14 to 2021-02-03)") %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table: Estimated Coefficients of the Best ARIMAX Model for Tranche 2 'Protesters'", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Table/Appendix/TableA7_Coef_ARIMAX1_Protesters.docx")

# Evaluate the model residuals

plot(model_best$residuals)
abline(h=0)

# To evaluate the residuals, we can use the test box
# Ho: Independent residuals
# Ha: No independent residuals
# If p-value < 0.05, Reject Ho

Box.test(model_best$residuals, lag = 1, 
         type = c("Ljung-Box"))

Box.test(model_best$residuals, lag = 2, 
         type = c("Ljung-Box"))

Box.test(model_best4$residuals, lag = 1, 
         type = c("Ljung-Box"))

Box.test(model_best4$residuals, lag = 2, 
         type = c("Ljung-Box"))

# The residuals are independent, since Ho is not rejected

#==============================================================================#
####        Protester Data Imputation (2019-10-14 to 2021-02-03)            ####
#==============================================================================#

inicio1 <- which(df$date == protestas1$date[1]) # Tomo la fecha inicial de donde 
# vuelvo a tener datos en protestas, después de la semilla
fin1 <- 3267 # Tomo una fecha superior entre los datos iniciales, datos vacios y datos finales de informacion para la imputacion

df2 <- df[inicio1:fin1,]
newxreg2 <- df2 %>% dplyr::select(date, RSUI_A_w, RSUI_B_m, acled_w)
new_y <- protestas2$personas[min(which(!is.na(protestas2$personas))):(min(which(!is.na(protestas2$personas)))+11)]
new_y <- ts(new_y, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)

xreg2 <- df[df$date %in% protestas1$date,]
xreg2 <- xreg2 %>% dplyr::select(date,RSUI_A_w, RSUI_B_m, acled_w)

xrsui_a_w1 <- xreg2 %>% dplyr::select(date, RSUI_A_w)
xrsui_b_m1 <- xreg2 %>% dplyr::select(date, RSUI_B_m)
xacled_w1 <- xreg2 %>% dplyr::select(date, acled_w)

for (i in 1:6) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xrsui_a_w1 <- xrsui_a_w1 %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  
  xrsui_b_m1 <- xrsui_b_m1 %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date)
    )
  col_name <- paste("RSUI_A_w", i, sep = "")  
  xacled_w1 <- xacled_w1 %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date)
    )
}

xreg2 <- xreg2 %>% dplyr:: select(-date)
xrsui_a_w1 <- xrsui_a_w1 %>% dplyr:: select(-date)
xrsui_b_m1 <- xrsui_b_m1 %>% dplyr:: select(-date)
xacled_w1 <- xacled_w1 %>% dplyr:: select(-date)

xrsui_a_w1 <- ts(xrsui_a_w1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)         # Decarar df como ts
xrsui_b_m1 <- ts(xrsui_b_m1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)         # Decarar df como ts
xacled_w1 <- ts(xacled_w1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)           # Decarar df como ts

# Leading variables ------------------------------------------------------------

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

xrsui_a_w_fut1 <- ts(xrsui_a_w_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)
xrsui_b_m_fut1 <- ts(xrsui_b_m_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)
xacled_w_fut1 <- ts(xacled_w_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)

x2 <- cbind(xrsui_a_w1[,1:k], xrsui_b_m1[,1:l], xacled_w1[,1:m],
            xrsui_a_w_fut1[,1:p_lead], xrsui_b_m_fut1[,1:q_lead], xacled_w_fut1[,1:r_lead])

model1 <- arima(new_y, order = c(j,0,0), xreg = x2, 
                fixed = coeficientes)


# Modelo previo a la actualizacion para comparar con el modelo ganador

x2_12 <- cbind(xrsui_a_w1[,1:5], xrsui_b_m1[,1:5], xacled_w1[,1:5])

model1_12 <- arima(new_y, order = c(12,0,0), xreg = x2_12, 
                   fixed = coeficientes_12)

#_____________________________________

nxrsui_a_w1 <- newxreg2 %>% dplyr::select(date, RSUI_A_w)
nxrsui_b_m1 <- newxreg2 %>% dplyr::select(date, RSUI_B_m)
nxacled_w1 <- newxreg2 %>% dplyr::select(date, acled_w)

for (i in 1:6) {
  col_name <- paste("RSUI_A_w", i, sep = "")  # Generar el nombre de la columna
  nxrsui_a_w1 <- nxrsui_a_w1 %>%
    mutate(
      !!col_name := lag(RSUI_A_w, n = i, order_by = date)
    )
  col_name <- paste("RSUI_B_m", i, sep = "")  
  nxrsui_b_m1 <- nxrsui_b_m1 %>%
    mutate(
      !!col_name := lag(RSUI_B_m, n = i, order_by = date)
    )
  col_name <- paste("RSUI_A_w", i, sep = "")  
  nxacled_w1 <- nxacled_w1 %>%
    mutate(
      !!col_name := lag(acled_w, n = i, order_by = date)
    )
}


newxreg2 <- newxreg2%>% dplyr:: select(-date)
nxrsui_a_w1 <- nxrsui_a_w1 %>% dplyr:: select(-date)
nxrsui_b_m1 <- nxrsui_b_m1 %>% dplyr:: select(-date)
nxacled_w1 <- nxacled_w1 %>% dplyr:: select(-date)

nxrsui_a_w1 <- ts(nxrsui_a_w1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxrsui_b_m1 <- ts(nxrsui_b_m1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)         # Decarar df como ts
nxacled_w1 <- ts(nxacled_w1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)           # Decarar df como ts

# Leading variables ------------------------------------------------------------

nxrsui_a_w_fut1 <- newxreg2 %>% dplyr::select(RSUI_A_w)
nxrsui_b_m_fut1 <- newxreg2 %>% dplyr::select(RSUI_B_m)
nxacled_w_fut1 <- newxreg2 %>% dplyr::select(acled_w)

for (i in 1:3) {
  col_name <- paste("RSUI_A_w", i, sep = "")  
  nxrsui_a_w_fut1 <- nxrsui_a_w_fut1 %>%
    mutate(
      !!col_name := lead(RSUI_A_w, n = i)
    )  
  col_name <- paste("RSUI_B_m", i, sep = "")  
  nxrsui_b_m_fut1 <- nxrsui_b_m_fut1 %>%
    mutate(
      !!col_name := lead(RSUI_B_m, n = i)
    )  
  col_name <- paste("acled_w", i, sep = "")  
  nxacled_w_fut1 <- nxacled_w_fut1 %>%
    mutate(
      !!col_name := lead(acled_w, n = i)
    )  
}

nxrsui_a_w_fut1 <- nxrsui_a_w_fut1[,-1]
nxrsui_b_m_fut1 <- nxrsui_b_m_fut1[,-1]
nxacled_w_fut1 <- nxacled_w_fut1[,-1]

nxrsui_a_w_fut1 <- ts(nxrsui_a_w_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)
nxrsui_b_m_fut1 <- ts(nxrsui_b_m_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)
nxacled_w_fut1 <- ts(nxacled_w_fut1, start =c(as.numeric(protestas1[1,"year"]),as.numeric(protestas1[1,"day"])), frequency = 365.35)

nx2 <- cbind(nxrsui_a_w1[,1:k], nxrsui_b_m1[,1:l], nxacled_w1[,1:m],
             nxrsui_a_w_fut1[,1:p_lead], nxrsui_b_m_fut1[,1:q_lead], nxacled_w_fut1[,1:r_lead])

p2<- predict(model1, newxreg = nx2 )

autoplot(new_y, series="Data") + 
  autolayer(p2$pred-mean(p2$pred, na.rm = T), series="Forecast") +
  autolayer(personas_ts, series="Real")+
  autolayer(personas_ts1, series="Real")

autoplot(new_y, series="Data") + 
  autolayer(p2$pred-(mean(p2$pred, na.rm = T)-mean(personas_ts1, na.rm = T)), series="Forecast") +
  autolayer(personas_ts, series="Real")+
  autolayer(personas_ts1, series="Real")

pronostico_personas <- as.data.frame(p$pred)


# Imputacion de datos ----------------------------------------------------------

y <- df2 %>% dplyr::select(personas)
y <- ts(y, start =c(as.numeric(protestas1[1,"year"]), as.numeric(protestas1[1,"day"])), frequency = 365.35)         # Declarar df como ts

# Identificar los índices de los valores faltantes
na_indices <- which(is.na(y))

# Inicializar una lista para almacenar las predicciones
predictions <- numeric(length(na_indices))

# Predecir y almacenar las predicciones
for (i in rev(seq_along(na_indices))) {
  idx <- na_indices[i]
  
  # Realizar la predicción con el modelo ARIMAX
  pred <- predict(model1, n.ahead = 1, newxreg = nx2[idx, , drop = FALSE])$pred
  
  # Almacenar la prediccion
  predictions[i] <- pred
  
  # Imputar el valor predicho en 'y', restando la media de las predicciones
  y[idx] <- pred    # Restar la media de las predicciones
}

plot(new_y)

autoplot(new_y, series="Data") + 
  autolayer(y, series="Forecast", na.rm =T) 

y_final_scaled <- y
y_final_scaled[1:length(new_y)] <- new_y

plot(y_final_scaled)

y_final <- y_final_scaled*iqr_train + median_train

plot(y_final)

protestas_imputadas <- data.frame(
  date = df2$date,
  personas_scaled = y_final_scaled,
  personas = y_final
)

# Guardar el data frame en un archivo CSV
write.csv(protestas_imputadas, quote = F, "Datos/Protest/Protesters Imputation - Tranche 2/protestas_imputadas_personas.csv")
