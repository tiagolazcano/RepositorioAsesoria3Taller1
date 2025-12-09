library(dplyr)
library(tidyr)
library(xts)
library(ggplot2)
library(lubridate)
library(forecast)
library(tidyverse)
library(zoo)
library(scales) # Para formato de ejes
library(knitr)
library(urca)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tseries)
library(fUnitRoots)
library(lmtest)
library(forecast)
library(MLmetrics)
library(psych)
library(nortest)
library(lmtest)
library(FinTS)


### Descriptivo 

base <- read_csv("urgenciaIRA.csv", show_col_types = FALSE)
base$fecha <- as.Date(base$fecha, format = "%d/%m/%Y")
base <- base %>% arrange(fecha)

base_pandemia <- base %>%
  select(fecha, anio, semana, Total) 

base_pais_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_pais_pandemia <- ts(base_pais_pandemia$Casos, frequency = 52)

plot(decompose(ts_pais_pandemia))

min(base_pais_pandemia$Casos)
which.min(base_pais_pandemia$Casos)

max(base_pais_pandemia$Casos)
which.max(base_pais_pandemia$Casos)

temp = base_pais_pandemia %>%
  filter(anio == 2022)
min(temp$Casos)
which.min(temp$Casos)

max(temp$Casos)
which.max(temp$Casos)

ts.plot(temp)

###### Lactantes #######

base_lactantes <- base %>%
  select(fecha, anio, semana, Menores_1) 

base_pais_lactantes <- base_lactantes %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Menores_1, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

temp = base_pais_preescolar %>%
  filter(anio == 2020)

which.min(temp$Casos)

max(base_pais_lactantes$Casos)
which.max(base_pais_lactantes$Casos)
min(base_pais_lactantes$Casos)
which.min(base_pais_lactantes$Casos)


###### Preescolar #######

base_preescolar <- base %>%
  select(fecha, anio, semana, De_1_a_4) 

base_pais_preescolar <- base_preescolar %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(De_1_a_4, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

temp = base_pais_preescolar %>%
  filter(anio == 2019)

which.max(temp$Casos)

max(base_pais_preescolar$Casos)
which.max(base_pais_preescolar$Casos)
min(base_pais_preescolar$Casos)
which.min(base_pais_preescolar$Casos)

###### Adultos mayores #######

base_abuelos <- base %>%
  select(fecha, anio, semana, De_65_y_mas) 

base_pais_abuelos <- base_abuelos %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(De_65_y_mas, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

temp = base_pais_abuelos %>%
  filter(anio == 2021)

which.max(temp$Casos)

which.min(temp$Casos)

max(base_pais_abuelos$Casos)
which.max(base_pais_abuelos$Casos)
min(base_pais_abuelos$Casos)
which.min(base_pais_abuelos$Casos)

ts.plot(base_pais_abuelos$Casos)

# ==============================================================================
# Análisis nivel pais paso a paso
# ==============================================================================

base <- read_csv("urgenciaIRA.csv", show_col_types = FALSE)
base$fecha <- as.Date(base$fecha, format = "%d/%m/%Y")
base <- base %>% arrange(fecha)

inicio_pandemia <- as.Date("2020-01-01")
fin_pandemia    <- as.Date("2021-12-31")

base_nopandemia <- base %>%
  filter(!(fecha >= inicio_pandemia & fecha <= fin_pandemia)) %>%
  select(fecha, anio, semana, Total) 

base_pais_nopandemia <- base_nopandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_pais_nopandemia <- ts(base_pais_nopandemia$Casos, frequency = 52)

var(ts_pais_nopandemia)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_pais_nopandemia))
ts_train <- subset(ts_pais_nopandemia, end = n_train)
ts_test  <- subset(ts_pais_nopandemia, start = n_train + 1)

# Graficamos

df_plot <- base_pais_nopandemia %>%
  mutate(Index = row_number()) %>% 
  mutate(Conjunto = ifelse(Index <= n_train, "Entrenamiento (80%)", "Prueba (20%)"))

# 2. Generamos el gráfico
ggplot(df_plot, aes(x = Index, y = Casos)) +
  # A. Línea de fondo gris (para conectar visualmente el corte)
  geom_line(color = "grey85", size = 0.8) +
  
  # B. Línea principal coloreada por grupo
  geom_line(aes(color = Conjunto), size = 1) +
  
  # C. Línea vertical que marca el corte exacto
  geom_vline(xintercept = n_train, linetype = "dashed", color = "black", alpha = 0.6) +
  
  # D. Estética y Colores (Azul corporativo y Naranja contraste)
  scale_color_manual(values = c("Entrenamiento (80%)" = "#0072B2", "Prueba (20%)" = "#D55E00")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  
  # E. Etiquetas profesionales
  labs(title = "Serie Temporal A Nivel País",
       subtitle = "Partición para Validación Cruzada",
       y = "Casos Totales",
       x = "Semanas (Índice Temporal)",
       color = NULL) + # NULL quita el título de la leyenda para que quede más limpio
  
  # F. Tema limpio
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  )

var(base_pais_nopandemia$Casos)

###### Transformación de Box #####

base_pais_nopandemia$Casos =  log(base_pais_nopandemia$Casos)
var(base_pais_nopandemia$Casos)

ts_pais_nopandemia <- ts(base_pais_nopandemia$Casos, frequency = 52)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_pais_nopandemia))
ts_train <- subset(ts_pais_nopandemia, end = n_train)
ts_test  <- subset(ts_pais_nopandemia, start = n_train + 1)

# Graficamos

df_plot <- base_pais_nopandemia %>%
  mutate(Index = row_number()) %>% 
  mutate(Conjunto = ifelse(Index <= n_train, "Entrenamiento (80%)", "Prueba (20%)"))

# 2. Generamos el gráfico
ggplot(df_plot, aes(x = Index, y = Casos)) +
  # A. Línea de fondo gris (para conectar visualmente el corte)
  geom_line(color = "grey85", size = 0.8) +
  
  # B. Línea principal coloreada por grupo
  geom_line(aes(color = Conjunto), size = 1) +
  
  # C. Línea vertical que marca el corte exacto
  geom_vline(xintercept = n_train, linetype = "dashed", color = "black", alpha = 0.6) +
  
  # D. Estética y Colores (Azul corporativo y Naranja contraste)
  scale_color_manual(values = c("Entrenamiento (80%)" = "#0072B2", "Prueba (20%)" = "#D55E00")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  
  # E. Etiquetas profesionales
  labs(title = "Serie Temporal A Nivel País",
       subtitle = "Partición para Validación Cruzada",
       y = "Casos Totales",
       x = "Semanas (Índice Temporal)",
       color = NULL) + # NULL quita el título de la leyenda para que quede más limpio
  
  # F. Tema limpio
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  )

var(base_pais_nopandemia$Casos)

# Descomposición

decomp_obj <- decompose(ts_pais_nopandemia, type = "additive")

autoplot(decomp_obj) +
  geom_line(color = "#0072B2", size = 0.7) +
  labs(title = "Descomposición Aditiva de la Serie (Nivel Nacional)",
       subtitle = "Desglose en: Tendencia, Estacionalidad y Residuo",
       x = "Semanas Epidemiológicas",
       y = "Componentes") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold")
  )

# ANALIZAR LA SERIE
# ANALIZAR LA SERIE
# CLARAMENTE TIENE UN COMPORTAMIENTO ADITIVO

### Análisis de ACF, PACF y Raíz unitaria ###

# d = 0, D = 0 #

acf(ts_train,lag.max=156)
pacf(ts_train,lag.max=156)
sd(ts_train)
var(ts_train)
adfTest(ts_train, lag = 1, type = c("nc"))
adfTest(ts_train, lag = 52, type = c("nc"))

# d = 1, D = 0 #

acf(diff(ts_train),lag.max=156)
pacf(diff(ts_train),lag.max=156)
sd(diff(ts_train))
adfTest(diff(ts_train), lag = 1, type = c("nc"))
adfTest(diff(ts_train), lag = 52, type = c("nc"))

# d = 0, D = 1 #

acf(diff(ts_train,52),lag.max=156)
pacf(diff(ts_train,52),lag.max=156)
sd(diff(ts_train,52))
adfTest(diff(ts_train,52), lag = 1, type = c("nc"))
adfTest(diff(ts_train,52), lag = 52, type = c("nc"))

# d = 1, D = 1 #

acf(diff(diff(ts_train,52)),lag.max=156)
pacf(diff(diff(ts_train,52)),lag.max=156)
sd(diff(diff(ts_train,52)))
adfTest(diff(diff(ts_train,52)), lag = 1, type = c("nc"))
adfTest(diff(diff(ts_train,52)), lag = 52, type = c("nc"))

# d = 2, D = 0 $

acf(diff(diff(ts_train)),lag.max=156)
pacf(diff(diff(ts_train)),lag.max=156)
sd(diff(diff(ts_train)))
adfTest(diff(diff(ts_train)), lag = 1, type = c("nc"))
adfTest(diff(diff(ts_train)), lag = 52, type = c("nc"))

### Gana d = 1, D = 0

base_nopandemia <- base %>%
  filter(!(fecha >= inicio_pandemia & fecha <= fin_pandemia)) %>%
  select(fecha, anio, semana, Total) 

base_pais_nopandemia <- base_nopandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_pais_nopandemia <- ts(base_pais_nopandemia$Casos, frequency = 52)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_pais_nopandemia))
ts_train <- subset(ts_pais_nopandemia, end = n_train)
ts_test  <- subset(ts_pais_nopandemia, start = n_train + 1)

fit_sarima_optimo <- auto.arima(ts_train, 
                                d = 1, D = 0, seasonal = TRUE,
                                stepwise = FALSE, approximation = FALSE,
                                lambda = 0)

fit_sarima_optimo <- auto.arima(ts_train, 
                                d = 1, D = 0, seasonal = TRUE,
                                stepwise = FALSE, approximation = FALSE,
                                lambda = 0) # <--- CAMBIO AQUÍ

fit_sarima_optimo
summary(fit_sarima_optimo)
checkresiduals(fit_sarima_optimo)

length(ts_train)

residuos <- residuals(fit_sarima_optimo)

autoplot(residuos) +
  geom_line(color = "#2c3e50", size = 0.6) +
  geom_hline(yintercept = 0, color = "firebrick", linetype = "dashed") +
  labs(y = "Residuos", title = "1. Residuos Estandarizados") +
  theme_bw()

ggAcf(residuos, lag.max = 249) +
  labs(title = "2. Función de Autocorrelación (ACF) de Residuos") +
  theme_bw()

lb_pvalues <- sapply(1:249, function(k) Box.test(residuos, lag = k, type = "Ljung-Box")$p.value)
df_lb <- data.frame(Lag = 1:249, Pvalue = lb_pvalues)

ggplot(df_lb, aes(x = Lag, y = Pvalue)) +
  geom_point(color = "#0072B2", size = 1.5) +
  geom_hline(yintercept = 0.05, color = "firebrick", linetype = "dashed") +
  annotate("text", x = 10, y = 0.1, label = "Límite de significancia (0.05)", color = "firebrick", size = 3) +
  labs(y = "P-Valor", title = "3. Test de Ljung-Box (Valores sobre la línea roja indican ruido blanco significativo)") +
  theme_bw()


t.test(residuos)
lillie.test(residuos)
t_opt <- 1:length(residuos)
bptest(residuos ~ t_opt)

# Predicción y validación
length(ts_test)
h_forecast <- 63
fc_final <- forecast(fit_sarima_optimo, h = h_forecast) 
ts_test_65 <- window(ts_test, end = time(ts_test)[h_forecast])

color_prediccion <- "#1B9E77" 
color_validacion <- "#D95F02" 
color_entrenamiento <- "gray40" 

df_fc <- as.data.frame(fc_final)
df_fc <- df_fc %>%
  rename(Lo95 = `Lo 95`, Hi95 = `Hi 95`, Lo80 = `Lo 80`, Hi80 = `Hi 80`, PointForecast = `Point Forecast`)

# Calcular el índice X del último punto de la predicción
# Usamos el tiempo (índice) del último punto de la predicción y le añadimos un pequeño margen (0.1)
max_x_limit <- max(time(fc_final$x)) + max(time(fc_final$mean)) - min(time(fc_final$mean)) + 0.1 

df_fc$Index <- time(fc_final$mean)

grafico <- ggplot() +
  
  autolayer(fc_final$x, series = "Datos de Entrenamiento", color = color_entrenamiento, size = 0.8) +
  
  # Bandas de Predicción (Ribbons)
  geom_ribbon(data = df_fc, aes(x = Index, ymin = Lo95, ymax = Hi95), fill = color_prediccion, alpha = 0.15) +
  geom_ribbon(data = df_fc, aes(x = Index, ymin = Lo80, ymax = Hi80), fill = color_prediccion, alpha = 0.25) +
  
  # Línea de Predicción Puntual
  geom_line(data = df_fc, aes(x = Index, y = PointForecast, color = "Predicción SARIMA"), size = 1.2) +
  
  # Datos Reales de Validación (Overlap)
  autolayer(ts_test_65, series = "Datos Reales (Validación)", size = 1.2) +
  
  # CORRECCIÓN: Forzar límites del eje X y Y
  coord_cartesian(ylim = c(0, 120000), xlim = c(min(time(fc_final$x)), max_x_limit)) +
  
  scale_color_manual(values = c("Datos de Entrenamiento" = color_entrenamiento, 
                                "Predicción SARIMA" = color_prediccion,
                                "Datos Reales (Validación)" = color_validacion)) + 
  
  labs(title = "Pronóstico SARIMA y Validación (63 Semanas)",
       subtitle = "El pronóstico de 63 semanas se compara con el conjunto de validación",
       y = "Casos Totales (IRA)",
       x = "Índice Temporal",
       color = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(grafico)


## Validación

fc_validacion <- forecast(fit_sarima_optimo, h = length(ts_test))
print(accuracy(fc_validacion, ts_test))

fc_validacion <- forecast(fit_sarima_optimo, h = 12)
print(accuracy(fc_validacion, ts_test))

###### FINAL

fit_final <- Arima(
  ts_pais_nopandemia,
  order = c(1, 1, 1),
  seasonal = c(1, 0, 0),
  lambda = 0
)

summary(fit_final)

fc_futuro <- forecast(fit_final, h = 12)

# Adaptando el forecast a la serie original

base_pandemia <- base %>%
  select(fecha, anio, semana, Total) 

base_pais_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(
    Casos = sum(Total, na.rm = TRUE),
    fecha = min(fecha), 
    .groups = "drop"
  ) %>%
  arrange(fecha)

ultima_fecha <- max(base_pais_pandemia$fecha)

fechas_nuevas <- seq(from = ultima_fecha + 7, by = "week", length.out = 12)

df_futuro <- data.frame(
  fecha = fechas_nuevas,
  Casos = as.numeric(fc_futuro$mean),
  Bajo  = as.numeric(fc_futuro$lower[,2]),
  Alto  = as.numeric(fc_futuro$upper[,2])
)

ggplot() +
  geom_line(data = base_pais_pandemia, aes(x = fecha, y = Casos), color = "black") +
  geom_ribbon(data = df_futuro, aes(x = fecha, ymin = Bajo, ymax = Alto), 
              fill = "blue", alpha = 0.2) +
  geom_line(data = df_futuro, aes(x = fecha, y = Casos), color = "blue", size = 1) +
  labs(title = "Proyección Nacional IRA Alta", 
       subtitle = "Pronóstico Modelo Suturado (Box-Cox)",
       y = "Número de Casos", x = "Año") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw()

#############

base_pandemia <- base %>%
  select(fecha, anio, semana, Total) 

base_pais_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_pais_pandemia <- ts(base_pais_pandemia$Casos, frequency = 52)


n_train <- floor(0.80 * length(ts_pais_pandemia))
ts_train <- subset(ts_pais_pandemia, end = n_train)
ts_test  <- subset(ts_pais_pandemia, start = n_train + 1)
n_train_actual <- length(ts_train)
dummy_pandemia_train <- rep(0, n_train_actual)
indice_inicio_pandemia <- 157
indice_fin_pandemia <- 261
dummy_pandemia_train[indice_inicio_pandemia : pmin(n_train_actual, indice_fin_pandemia)] <- 1

fit_sarima_optimo <- auto.arima(
  ts_train, 
  xreg = dummy_pandemia_train,
  lambda = 0, seasonal = TRUE, approximation = FALSE
)
