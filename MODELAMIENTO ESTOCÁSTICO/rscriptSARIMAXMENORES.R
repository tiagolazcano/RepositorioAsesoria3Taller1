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



# ==============================================================================
# Análisis nivel pais paso a paso
# ==============================================================================

base <- read_csv("urgenciaIRA.csv", show_col_types = FALSE)
base$fecha <- as.Date(base$fecha, format = "%d/%m/%Y")
base <- base %>% arrange(fecha)

base_pandemia <- base %>%
  mutate(Menores_5 = Menores_1 + De_1_a_4) %>%
  select(fecha, anio, semana, Menores_5)

base_pandemia <- base_pandemia %>%
  mutate(semana = if_else(semana == 53, 52, semana))

base_menores_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Menores_5, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

sum(base_menores_pandemia$Casos)

ts_menores_pandemia <- ts(base_menores_pandemia$Casos, frequency = 52)

var(ts_menores_pandemia)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_menores_pandemia))
ts_train <- subset(ts_menores_pandemia, end = n_train)
ts_test  <- subset(ts_menores_pandemia, start = n_train + 1)

# Graficamos

df_plot <- base_menores_pandemia %>%
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

###### Transformación de Box #####

base_menores_pandemia$Casos =  log(base_menores_pandemia$Casos)
var(base_menores_pandemia$Casos)

ts_menores_pandemia <- ts(base_menores_pandemia$Casos, frequency = 52)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_menores_pandemia))
ts_train <- subset(ts_menores_pandemia, end = n_train)
ts_test  <- subset(ts_menores_pandemia, start = n_train + 1)

# Graficamos

df_plot <- base_menores_pandemia %>%
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


# Descomposición

decomp_obj <- decompose(ts_menores_pandemia, type = "additive")

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

acf(ts_train,lag.max=156, main = 'ACF de los residuos de la serie temporal')
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

### Gana d = 1, D = 0?

base_pandemia <- base %>%
  mutate(Menores_5 = Menores_1 + De_1_a_4) %>%
  select(fecha, anio, semana, Menores_5)

base_pandemia <- base_pandemia %>%
  mutate(semana = if_else(semana == 53, 52, semana))

base_menores_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Menores_5, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_menores_pandemia <- ts(base_menores_pandemia$Casos, frequency = 52)

# Datos de entrenamiento

n_train <- floor(0.80 * length(ts_menores_pandemia))
ts_train <- subset(ts_menores_pandemia, end = n_train)
ts_test  <- subset(ts_menores_pandemia, start = n_train + 1)

t <- 1:n_train
dummy_pandemia <- rep(0, n_train)
dummy_pandemia[169:270] <- 1
dummy_interaccion_train = dummy_pandemia * t
matriz_xreg_train <- cbind(dummy_pandemia, dummy_interaccion_train)
colnames(matriz_xreg_train) <- c("Nivel", "Tendencia") 

ts.plot(as.vector(diff(ts_menores_pandemia)), 
        gpars = list(
          main = "Evolución de la Serie con Periodo Pandemia",
          ylab = "Cantidad de Atenciones",
          xlab = "Semanas (Índice)",
          col = "darkblue",
          lwd = 2 
        ))

ts.plot(as.vector(ts_menores_pandemia), 
        gpars = list(
          main = "Evolución de la Serie con Periodo Pandemia",
          ylab = "Cantidad de Atenciones",
          xlab = "Semanas (Índice)",
          col = "darkblue",
          lwd = 2 
        ))

abline(v = c(169, 270), col = "red", lty = 2, lwd = 2)

fit_sarima_optimo <- auto.arima(ts_train, xreg = matriz_xreg_train, lambda = 0,
                                stepwise = FALSE,
                                d = 1, D = 0)

fit_sarima_optimo <- Arima(ts_train,
                           order = c(2, 1, 2),
                           seasonal = c(1, 0, 0),
                           xreg = matriz_xreg_train,
                           lambda = 0,
                           include.constant = FALSE) 
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


length(residuos)
t.test(residuos)
jarque.bera.test(residuos)
t_opt <- 1:length(residuos)
bptest(residuos ~ t_opt)
ArchTest(residuos, lags = 1)

# Predicción y validación

h_forecast <- length(ts_test)
nuevos_xreg <- matrix(0, nrow = h_forecast, ncol = 2)
colnames(nuevos_xreg) <- c("Nivel", "Tendencia") 

fc_final <- forecast(fit_sarima_optimo, 
                     h = h_forecast, 
                     xreg = nuevos_xreg)

autoplot(fc_final, PI = FALSE) +  # <--- AQUÍ ESTÁ EL TRUCO
  autolayer(ts_test, series="Datos Reales", size=1) +
  ggtitle("Pronóstico Puntual (Sin Bandas Confidenciales)") +
  ylab("Atenciones") +
  theme_minimal() +
  scale_color_manual(values = c("black", "orange")) # Opcional: ajustar colores

plot_clean <- autoplot(fc_final, PI = FALSE) + 
  autolayer(ts_test, series="Real", size=0.8) +
  ggtitle("A. Comparación Directa (Predicción vs Realidad)") +
  theme_minimal() +
  coord_cartesian(xlim = c(time(ts_test)[1], tail(time(ts_test),1))) # Zoom al final

# 2. Gráfico B: Solo las bandas (para análisis de riesgo)
plot_bands <- autoplot(fc_final) +
  ggtitle("B. Niveles de Incertidumbre (Bandas de Confianza)") +
  ylab("Rango posible") +
  theme_minimal() + 
  theme(legend.position = "none") # Quitamos leyenda para limpiar

library(gridExtra)

# 3. Mostrar ambos juntos
grid.arrange(plot_clean, plot_bands, nrow = 2)

## Validación

h_total <- length(ts_test)
xreg_futuro <- matrix(0, nrow = h_total, ncol = 2)
colnames(xreg_futuro) <- c("Nivel", "Tendencia")

fc_validacion <- forecast(fit_sarima_optimo, 
                          h = h_total, 
                          xreg = xreg_futuro)

# Paso C: Medir precisión
print(accuracy(fc_validacion, ts_test))

###### FINAL

t <- 1:length(ts_menores_pandemia)
dummy_pandemia <- rep(0, length(ts_menores_pandemia))
dummy_pandemia[169:270] <- 1

dummy_interaccion <- dummy_pandemia * t
matriz_xreg_total <- cbind(Nivel = dummy_pandemia, Tendencia = dummy_interaccion)


fit_final <- Arima(ts_menores_pandemia,
                   order = c(2, 1, 2),        # Parte no estacional (p, d, q)
                   seasonal = c(1, 0, 0),     # Parte estacional (P, D, Q)
                   xreg = matriz_xreg_total,        # Tus variables dummy
                   lambda = 0,                # Transformación Logarítmica
                   include.constant = FALSE)  # Generalmente FALSE si d=1



summary(fit_final)


h_futuro <- 12
xreg_futuro_12 <- matrix(0, nrow = h_futuro, ncol = 2)
colnames(xreg_futuro_12) <- c("Nivel", "Tendencia")

fc_futuro <- forecast(fit_final, 
                      h = h_futuro, 
                      xreg = xreg_futuro_12)


print(fc_futuro)
autoplot(fc_futuro, main = "")


