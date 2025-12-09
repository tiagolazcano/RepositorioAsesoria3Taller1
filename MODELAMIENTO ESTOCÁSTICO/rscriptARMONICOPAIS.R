library(dplyr)
library(tidyr)
library(xts)
library(ggplot2)
library(lubridate)
library(forecast)
library(tidyverse)
library(zoo)
library(scales)
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
library(moments)

########################################

base <- read_csv("urgenciaIRA.csv", show_col_types = FALSE)
base$fecha <- as.Date(base$fecha, format = "%d/%m/%Y")
base <- base %>% arrange(fecha)

base_pandemia <- base %>%
  select(fecha, anio, semana, Total) %>%
  mutate(semana = if_else(semana == 53, 52, semana))

base_pais_pandemia <- base_pandemia %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  arrange(anio, semana)

ts_pais_pandemia <- ts(base_pais_pandemia$Casos, start = c(2017, 1), frequency = 52)

n_train <- floor(0.80 * length(ts_pais_pandemia))
ts_train <- subset(ts_pais_pandemia, end = n_train)
ts_test  <- subset(ts_pais_pandemia, start = n_train + 1)

t_train <- 1:n_train
t2_train <- t_train^2

dummy_pandemia <- rep(0, n_train)
dummy_pandemia[169:270] <- 1 
dummy_interaccion_train <- dummy_pandemia * t_train

n.armonicos = 5

armonicos_train <- fourier(ts_train, K = n.armonicos)

datos_train <- data.frame(
  Casos = as.numeric(ts_train),
  t = t_train, 
  t2 = t2_train,
  dummy_pandemia = dummy_pandemia,
  dummy_interaccion = dummy_interaccion_train,
  armonicos_train
)

modelo_log <- lm(log(Casos) ~ dummy_pandemia + dummy_interaccion + 
                   S1.52 + C1.52 + S2.52 + C2.52  + 
                   S4.52 + C4.52 + S5.52 + C5.52, 
                 data = datos_train)

summary(modelo_log)

### Supuestos residuos

residuos = modelo_log$residuals

t.test(residuos)
jarque.test(residuos)
t_opt <- 1:length(residuos)
bptest(residuos~t_opt)
dwtest(residuos~t_opt)
acf(residuos, lag =)

lb_pvalues <- sapply(1:249, function(k) Box.test(residuos, lag = k, type = "Ljung-Box")$p.value)
df_lb <- data.frame(Lag = 1:249, Pvalue = lb_pvalues)

ggplot(df_lb, aes(x = Lag, y = Pvalue)) +
  geom_point(color = "#0072B2", size = 1.5) +
  geom_hline(yintercept = 0.05, color = "firebrick", linetype = "dashed") +
  annotate("text", x = 10, y = 0.1, label = "Límite de significancia (0.05)", color = "firebrick", size = 3) +
  labs(y = "P-Valor", title = "3. Test de Ljung-Box (Valores sobre la línea roja indican ruido blanco significativo)") +
  theme_bw()

### Validación

prediccion_ajustada <- exp(fitted(modelo_log))
plot(ts_train, main = "Ajuste Modelo Log-Lineal (Train)", ylab = "Casos", lwd=1)
lines(ts(prediccion_ajustada, start=start(ts_train), frequency = 52), col = "red", lwd = 2)

n_test <- length(ts_test)
t_test <- (n_train + 1):(n_train + n_test)

armonicos_test <- fourier(ts_train, K = n.armonicos, h = n_test)
armonicos_test = armonicos_test[, -c(5, 6)]

dummy_pandemia_test <- rep(0, n_test)
dummy_interaccion_test <- dummy_pandemia_test * t_test

newdata_test <- data.frame(
  dummy_pandemia = dummy_pandemia_test,
  dummy_interaccion = dummy_interaccion_test,
  armonicos_test
)

pred_log <- predict(modelo_log, newdata = newdata_test)
pred_real <- ts(exp(pred_log), start = start(ts_test), frequency = 52)
pred_train_ts <- ts(prediccion_ajustada, start = start(ts_train), frequency = 52)

objeto_forecast <- structure(list(
  mean = pred_real,
  x = ts_train,
  fitted = pred_train_ts
), class = "forecast")

accuracy(objeto_forecast, ts_test)

par(mfrow=c(2,2))
plot(ts_pais_pandemia, main="Ajuste Global (Validación)", ylab="Casos", col="gray")
lines(ts(exp(fitted(modelo_log)), start=start(ts_train), frequency=52), col="blue")
lines(pred_real, col="red", lwd=2)
legend("topleft", c("Real", "Train", "Test"), col=c("gray","blue","red"), lty=1, cex=0.7)

residuos <- residuals(modelo_log)
plot(residuos, main="Residuos en el tiempo", ylab="Error Log")
abline(h=0, col="red")
hist(residuos, main="Distribución de Errores", freq=FALSE, breaks=20)
curve(dnorm(x, mean=mean(residuos), sd=sd(residuos)), add=TRUE, col="blue")
plot(fitted(modelo_log), residuals(modelo_log), main="Residuos vs Predicción")
abline(h=0, col="red")
par(mfrow=c(1,1))


### Predicción

n_total <- length(ts_pais_pandemia)
t_total <- 1:n_total

armonicos_total <- fourier(ts_pais_pandemia, K = n.armonicos)
armonicos_total = armonicos_total[, -c(5, 6)]

dummy_pandemia_total <- rep(0, n_total)
dummy_pandemia_total[169:270] <- 1 
dummy_interaccion_total <- dummy_pandemia_total * t_total

datos_total <- data.frame(
  Casos = as.numeric(ts_pais_pandemia),
  dummy_pandemia = dummy_pandemia_total,
  dummy_interaccion = dummy_interaccion_total,
  armonicos_total
)

modelo_final <- lm(log(Casos) ~ ., data = datos_total)

summary(modelo_final)

h <- 52 
t_futuro <- (n_total + 1):(n_total + h)

armonicos_futuro <- fourier(ts_pais_pandemia, K = n.armonicos, h = h)
armonicos_futuro = armonicos_futuro[, -c(5, 6)]

dummy_pandemia_futuro <- rep(0, h) 
dummy_interaccion_futuro <- dummy_pandemia_futuro * t_futuro

newdata_futuro <- data.frame(
  dummy_pandemia = dummy_pandemia_futuro,
  dummy_interaccion = dummy_interaccion_futuro,
  armonicos_futuro
)

pred_log_futuro <- predict(modelo_final, newdata = newdata_futuro, interval = "prediction", level = 0.95)
pred_real_futuro <- exp(pred_log_futuro)

ts_fit <- ts(pred_real_futuro[, "fit"], start = end(ts_pais_pandemia) + c(0, 1)/52, frequency = 52)
ts_lwr <- ts(pred_real_futuro[, "lwr"], start = end(ts_pais_pandemia) + c(0, 1)/52, frequency = 52)
ts_upr <- ts(pred_real_futuro[, "upr"], start = end(ts_pais_pandemia) + c(0, 1)/52, frequency = 52)

plot(ts_pais_pandemia, xlim = c(2017, 2026), ylim = c(0, 165000), 
     main = "Proyección Total Nacional (Próximas 52 Semanas)", 
     ylab = "Casos Totales", xlab = "Año")

lines(ts(exp(fitted(modelo_final)), start = start(ts_pais_pandemia), frequency = 52), col = "blue")
lines(ts_fit, col = "red", lwd = 2)

lines(ts_lwr, col = "red", lty = 2)
lines(ts_upr, col = "red", lty = 2)
legend("topleft", c("Histórico", "Ajuste Modelo", "Proyección", "IC 95%"), 
       col = c("black", "blue", "red", "red"), lty = c(1, 1, 1, 2), cex = 0.8)

