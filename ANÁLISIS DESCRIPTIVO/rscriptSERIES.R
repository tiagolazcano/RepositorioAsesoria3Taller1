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

base = read_csv("urgenciaIRA.csv")
base$fecha <- as.Date(base$fecha, format = "%d/%m/%Y")
base <- base %>% arrange(fecha)

base <- base %>%
  mutate(
    # IDs y glosas como texto
    IdEstablecimiento = as.character(IdEstablecimiento),
    NEstablecimiento = as.character(NEstablecimiento),
    GlosaCausa = as.character(GlosaCausa),
    GLOSATIPOESTABLECIMIENTO = as.character(GLOSATIPOESTABLECIMIENTO),
    GLOSATIPOATENCION = as.character(GLOSATIPOATENCION),
    GlosaTipoCampana = as.character(GlosaTipoCampana),
    
    # Variables numéricas
    IdCausa = as.integer(IdCausa),
    Total = as.integer(Total),
    Menores_1 = as.integer(Menores_1),
    De_1_a_4 = as.integer(De_1_a_4),
    De_5_a_14 = as.integer(De_5_a_14),
    De_15_a_64 = as.integer(De_15_a_64),
    De_65_y_mas = as.integer(De_65_y_mas),
    semana = as.integer(semana),
    CodigoRegion = as.integer(CodigoRegion),
    anio = as.integer(anio),
    
    # Fechas corregidas
    fecha = as.Date(fecha)
  )

datos_long <- base %>%
  select(CodigoRegion, anio, semana, Total, Menores_1, De_1_a_4, De_5_a_14, De_15_a_64, De_65_y_mas) %>%
  pivot_longer(
    cols = c("Total", "Menores_1", "De_1_a_4", "De_5_a_14", "De_15_a_64", "De_65_y_mas"),
    names_to = "Grupo_Etario",
    values_to = "Casos"
  ) %>%
  # Agregación Semanal
  group_by(CodigoRegion, anio, semana, Grupo_Etario) %>%
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
  # --- AQUÍ SE CREA LA FECHA (Requiere lubridate) ---
  mutate(
    Fecha_Base = make_date(anio, 1, 1),
    Fecha_Temporal = Fecha_Base + weeks(semana - 1),
    Fecha_Semana = floor_date(Fecha_Temporal, unit = "week", week_start = 1)
  ) %>%
  select(-Fecha_Base, -Fecha_Temporal)

mapa_edad <- c(
  "Menores_1" = "Menores de 1 año",
  "De_1_a_4" = "Niños 1 a 4 años", # Asumiendo 1-5 solicitado corresponde a la var 1-4
  "De_65_y_mas" = "65 años y más"
)

############### SEMANAL NIVEL PAIS ###############

df_pais <- datos_long %>%
  filter(Grupo_Etario == "Total") %>%
  group_by(Fecha_Semana) %>%
  summarise(Casos = sum(Casos))

ggplot(df_pais, aes(x = Fecha_Semana, y = Casos)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_area(fill = "#2c3e50", alpha = 0.1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
  labs(title = "Evolución Semanal de Casos IRA - Total País",
       subtitle = "Agregado Nacional (Todas las edades)",
       x = "Fecha (Semanas)", y = "N° de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### SEMANAL NIVEL REGIONAL (RM, COQUIMBO Y AYSEN) ###############

target_regions <- c(4, 11, 13)

mapa_regiones <- c(
  "4" = "Coquimbo (IV)",
  "11" = "Aysén (XI)",
  "13" = "Metropolitana (RM)"
)

df_regiones <- datos_long %>%
  filter(Grupo_Etario == "Total", CodigoRegion %in% target_regions) %>%
  mutate(Nombre_Region = map_chr(as.character(CodigoRegion), ~mapa_regiones[.x]))

ggplot(df_regiones, aes(x = Fecha_Semana, y = Casos, color = Nombre_Region)) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~Nombre_Region, scales = "free_y", ncol = 1) +
  labs(title = "Dinámica Regional Comparada",
       subtitle = "Series Semanales (Escalas Y independientes para visualizar tendencia)",
       x = "Fecha", y = "Casos Semanales", color = "Región") +
  theme_bw() +
  theme(legend.position = "none")



############### SEMANAL NIVEL PAIS GRUPOS DE RIESGO ###############

target_ages <- c("Menores_1", "De_1_a_4", "De_65_y_mas")

df_edad_pais <- datos_long %>%
  filter(Grupo_Etario %in% target_ages) %>%
  group_by(Fecha_Semana, Grupo_Etario) %>%
  summarise(Casos = sum(Casos)) %>%
  mutate(Nombre_Edad = map_chr(Grupo_Etario, ~mapa_edad[.x]))

ggplot(df_edad_pais, aes(x = Fecha_Semana, y = Casos, color = Nombre_Edad)) +
  geom_line(size = 1) +
  labs(title = "Curvas por Grupo Etario (Nivel País)",
       subtitle = "Comparación de magnitud y estacionalidad en grupos de riesgo",
       x = "Fecha", y = "Casos", color = "Grupo Etario") +
  theme_minimal() +
  theme(legend.position = "bottom")

############### SEMANAL NIVEL GRUPO ETARIO X REGION ###############

df_matriz <- datos_long %>%
  filter(CodigoRegion %in% target_regions, Grupo_Etario %in% target_ages) %>%
  mutate(
    Nombre_Region = factor(map_chr(as.character(CodigoRegion), ~mapa_regiones[.x]), 
                           levels = c("Metropolitana (RM)", "Coquimbo (IV)", "Aysén (XI)")),
    Nombre_Edad = factor(map_chr(Grupo_Etario, ~mapa_edad[.x]),
                         levels = c("Menores de 1 año", "Niños 1 a 4 años", "65 años y más"))
  )

ggplot(df_matriz, aes(x = Fecha_Semana, y = Casos)) +
  geom_line(color = "#c0392b", size = 0.7) +
  # Facet Grid cruza las dos variables: Regiones en filas, Edades en columnas
  facet_grid(Nombre_Region ~ Nombre_Edad, scales = "free_y") +
  scale_x_date(date_labels = "'%y") +
  labs(title = "Matriz de Vigilancia: Región vs Grupo Etario",
       subtitle = "Evolución semanal detallada (Filas: Regiones | Columnas: Edades)",
       x = "Año", y = "Casos") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#ecf0f1"),
        strip.text = element_text(face = "bold"))

# --- TABLA RESUMEN ESTADÍSTICO ---
tabla_resumen <- df_regiones %>%
  group_by(Nombre_Region, anio) %>%
  summarise(
    Total_Anual = sum(Casos),
    Promedio_Semanal = round(mean(Casos), 1),
    Peak_Maximo = max(Casos),
    Semana_Del_Peak = semana[which.max(Casos)], # Te dice en qué semana ocurrió el peak
    Desviacion_Estandar = round(sd(Casos), 1)
  )

# Visualizar la tabla (puedes usar kable() en el informe para que se vea bonita)
print(tabla_resumen)

######################### BOXPLOT DE ESTACIONALIDAD (CORREGIDO) #########################

# --- 1. CONFIGURACIÓN ---
region_elegida <- 13        
grupo_elegido  <- "Total"   
quitar_pandemia <- FALSE    

# --- 2. PREPARACIÓN DE DATOS ---
df_box <- datos_long %>%
  filter(Grupo_Etario == grupo_elegido, CodigoRegion == region_elegida)

# Filtro de Pandemia (Opcional) - Usamos Fecha_Semana que sí existe en datos_long
if(quitar_pandemia) {
  df_box <- df_box %>% 
    filter(!(Fecha_Semana >= as.Date("2020-03-15") & Fecha_Semana <= as.Date("2021-12-31")))
}

# --- AGREGACIÓN PARA EL GRÁFICO ---
df_semanal <- df_box %>%
  group_by(anio, semana) %>%
  summarise(Casos_Semanales = sum(Casos, na.rm=TRUE), .groups = "drop") %>%
  # AQUÍ ESTÁ EL ARREGLO: Recalculamos la fecha matemáticamente
  mutate(
    Fecha_Ref = make_date(anio, 1, 1) + weeks(semana - 1)
  )

# Creación de la variable Mes ordenada
df_plot <- df_semanal %>%
  mutate(Mes_Num = month(Fecha_Ref)) %>% # Extraemos el mes numérico (1-12)
  mutate(Mes = factor(Mes_Num, levels = 1:12, 
                      labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")))

# --- 3. GENERACIÓN DEL GRÁFICO ---
ggplot(df_plot, aes(x = Mes, y = Casos_Semanales)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.6, outlier.alpha = 0.3) +
  labs(title = paste("Distribución Estacional de Casos - Región", region_elegida),
       subtitle = paste("Grupo:", grupo_elegido, "| Distribución histórica mensual"),
       y = "Casos Semanales", 
       x = "Mes del Año") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


########### BOXPLOT PÄIS ##############

# --- 1. CONFIGURACIÓN ---
grupo_elegido   <- "Total"    
quitar_pandemia <- FALSE     

# --- 2. PREPARACIÓN DE DATOS ---
# NOTA: Aquí quitamos el filtro de región para que tome todo Chile
df_box_pais <- datos_long %>%
  filter(Grupo_Etario == grupo_elegido)

# Filtro de Pandemia (Opcional)
if(quitar_pandemia) {
  df_box_pais <- df_box_pais %>% 
    filter(!(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2021-12-31")))
}

# --- AGREGACIÓN PARA EL GRÁFICO ---
df_semanal_pais <- df_box_pais %>%
  group_by(anio, semana) %>%
  # Al sumar aquí sin agrupar por región, obtenemos el TOTAL NACIONAL
  summarise(Casos_Semanales = sum(Casos, na.rm=TRUE), .groups = "drop") %>%
  mutate(
    # Recalculamos la fecha de referencia para sacar el mes
    Fecha_Ref = make_date(anio, 1, 1) + weeks(semana - 1)
  )

# Creación de la variable Mes ordenada
df_plot_pais <- df_semanal_pais %>%
  mutate(Mes_Num = month(Fecha_Ref)) %>% 
  mutate(Mes = factor(Mes_Num, levels = 1:12, 
                      labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")))

# --- 3. GENERACIÓN DEL GRÁFICO ---
ggplot(df_plot_pais, aes(x = Mes, y = Casos_Semanales)) +
  # Usamos un color distinto (ej: naranja o azul oscuro) para diferenciarlo del regional
  geom_boxplot(fill = "#D55E00", alpha = 0.6, outlier.alpha = 0.3) +
  
  labs(title = "Distribución Estacional de Casos - TOTAL PAÍS",
       subtitle = paste("Grupo:", grupo_elegido, "| Distribución histórica acumulada nacional"),
       y = "Casos Semanales (Nacional)", 
       x = "Mes del Año") +
  
  # Formato de miles en el eje Y
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


############### COMPARATIVA ANUAL (SPAGHETTI PLOT) ###############
# Objetivo: Superponer los años para comparar "intensidad" de inviernos

# 1. Elegimos una región (ej: RM) y grupo total
df_spaghetti <- datos_long %>%
  filter(CodigoRegion == 13, Grupo_Etario == "Total") %>%
  group_by(anio, semana) %>%
  summarise(Casos = sum(Casos), .groups = "drop") %>%
  mutate(
    # Creamos una categoría para destacar el año actual o pandemia
    Periodo = case_when(
      anio %in% c(2020, 2021) ~ "Pandemia",
      anio == 2024 ~ "2024 (Actual)",
      TRUE ~ "Histórico"
    )
  )

ggplot(df_spaghetti, aes(x = semana, y = Casos, group = anio, color = as.factor(anio))) +
  # Mantenemos la lógica del grosor dentro de aes()
  geom_line(aes(size = (anio == 2021)), alpha = 0.8) +
  
  scale_color_viridis_d(option = "turbo") +
  # Definimos los grosores: 0.5 para los antiguos (FALSE), 1.2 para 2024 (TRUE)
  scale_size_manual(values = c(0.5, 1.2)) + 
  
  labs(title = "Comparativa de Curvas Epidémicas (Región Metropolitana)",
       subtitle = "Superposición de semanas epidemiológicas (2017-2024)",
       x = "Semana Epidemiológica", 
       y = "Casos Totales", 
       color = "Año") +
  
  theme_minimal() +
  theme(legend.position = "right") +
  
  # --- ESTA ES LA LÍNEA MÁGICA QUE BORRA EL TRUE/FALSE ---
  guides(size = "none")


######################## SPAGHETTI PAIS ######################

############### COMPARATIVA ANUAL PAÍS (SPAGHETTI PLOT) ###############

# 1. Preparar datos Nivel País (Sumando todas las regiones)
df_spaghetti_pais <- datos_long %>%
  filter(Grupo_Etario == "Total") %>%  # Solo filtramos Total, sin región específica
  group_by(anio, semana) %>%
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Creamos la etiqueta de periodo
    Periodo = case_when(
      anio %in% c(2020, 2021) ~ "Pandemia",
      anio == 2024 ~ "2024 (Actual)",
      TRUE ~ "Histórico"
    )
  )

# 2. Graficar
ggplot(df_spaghetti_pais, aes(x = semana, y = Casos, group = anio, color = as.factor(anio))) +
  
  # Lógica de grosor: Si es 2024 (o el año que quieras destacar) es grueso, el resto fino
  geom_line(aes(size = (anio == 2021)), alpha = 0.8) +
  
  # Colores vibrantes para distinguir los años
  scale_color_viridis_d(option = "turbo") +
  
  # Definimos grosores: FALSE (históricos) = 0.5, TRUE (2024) = 1.2
  scale_size_manual(values = c(0.5, 1.2)) + 
  
  labs(title = "Comparativa de Curvas Epidémicas (Total País)",
       subtitle = "Superposición de semanas epidemiológicas (2017-2024)",
       x = "Semana Epidemiológica", 
       y = "Total de Casos Nacional", 
       color = "Año") +
  
  # Formato de miles en el eje Y (ej: 100.000)
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  
  theme_minimal() +
  theme(legend.position = "right") +
  
  # Borramos la leyenda del grosor para que no moleste
  guides(size = "none")