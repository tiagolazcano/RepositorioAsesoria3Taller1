library(dplyr)
library(readxl)
library(stringdist)

for(i in 17:24) {
  ruta_entrada = paste0("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/AtencionesUrgencia20", i, ".csv")
  nombre_archivo_salida = paste0("urgenciaIRA20", i)
  
  df_temporal = read.csv(ruta_entrada, sep=";", comment.char="#")
  
  df_temporal = df_temporal %>% 
    filter(IdCausa == 10)
  
  df_temporal$anio = 2000 + i
  
  write.csv(
    df_temporal,
    file = nombre_archivo_salida,
    row.names = FALSE,
    quote = TRUE
  )
  rm(df_temporal)
  gc()
}

urgenciaIRA2023 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2023", comment.char="#")
urgenciaIRA2024 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2024", comment.char="#")

urgenciaIRA2023$NombreComuna = NULL
urgenciaIRA2023$CodigoComuna = NULL
urgenciaIRA2023$NombreDependencia = NULL
urgenciaIRA2023$CodigoDependencia = NULL
urgenciaIRA2023$NombreRegion = NULL

urgenciaIRA2024$NombreComuna = NULL
urgenciaIRA2024$CodigoComuna = NULL
urgenciaIRA2024$NombreDependencia = NULL
urgenciaIRA2024$CodigoDependencia = NULL
urgenciaIRA2024$NombreRegion = NULL

OPLAOLA <- read_excel("OPLAOLA.xlsx")

OPLAOLA <- OPLAOLA %>% 
  distinct(IdEstablecimiento, .keep_all = TRUE)

########################### 2024 ###########################

faltantes <- urgenciaIRA2024 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2024$CodigoRegion[is.na(urgenciaIRA2024$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2024$CodigoRegion))

urgenciaIRA2024 <- urgenciaIRA2024 %>%
  select(1:15, 17, 16, everything())

########################### 2023 ###########################

faltantes <- urgenciaIRA2023 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2023$CodigoRegion[is.na(urgenciaIRA2023$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2023$CodigoRegion))

urgenciaIRA2023 <- urgenciaIRA2023 %>%
  select(1:15, 17, 16, everything())

########################### 2022 ###########################

urgenciaIRA2022 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2022")

urgenciaIRA2022 <- urgenciaIRA2022 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2022 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2022$CodigoRegion[is.na(urgenciaIRA2022$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2022$CodigoRegion))

urgenciaIRA2022 <- urgenciaIRA2022 %>%
  select(1:15, 17, 16, everything())

########################### 2021 ###########################

urgenciaIRA2021 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2021")

urgenciaIRA2021 <- urgenciaIRA2021 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2021 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2021$CodigoRegion[is.na(urgenciaIRA2021$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2021$CodigoRegion))

urgenciaIRA2021 <- urgenciaIRA2021 %>%
  select(1:15, 17, 16, everything())

########################### 2020 ###########################

urgenciaIRA2020 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2020")

urgenciaIRA2020 <- urgenciaIRA2020 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2020 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2020$CodigoRegion[is.na(urgenciaIRA2020$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2020$CodigoRegion))

urgenciaIRA2020 <- urgenciaIRA2020 %>%
  select(1:15, 17, 16, everything())

########################### 2019 ###########################

urgenciaIRA2019 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2019")

urgenciaIRA2019 <- urgenciaIRA2019 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2019 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2019$CodigoRegion[is.na(urgenciaIRA2019$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2019$CodigoRegion))

urgenciaIRA2019 <- urgenciaIRA2019 %>%
  select(1:15, 17, 16, everything())

########################### 2018 ###########################

urgenciaIRA2018 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2018")

urgenciaIRA2018 <- urgenciaIRA2018 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2018 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2018$CodigoRegion[is.na(urgenciaIRA2018$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2018$CodigoRegion))

urgenciaIRA2018 <- urgenciaIRA2018 %>%
  select(1:15, 17, 16, everything())

########################### 2017 ###########################

urgenciaIRA2017 <- read.csv("D:/Users/Tiago Lazcano/Desktop/Taller 1/Asesorías+/Asesoría 3/urgenciaIRA2017")

urgenciaIRA2017 <- urgenciaIRA2017 %>% 
  left_join(
    OPLAOLA %>% select(IdEstablecimiento, CodigoRegion),
    by = "IdEstablecimiento"
  )

faltantes <- urgenciaIRA2017 %>% filter(is.na(CodigoRegion))

hosp_ref <- OPLAOLA$IdEstablecimiento

matches <- amatch(faltantes$IdEstablecimiento,
                  hosp_ref,
                  maxDist = 5,
                  method = "lv")

faltantes$CodigoRegion <- OPLAOLA$CodigoRegion[matches]
urgenciaIRA2017$CodigoRegion[is.na(urgenciaIRA2017$CodigoRegion)] <- faltantes$CodigoRegion

sum(is.na(urgenciaIRA2017$CodigoRegion))

urgenciaIRA2017 <- urgenciaIRA2017 %>%
  select(1:15, 17, 16, everything())

############# BIND ###############

urgenciaIRA <- bind_rows(
  urgenciaIRA2017,
  urgenciaIRA2018,
  urgenciaIRA2019,
  urgenciaIRA2020,
  urgenciaIRA2021,
  urgenciaIRA2022,
  urgenciaIRA2023,
  urgenciaIRA2024
)



write.csv(
  urgenciaIRA,
  file = 'urgenciaIRA.csv',
  row.names = FALSE,
  quote = TRUE
)