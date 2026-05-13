library(RODBC)
library(readxl)
library(dplyr)
library(INVAMER.MUESTREO)
library(openxlsx)
library(stringr)
#Conexión a base de datos mediante odbc (cambiar los datos que se encuentran entre "")

con <- odbcConnect("integra")

usuario <- Sys.info()["user"]
nombre_estudio <- "009200260000 COLOMBIA OPINA No 21"
codigo<-"009200260000"

#Reemplazo 1
#Fecha 15/04/2026

ID_ORIGINAL<- c(540,686) #Lista de los ID que se desean reemplazar
R <- 1 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 2
#Fecha 15/04/2026

ID_ORIGINAL<- c(20) #Lista de los ID que se desean reemplazar
R <- 2 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 5

#Fecha 17/04/2026
ID_ORIGINAL<- c(627) #Lista de los ID que se desean reemplazar
R <- 5 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")


#Reemplazo 6
#Fecha 17/04/2026

ID_ORIGINAL<- c(734) #Lista de los ID que se desean reemplazar
R <- 6 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 7
#Fecha 20/04/2026

ID_ORIGINAL<- c(589,756,762,790) #Lista de los ID que se desean reemplazar
R <- 7 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 8
#Fecha 20/04/2026

ID_ORIGINAL<- c(801,802,820,822) #Lista de los ID que se desean reemplazar
R <- 8 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 9
#Fecha 21/04/2026

ID_ORIGINAL<- c(807) #Lista de los ID que se desean reemplazar
R <- 9 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 10
#Fecha 21/04/2026

ID_ORIGINAL<- c(994) #Lista de los ID que se desean reemplazar
R <- 10 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 11
#Fecha 21/04/2026

ID_ORIGINAL<- c(833,834) #Lista de los ID que se desean reemplazar
R <- 11 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 13
#Fecha 22/04/2026

ID_ORIGINAL<- c(530,214) #Lista de los ID que se desean reemplazar
R <- 12 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 13
#Fecha 22/04/2026

ID_ORIGINAL<- c(1007) #Lista de los ID que se desean reemplazar
R <- 13 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 14
#Fecha 22/04/2026

ID_ORIGINAL<- c(810) #Lista de los ID que se desean reemplazar
R <- 14 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 15
#Fecha 23/04/2026

ID_ORIGINAL<- c(999) #Lista de los ID que se desean reemplazar
R <- 15 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 16
#Fecha 23/04/2026

ID_ORIGINAL<- c(952) #Lista de los ID que se desean reemplazar
R <- 16 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 17
#Fecha 23/04/2026

ID_ORIGINAL<- c(952) #Lista de los ID que se desean reemplazar
R <- 17 # Primer reemplazo del estudio

set.seed(1012)

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))

# Filtrar para EXCLUIR (usando el operador !)
df_filtrado <- df_filtrado %>%
  filter(!str_detect(BARRIO, regex("URBANIZACION|PARCELACION|PARCELACIONES", ignore_case = TRUE)))

#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")

#Reemplazo 18
#Fecha 24/04/2026

ID_ORIGINAL<- c(655,683) #Lista de los ID que se desean reemplazar
R <- 18 # Primer reemplazo del estudio

set.seed(ID_ORIGINAL[1])

#Importar las rutas cargadas en la estructura
Distribucion<-sqlQuery(con,paste0("SELECT * FROM rutas_",codigo,"_1"))
#Importar los reemplazos
#Para los reemplazos de Caribe usar la siguiente ruta.
Reemplazos <- read_excel("ruta")


#Variables del master que debe cumplir el reemplazo Validar que esten escritas igual que en la estructura
variables<-c("COD_MUNICIPIO",
             "ESTRATO"
)

# Se excluyen las manzanas de reemplazo enviadas anteriormente.
Reemplazo_NN <- Distribucion %>%
  filter(MANZANA_ORI==2)

Reemplazos <-  Reemplazos %>% anti_join(Reemplazo_NN, by = "COD_MANZANA")  


valores_id <- Distribucion[Distribucion$ID_MUESTRAL %in% ID_ORIGINAL, variables]
valores_id$cuota <- 1

# Validar que valores_id no presente cuotas duplicadas.
# Ejemplo:
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  1
#     5001     -  3    -  1

# La forma correcta sería unificar la cuota y quedaria de la siguiente manera.
# COD_MUNICIPIO-ESTRATO-CUOTA
#     5001     -  3    -  2

valores_id <- aggregate(cuota ~ COD_MUNICIPIO + ESTRATO, data = valores_id, FUN = sum)


ruta_original<-Distribucion %>% filter(ID_MUESTRAL %in% ID_ORIGINAL)



#Filtramos de los reemplazos disponibles los que tienen las mismas caracteristicas de la manzana original
df_filtrado <- Reemplazos %>%
  filter(across(
    all_of(variables),
    ~ .x == valores_id[[cur_column()]] | 
      (is.na(.x) & is.na(valores_id[[cur_column()]]))
  ))


#Este paso aleatoriza el reemplazo seleccionado
Reemplazo<-MuestreoEstratificado(datos = df_filtrado,InfoMuestra =valores_id,SubMuestreo = "simple")

df_reemplazo<-Reemplazo$muestra

#Estos campos llenan la información del reemplazo.
ID_INICIO <- max(Distribucion$ID_MUESTRAL)+1

df_reemplazo<-df_reemplazo %>%  mutate(
  MANZANA_ORI = 2) #Siempre es 2 en los reemplazos
df_reemplazo$ID_MUESTRAL <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)
df_reemplazo$ID_MANZANA <- ID_INICIO:(ID_INICIO + nrow(df_reemplazo) - 1)

# Agrego los ceros adelante de la variable ID_MUESTRAL:
aux <- vector(mode = "character", length = nrow(df_reemplazo))
for (i in seq_along(aux)) {
  
  if (df_reemplazo$ID_MUESTRAL[i] < 10) {
    aux[i] <- paste0("0000", df_reemplazo$ID_MUESTRAL[i]) 
  } else if (df_reemplazo$ID_MUESTRAL[i] >= 10) {
    if (df_reemplazo$ID_MUESTRAL[i] < 100) {
      aux[i] <- paste0("000", df_reemplazo$ID_MUESTRAL[i]) 
    } else if (df_reemplazo$ID_MUESTRAL[i] >= 100) {
      if (df_reemplazo$ID_MUESTRAL[i] < 1000) {
        aux[i] <- paste0("00", df_reemplazo$ID_MUESTRAL[i])
      } else if (df_reemplazo$ID_MUESTRAL[i] >= 1000) {
        if (df_reemplazo$ID_MUESTRAL[i] < 10000) {
          aux[i] <- paste0("0", df_reemplazo$ID_MUESTRAL[i])
        } else {aux[i] <- as.character(df_reemplazo$ID_MUESTRAL[i])
        }  
      } 
    }
  }
}

df_reemplazo["ID_MUESTRAL"] <- aux

df_reemplazo["ID_MANZANA"] <- aux

# En caso de ser ruta adicional de una manzana donde se lograron algunas encuestas 
# añadir las variables de encuestas necesarias.

#Este for llena todos los campos del reemplazo con la información de la manzana original 
for (col in names(df_reemplazo)) {
  # Convertir ambas columnas al mismo tipo 
  df_reemplazo[[col]] <- as.character(df_reemplazo[[col]])
  ruta_original[[col]] <- as.character(ruta_original[[col]])
  # Ahora coalesce() para llenar los vacios del reemplazo con la misma información del original
  df_reemplazo[[col]] <- coalesce(df_reemplazo[[col]], ruta_original[[col]])
}

# modficamos los nulos:
Cuales_nulos <- which(df_reemplazo$DIRECCION_1 == "NULL")
df_reemplazo$DIRECCION_1[Cuales_nulos] <- ""
Cuales_nulos <- which(df_reemplazo$COMUNA_LOCALIDAD == "NULL")
df_reemplazo$COMUNA_LOCALIDAD[Cuales_nulos] <- ""

# Convertir variables de interés a numericas (modificar en caso de ser necesario)
df_reemplazo <- df_reemplazo %>%
  mutate(across(c(MANZANA_ORI, ESTRATO, ENUMERACION_INICIAL, 
                  ENUMERACION_FINAL, ENCUESTAS, ENCUESTAS_RUTA,
                  COD_REGION, COD_DEPARTAMENTO, COD_MUNICIPIO,
                  CODIGO_PLAZA, COD_ZONA, BDTIPO, COD_TAMANO), as.numeric))

#Guardar el reemplzo en una ruta
write.xlsx(df_reemplazo, "ruta")
