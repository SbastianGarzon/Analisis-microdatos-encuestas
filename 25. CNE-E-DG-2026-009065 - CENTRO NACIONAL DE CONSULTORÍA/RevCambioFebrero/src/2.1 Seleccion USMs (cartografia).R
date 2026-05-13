rm(list = ls())

library(pacman)

# --- Librerias

p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
       tmap, tmaptools, leaflet, rgdal, stratification,tmap)

# --- Funciones

source("src/0. Funciones.R")
source("src/funcioncarto.R")

# --- Data

marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F)


Municipios <- read_excel("output/20260223 Muestra OP R.Cambio.xlsx",sheet = "Municipios") |> 
  mutate(dane = str_pad(dane, 
                        width = 5, 
                        side = "left", 
                        pad = "0")) |> 
  rename(mpio = dane,
         encuestas = encuestas) 
  

alpha = 0.2

muestraurb <- read_excel("data/Energia_SUI VF.xlsx", sheet = "Datos") |>
  mutate(
    NSE = case_when(
      estrato %in% c(1, 2, 3) ~ "Bajo",
      estrato == 4            ~ "Medio",
      estrato %in% c(5, 6)    ~ "Alto",
      TRUE ~ NA_character_
    ),
    `Urbano 2023` = as.numeric(`Urbano 2023`)
  ) |>
  select(`Codigo Dane`,TAMANO, Departamento, Municipio, `Urbano 2023`, NSE) |>
  pivot_wider(
    names_from  = NSE,
    values_from = `Urbano 2023`,
    values_fn   = sum,        
    values_fill = 0
  )|>
  mutate(
    Total = Bajo^alpha + Medio^alpha + Alto^alpha,
    prop_Bajo  = Bajo^alpha  / Total,
    prop_Medio = Medio^alpha/ Total,
    prop_Alto  = Alto^alpha  / Total
  ) |> 
  select(-Bajo,-Medio,-Alto,-Total) |> 
  filter(`Codigo Dane` %in% Municipios$mpio) |> 
  left_join(Municipios|>  select(mpio,Urbano), by = c("Codigo Dane" = "mpio") ) |> 
  mutate(Bajo = ifelse(!(TAMANO %in% c("Pequeños","Medianos")), round_preserve_sum(prop_Bajo * Urbano),NA),
         Medio = ifelse(!(TAMANO %in% c("Pequeños","Medianos")), round_preserve_sum(prop_Medio * Urbano),NA),
         Alto = ifelse(!(TAMANO %in% c("Pequeños","Medianos")), round_preserve_sum(prop_Alto * Urbano),NA)) |> 
  arrange(TAMANO,desc(TAMANO)) |> 
  filter(!(TAMANO %in% c("Pequeños","Medianos"))) |> 
  rename(dane=`Codigo Dane`)

### Cartografia con estrato

df=muestraurb|>
  select(dane,Bajo, Medio, Alto) |> 
  pivot_longer(cols = c(-dane), names_to = "NSE", values_to = "muestra") |> 
  rename(estrato=NSE)


carto1<- fcartografia(marco = marco,df=df,col_dane = "dane",
                      col_encuestas = "muestra",col_desag = "estrato",
                      encxmanzana = 4,
                      desag = "n",semilla = 20260223)

(prueba1<- carto1 |>  count(Municipio,NIVEL_SCE) |> mutate(nh=n*8))


# --- Cartografia Urbana sin estrato 

muestraurbSE <-  Municipios |> 
  filter(!(mpio %in% carto1$COD_MPIO)) |> 
  select(dane = mpio, encuestas = Urbano)

df= muestraurbSE

carto2<- fcartografia(marco = marco,df=df,col_dane = "dane",
                      col_encuestas = "encuestas",semilla = 20260223,
                      encxmanzana =  4, inicio_idmapa = max(carto1$ID_MAPA))

(prueba2<- carto2 |>  count(Municipio) |> mutate(nh  =n*8))


# --- Cartografia rural

muestrarur <- Municipios |> 
  select(mpio, Rural) |> 
  filter(Rural>0)

df<- muestrarur|>
  select(dane = mpio, nrural = Rural) 

carto3<- fcartografia(marco = marco,df=df,col_dane = "dane",
                      col_encuestas = "nrural",
                      semilla = 20260223,encxmanzana = 10,manzanas = "vereda",
                      inicio_idmapa = max(carto2$ID_MAPA))

(prueba<- carto3|>  count(Municipio) |> mutate(nh=n*10))



############ Reemplazo Cantagallo por la Paz

# --- Cartografia Urbana sin estrato 

muestraurbSE <-  Municipios |> 
  select(dane = mpio, encuestas = Urbano) |> 
  filter(dane == "13160") |> 
  mutate(dane = "20621")

df= muestraurbSE

carto<- fcartografia(marco = marco,df=df,col_dane = "dane",
                      col_encuestas = "encuestas",semilla = 20260224,
                      encxmanzana =  4)

(prueba<- carto |>  count(Municipio) |> mutate(nh  =n*8))


############ Adicional Santa Marta


m_Santa_Marta <- marco |> 
  filter(COD_MPIO == "47001") |> 
  filter(ESTRATO == 1)

df <- data.frame(mpio= c("47001"),
                 nenc= 4)

cartoS<- fcartografia(marco = m_Santa_Marta, df=df, col_dane = "mpio",
                      col_encuestas = "nenc", semilla = 20260224)

(prueba<- cartoS |>  count(Municipio) |> mutate(nh  =n*8))


############ Adicional Cartagena


m_Cartagena <- marco |> 
  filter(COD_MPIO == "13001") |> 
  filter(NIVEL_SCE == "Alto")

df <- data.frame(mpio= c("13001"),
                 nenc= 4)

cartoC<- fcartografia(marco = m_Cartagena, df=df, col_dane = "mpio",
                      col_encuestas = "nenc", semilla = 20260225)

(prueba<- cartoC |>  count(Municipio) |> mutate(nh  =n*8))

############ Adicional Barranquilla bajo


m_Barranquilla <- marco |> 
  filter(COD_MPIO == "08001") |> 
  filter(NIVEL_SCE == "Bajo")

df <- data.frame(mpio= c("08001"),
                 nenc= 6)

cartoB<- fcartografia(marco = m_Barranquilla, df=df, col_dane = "mpio",
                      col_encuestas = "nenc", semilla = 20260225)

(prueba<- cartoB |>  count(Municipio) |> mutate(nh  =n*8))

################## Reemplazos Cauca

muestra<- tibble(dane=c("76828","19548"),muestra=c(13,9),rural=c(0,9))

carto_urb_sin<- fcartografia(marco = marco,
                             df = muestra,
                             col_dane = "dane",
                             col_encuestas = "muestra",
                             minvivienda = 8,
                             encxmanzana = 4,
                             semilla = "20260226")


carto_rur<- fcartografia(marco = marco,df = muestra,col_dane = "dane",
                         manzanas = "vereda",
                         col_encuestas = "rural")
