rm(list = ls())
library(pacman)
p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 

source("src/0. Funciones.R")
#############################################----------------------

nEncuestas <- samplesize4surveys::ss4p(N=40000000, P = 0.5, DEFF = 1.5, conf = 0.95, error = "me", delta = 0.0333)

#############################################----------------------

poblacion<- readxl::read_xlsx(here::here("data/Poblacion 2026 POSTCOVID TAM OP.xlsx")) |>
  clean_names() |> 
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  mutate(edades = ifelse(edades_simples == "85 y más", 85, as.numeric(edades_simples))) |>
  filter(edades >= 18)

regiones<- poblacion |> 
  select(mpio,region,tamano) |> 
  unique()

energia<- readxl::read_xlsx("data/Energia_SUI VF.xlsx",sheet = 2) |> 
  mutate(RESTRATO = factor(case_when(
    estrato %in% c(0, 1, 2) ~ 1,
    estrato== 3 ~ 2,
    estrato%in% c(4,5,6) ~ 3),
    levels = 1:3,
    labels = c("Bajo", "Medio", "Alto"))) |> 
  left_join(regiones,by = c("Codigo Dane"="mpio")) |> 
  group_by(region,tamano,RESTRATO) |> 
  summarise(pob=sum(`Urbano 2023`,na.rm = T)) |> 
  group_by(region,tamano) |> 
  mutate(prop=proportions(pob)) |> 
  select(-pob) |> 
  ungroup() |> 
  pivot_wider(names_from = RESTRATO,values_from = prop)


pob <- poblacion|> 
  mutate(redad = factor(case_when(
    between(edades, 18, 24) ~ 1,
    between(edades, 25, 34) ~ 2,
    between(edades, 35, 44) ~ 3,
    between(edades, 45, 54) ~ 4,
    between(edades, 55, 100) ~ 5,
    TRUE ~ NA
  ), levels = 1:5,
  labels = c("18_24", "25_34",  "35_44",  "45_54",  "55 o más"))) |> 
  filter(!is.na(redad)) |> 
  group_by(region, tamano, redad) |> 
  summarise(Masculino = sum(hombres),
            Femenino = sum(mujeres)) |> 
  pivot_longer(cols = c("Masculino", "Femenino"),
               names_to = "sexo", values_to = "total") |> 
  filter(total !=0) %>% 
  left_join(energia, by = join_by(region, tamano)) %>% 
  pivot_longer(cols = c("Bajo", "Medio", "Alto"),
               names_to = "estrato", values_to = "prop") %>% 
  filter(prop !=0) %>% 
  mutate(pob = total * prop) %>% 
  select(-total, -prop)

sum(pob$pob)  ## Universo a representar

###### nEncuestas x Region

alpha <- 0.5

(nReg <- pob |>
    group_by(region) |>
    summarise(pob=sum(pob)) |>
    mutate(nR = round_preserve_sum((nEncuestas ) * pob^alpha/sum(pob^alpha))) |> 
    ungroup())



sum(nReg$nR) == nEncuestas ### Verifica que se cumple el total de encuestas


### Distribuciones por municipio
alpha <- 0.35
(asigna <- poblacion |> 
    group_by(region, tamano, dp, dpnom, mpio) |> 
    summarise(pob = sum(total)) |> 
    group_by(region, tamano) |> 
    summarise(pob = sum(pob)) |> 
    left_join(nReg |> select(-pob)) |> 
    group_by(region) |>   ###Se debe dar buena muestra a las 5 principales para desagregar
    mutate(tipo = ifelse(!grepl('Peq|Media|Grand', tamano), 1, 2)) |>
    mutate(tipo = ifelse(grepl('Cartagena|Cúcuta|Soacha|Villavicencio|Ibagué', tamano), 2, tipo)) |> 
    mutate(ni = case_when(
      tamano %in% c("Medellín", "Cali") ~ round(0.45*nR),
      tamano == "Barranquilla" ~ round(0.38*nR),
      tamano == "Bucaramanga"  ~ round(0.3*nR),
      grepl("Bogotá", tamano) ~ nR,
      TRUE ~ 0
    )) |> 
    group_by(region) |> 
    mutate(ni = ifelse(ni==0, max(nR)-sum(ni), ni)) |> 
    group_by(region, tipo) |> 
    mutate(encuestas = ifelse(tipo ==1, ni,
                              round_preserve_sum(ni*pob^alpha/sum(pob^alpha,na.rm = T), 0))) |> 
    select(-ni, -tipo))

sum(asigna$encuestas)

muestraUPM <- asigna

asigna |> 
  group_by(region) |> 
  summarise(enc = sum(encuestas),
            nR = max(nR)) |> 
  verify(enc==nR)


######### Muestra por edad y por estratos

nDom <- pob |> 
  group_by(region, tamano, redad, estrato) |> 
  summarise(pob = sum(pob)) |> 
  left_join(asigna |> select(region, tamano, encuestas), by = join_by(region, tamano)) |> 
  group_by(region, tamano) |> 
  mutate(nDom = round_preserve_sum(encuestas * pob^0.7/sum(pob^0.7), 0))

sum(nDom$pob)
sum(nDom$nDom)

#################################### Edad
nEdad <- nDom |> 
  group_by(region, tamano, redad) |> 
  summarise(n = sum(nDom)) 

sum(nEdad$n)

(nEdad <- pivot_wider(nEdad, names_from = redad, values_from = n))

#################################### Estrato

nEstrato <- nDom |> 
  group_by(region, tamano, estrato) |> 
  summarise(n = sum(nDom)) 

sum(nEstrato$n)

(nEstrato <- pivot_wider(nEstrato, names_from = estrato, values_from = n))

##### Distribución Bogotá por Localidades

poblacion<- read_xlsx(here::here("data/anexo-proyecciones-poblacion-bogota-desagreacion-loc-2018-2035-UPZ-2018-2024.xlsx"),sheet = "r loc")

pobR<- poblacion |> 
  filter(AÑO==2026) |> 
  select(1:3,contains("Total",ignore.case=F)) |> 
  pivot_longer(cols = contains("Total")) |> 
  separate(name,sep = "_",into = c("tbd","edad")) |> 
  mutate(edad=ifelse(edad=="100 y más","100",edad)) |> 
  select(-tbd) |> 
  mutate(edad=as.numeric(edad)) 

pob18<- pobR |> 
  filter(edad>=18) |> 
  pivot_wider(names_from = "AREA",values_from = value) |> 
  clean_names() |> 
  group_by(cod_loc,nom_loc) |> 
  summarise(Cabecera=sum(cabecera_municipal),
            Rural=sum(centros_poblados_y_rural_disperso),
            Total=sum(total)) |> 
  filter(cod_loc != "20")

ssize<- 201

alfa<- 1/5

tabmuestra<- pob18 |>
  ungroup() |> 
  mutate(Muestra=round_preserve_sum(Total^alfa/sum(Total^alfa)*ssize)) |> 
  janitor::adorn_totals() |> 
  select(cod_loc, nom_loc, Muestra)

