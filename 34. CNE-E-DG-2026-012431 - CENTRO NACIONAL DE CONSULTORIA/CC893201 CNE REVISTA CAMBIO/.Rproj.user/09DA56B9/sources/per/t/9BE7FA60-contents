########################################
## Programado por: Maria Paula Torres
## 12 de marzo de 2026
########################################

rm(list = ls())
library(pacman)
p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr,tidyr,samplesize4surveys)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 

source("src/0. Funciones.R")

############################################--------------------

poblacion <- readxl::read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP_actualizatam.xlsx") |>
  clean_names() |> 
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  filter(edades>=18)

nEncuestas <-ss4p(N = sum(poblacion$total),
                   P = 0.5,DEFF = 2,
                   error = "me",delta = 0.02997213)

poblacion |> 
  select(region,tamano,edades) |> 
  is.na() |> 
  colSums()

poblacion |> 
  group_by(tamano, mpio) |> 
  summarise(total = sum(total)) |> 
  group_by(tamano) |> 
  summarise(min = min(total),
            max = max(total)) |> 
  filter(tamano %in% c("Grandes", "Medianos", "Pequeños"))

sum(poblacion$total)

regiones <- poblacion |> 
  distinct(codigo_dane=mpio, region, tamano) |> 
  verify(!is.na(region))

energia<- readxl::read_xlsx("data/Energia_SUI VF.xlsx",sheet = 2) |> 
  mutate(RESTRATO = factor(case_when(
    estrato %in% c(0, 1, 2) ~ 1,
    estrato== 3 ~ 2,
    estrato%in% c(4,5,6) ~ 3),
    levels = 1:3,
    labels = c("Bajo", "Medio", "Alto"))) |> 
  left_join(regiones,by = c("Codigo Dane"="codigo_dane")) |> 
  group_by(region,tamano,RESTRATO) |> 
  summarise(Urbano=sum(`Urbano 2023`,na.rm = T),
            Rural=sum(`Rural 2023`,na.rm = T)) |> 
  pivot_longer(cols = c(Urbano,Rural),names_to = "area",values_to = "pob") |> 
  group_by(region,tamano,area) |> 
  mutate(prop=proportions(pob)) |> 
  select(-pob) |> 
  ungroup() |> 
  pivot_wider(names_from = RESTRATO,values_from = prop) |> 
  mutate(Bajo=ifelse(is.na(Bajo),1,Bajo)) |> 
  mutate(across(all_of(c("Medio","Alto")),~replace_na(.,0)))

###----------------->  Cabecera + Rural

pob <- poblacion|> 
  mutate(redad = factor(case_when(
    between(edades, 18, 24) ~ 1,
    between(edades, 25, 34) ~ 2,
    between(edades, 35, 44) ~ 3,
    between(edades, 45, 54) ~ 4,
    between(edades, 55, 100) ~ 5,
    TRUE ~ NA
  ), levels = 1:5,
  labels = c("18_24", "25_34",  "35_44",  "45_54",  "55_mas"))) |> 
  filter(!is.na(redad)) |> 
  group_by(region, tamano, redad) |> 
  summarise(cabecera_hombres = sum(cabecera_hombres),
            cabecera_mujeres = sum(cabecera_mujeres),
            rural_hombres = sum(rural_hombres),
            rural_mujeres = sum(rural_mujeres)) |> 
  pivot_longer(cols =cabecera_hombres:rural_mujeres,
               names_to = "area_sexo", values_to = "total") |> 
separate(area_sexo,into = c("area","sexo")) |> 
  mutate(area=ifelse(area=="cabecera","Urbano","Rural")) |> 
##  filter(area=="Urbano") |> ## si es solo urbano
 # filter(total !=0) %>% 
  left_join(energia, by = join_by(region, tamano,area)) %>% 
  pivot_longer(cols = c("Bajo", "Medio", "Alto"),
               names_to = "estrato", values_to = "prop") %>% 
  filter(prop !=0) %>% 
  mutate(pob = total * prop) %>% 
  select(-total, -prop)

sum(pob$pob)  ## Universo a representar TODO urbano-rural

###### nEncuestas x Region

alpha <- 0.7

(nReg <- pob |>
    mutate(tipo = ifelse(region == "Bogotá", 1, 2)) |>
    group_by(region, tipo) |>
    summarise(pob=sum(pob)) |>
    group_by(tipo) |>
    mutate(nR = ifelse(grepl("Bogot",region), round(nEncuestas * 0.16),
                       round_preserve_sum((nEncuestas - round(nEncuestas * 0.16)) * pob^alpha/sum(pob^alpha)))) |> ungroup() |>
    select(-tipo) |> 
    mutate(p = nR/sum(nR),
           pi = pob/sum(pob)))

sum(nReg$nR) == nEncuestas ### Verifica que se cumple el total de encuestas

### Distribuciones por tam municipio

alpha <- 0.30

(asigna <- poblacion |> 
    group_by(region, tamano, dp, dpnom, mpio) |> 
    summarise(pob = sum(total)) |> 
    group_by(region, tamano) |> 
    summarise(NhMpios = n(),
              pob = sum(pob)) |> 
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
    select(-ni, -tipo) |> 
    mutate(nMpios = case_when(
      grepl('Bogot|Medell|Cali|Barran|Bucara|Cartagena|Cúcuta|Soacha|Villavicencio|Ibagué', tamano) ~ 1, 
      grepl('Grande', tamano) ~ round(encuestas/50),
      grepl('Median', tamano) ~ round(encuestas/35),
      grepl('Peque', tamano) ~ round(encuestas/15))) |> 
    mutate(nMpios = ifelse(
      region %in% c("Centro - Oriente", "Llano", "Pacífico") & tamano == "Medianos", 2,
      ifelse(region %in% c("Centro - Sur - Amazonía", "Llano") & tamano == "Grandes", 2, nMpios
      ))) |> 
    ungroup())

sum(asigna$encuestas)
sum(asigna$nMpios)

muestraUPM <- asigna
#dir.create("output")
save(muestraUPM, file="output/muestraUPM.rds")

sum(asigna$nMpios)
tapply(asigna$nMpios, asigna$tamano, sum)

asigna |> 
  group_by(region) |> 
  summarise(enc = sum(encuestas),
            nR = max(nR)) |> 
  verify(enc==nR)

#####----- Seleccion de UPMs

UPMs <- poblacion |>
  group_by(region, tamano, dp, dpnom, mpio, dpmp) |> 
  summarise(pob = sum(total),
            cab = sum(cabecera_total),
            rural = sum(rural_total)) |> 
  mutate(prur=rural/pob) |> 
  filter(prur < 0.9) |>  #Quito municipios exclusivamente rurales
  left_join(asigna |> ungroup() |> 
              select(region, tamano, nMpios)) |> 
  mutate(Strata = paste0(region, "_", tamano)) 

{set.seed(31226)  # Fecha de setup de la muestra
  nh <- UPMs |> ungroup() |>  distinct(Strata, nMpios) |> pull()
  res <- TeachingSampling::S.STpiPS(UPMs$Strata, x = UPMs$pob, nh = nh)
  sam <- UPMs[res[,1],]
}

UPMs_sel <- sam
