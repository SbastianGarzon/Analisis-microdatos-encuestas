rm(list = ls())
library(pacman)
p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 

source("src/0. Funciones.R")

#############################################----------------------

nEncuestas <- samplesize4surveys::ss4p(N=40000000, P = 0.5, DEFF = 2.0, conf = 0.95, error = "me", delta = 0.02998)+1

#############################################----------------------

senado18 <- readxl::read_excel("data/2018 Senado.xlsx") |> 
  group_by(codmpio) |> 
  summarise(votos18 = sum(votos))

senado22 <- readxl::read_excel("data/2022 Senado.xlsx")|> 
  group_by(codmpio) |> 
  summarise(votos22 = sum(votos))

senado <- senado18 |> 
  left_join(senado22) |> 
  mutate(votos = votos18*0.3 + votos22*0.7)

poblacion<- readxl::read_xlsx(here::here("data/Poblacion 2026 POSTCOVID TAM OP.xlsx")) |>
  clean_names() |> 
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  mutate(edades = ifelse(edades_simples == "85 y más", 85, as.numeric(edades_simples))) |>
  filter(edades >= 18) |> 
  filter(dp != "23")

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

### Distribuciones por municipio
alpha <- 0.35
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
            grepl('Grande', tamano) ~ round(encuestas/30),
            grepl('Median', tamano) ~ round(encuestas/18),
            grepl('Peque', tamano) ~ round(encuestas/15))))  

sum(asigna$encuestas)
sum(asigna$nMpios)

save(asigna, file = "data/asigna.rdata")

#####----- Seleccion Municipios

UPMs <- poblacion |>
  group_by(region, tamano, dp, dpnom, mpio, dpmp) |> 
  summarise(pob = sum(total),
            cab = sum(cabecera_total),
            rural = sum(rural_total)) |> 
  left_join(senado, by = c("mpio" = "codmpio")) |> 
  filter(pob > 0) |>  #Quito municipios sin pob
  left_join(asigna |> ungroup() |> 
              select(region, tamano, nMpios)) |> 
  mutate(Strata = paste0(region, "_", tamano)) |> 
  mutate(porcv = votos/pob) |> 
  mutate(votos = ifelse(is.na(votos), pob*0.47, votos))

nRural <- 286

{set.seed(260221)
  nh <- UPMs |> ungroup() |>  distinct(Strata, nMpios) |> pull()
  res <- TeachingSampling::S.STpiPS(UPMs$Strata, x = UPMs$votos, nh = nh)
  sam <- UPMs[res[,1],]
}


############# Reemplazo Caribe pequeño por seguridad

n_reemp <- data.frame(
  Strata = c("Caribe_Pequeños"),
  n_reemp = c(1)
)


UPMs_restantes <- UPMs |> 
  filter(!mpio %in% sam$mpio) |> 
  filter(Strata %in% n_reemp$Strata)


{set.seed(240226)
  nh <- n_reemp |> ungroup() |>  distinct(Strata, n_reemp) |> pull()
  res <- TeachingSampling::S.STpiPS(UPMs_restantes$Strata, x = UPMs_restantes$votos, nh = nh)
  samr <- UPMs_restantes[res[1],]
}


############# Reemplazo Pacifico pequeño por seguridad

n_reemp2 <- data.frame(
  Strata = c("Pacífico_Pequeños"),
  n_reemp = c(2)
)


UPMs_restantes <- UPMs |> 
  filter(!mpio %in% sam$mpio) |> 
  filter(!mpio %in% samr$mpio) |> 
  filter(Strata %in% n_reemp2$Strata)

{set.seed(260222)
  nh <- n_reemp2 |> ungroup() |>  distinct(Strata, n_reemp) |> pull()
  res <- TeachingSampling::S.STpiPS(UPMs_restantes$Strata, x = UPMs_restantes$votos, nh = nh)
  samr1 <- UPMs_restantes[res[,1],]
  }

##########################################################
########### Municipios seleccionados #####################
##########################################################

Municipios <- sam |> 
  filter(!mpio %in% "13160") |>      # Sale Cantagallo Bolivar por problemas de Orden Público
  bind_rows(samr) |>                 # Entra el municipio de reemplazo (La Paz en Cesar) de la misma región y tamaño de Cantagallo Bolivar
  filter(!mpio %in% c("19785", "19807")) |> # Salen los municipios de Sucre y Timbio en Cauca por problemas de Orden Público
  bind_rows(samr1)                    # Entran los municipios de reemplazo (Piendamo - Tunia en cauca y Trujillo en Valle del Cauca) de la misma región y tamaño de Sucre y Timbio en Cauca
