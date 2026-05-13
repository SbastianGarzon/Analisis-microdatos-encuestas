rm(list = ls())
library(pacman)

p_load(tidyverse, haven, janitor, survey, srvyr, readxl, assertr, samplesize4surveys)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T,
        scipen=99999) 

load("output/fexp1.rds")
load("output/universos.rds")

################################################################################
###############################################################################  
  
t_municipios<- read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP.xlsx") |> 
  clean_names() |> 
              mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
               mutate(region = ifelse(mpio == "11001", "Bogotá", region)) 

regiones<- t_municipios |> 
  select(mpio,region,tamano) |> 
  unique()

poblacion<- t_municipios |>
  filter(edades >=18 ) |> 
  mutate(REDAD = case_when( between(edades, 18, 24) ~ 1,
                     between(edades, 25, 34) ~ 2,
                     between(edades, 35, 44) ~ 3,
                     between(edades, 45, 54) ~ 4,
                     edades>=55~ 5)) |> 
  mutate(REDAD = factor(REDAD, 
                        levels = 1:5,
                        c("18 a 24",
                          "25 a 34",
                          "35 a 44",
                          "45 a 54",
                          "55 o más"))) |> 
  group_by(region,tamano,REDAD) |> 
  summarise(Urbano_hombres=sum(cabecera_hombres),
            Urbano_mujeres=sum(cabecera_mujeres),
            Rural_hombres=sum(rural_hombres),
            Rural_mujeres=sum(rural_mujeres)) |> 
  pivot_longer(cols = c(Urbano_hombres, Urbano_mujeres,
                        Rural_hombres, Rural_mujeres,),
               names_to = "ZONA_SEXO",values_to = "total") |> 
  separate(ZONA_SEXO, into = c("ZONA", "SEXO"), sep = "_") |> 
  mutate(SEXO=ifelse(SEXO=="hombres","Hombre","Mujer"))  |> 
  mutate(ZONA = ifelse(region == "Bogotá", "Urbano", ZONA)) |> 
  group_by(region, tamano, REDAD, ZONA, SEXO) |> 
  summarise(total = sum(total)) |> ungroup()

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

pobR<- poblacion |> 
       left_join(energia) |> 
       mutate(Bajo=total*Bajo) |> 
       mutate(Medio=total*Medio) |> 
       mutate(Alto=total*Alto) |> 
       select(-total) |> 
       pivot_longer(cols = c(Alto,Medio,Bajo),names_to = "RESTRATO",values_to = "pob")

sum(pobR$pob)

fexp1 <- fexp1 |> select(-tamano) |> 
         left_join(regiones |> select(mpio, tamano))

###### poblaciones

(wregion_tamano<- pobR |>
   group_by(region,tamano) |> 
   summarise(pob=sum(pob)) |> 
   left_join(fexp1 |> count(region,tamano)) |> 
   verify(!is.na(n))) 

sum(wregion_tamano$pob)
sum(wregion_tamano$n)

(wregion_edad<- pobR |>
    group_by(region,REDAD) |> 
    summarise(pob=sum(pob)) |> 
    left_join(fexp1 |> count(region,REDAD))|> 
    verify(!is.na(n))) 
sum(wregion_edad$pob)
sum(wregion_edad$n)

(wregion_sexo<- pobR |>
    group_by(region,SEXO) |> 
    summarise(pob=sum(pob)) |> 
    left_join(fexp1 |> count(region,SEXO))|> 
    verify(!is.na(n)))

sum(wregion_sexo$pob)
sum(wregion_sexo$n)

(wregion_estrato<- pobR |>
    group_by(region,RESTRATO) |> 
    summarise(pob=sum(pob)) |> 
    left_join(fexp1 |>
                count(region,RESTRATO))|> 
    verify(!is.na(n))) 

sum(wregion_estrato$pob)
sum(wregion_estrato$n)

(wregion_zona<- pobR |>
    group_by(region,ZONA) |> 
    summarise(pob=sum(pob)) |> 
    left_join(fexp1 |> count(region,ZONA))|> 
    verify(!is.na(n))) 

sum(wregion_zona$pob)
sum(wregion_zona$n)

#####

pob.region.tamano<- wregion_tamano$pob
names(pob.region.tamano)<- str_replace_all(paste0(wregion_tamano$region,"_",wregion_tamano$tamano),"\\s|-|,|\\+","_")
pob.region.tamano

pob.region.edad<- wregion_edad$pob
names(pob.region.edad)<- str_replace_all(paste0(wregion_edad$region,"_",wregion_edad$REDAD),"\\s|-|,|\\+","_")
pob.region.edad

pob.region.sexo<- wregion_sexo$pob
names(pob.region.sexo)<- str_replace_all(paste0(wregion_sexo$region,"_",wregion_sexo$SEXO),"\\s|-|,|\\+","_")
pob.region.sexo

pob.region.estrato<- wregion_estrato$pob
names(pob.region.estrato)<- str_replace_all(paste0(wregion_estrato$region,"_",wregion_estrato$RESTRATO),"\\s|-|,|\\+","_")
pob.region.estrato

pob.region.zona<- wregion_zona$pob
names(pob.region.zona)<- str_replace_all(paste0(wregion_zona$region,"_",wregion_zona$ZONA),"\\s|-|,|\\+","_")
pob.region.zona

## matriz de diseño

y1 <- fexp1 |> 
  mutate(llave=str_replace_all(paste0(region,"_",tamano),"\\s|-|,|\\+","_"),
         n=1) |>
  select(REGISTRO,llave,n) |> 
  pivot_wider(names_from = llave,values_from = n) |> 
  mutate(across(where(is.numeric),~replace_na(.,0))) |> 
  relocate(REGISTRO,any_of(names(pob.region.tamano))) 

y2 <- fexp1 |> 
  mutate(llave=str_replace_all(paste0(region,"_",REDAD),"\\s|-|,|\\+","_"),
         n=1) |>
  select(REGISTRO,llave,n) |> 
  pivot_wider(names_from = llave,values_from = n) |> 
  mutate(across(where(is.numeric),~replace_na(.,0))) |> 
  relocate(REGISTRO,any_of(names(pob.region.edad))) 

y3 <- fexp1 |> 
  mutate(llave=str_replace_all(paste0(region,"_",SEXO),"\\s|-|,|\\+","_"),
         n=1) |>
  select(REGISTRO,llave,n) |> 
  pivot_wider(names_from = llave,values_from = n) |> 
  mutate(across(where(is.numeric),~replace_na(.,0))) |> 
  relocate(REGISTRO,any_of(names(pob.region.sexo))) 

y4 <- fexp1 |> 
  mutate(llave=str_replace_all(paste0(region,"_",RESTRATO),"\\s|-|,|\\+","_"),
         n=1) |>
  select(REGISTRO,llave,n) |> 
  pivot_wider(names_from = llave,values_from = n) |> 
  mutate(across(where(is.numeric),~replace_na(.,0))) |> 
  relocate(REGISTRO,any_of(names(pob.region.estrato))) 

y5 <- fexp1 |>
  mutate(llave=str_replace_all(paste0(region,"_",ZONA),"\\s|-|,|\\+","_"),
         n=1) |>
  select(REGISTRO,llave,n) |>
  pivot_wider(names_from = llave,values_from = n) |>
  mutate(across(where(is.numeric),~replace_na(.,0))) |>
  relocate(REGISTRO,any_of(names(pob.region.zona)))

enc<- fexp1 |> 
  select(REGISTRO,estrato=region,d1k) |> 
  left_join(y1) |> 
  left_join(y2) |>
  left_join(y3) |>
  left_join(y4) |> 
  left_join(y5) |> 
  relocate(REGISTRO,estrato,d1k,
          any_of(names(pob.region.tamano)),
           any_of(names(pob.region.edad)),
           any_of(names(pob.region.sexo)),
          any_of(names(pob.region.estrato)),
           any_of(names(pob.region.zona))
          )

dsg<- enc |> 
  as_survey_design(ids = 1,strata = estrato,weights = d1k)

pob.tot<- c(pob.region.tamano,pob.region.edad,pob.region.sexo,pob.region.estrato,pob.region.zona)

(diffe<- setdiff(names(dsg$variables),names(pob.tot))) ## solo registro estrato y d1k

dd<- c(names(dsg$variables)[!names(dsg$variables)%in% diffe])
a<- names(pob.tot)

setdiff(a,dd)

rake.dsg<- calibrate(design = dsg,
                     formula = ~0+.-REGISTRO-estrato-d1k,
                     calfun = "raking",
                     population = pob.tot,
                     force = TRUE, maxit = 1000)

quantile(weights(rake.dsg), probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 1))
trim.design<- trimWeights(design= rake.dsg, lower= 1, upper= quantile(weights(rake.dsg), 0.90))

rake.dsg<- calibrate(design = trim.design,
                     formula = ~0+.-REGISTRO-estrato-d1k,
                     calfun = "raking",
                     population = pob.tot,
                     maxit = 3000,
                     force = TRUE)
### va con trim
quantile(weights(rake.dsg), probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.97, 1))

enc$FACTOR<- weights(rake.dsg) 

encuestas<- fexp1 |> 
            left_join(enc |> select(REGISTRO,FACTOR),by=c("REGISTRO")) |>
            verify(FACTOR >= 1)

## fase previa

(ver_1<- poblacion |>
  group_by(region,tamano) |> 
  summarise(p=sum(total)) |> 
  left_join(encuestas |> group_by(region,tamano) |> summarise(urep=sum(FACTOR))) |> 
  verify(round(urep)==round(p) ))

(ver_2<- poblacion |>
  group_by(region,REDAD) |> 
  summarise(p=sum(total)) |> 
  left_join(encuestas |> group_by(region,REDAD) |> summarise(urep=sum(FACTOR))) |> 
  verify(round(urep)==round(p) ))

(ver_3<- poblacion |>
  group_by(region,SEXO) |> 
  summarise(p=sum(total)) |> 
  left_join(encuestas |> group_by(region,SEXO) |> summarise(urep=sum(FACTOR))) |> 
  verify(round(urep)==round(p) ))

(ver_4<- pobR |>
   group_by(region,RESTRATO) |> 
  summarise(p=sum(pob)) |> 
  left_join(encuestas |>               
                 group_by(region,RESTRATO) |> 
              summarise(urep=sum(FACTOR))) |> 
  verify(round(urep)==round(p) ))

(ver_5<- poblacion |>
    group_by(region,ZONA) |> 
    summarise(p=sum(total)) |> 
    left_join(encuestas |> group_by(region,ZONA) |> summarise(urep=sum(FACTOR))) |> 
    verify(round(urep)==round(p) ))

(flaboral <- encuestas |> 
            group_by(ACTIV_SEMANA_PASADA) |> 
            summarise(ty = sum(FACTOR)) |> 
            mutate(p = ty/sum(ty)) |> 
    arrange(p))

tablas[[7]] |> mutate(p = ECV/sum(ECV)) |> arrange(p)

(nEduc <- encuestas |> 
    group_by(NIVEL_EDU) |> 
    summarise(ty = sum(FACTOR)) |> 
    mutate(p = ty/sum(ty)) |> 
    arrange(p))

tablas[[6]] |> mutate(p = ECV/sum(ECV)) |> arrange(p)

(salud <- encuestas |> 
    group_by(REGIMEN_SS) |> 
    summarise(ty = sum(FACTOR)) |> 
    mutate(p = ty/sum(ty)) |> 
    arrange(p))

tablas[[8]] |> mutate(p = ECV/sum(ECV)) |> arrange(p)

(tipo_viv <- encuestas |> 
    group_by(TIPO_VIVIENDA) |> 
    summarise(ty = sum(FACTOR)) |> 
    mutate(p = ty/sum(ty)) |> 
    arrange(p))

tablas[[9]] |> mutate(p = ECV/sum(ECV)) |> arrange(p)

(zon <- encuestas |> 
    group_by(ZONA) |> 
    summarise(ty = sum(FACTOR)) |> 
    mutate(p = ty/sum(ty)))

tablas[[10]] |> mutate(p = ECV/sum(ECV))

res <- encuestas |> 
  mutate(TAMA = as.character(tamano)) |> 
  mutate(
    CLASIF = case_when(
      TAMA %in% c("Bogotá, D.C.", "Medellín", "Cali", "Barranquilla", "Bucaramanga") ~ "Principales",
      TAMA %in% c("Grandes", "Ibagué", "Soacha", "Cartagena de Indias", "Villavicencio", "San José de Cúcuta") ~ "Intermedios",
      TRUE ~ "Resto"
    ))

table(res$CLASIF, useNA = "a")

encuestas_out<- res |> 
                mutate(region = as.factor(region),
                       ciudades = as.factor(CLASIF)) |>   ## union
                select(REGISTRO,FACTOR, ciudades)

skimr::skim(encuestas_out$FACTOR)
sum(encuestas_out$FACTOR)
sum(poblacion$total)

poblacion |> ungroup() |>  group_by(region) |> summarise(total=sum(total)) |> mutate(p = round(total/sum(total)*100, 1))
  
glimpse(encuestas)

write_sav(encuestas_out,"output/FACTOR_CC893201HPOL_RCMMZ16.sav")

##### Ficha técnica

e4p(N=sum(poblacion$total), 
    n=nrow(encuestas_out), 
    DEFF=2.0, P=0.5)$Margin_of_error

## mpios
length(
expss::read_spss("data/CC893201_BASE_POLITICA_REVISTA_CAMBIO.sav") |> 
  mutate(DANE = str_pad(DANE, 5, "left", pad = "0")) |> 
  select(DANE) |> 
  unique() |> pull()
)

## encuestas por region
encuestas |> 
  count(region) |> 
  adorn_totals()


