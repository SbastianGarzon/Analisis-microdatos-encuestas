rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, haven, survey, srvyr, readxl)

packageVersion("nonprobsvy") # la versión utilizada para el calculo del propensity es la 0.1.1
library(nonprobsvy)
options(warn = -1)

load("output/dataprob.rdata")

regiones<- read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP.xlsx") |> 
  select(1:4,region,tamano) |>  
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  unique()

dir("data/")

t_encuestas <- foreign::read.spss("data/CC892901_BASE_REVISTA_CAMBIO.sav",use.value.labels = F,to.data.frame = T) |> 
  filter(METODO == 3)

table(t_encuestas$GENERO, useNA = "a")

encuestas<- t_encuestas |> 
            select(REGISTRO,mpio=DANE,GENERO,GR_ESTRATO,REDAD, ZONA,
                   edu=P20, #NIVEL_EDU
                   viv=P18, #TIPO_VIVIENDA
                   activ=P19, #ACTIV_SEMANA_PASADA
                   ss = P21) |> #REGIMEN_SS   
  mutate(mpio=str_pad(str_trim(mpio), 5, pad="0")) |> 
  mutate(dp=substr(mpio,1,2)) |> 
  relocate(dp,.after = REGISTRO) |> 
            mutate(SEXO = factor(
                      ifelse(GENERO == 1, 1,
                      ifelse(GENERO == 2, 2, NA)),
                      levels = 1:2, 
                      labels = c("Hombre", "Mujer"))) |> 
  mutate(ZONA = factor(ZONA, levels = 1:2, labels = c("Urbano", "Rural"))) |> 
  mutate(REDAD = factor(REDAD, levels = 1:5,
                        labels =  c("18 a 24",
                                    "25 a 34",
                                    "35 a 44",
                                    "45 a 54",
                                    "55 o más"
                        ))) |> 
  mutate(RESTRATO = factor(GR_ESTRATO,levels=1:3,
                           c("Bajo", "Medio", "Alto"))) |> 
  mutate(NIVEL_EDU= factor(case_when(
                              edu %in% c(1,2,3)~1,
                              edu %in% c(4)~2,
                              edu %in% c(5,6,8,10)~3,
                              edu %in% c(7,9)~4,
                              edu %in% c(11,12)~5,
                              edu %in% c(13)~6),
                              levels = 1:6, 
                              labels = c('Primario o menos',
                                         'Básica secundaria (6 a 9)',
                                         'Media (10 a 13)',
                                         'Técnico/tecnológico',
                                         "Universitario",
                                         "Posgrado")))  |> 
  mutate(ACTIV_SEMANA_PASADA=factor(activ, levels = 1:6, 
                        labels = c('Trabajando',
                                   'Buscando trabajo',
                                   'Estudiando',
                                   'Oficios del hogar',
                                   'Incapacitado permanente para trabajar',
                                   'Otra actividad'))) |> 
  mutate(REGIMEN_SS=factor(ss,levels=c(1:3,9), labels=c("Contributivo",
                                                         "Especial",
                                                         "Subsidiado",
                                                         "NS/NR"))) |> 
  mutate(TIPO_VIVIENDA = factor(viv, levels = 1:5, labels = c("Casa",
                                                              "Apartamento",
                                                              "Cuarto(s)",
                                                              "Vivienda tradicional indigena",
                                                              "Otro"))) |> 
  left_join(regiones) |> 
  mutate(y=1) |> 
  mutate(tamano = ifelse(!tamano %in% c("Grandes", "Medianos", "Pequeños"), "IF", tamano)) |> 
  mutate(tamano = as.factor(gsub(",", "", tamano))) |> 
  select(REGISTRO,dp,mpio,region,tamano,
         SEXO,RESTRATO,TIPO_VIVIENDA, REDAD,
         NIVEL_EDU,ACTIV_SEMANA_PASADA,REGIMEN_SS,ZONA, y) 

########### df ECV


Prob <- df |> 
  mutate(REGISTRO = 1000000 + 1:n(),
         y=0) |> 
  mutate(tamano = ifelse(!tamano %in% c("Grandes", "Medianos", "Pequeños"), "IF", tamano)) |> 
  mutate(tamano = as.factor(gsub(",", "", tamano))) |> 
  select(-contains("CONS"),-DIRECTORIO)   


janitor::compare_df_cols(encuestas,Prob)

dataModel <- bind_rows(encuestas, Prob) 

table(dataModel$region, dataModel$y, useNA = "a")
table(dataModel$tamano, dataModel$y, useNA = "a")
table(dataModel$SEXO, dataModel$y, useNA = "a")
table(dataModel$REDAD, dataModel$y, useNA = "a")
table(dataModel$RESTRATO, dataModel$y, useNA = "a")
table(dataModel$NIVEL_EDU, dataModel$y, useNA = "a")
table(dataModel$ACTIV_SEMANA_PASADA, dataModel$y, useNA = "a")
table(dataModel$REGIMEN_SS, dataModel$y, useNA = "a")
table(dataModel$TIPO_VIVIENDA, dataModel$y, useNA = "a")
table(dataModel$ZONA, dataModel$y, useNA = "a")

(N <- sum(dataModel$FEX_C, na.rm = T))

###############----------- Modelo IPW

dsg <- svydesign(ids = ~1, 
                 weights = ~FEX_C,
                 strata = ~region,
                 data = Prob)

svytotal(~region, dsg)

names(encuestas)

#packageVersion("nonprobsvy") # la versión utilizada para el calculo del propensity es la 0.1.1


est <- nonprob(
              selection = ~ region+ tamano + ZONA + RESTRATO + SEXO + REDAD + NIVEL_EDU + ACTIV_SEMANA_PASADA + REGIMEN_SS + TIPO_VIVIENDA,
              outcome = y ~ region +tamano + ZONA + RESTRATO + SEXO + REDAD  + NIVEL_EDU + ACTIV_SEMANA_PASADA + REGIMEN_SS + TIPO_VIVIENDA,
              svydesign = dsg,
              data = encuestas,
              method_selection = "logit",
              method_outcome = "glm",
              family_outcome = "binomial",
              control_selection = controlSel(h = 1, est_method_sel = "gee")
)

round(sum(est$weights)) == round(sum(Prob$FEX_C))
summary(est)

fexp1 <- data.frame(encuestas, d1k = weights(est))
skimr::skim(fexp1$d1k)
options(scipen = 999999)
quantile(fexp1$d1k, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))

sum(fexp1$d1k)
#test

test <- function(agrupacion){
  fexp1 |> 
    group_by({{ agrupacion }}) |> 
    summarise(rep = sum(d1k)) |> 
    left_join(
      Prob |> 
        group_by({{ agrupacion }}) |> 
        summarise(ECV = sum(FEX_C)),
      by = as.character(substitute(agrupacion))
    )
}

(t1 <- test(agrupacion = region))
(t2 <- test(agrupacion = tamano))
(t3 <- test(agrupacion = RESTRATO))
(t4 <- test(agrupacion = SEXO))
(t5 <- test(agrupacion = REDAD))
(t6 <- test(agrupacion = NIVEL_EDU))
(t7 <- test(agrupacion = ACTIV_SEMANA_PASADA))
(t8 <- test(agrupacion = REGIMEN_SS))
(t9 <- test(agrupacion = TIPO_VIVIENDA))
(t10 <- test(agrupacion = ZONA))

skimr::skim(fexp1$d1k)
save(fexp1, file = "output/fexp1.rds")


tablas <- list(t1=t1, t2=t2, t3=t3, t4=t4, t5=t5, t6=t6, t7=t7, t8=t8, t9=t9, t10 = t10)
save(tablas, file = "output/universos.rds")

