rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, haven, readxl)

#### Encuesta Probabilística Encuesta nacional de calidad de vida - 2024 DANE

#nivel personas

t_educacion<- read_sav("data/prob2024/Educación.sav") |> 
              select(DIRECTORIO,
                     CONS_HOGAR = SECUENCIA_P,
                     CONS_PERSONA = SECUENCIA_ENCUESTA,
                     nivel_edu=P8587)


t_trabajo<-  read_sav("data/prob2024/Fuerza de trabajo.sav") |> 
             select(DIRECTORIO,
                    CONS_HOGAR = SECUENCIA_P,
                    CONS_PERSONA = SECUENCIA_ENCUESTA,
                    semana= P6240)

t_salud<-  read_delim("data/prob2024/Salud.CSV", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

t_salud<- t_salud |> 
  select(DIRECTORIO,
         CONS_HOGAR = SECUENCIA_P,
         CONS_PERSONA = SECUENCIA_ENCUESTA,
         seg_social= P6100,afiliado=P6090)


t_personas<- read_sav("data/prob2024/Características y composición del hogar.sav") |> 
  select(DIRECTORIO,
         CONS_HOGAR = SECUENCIA_P, 
         CONS_PERSONA = SECUENCIA_ENCUESTA,
         FEX_C,SEXO = P6020, EDAD = P6040)

# nivel vivenda

t_vivienda<- read_sav("data/prob2024/Datos de la vivienda.sav") |> 
mutate(mpio=paste0(P1_DEPARTAMENTO,P1_MUNICIPIO)) |> 
  select(DIRECTORIO,mpio,estrato=P8520S1A1, tipo_viv = P1070, area = CLASE)

t_municipios<- read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP.xlsx") |> 
               select(1:4,region,tamano) |>  
  clean_names() |> 
               mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
               mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
               unique()



df1<- reduce(list(t_personas,t_salud,t_trabajo,t_educacion),
                 left_join) |> 
      left_join(t_vivienda) |> 
      left_join(t_municipios)

get_dupes(df1, DIRECTORIO, CONS_HOGAR, CONS_PERSONA) # bien

df<- df1 |> 
  filter(EDAD>=18) |> 
#  filter(area == 1) |>  # Usamos el total NAL
  filter(estrato!="9",estrato!="8" ,!is.na(estrato)) |> 
  mutate(RESTRATO = factor(case_when(
                              estrato %in% c(0, 1, 2) ~ 1,
                              estrato== 3 ~ 2,
                              estrato%in% c(4,5,6) ~ 3),
                     levels = 1:3,
                     labels = c("Bajo", "Medio", "Alto"))) |>  
  mutate(SEXO = factor(SEXO, levels = 1:2, 
                              labels = c("Hombre", "Mujer")),
         REDAD = case_when( between(EDAD, 18, 24) ~ 1,
                            between(EDAD, 25, 34) ~ 2,
                            between(EDAD, 35, 44) ~ 3,
                            between(EDAD, 45, 54) ~ 4,
                            EDAD>=55~ 5)) |> 
  mutate(REDAD = factor(REDAD, 
                        levels = 1:5,
                        c("18 a 24",
                          "25 a 34",
                          "35 a 44",
                          "45 a 54",
                          "55 o más"))) |> 
  mutate(NIVEL_EDU=factor(case_when(
                            nivel_edu %in% c(1,2,3)~1,
                            nivel_edu %in% c(4)~2,
                            nivel_edu %in% c(5,6,8,10)~3,
                            nivel_edu %in% c(7,9)~4,
                            nivel_edu %in% c(11,12)~5,
                            nivel_edu %in% c(13)~6),
                            levels = 1:6, 
                        labels = c('Primario o menos',
                                   'Básica secundaria (6 a 9)',
                                   'Media (10 a 13)',
                                   'Técnico/tecnológico',
                                   "Universitario",
                                   "Posgrado"))) |> 
  mutate(ACTIV_SEMANA_PASADA=factor(semana, 
                                    levels = 1:6, 
                                    labels = c('Trabajando',
                                               'Buscando trabajo',
                                               'Estudiando',
                                               'Oficios del hogar',
                                               'Incapacitado permanente para trabajar',
                                               'Otra actividad'))) |> 
  mutate(REGIMEN_SS=case_when(seg_social ==1~1,
                              seg_social==2~2,
                              seg_social==3~3,
                              TRUE~4)) |> 
  mutate(REGIMEN_SS=factor(REGIMEN_SS,levels=1:4,labels=c("Contributivo",
                                                          "Especial",
                                                          "Subsidiado",
                                                          "NS/NR"))) |> 
  mutate(TIPO_VIVIENDA = factor(tipo_viv, levels = 1:5, labels = c("Casa",
                                                                   "Apartamento",
                                                                   "Cuarto(s)",
                                                                   "Vivienda tradicional indigena",
                                                                   "Otro"))) |> 
  mutate(ZONA = factor(area, levels = 1:2, labels = c("Urbano", "Rural"))) |> 
  mutate(dp=substr(mpio,1,2)) |> 
  filter(!is.na(NIVEL_EDU)) |> 
  select(DIRECTORIO,CONS_HOGAR,CONS_PERSONA,FEX_C,dp,mpio,region,tamano,
         SEXO,RESTRATO,TIPO_VIVIENDA,REDAD,
         NIVEL_EDU,ACTIV_SEMANA_PASADA,REGIMEN_SS, ZONA) 


table(df$region, useNA = "a")
table(df$tamano, useNA = "a")
table(df$SEXO,useNA = "a")  
table(df$RESTRATO,useNA = "a")
table(df$TIPO_VIVIENDA, useNA = "a")
table(df$REDAD,useNA = "a")
table(df$NIVEL_EDU,useNA = "a")
table(df$ACTIV_SEMANA_PASADA,useNA = "a")
table(df$REGIMEN_SS,useNA = "a")
table(df$ZONA,useNA = "a")

sum(df$FEX_C)

df |> 
  group_by(ZONA) |> 
  summarise(Fi = sum(FEX_C)) |> 
  mutate(pi = Fi/sum(Fi))

#dir.create("output")
save(df, file = "output/dataProb.rdata")

  
glimpse(df)
  
