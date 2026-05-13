rm(list = ls())
options(warn =  -1)
library(pacman)

p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr,expss)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 

####################   PROCESAMIENTO
#########---Lectura de la base de datos

datos <- haven::read_sav("data/CC892901_BASE_REVISTA_CAMBIO.sav")

names(datos)
vars <- datos |> select(F1:P21) |> names()

df <- datos |> 
  mutate(across(any_of(vars), ~as_factor(.))) |> 
  select(REGISTRO, any_of(vars),REGION,GENERO, FACTOR) |> 
  pivot_longer(cols = -c(REGISTRO, FACTOR, REGION, GENERO),
               names_to = "pregunta",
               values_to = "respuesta")

n_tot<- df |> 
  count(pregunta,respuesta) |> 
  mutate(agrupacion=as_factor("TOTAL"))

n_agrupa<- df |> 
  mutate(agrupacion=as_factor(GENERO)) |> 
  count(pregunta,respuesta,agrupacion) 

n_efectivos<- bind_rows(n_tot,n_agrupa)
rm(n_tot,n_agrupa)

dsg <- df |> 
  as_survey_design(ids = REGISTRO,
                   weights = FACTOR,
                   nest = TRUE)

res <- dsg |> 
  group_by(pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T, deff = T)) |> 
  mutate(me = 1.96*theta_se) |> 
  mutate(agrupacion = "TOTAL")

res2 <- dsg |> 
  mutate(agrupacion=as_factor(GENERO)) |> 
  group_by(agrupacion, pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T, deff = T)) |> 
  mutate(me = 1.96*theta_se) |> 
  filter(!grepl("NS/NR", agrupacion))

quantile(res$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))
quantile(res2$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))

etiq<- c()
for (i in 1:length(vars)) {
  etiq[i]<- var_lab(datos |> pull(vars[i])) 
}

etiquetas<- data.frame(pregunta=vars,etiqueta=etiq)

errores <- bind_rows(res, res2) |> 
  left_join(n_efectivos,by = join_by(pregunta, respuesta, agrupacion)) |> 
  filter(!is.na(respuesta)) |> 
  left_join(etiquetas) |> 
  mutate(pregunta = as.numeric(str_extract(pregunta, "(?<=P)[0-9]+"))) |> 
  mutate(respuesta = str_replace(respuesta, "^[0-9\\.\\s]+", "")) |> 
  relocate(agrupacion, pregunta,etiqueta, respuesta, theta, theta_se, marg_err = me)

conteos <- df |> 
  filter(!is.na(respuesta)) |> 
  count(pregunta)



############### Consultas

df <- datos |> 
  mutate(across(any_of(vars), ~as_factor(.))) |> 
  select(REGISTRO, any_of(vars),GENERO, FACTOR) |> 
  pivot_longer(cols = -c(REGISTRO, FACTOR, P1_CONSULTA, GENERO),
               names_to = "pregunta",
               values_to = "respuesta")

n_agrupa<- df |> 
  mutate(agrupacion=as_factor(P1_CONSULTA)) |> 
  count(pregunta,respuesta,agrupacion) 

n_efectivos<- bind_rows(n_agrupa)
rm(n_agrupa)

dsg <- df |> 
  as_survey_design(ids = REGISTRO,
                   weights = FACTOR,
                   nest = TRUE)

res3 <- dsg |> 
  mutate(agrupacion=as_factor(P1_CONSULTA)) |> 
  group_by(agrupacion, pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T, deff = T)) |> 
  mutate(me = 1.96*theta_se) |> 
  filter(!grepl("NS/NR", agrupacion)) |> 
  filter(pregunta == "P1")

quantile(res3$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))

etiq<- c()
for (i in 1:length(vars)) {
  etiq[i]<- var_lab(datos |> pull(vars[i])) 
}

etiquetas<- data.frame(pregunta=vars,etiqueta=etiq)

errores1 <- res3 |> 
  left_join(n_efectivos,by = join_by(pregunta, respuesta, agrupacion)) |> 
  filter(!is.na(respuesta)) |> 
  left_join(etiquetas) |> 
  mutate(pregunta = as.numeric(str_extract(pregunta, "(?<=P)[0-9]+"))) |> 
  mutate(respuesta = str_replace(respuesta, "^[0-9\\.\\s]+", "")) |> 
  relocate(agrupacion, pregunta,etiqueta, respuesta, theta, theta_se, marg_err = me)|> 
  filter(!is.na(pregunta)) |> 
  filter(!pregunta %in% c(13, 15, 16))

############### Imagen Pres

df <- datos |> 
  mutate(across(any_of(vars), ~as_factor(.))) |> 
  select(REGISTRO, any_of(vars),GENERO, FACTOR) |> 
  pivot_longer(cols = -c(REGISTRO, FACTOR, P14, GENERO),
               names_to = "pregunta",
               values_to = "respuesta")

n_agrupa<- df |> 
  mutate(agrupacion=as_factor(P14)) |> 
  count(pregunta,respuesta,agrupacion) 

n_efectivos<- bind_rows(n_agrupa)
rm(n_agrupa)

dsg <- df |> 
  as_survey_design(ids = REGISTRO,
                   weights = FACTOR,
                   nest = TRUE)

res4 <- dsg |> 
  mutate(agrupacion=as_factor(P14)) |> 
  group_by(agrupacion, pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T, deff = T)) |> 
  mutate(me = 1.96*theta_se)|> 
  filter(!grepl("NS/NR", agrupacion))


quantile(res4$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))

etiq<- c()
for (i in 1:length(vars)) {
  etiq[i]<- var_lab(datos |> pull(vars[i])) 
}

etiquetas<- data.frame(pregunta=vars,etiqueta=etiq)

errores2 <- res4 |> 
  left_join(n_efectivos,by = join_by(pregunta, respuesta, agrupacion)) |> 
  filter(!is.na(respuesta)) |> 
  left_join(etiquetas) |> 
  mutate(pregunta = as.numeric(str_extract(pregunta, "(?<=P)[0-9]+"))) |> 
  mutate(respuesta = str_replace(respuesta, "^[0-9\\.\\s]+", "")) |> 
  relocate(agrupacion, pregunta,etiqueta, respuesta, theta, theta_se, marg_err = me) |> 
  filter(!is.na(pregunta)) |> 
  filter(!pregunta %in% c(13, 15, 16))

############### Hablo de politica

df <- datos |> 
  mutate(across(any_of(vars), ~as_factor(.))) |> 
  select(REGISTRO, any_of(vars),GENERO, FACTOR) |> 
  pivot_longer(cols = -c(REGISTRO, FACTOR, P17, GENERO),
               names_to = "pregunta",
               values_to = "respuesta")

n_agrupa<- df |> 
  mutate(agrupacion=as_factor(P17)) |> 
  count(pregunta,respuesta,agrupacion) 

n_efectivos<- bind_rows(n_agrupa)
rm(n_agrupa)

dsg <- df |> 
  as_survey_design(ids = REGISTRO,
                   weights = FACTOR,
                   nest = TRUE)

res5 <- dsg |> 
  mutate(agrupacion=as_factor(P17)) |> 
  group_by(agrupacion, pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T, deff = T)) |> 
  mutate(me = 1.96*theta_se) |> 
  filter(!grepl("NS/NR", agrupacion))


quantile(res5$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))

etiq<- c()
for (i in 1:length(vars)) {
  etiq[i]<- var_lab(datos |> pull(vars[i])) 
}

etiquetas<- data.frame(pregunta=vars,etiqueta=etiq)

errores3 <- res5 |> 
  left_join(n_efectivos,by = join_by(pregunta, respuesta, agrupacion)) |> 
  filter(!is.na(respuesta)) |> 
  left_join(etiquetas) |> 
  mutate(pregunta = as.numeric(str_extract(pregunta, "(?<=P)[0-9]+"))) |> 
  mutate(respuesta = str_replace(respuesta, "^[0-9\\.\\s]+", "")) |> 
  relocate(agrupacion, pregunta,etiqueta, respuesta, theta, theta_se, marg_err = me) |> 
  filter(!is.na(pregunta)) |> 
  filter(!pregunta %in% c(13, 15, 16))

erroresf <- bind_rows(errores, errores1, errores2, errores3) |> 
  arrange(pregunta)


writexl::write_xlsx(list(erroresf, conteos), "output/margenes_de_error.xlsx")

