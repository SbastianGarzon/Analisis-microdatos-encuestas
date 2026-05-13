rm(list = ls())
options(warn =  -1)
library(pacman)

p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr,expss)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 

####################   PROCESAMIENTO
#########---Lectura de la base de datos

datos <- expss::read_spss("data/CC893201_BASE_POLITICA_REVISTA_CAMBIO.sav")

rm(expande,datosr)

names(datos)
vars <-  datos |>
  select(F1:P20) |>
  select(-contains("OTRO")) |>  # otro cuales
  select(!where(is.character)) |> # no preg abiertas
  names()

df <- datos |> 
  mutate(across(any_of(vars), ~as_factor(.))) |> 
  select(REGISTRO, any_of(vars),desag=REGION,TAMA, FACTOR) |> 
  mutate(strata=paste0(desag,"_",TAMA)) |> 
  pivot_longer(cols = -c(REGISTRO, FACTOR,desag,TAMA,strata),
               names_to = "pregunta",
               values_to = "respuesta") 

## totales efectivos por pregunta
n_tot<- df |> 
  count(pregunta,respuesta) |> 
  mutate(agrupacion=as_factor("TOTAL"))

n_efectivos<- bind_rows(n_tot)
rm(n_tot,n_agrupa)

dsg <- df |> 
  as_survey_design(ids = REGISTRO,
                   weights = FACTOR,
                   nest = TRUE)

res <- dsg |> 
  group_by(pregunta, respuesta) |> 
  summarise(theta = survey_prop(vartype=c("se", "ci"), na.rm = T)) |> 
  mutate(me = 1.96*theta_se) |> 
  mutate(agrupacion = "TOTAL")

quantile(res$me, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1))

etiq<- c()
for (i in 1:length(vars)) {
  etiq[i]<- var_lab(datos |> pull(vars[i])) 
}

etiquetas<- data.frame(pregunta=vars,etiqueta=etiq)

errores <- bind_rows(res) |> 
  left_join(n_efectivos,by = join_by(pregunta, respuesta, agrupacion)) |> 
  filter(!is.na(respuesta)) |> 
  left_join(etiquetas) |> 
  mutate(pregunta = as.numeric(str_extract(pregunta, "(?<=P)[0-9]+"))) |> 
  mutate(respuesta = str_replace(respuesta, "^[0-9\\.\\s]+", "")) |>
  mutate(respuesta = gsub("<strong>\\(NO LEER\\)</strong>", "", respuesta)) |> 
  mutate(respuesta = gsub("<strong>\\s*\\(E:\\s*NO LEER\\)\\s*</strong>", "", respuesta)) |>
  mutate(respuesta = gsub("<strong>@1</strong>", "", respuesta)) |> 
  relocate(agrupacion, pregunta,etiqueta, respuesta, theta, theta_se, marg_err = me) |> 
  arrange(desc(agrupacion), pregunta, desc(theta)) |> 
  mutate(Publicable=ifelse(marg_err<0.03,"Si","No"))

table(errores$Publicable,errores$agrupacion)

conteos <- df |> 
  filter(!is.na(respuesta)) |> 
  count(pregunta)

writexl::write_xlsx(list(Errores= errores, conteos_pregunta=conteos), "output/margenes_de_error.xlsx")
