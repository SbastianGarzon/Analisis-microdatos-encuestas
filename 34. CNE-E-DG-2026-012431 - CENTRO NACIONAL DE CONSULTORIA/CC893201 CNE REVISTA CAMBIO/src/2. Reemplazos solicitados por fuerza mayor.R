#########################################################################
########### SE SOLICITAN REEMPLAZOS PARA NARIÑO POR PROBLEMAS ########### 
########### DE ORDEN PUBLICO EN LOS MUNICIPIOS DE LA MUESTRA  ###########
#########################################################################

library(pacman)
p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr,tidyr)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 


source("src/0. Funciones.R")

muestra<- readxl::read_xlsx("output/20260313 Muestra OP 2138 (E).xlsx",
                            sheet = "Municipios") |> clean_names() |> 
mutate(Strata=paste0(region,"_",tamano))

cambios<- read_xlsx("data/cambios1.xlsx") |> clean_names()|> 
  mutate(Strata=paste0(region,"_",tamano))

poblacion <- readxl::read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP_actualizatam.xlsx") |>
  clean_names() |> 
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  filter(edades>=18)



nmuestra<- cambios |> 
  count(Strata,name = "nMpios") 

UPMs <- poblacion |>
  group_by(region, tamano, dp, dpnom, mpio, dpmp) |> 
  summarise(pob = sum(total),
            cab = sum(cabecera_total),
            rural = sum(rural_total)) |> 
  mutate(prur=rural/pob) |> 
  filter(prur < 0.9) |> 
  mutate(Strata=paste0(region,"_",tamano)) |> 
  filter(Strata %in% nmuestra$Strata) |> 
  left_join(nmuestra) |> 
  filter(!mpio %in% muestra$mpio)  ## ya seleccionados

{set.seed(20260318)  # Fecha de setup de la muestra
    nh <- UPMs |> ungroup() |>  distinct(Strata, nMpios) |> pull()
    res <- TeachingSampling::S.STpiPS(UPMs$Strata, x = UPMs$pob, nh = nh)
    sam1 <- UPMs[res[,1],]
}

reemplazos <- sam1

#########################################################################
########### SE SOLICITA REEMPLAR FACATATIVA POR CAUSAS DE     ########### 
########### EMERGENCIA CLIMATICA                              ###########
#########################################################################

library(pacman)
p_load(tidyverse,haven,janitor,survey,srvyr,readxl, assertr,tidyr)

options(survey.lonely.psu = "adjust",
        survey.multicore = T,
        survey.ultimate.cluster = T) 


source("src/0. Funciones.R")


muestra<- readxl::read_xlsx("output/20260313 Muestra OP 2138 (E).xlsx",
                            sheet = "Municipios") |> clean_names() |> 
  mutate(Strata=paste0(region,"_",tamano))

cambios<- read_xlsx("data/cambios2.xlsx") |> clean_names()|> 
  mutate(Strata=paste0(region,"_",tamano))

poblacion <- readxl::read_xlsx("data/Poblacion 2026 POSTCOVID TAM OP_actualizatam.xlsx") |>
  clean_names() |> 
  mutate(tamano = ifelse(tamano == "IF" | mpio %in% c("68001", "73001", "50001") , dpmp, tamano)) |> 
  mutate(region = ifelse(mpio == "11001", "Bogotá", region)) |> 
  filter(edades>=18)

nmuestra<- cambios |> 
  count(Strata,name = "nMpios") 

UPMs <- poblacion |>
  group_by(region, tamano, dp, dpnom, mpio, dpmp) |> 
  summarise(pob = sum(total),
            cab = sum(cabecera_total),
            rural = sum(rural_total)) |> 
  mutate(prur=rural/pob) |> 
  filter(prur < 0.9) |> 
  mutate(Strata=paste0(region,"_",tamano)) |> 
  filter(Strata %in% nmuestra$Strata) |> 
  left_join(nmuestra) |> 
  filter(!mpio %in% muestra$mpio)  ## ya seleccionados


{set.seed(20260318)  # Fecha de setup de la muestra
    nh <- UPMs |> ungroup() |>  distinct(Strata, nMpios) |> pull()
    res <- TeachingSampling::S.STpiPS(UPMs$Strata, x = UPMs$pob, nh = nh)
    sam2 <- UPMs[res[1],]
}

reemplazos2 <- sam2
