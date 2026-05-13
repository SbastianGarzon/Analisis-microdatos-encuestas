rm(list = ls())

library(pacman)

p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
       tmap, tmaptools, leaflet, rgdal, stratification,tmap)

source("src/funcioncarto.R")

marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 

muestra<- read_excel("distribuciones/20260316 Muestra OP 2138 (E).xlsx",sheet = 5) |> 
  mutate(encuestas=Muestra,dane=mpio)
# 
muestra_con<- muestra |> 
  filter(!Tamaño %in% c("Medianos","Pequeños")) |> 
  mutate(Bajo=encuestas*0.46, Medio=encuestas*0.33,Alto=encuestas*0.28) |> 
  select(dane, Alto,Medio,Bajo) |> 
  pivot_longer(cols = -dane)

muestra_rur<- muestra |> 
  filter(Rural!=0)

muestra_sin<-  muestra |> 
  filter(Tamaño %in% c("Medianos","Pequeños")| dane %in% c("18001" ,"41551" ,"81001", "85001", "27001")) 

  
carto_urb_con<- fcartografia(marco = marco,df = muestra_con,col_dane = "dane",
                     col_encuestas = "value",minvivienda = 10,encxmanzana = 4,
                     desag = "n",
                     col_desag = "name",semilla = 20260316)

carto_urb_sin<- fcartografia(marco = marco,df = muestra_sin,col_dane = "dane",
                             col_encuestas = "encuestas",minvivienda = 8,
                             encxmanzana = 4,inicio_idmapa  = max(carto_urb_con$ID_MAPA),
                             semilla = 20260316)

carto_rur<- fcartografia(marco = marco,df = muestra_rur,col_dane = "dane",
                         manzanas = "vereda",
                         col_encuestas = "Rural",
                         semilla=20260316) |> 
  mutate(COD_DPTO=substr(COD_MPIO,1,2))

ver<- carto_urb_con |> 
  count(Municipio,NIVEL_SCE)

carto<- bind_rows(carto_urb_con,carto_urb_sin)

test<- carto |> 
  count(COD_MPIO) |> 
  mutate(n=n*4) |> 
  full_join(muestra,by = c("COD_MPIO"="dane")) |> 
  mutate(test=encuestas<=n)

test2<- carto |> 
  select(COD_MPIO,Municipio) |> 
  as_tibble() |> 
  unique()

nrow(muestra_rur)==length(unique(carto_rur$COD_MPIO))

length(unique(c(carto$COD_MPIO)))
length(unique(muestra$dane))

setdiff(muestra$dane,carto$COD_MPIO)

## empieza outputs

dir.create("salida")
dir.create("capas")

proyecto<- "OP_2138"

nombre<-paste0("salida/",
               gsub("-","", Sys.Date()),
               "_Cartografia_",proyecto,".xlsx")

write_xlsx(list(Urbano=bind_rows(carto_urb_sin,carto_urb_con),
                Rural=carto_rur),path = nombre)

# kml
carto<- bind_rows(carto_urb_con,carto_urb_sin)

ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
  unique()
mpios<- ver$cods
nombres<- gsub("\\."," ", ver$noms) 
deptos<- gsub("\\."," ", ver$deps) 

for (i in 1:length(mpios)) {
  
  salida_capa<- marco |> 
    filter(COD_MPIO==mpios[i]) |> 
    filter(COD_DANE %in% carto$COD_DANE) |> 
    select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
    st_simplify(preserveTopology = T,dTolerance = 1)
  
  nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
  
  st_write(salida_capa,nombre,driver = "kml",append = F)
  
}

 carto<- carto_rur
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
for (i in 1:length(mpios)) {

  salida_capa<- marco |>
    filter(COD_MPIO==mpios[i]) |>
    filter(CODIGO_VER %in% carto$CODIGO_VER) |>
    select(NOMBRE_VER,id_manzana,COD_MPIO,NOMBRE_DPT,NOMBRE_MPI) |>
    st_simplify(preserveTopology = T,dTolerance = 1)

  nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_RUR_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
  
  st_write(salida_capa,nombre,driver = "kml",append = F)

}
 
 ################ Carto adicional Soledad
 ### Las manzanas enviadas eran peligrosas para realizar el campo
 
 rm(list = ls())
 
 library(pacman)
 
 p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
        tmap, tmaptools, leaflet, rgdal, stratification,tmap)
 
 source("src/funcioncarto.R")
 
 marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 
 
 ### manzanas ya enviadas
 
 list.files("data")
 
 files <- list.files(
   path = "salida",
   pattern = "^2026.*2138.*\\.xlsx$",
   full.names = TRUE
 )
 
 yasel <- map_dfr(files, read_excel)
 
 muestra<- tibble(dane="08758",encuestas=34)  #Soledad peligrosas
 
 nuevo_marco<- marco |> filter(!id_manzana %in% yasel$id_manzana)
 
 
 carto_urb_sin<- fcartografia(marco = marco,df = muestra,col_dane = "dane",
                              col_encuestas = "encuestas",minvivienda = 8,
                              encxmanzana = 4,
                              semilla = 20260317)
 
 ver<- carto_urb_con |> 
   count(Municipio,NIVEL_SCE)
 
 carto<- bind_rows(carto_urb_con,carto_urb_sin)
 
 test<- carto |> 
   count(COD_MPIO) |> 
   mutate(n=n*4) |> 
   full_join(muestra,by = c("COD_MPIO"="dane")) |> 
   mutate(test=encuestas<=n)
 
 test2<- carto |> 
   select(COD_MPIO,Municipio) |> 
   as_tibble() |> 
   unique()
 
 nrow(muestra_rur)==length(unique(carto_rur$COD_MPIO))
 
 length(unique(c(carto$COD_MPIO)))
 length(unique(muestra$dane))
 
 setdiff(muestra$dane,carto$COD_MPIO)
 
 ## empieza outputs
 
 proyecto<- "OP_2138_adicional"
 
 nombre<-paste0("salida/",
                gsub("-","", Sys.Date()),
                "_Cartografia_",proyecto,".xlsx")
 
 write_xlsx(list(Urbano=bind_rows(carto_urb_sin)),path = nombre)
 
 # kml
 carto<- bind_rows(carto_urb_sin)# carto_urb_con
 
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |> 
     filter(COD_MPIO==mpios[i]) |> 
     filter(COD_DANE %in% carto$COD_DANE) |> 
     select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
 
 ################ Carto municipios reemplazos por peligro

 rm(list = ls())
 
 library(pacman)
 
 p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
        tmap, tmaptools, leaflet, rgdal, stratification,tmap)
 
 source("src/funcioncarto.R")
 
 marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 
 
 ### manzanas ya enviadas
 
 list.files("data")
 
 files <- list.files(
   path = "salida",
   pattern = "^2026.*2138.*\\.xlsx$",
   full.names = TRUE
 )
 
 yasel <- map_dfr(files, read_excel)
 
 muestra<- read_excel("distribuciones/20260318_reemplazos.xlsx") 
 
 nuevo_marco<- marco |> filter(!id_manzana %in% yasel$id_manzana)
 
 carto_urb_sin<- fcartografia(marco = nuevo_marco,df = muestra,col_dane = "dane",
                              col_encuestas = "encuestas",minvivienda = 8,
                              encxmanzana = 4,
                              semilla = 20260318)
 
 carto_rur<- fcartografia(marco = nuevo_marco,df = muestra,col_dane = "dane",
                          manzanas = "vereda",
                          col_encuestas = "Rural",
                          semilla=20260318) |>
   mutate(COD_DPTO=substr(COD_MPIO,1,2))
 
 ver<- carto_urb_con |> 
   count(Municipio,NIVEL_SCE)
 
 carto<- bind_rows(carto_urb_sin)
 
 test<- carto |> 
   count(COD_MPIO) |> 
   mutate(n=n*4) |> 
   full_join(muestra,by = c("COD_MPIO"="dane")) |> 
   mutate(test=encuestas<=n)
 
 test2<- carto |> 
   select(COD_MPIO,Municipio) |> 
   as_tibble() |> 
   unique()
 
 nrow(muestra)==length(unique(carto_rur$COD_MPIO))
 
 length(unique(c(carto$COD_MPIO)))
 length(unique(muestra$dane))
 
 setdiff(muestra$dane,carto$COD_MPIO)
 
 ## empieza outputs
 
 proyecto<- "OP_2138_reemplazos"
 
 nombre<-paste0("salida/",
                gsub("-","", Sys.Date()),
                "_Cartografia_",proyecto,".xlsx")
 
 write_xlsx(list(Urbano=bind_rows(carto_urb_sin),
                 Rural=carto_rur),path = nombre)
 
 # kml
 carto<- bind_rows(carto_urb_sin)# carto_urb_con
 
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |> 
     filter(COD_MPIO==mpios[i]) |> 
     filter(COD_DANE %in% carto$COD_DANE) |> 
     select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
 
 carto<- carto_rur
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |>
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms)
 deptos<- gsub("\\."," ", ver$deps)
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |>
     filter(COD_MPIO==mpios[i]) |>
     filter(CODIGO_VER %in% carto$CODIGO_VER) |>
     select(NOMBRE_VER,id_manzana,COD_MPIO,NOMBRE_DPT,NOMBRE_MPI) |>
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_RUR_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
 
 ################ Carto adicional
 ##### Cartagena estrato alto
 
 rm(list = ls())
 
 library(pacman)
 
 p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
        tmap, tmaptools, leaflet, rgdal, stratification,tmap)
 
 source("src/funcioncarto.R")
 
 marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 
 
 ### manzanas ya enviadas
 
 list.files("data")
 
 files <- list.files(
   path = "salida",
   pattern = "^2026.*2138.*\\.xlsx$",
   full.names = TRUE
 )
 
 yasel <- map_dfr(files, read_excel)
 
 muestra<- tibble(dane="13001",encuestas=34,nse="Alto")  #Cartagena alto
 
 nuevo_marco<- marco |> filter(!id_manzana %in% yasel$id_manzana)
 
 carto_urb_con<- fcartografia(marco = nuevo_marco,df = muestra,col_dane = "dane",
                              col_encuestas = "encuestas",minvivienda = 10,encxmanzana = 4,
                              desag = "n",
                              col_desag = "nse",semilla = 20260319)
 
 ver<- carto_urb_con |> 
   count(Municipio,NIVEL_SCE)
 
 carto<- bind_rows(carto_urb_con,carto_urb_sin)
 
 test<- carto |> 
   count(COD_MPIO) |> 
   mutate(n=n*4) |> 
   full_join(muestra,by = c("COD_MPIO"="dane")) |> 
   mutate(test=encuestas<=n)
 
 test2<- carto |> 
   select(COD_MPIO,Municipio) |> 
   as_tibble() |> 
   unique()
 
 nrow(muestra_rur)==length(unique(carto_rur$COD_MPIO))
 
 length(unique(c(carto$COD_MPIO)))
 length(unique(muestra$dane))
 
 setdiff(muestra$dane,carto$COD_MPIO)
 
 ## empieza outputs
 
 proyecto<- "OP_2138_adicional_ctg"
 
 nombre<-paste0("salida/",
                gsub("-","", Sys.Date()),
                "_Cartografia_",proyecto,".xlsx")
 
 write_xlsx(list(Urbano=bind_rows(carto_urb_con)),path = nombre)
 
 # kml
 carto<- bind_rows(carto_urb_con)# carto_urb_con
 
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |> 
     filter(COD_MPIO==mpios[i]) |> 
     filter(COD_DANE %in% carto$COD_DANE) |> 
     select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
 
 tmap_mode("view")
 tm_shape(marco |> filter(id_manzana %in% carto$id_manzana)) +
   tm_polygons()
 
 ################ Carto adicional
 ##### Bucaramanga 
 
 rm(list = ls())
 
 library(pacman)
 
 p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
        tmap, tmaptools, leaflet, rgdal, stratification,tmap)
 
 source("src/funcioncarto.R")
 
 marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 
 
 ### manzanas ya enviadas
 
 list.files("data")
 
 files <- list.files(
   path = "salida",
   pattern = "^2026.*2138.*\\.xlsx$",
   full.names = TRUE
 )
 
 yasel <- map_dfr(files, read_excel)
 
 ## mapas*4*3*
 
 muestra<- tibble(dane=c("68001","68001"),
                  encuestas=c(48,12),
                  nse=c(3,2))  #Bucaramnga
 
 nuevo_marco<- marco |> filter(!id_manzana %in% yasel$id_manzana)
 
 
 carto_urb_con<- fcartografia(marco = nuevo_marco,df = muestra,col_dane = "dane",
                              col_encuestas = "encuestas",
                              minvivienda = 10,
                              encxmanzana = 4,
                              desag = "e",
                              col_desag = "nse",semilla = 20260319)
 
 ver<- carto_urb_con |> 
   count(Municipio,NIVEL_SCE) 
 
 carto<- bind_rows(carto_urb_con,carto_urb_sin)
 
 test<- carto |> 
   count(COD_MPIO) |> 
   mutate(n=n*4) |> 
   full_join(muestra,by = c("COD_MPIO"="dane")) |> 
   mutate(test=encuestas<=n)
 
 test2<- carto |> 
   select(COD_MPIO,Municipio) |> 
   as_tibble() |> 
   unique()
 
 nrow(muestra_rur)==length(unique(carto_rur$COD_MPIO))
 
 length(unique(c(carto$COD_MPIO)))
 length(unique(muestra$dane))
 
 setdiff(muestra$dane,carto$COD_MPIO)
 
 ## empieza outputs
 
 proyecto<- "OP_2138_adicional_bga"
 
 nombre<-paste0("salida/",
                gsub("-","", Sys.Date()),
                "_Cartografia_",proyecto,".xlsx")
 
 write_xlsx(list(Urbano=bind_rows(carto_urb_con)),path = nombre)
 
 # kml
 carto<- bind_rows(carto_urb_con)# carto_urb_con
 
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |> 
     filter(COD_MPIO==mpios[i]) |> 
     filter(COD_DANE %in% carto$COD_DANE) |> 
     select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
 
 tmap_mode("view")
 tm_shape(marco |> filter(id_manzana %in% carto$id_manzana)) +
   tm_polygons()
 
 ################ Carto reemplazos 
 ##### Faca emergencia climatica
 
 rm(list = ls())
 
 library(pacman)
 
 p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
        tmap, tmaptools, leaflet, rgdal, stratification,tmap)
 
 source("src/funcioncarto.R")
 
 marco <- st_read("data/MARCO_2026.shp", stringsAsFactors=F) 
 
 ### manzanas ya enviadas
 
 list.files("data")
 
 files <- list.files(
   path = "salida",
   pattern = "^2026.*2138.*\\.xlsx$",
   full.names = TRUE
 )
 
 yasel <- map_dfr(files, read_excel)
 
 muestra<- read_excel("distribuciones/20260320_Op2138_reemplazos.xlsx") 
 
 
 nuevo_marco<- marco |> filter(!id_manzana %in% yasel$id_manzana)
 
 # 
 muestra_con<- muestra |>
   filter(!Tamaño %in% c("Medianos","Pequeños")) |>
   mutate(Bajo=encuestas*0.46, Medio=encuestas*0.33,Alto=encuestas*0.28) |>
   select(dane, Alto,Medio,Bajo) |>
   pivot_longer(cols = -dane)
 
 carto_urb_con<- fcartografia(marco = nuevo_marco,df = muestra_con,col_dane = "dane",
                              col_encuestas = "value",minvivienda = 10,encxmanzana = 4,
                              desag = "n",
                              col_desag = "name",semilla = 20260320)
 

 ver<- carto_urb_con |> 
   count(Municipio,NIVEL_SCE)
 
 carto<- bind_rows(carto_urb_con)
 
 test<- carto |> 
   count(COD_MPIO) |> 
   mutate(n=n*4) |> 
   full_join(muestra,by = c("COD_MPIO"="dane")) |> 
   mutate(test=encuestas<=n)
 
 test2<- carto |> 
   select(COD_MPIO,Municipio) |> 
   as_tibble() |> 
   unique()
 
 nrow(muestra)==length(unique(carto_rur$COD_MPIO))
 
 length(unique(c(carto$COD_MPIO)))
 length(unique(muestra$dane))
 
 setdiff(muestra$dane,carto$COD_MPIO)
 
 ## empieza outputs
 
 proyecto<- "OP_2138_reemplazos_funza"
 
 nombre<-paste0("salida/",
                gsub("-","", Sys.Date()),
                "_Cartografia_",proyecto,".xlsx")
 
 write_xlsx(list(Urbano=bind_rows(carto_urb_con)),path = nombre)
 
 # kml
 carto<- bind_rows(carto_urb_con)# carto_urb_con
 
 ver<- data.frame(cods=carto$COD_MPIO,noms=carto$Municipio,deps=carto$Departamento) |> 
   unique()
 mpios<- ver$cods
 nombres<- gsub("\\."," ", ver$noms) 
 deptos<- gsub("\\."," ", ver$deps) 
 
 for (i in 1:length(mpios)) {
   
   salida_capa<- marco |> 
     filter(COD_MPIO==mpios[i]) |> 
     filter(COD_DANE %in% carto$COD_DANE) |> 
     select(id_manzana,NOMBRE_DPT,NOMBRE_MPI,ESTRATO,LocNombre) |> 
     st_simplify(preserveTopology = T,dTolerance = 1)
   
   nombre<-paste0("capas/",format(Sys.Date(),"%Y%m%d"),"_capa_URB_",proyecto,"_",nombres[i],"_",deptos[i],".kml")
   
   st_write(salida_capa,nombre,driver = "kml",append = F)
   
 }
