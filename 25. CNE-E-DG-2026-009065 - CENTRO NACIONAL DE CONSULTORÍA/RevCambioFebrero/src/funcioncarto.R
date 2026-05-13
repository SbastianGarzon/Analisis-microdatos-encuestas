
library(pacman)

p_load(sf, tidyverse, janitor, readxl, here, janitor, writexl,
       tmap, tmaptools, leaflet, rgdal, stratification)


# marco= MARCO 2026

# df = data frame con la cantidad de encuestas en formato vertical

# col_dane = nombre columna codigos dane

# col_encuestas = nombre columna con numero de encuestas

# desag = desagregacion , puede no usarse cuando es solo el mpio, o ser ("e", "E", "estrato"),
# ("n", "N", "nivel") ,o ("l","L", "localidad"),
#localidad puede ser solo para bogota, medellin, ibague, cartagena, cali o barranquilla

#  col_desag, nombre de la columna con desagregaciones, estrato debe ser solo (1,2,3,4,5,6)
# para localidades revisar que coincida con LocNombre O LocCodigo

# encxmanzana= cantidad de encuestas por manzana, valor por default 4

# minvivienda= minimo de viviendas en la manzana, valor por default 10

#manzanas= tipo de manzana a seleccionar, valor por default "manz_urb", 
# puede ser tambien "manz_rural" o "vereda"  si es vereda o manz_rural desag no debe ser usado

# semilla = semilla para aleatorizar, valor por default  "hoy", que pone la
#fecha del sistema en formato YYYYMMDD , tambien se puede colocar manualmente para reproducibilidad,
# se recomienda usar la fecha.

#locnombre = si la desagregacion es por localidad, selecciona si el match es por nombre o codigo.
#valor por default es F, esdecir por codigo

# inicio_idmapa=  para el caso de varias cartografias de una sola muestra, utilizar el valor maximo de la
#cartografia previa, valor por default 0.

fcartografia<- function(marco,df,col_dane,col_encuestas,
                        desag=NULL,col_desag=NULL,
                        encxmanzana=4,minvivienda=10,manzanas="manz_urb",semilla="hoy",
                        locnombre=F,inicio_idmapa=0){
  
  mpios<- df |> pull(any_of(col_dane))
  principales<- c("05001","11001","08001","68001","76001")
  
  if (semilla=="hoy"){
    
    semilla= as.numeric(format(Sys.Date(),"%Y%m%d"))
    
  }
  
  if (manzanas !="vereda") {
  
 
    marcoR<- marco |>
     filter(VIV>=minvivienda) |> 
      filter(COD_MPIO %in% mpios) |> 
      filter(tipo==manzanas)
    
  
  
  if (is.null(desag)) {
    
    maxcong<- marcoR |>
      as.data.frame() |> group_by(COD_MPIO) |> 
      summarise(nsecc=n_distinct(COD_SECC))
    
    dfR<- df |> 
      rename(nh=!!col_encuestas,COD_MPIO=!!col_dane) |> 
      mutate(nseg = ceiling(nh/encxmanzana*1.5),
             conglomerados = ceiling(nseg/3)) |> 
      left_join(maxcong,by = "COD_MPIO") |> 
      mutate(conglomerados=ifelse(conglomerados>=nsecc,nsecc,conglomerados)) |> 
      select(-nsecc)
    
    et1 <- marcoR %>% as.data.frame() %>% 
      count(COD_MPIO, COD_SECC) |> 
      filter(n>=3)|> select(-n)
    
    set.seed(semilla)
    et1$aleatorio <- runif(nrow(et1))
    
    segmentos_sel <- et1 %>% 
      left_join(dfR,by = "COD_MPIO") %>% 
      arrange(COD_MPIO, -aleatorio) %>% 
      group_by(COD_MPIO) %>% 
      mutate(Prioridad = 1:n()) %>% 
      filter(Prioridad <= conglomerados) |> 
      select(-aleatorio)
    
    set.seed(semilla)
    marcoR$aleatorio <- runif(nrow(marcoR))
    
    muestra <- marcoR %>%
      left_join(segmentos_sel,by=c("COD_MPIO","COD_SECC")) |> 
      filter(!is.na(conglomerados)) %>% 
      arrange( -aleatorio) %>% 
      group_by( COD_SECC) %>% 
      mutate(Prioridad = 1:n()) %>%
      ungroup() %>% 
      mutate(aselecc=ceiling(nseg/conglomerados)) |> 
      filter(Prioridad <= aselecc) |> 
      select(-aselecc)
    
    coord <- muestra |> 
      st_coordinates() |> 
      as_tibble() |> 
      group_by(L3) |> 
      summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
    
    muestra_out <- bind_cols(muestra, coord) |> as_tibble() |> 
      mutate(SECTOR = substr(COD_DANE, 15, 18),
             SECCION = substr(COD_DANE, 19, 20),
             MANZANA = substr(COD_DANE, 21, 22))
    
    
  } 
    else  if (desag %in% c("e","E","estrato","ESTRATO","Estrato")) {
      
    maxcong<-marcoR |>
      as.data.frame() |> count(COD_MPIO,ESTRATO,COD_SECC) |> 
      filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
      count(COD_MPIO,ESTRATO) |> 
      rename(nsecc=n)
    
    dfR<- df |> 
      rename(nh=!!col_encuestas,COD_MPIO=!!col_dane,ESTRATO= !!col_desag) |> 
      mutate(nseg = ceiling(nh/encxmanzana*1.5),
             conglomerados = ceiling(nseg/3),ESTRATO=as.integer(ESTRATO))  |> 
      left_join(maxcong,by = c("COD_MPIO","ESTRATO")) |> 
      mutate(conglomerados=replace_na(ifelse(conglomerados>nsecc,nsecc,conglomerados),0)) |> 
      select(-nsecc)
    
    et1 <- marcoR %>% as.data.frame() %>% 
      count(COD_MPIO, COD_SECC,ESTRATO) |> 
      filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
      select(-n)
    
    set.seed(semilla)
    et1$aleatorio <- runif(nrow(et1))
    
    segmentos_sel <- et1 %>% 
      left_join(dfR, by = c("COD_MPIO","ESTRATO")) %>% 
      arrange(COD_MPIO, -aleatorio) %>% 
      group_by(COD_MPIO,ESTRATO) %>% 
      mutate(Prioridad = 1:n()) %>% 
      filter(Prioridad <= conglomerados) |> 
      select(-aleatorio)
    
    set.seed(semilla)
    marcoR$aleatorio <- runif(nrow(marcoR))
    
    muestra <- marcoR %>%
      left_join(segmentos_sel,by=c("COD_MPIO","COD_SECC","ESTRATO")) |> 
      filter(!is.na(conglomerados)) %>% 
      arrange( -aleatorio) %>% 
      group_by( COD_SECC,ESTRATO) %>% 
      mutate(Prioridad = 1:n()) %>% ungroup() %>% 
      mutate(aselecc=ceiling(nseg/conglomerados)) |> 
      filter(Prioridad <= aselecc) |> 
      select(-aselecc) |> 
      filter(!is.na(ESTRATO))
    
    coord <- muestra |> 
      st_coordinates() |> 
      as_tibble() |> 
      group_by(L3) |> 
      summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
    
    muestra_out <- bind_cols(muestra, coord) |> as_tibble() |> 
      mutate(SECTOR = substr(COD_DANE, 15, 18),
             SECCION = substr(COD_DANE, 19, 20),
             MANZANA = substr(COD_DANE, 21, 22))
    
    
  }
    else  if (desag %in% c("n","N","nivel","NIVEL","Nivel")) {
      
      maxcong<- marcoR |>
        as.data.frame() |> group_by(COD_MPIO,NIVEL_SCE) |> 
        summarise(nsecc=replace_na(n_distinct(COD_SECC),0)) |> 
        filter(!is.na(NIVEL_SCE))
      
      dfR<- df |> 
        rename(nh=!!col_encuestas,COD_MPIO=!!col_dane,NIVEL_SCE= !!col_desag) |> 
        mutate(nseg = ceiling(nh/encxmanzana*1.5),
               conglomerados = ceiling(nseg/3),NIVEL_SCE=as.character(NIVEL_SCE))  |> 
        left_join(maxcong,by = c("COD_MPIO","NIVEL_SCE")) |> 
        mutate(conglomerados=replace_na(ifelse(conglomerados>nsecc,nsecc,conglomerados),0)) |> 
        select(-nsecc)
      
      et1 <- marcoR %>% as.data.frame() %>% 
        count(COD_MPIO, COD_SECC,NIVEL_SCE) |> 
        filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
        select(-n)
      
      set.seed(semilla)
      et1$aleatorio <- runif(nrow(et1))
      
      segmentos_sel <- et1 %>% 
        left_join(dfR, by = c("COD_MPIO","NIVEL_SCE")) %>% 
        arrange(COD_MPIO, -aleatorio) %>% 
        group_by(COD_MPIO,NIVEL_SCE) %>% 
        mutate(Prioridad = 1:n()) %>% 
        filter(Prioridad <= conglomerados) |> 
        select(-aleatorio)
      
      set.seed(semilla)
      marcoR$aleatorio <- runif(nrow(marcoR))
      
      muestra <- marcoR %>%
        left_join(segmentos_sel,by=c("COD_MPIO","COD_SECC","NIVEL_SCE")) |> 
        filter(!is.na(conglomerados)) %>% 
        arrange( -aleatorio) %>% 
        group_by( COD_SECC) %>% 
        mutate(Prioridad = 1:n()) %>% ungroup() %>% 
        mutate(aselecc=ceiling(nseg/conglomerados)) |> 
        filter(Prioridad <= aselecc) |> 
        select(-aselecc) |> 
        filter(!is.na(NIVEL_SCE))
      
      coord <- muestra |> 
        st_coordinates() |> 
        as_tibble() |> 
        group_by(L3) |> 
        summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
      
      muestra_out <- bind_cols(muestra, coord) |> as_tibble() |> 
        mutate(SECTOR = substr(COD_DANE, 15, 18),
               SECCION = substr(COD_DANE, 19, 20),
               MANZANA = substr(COD_DANE, 21, 22))
      
      
    }
    
    else  if (desag %in% c("l","L", "localidad","LOCALIDAD","Localidad")) {
    
    if (locnombre==T) {
      
      maxcong<- marcoR |>
        as.data.frame() |> count(COD_MPIO,LocNombre,COD_SECC) |> 
        filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
        count(COD_MPIO,LocNombre) |> 
        rename(nsecc=n)
      
    dfR<- df |> 
      rename(nh=!!col_encuestas,COD_MPIO=!!col_dane,LocNombre= !!col_desag) |> 
      mutate(nseg = ceiling(nh/encxmanzana*1.5),
             conglomerados = ceiling(nseg/3)) |> 
      left_join(maxcong,by = c("COD_MPIO","LocNombre")) |> 
      mutate(conglomerados=replace_na(ifelse(conglomerados>nsecc,nsecc,conglomerados),0)) |> 
      select(-nsecc)
    
    et1 <- marcoR %>% as.data.frame() %>% 
      count(COD_MPIO, COD_SECC,LocNombre) |> 
      filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
      select(-n)
    
    set.seed(semilla)
    et1$aleatorio <- runif(nrow(et1))
    
    segmentos_sel <- et1 %>% 
      left_join(dfR, by = c("COD_MPIO","LocNombre")) %>% 
      arrange(COD_MPIO, -aleatorio) %>% 
      group_by(COD_MPIO,LocNombre) %>% 
      mutate(Prioridad = 1:n()) %>% 
      filter(Prioridad <= conglomerados) |> 
      select(-aleatorio)
    
    set.seed(semilla)
    marcoR$aleatorio <- runif(nrow(marcoR))
    
    muestra <- marcoR %>%
      left_join(segmentos_sel,by=c("COD_MPIO","COD_SECC","LocNombre")) |> 
      filter(!is.na(conglomerados)) %>% 
      arrange( -aleatorio) %>% 
      group_by( COD_SECC,LocNombre) %>% 
      mutate(Prioridad = 1:n()) %>%
      ungroup() %>% 
      mutate(aselecc=ceiling(nseg/conglomerados)) |> 
      filter(Prioridad <= aselecc) |> 
      select(-aselecc) |> 
      filter(!is.na(LocNombre))
    
    coord <- muestra |> 
      st_coordinates() |> 
      as_tibble() |> 
      group_by(L3) |> 
      summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
    
    muestra_out <- bind_cols(muestra, coord) |> as_tibble() |> 
      mutate(SECTOR = substr(COD_DANE, 15, 18),
             SECCION = substr(COD_DANE, 19, 20),
             MANZANA = substr(COD_DANE, 21, 22))
    
    } else {
      
      
      maxcong<- marcoR |>
        as.data.frame() |> count(COD_MPIO,LocCodigo,COD_SECC) |> 
        filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
        count(COD_MPIO,LocCodigo) |> 
        rename(nsecc=n)
      
      dfR<- df |> 
        rename(nh=!!col_encuestas,COD_MPIO=!!col_dane,LocCodigo= !!col_desag) |> 
        mutate(nseg = ceiling(nh/encxmanzana*1.5),
               conglomerados = ceiling(nseg/3),LocCodigo=as.character(LocCodigo)) |> 
        left_join(maxcong,by = c("COD_MPIO","LocCodigo")) |> 
        mutate(conglomerados=replace_na(ifelse(conglomerados>nsecc,nsecc,conglomerados),0)) |> 
        select(-nsecc) 
      
      et1 <- marcoR %>% as.data.frame() %>% 
        count(COD_MPIO, COD_SECC,LocCodigo) |> 
        filter((COD_MPIO %in% principales & n>=3)| !COD_MPIO %in% principales) |> 
        select(-n)
      
      set.seed(semilla)
      et1$aleatorio <- runif(nrow(et1))
      
      segmentos_sel <- et1 %>% 
        left_join(dfR, by = c("COD_MPIO","LocCodigo")) %>% 
        arrange(COD_MPIO, -aleatorio) %>% 
        group_by(COD_MPIO,LocCodigo) %>% 
        mutate(Prioridad = 1:n()) %>% 
        filter(Prioridad <= conglomerados) |> 
        select(-aleatorio)
      
      set.seed(semilla)
      marcoR$aleatorio <- runif(nrow(marcoR))
      
      muestra <- marcoR %>%
        left_join(segmentos_sel,by=c("COD_MPIO","COD_SECC","LocCodigo")) |> 
        filter(!is.na(conglomerados)) %>% 
        arrange( -aleatorio) %>% 
        group_by( COD_SECC,LocCodigo) %>% 
        mutate(Prioridad = 1:n()) %>% ungroup() %>% 
        mutate(aselecc=ceiling(nseg/conglomerados)) |> 
        filter(Prioridad <= aselecc) |> 
        select(-aselecc) |> 
        filter(!is.na(LocCodigo))
      
      coord <- muestra |> 
        st_coordinates() |> 
        as_tibble() |> 
        group_by(L3) |> 
        summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
      
      muestra_out <- bind_cols(muestra, coord) |> as_tibble() |> 
        mutate(SECTOR = substr(COD_DANE, 15, 18),
               SECCION = substr(COD_DANE, 19, 20),
               MANZANA = substr(COD_DANE, 21, 22))
      
    }
    
  }
  
  } else {
    
    if (manzanas=="manz_rural") {
      
    marcoR<- marco |>
     filter(VIV>=minvivienda) |> 
      filter(COD_MPIO %in% mpios) |> 
      filter(tipo==manzanas)
    
    maxcong<- marcoR |>
      as.data.frame() |> group_by(COD_MPIO) |> 
      summarise(nsecc=n_distinct(COD_SECC))
    
    dfR<- df |> 
      rename(nh=!!col_encuestas,COD_MPIO=!!col_dane) |> 
      mutate(nseg = ceiling(nh/encxmanzana*1.5),
             conglomerados = ceiling(nseg))|> 
      left_join(maxcong,by = "COD_MPIO") |> 
      mutate(conglomerados=ifelse(conglomerados>=nsecc,nsecc,conglomerados)) |> 
      select(-nsecc)
    
    
    set.seed(semilla)
    marcoR$aleatorio <- runif(nrow(marcoR))
    
    muestra <- marcoR %>%
      left_join(dfR,by=c("COD_MPIO")) |> 
      filter(!is.na(conglomerados)) %>% 
      arrange( -aleatorio) %>% 
      group_by( COD_MPIO,COD_SECC)  %>% 
      mutate(Prioridad = 1:n()) %>%
      ungroup() %>% 
      mutate(aselecc=ceiling(nseg/conglomerados)) |> 
      filter(Prioridad <= aselecc) |> 
      select(-aselecc)
    
    coord <- muestra |> 
      st_coordinates() |> 
      as_tibble() |> 
      group_by(L3) |> 
      summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
    
    muestra_out <- bind_cols(muestra, coord) |> as_tibble()
    
    
    
    }else
      {
        
        marcoR<- marco |>
         # filter(VIV>=minvivienda) |> 
          filter(COD_MPIO %in% mpios) |> 
          filter(tipo==manzanas)
        
        dfR<- df |> 
          rename(nh=!!col_encuestas,COD_MPIO=!!col_dane) |> 
          mutate(nseg = ceiling(nh/encxmanzana*1.5),
                 conglomerados = ceiling(nseg)) |> 
          mutate(conglomerados=ifelse(conglomerados==1,2,conglomerados))
        set.seed(semilla)
        marcoR$aleatorio <- runif(nrow(marcoR))
        
        muestra <- marcoR %>%
          left_join(dfR,by=c("COD_MPIO")) |> 
          filter(!is.na(conglomerados)) %>% 
          arrange( -aleatorio) %>% 
          group_by( COD_MPIO)  %>% 
          mutate(Prioridad = 1:n()) %>%
          ungroup() %>% 
          filter(Prioridad <= conglomerados) 
        
        coord <- muestra |> 
          st_coordinates() |> 
          as_tibble() |> 
          group_by(L3) |> 
          summarise(across(c("X", "Y"), ~mean(., rm.na = T)))
        
        muestra_out <- bind_cols(muestra, coord) |> as_tibble()
        
      }
    
    
    
    
  }
if (manzanas=="vereda") {
  
  salida<- muestra_out |> 
    group_by(CODIGO_VER) |> 
    mutate(ID_MAPA=cur_group_id()) |> 
    ungroup() |> 
    mutate(ID_MAPA=ID_MAPA+inicio_idmapa) |> 
    select(ID_MAPA,id_manzana,
           COD_MPIO,Municipio=NOMBRE_MPI,COD_DPTO,Departamento=NOMBRE_DPT,CODIGO_VER,NOMBRE_VER,X,Y)
  
} else if (manzanas=="manz_rural"){
  
  salida<- muestra_out |> 
    group_by(COD_SECC) |> 
    mutate(ID_MAPA=cur_group_id()) |> 
    ungroup() |> 
    mutate(ID_MAPA=ID_MAPA+inicio_idmapa) |> 
    select(ID_MAPA,id_manzana,
           COD_MPIO,Municipio=NOMBRE_MPI,COD_DPTO,Departamento=NOMBRE_DPT,NOMBR_CPOB,COD_CPOB,viviendas_censo=cns_viv,LocNombre,LocCodigo,COD_DANE,COD_SECC,SECTOR,SECCION,MANZANA,X,Y)
  
  
}else
  
  {
    
  salida<- muestra_out |> 
    group_by(COD_SECC) |> 
    mutate(ID_MAPA=cur_group_id()) |> 
    ungroup() |> 
    mutate(ID_MAPA=ID_MAPA+inicio_idmapa) |> 
    select(ID_MAPA,id_manzana,
           COD_MPIO,Municipio=NOMBRE_MPI,COD_DPTO,Departamento=NOMBRE_DPT,ESTRATO,NIVEL_SCE,viviendas_censo=cns_viv,LocNombre,LocCodigo,COD_DANE,COD_SECC,SECTOR,SECCION,MANZANA,X,Y)
  
   }
    
    
return(salida)
    
  
  
}


