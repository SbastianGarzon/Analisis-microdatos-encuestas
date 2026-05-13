data_estetica <- function(Data, MANZ_ORI, reemplazos = FALSE, estudio = '009200260000 COLOMBIA OPINA No 21' ){
  t1 <- Sys.time()

  # Creo las siguientes columnas unas vacias y otras con valores iniciales.
  Data <- Data %>% rename(DIRECCION_1 = DIRECCION)
  Data["PUNTO_INICIO"] <- ""
  Data["ENUMERACION_INICIAL"] <- ""
  Data["ENUMERACION_FINAL"] <- ""
  Data["PLAZA"] <- ""
  Data["CODIGO_PLAZA"] <- ""
  Data["NOM_ESTUDIO"] <- estudio
  Data["MANZANA_ORI"] <- MANZ_ORI
  Data["BDTIPO"] <- 0
  Data["NOM_BDTIPO"] <- "INVAMER"
  Data["MAPA_LINK"] <- paste('https://www.google.com/maps/place/', Data$CY, '+', Data$CX, '/', sep = '')
  Data["OPCION_1"] <- ""
  Data["OPCION_2"] <- ""
  Data["COD_REGION"] <- ""
  Data["REGION"] <- ""
  Data["COD_ZONA"] <- ""
  Data["ZONA"] <- ""
  Data["COD_TAMANO"] <- ""
  Data["TAMANO"] <- ""
  
  Data <- Data %>% arrange(MUNICIPIO, ESTRATO)
  
  # Realizo condición para los reemplazos:
  if (!reemplazos) {
    Data["ID_MUESTRAL"] <- 1:nrow(Data)
    Data["ID_MANZANA"] <- 1:nrow(Data)
    Data["ENCUESTAS"] <- 4
    Data["ENCUESTAS_RUTA"] <- 4
  } else{
    Data["ID_MUESTRAL"] <- ""
    Data["ID_MANZANA"] <- ""
    Data["ENCUESTAS"] <- ""
    Data["ENCUESTAS_RUTA"] <- ""
  }
  
  # Aqui se crean algunas columnas:
  Data <- Data %>% mutate(
                           COD_DEPARTAMENTO = 
                              ifelse(COD_MUNICIPIO == "68001", '68', COD_DEPARTAMENTO),
                            
                            DEPARTAMENTO = 
                              ifelse(COD_MUNICIPIO == "68001", 'SANTANDER', DEPARTAMENTO)
                            )
  
  
  # Vector para seleccionar las columnas en el orden que se necesita:
  orden_columnas <- c("MANZANA_ORI",	"ID_MANZANA",	"COD_MANZANA",	"MUNICIPIO",	"ESTRATO",
                      "DIRECCION_1",	"BARRIO",	"COMUNA_LOCALIDAD",
                      "PUNTO_INICIO","ENUMERACION_INICIAL",	"ENUMERACION_FINAL",	"ENCUESTAS",	"ID_MUESTRAL",	
                      "ENCUESTAS_RUTA","CY", "CX","OPCION_1","OPCION_2","COD_REGION", "REGION",	 "COD_DEPARTAMENTO",
                      "DEPARTAMENTO","COD_MUNICIPIO", "CODIGO_PLAZA", "PLAZA","COD_ZONA","ZONA","COD_TAMANO","TAMANO",
                       "BDTIPO", "NOM_BDTIPO", "MAPA_LINK")
  t1 <- Sys.time()
  Data <- Data %>% select(orden_columnas)
  t2 <- Sys.time()
  t2-t1
  
  Data$COD_MUNICIPIO <- as.numeric(Data$COD_MUNICIPIO)
  Data$CODIGO_PLAZA <- as.numeric(Data$CODIGO_PLAZA)
  
  # modficamos los nulos:
  Cuales_nulos <- which(Data$DIRECCION_1 == "NULL")
  Data$DIRECCION_1[Cuales_nulos] <- ""
  
  return(Data)
}
