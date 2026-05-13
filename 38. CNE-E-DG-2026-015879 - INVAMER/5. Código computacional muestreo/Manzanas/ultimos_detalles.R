ultimos_detalles <- function(Data) {
  
  # Se crean dos vectores, para poder realizar realizar el ENUMERACION_INICIAL ENUMERACION_FINAL de forma más eficiente:
  inicial <- Data$ENCUESTAS[1]
  aux1 <- c(1, vector(mode = "integer", length = nrow(Data) - 1) )
  aux2 <- c(inicial, vector(mode = "integer", length = nrow(Data) - 1) )
  
  for (i  in 2:nrow(Data)) {
    aux1[i] <- aux2[i-1] + 1
    aux2[i] <- aux1[i] + Data$ENCUESTAS[i] - 1
  }
  
  Data$ENUMERACION_INICIAL <- aux1
  Data$ENUMERACION_FINAL <- aux2
  
  
  Data$PUNTO_INICIO <- PUNTO_INICIO(n = nrow(Data))
  
  # Agrego los ceros adelante de la variable ID_MUESTRAL:
  aux <- vector(mode = "character", length = nrow(Data))
  for (i in seq_along(aux)) {
    
    if (Data$ID_MUESTRAL[i] < 10) {
      aux[i] <- paste0("0000", Data$ID_MUESTRAL[i]) 
    } else if (Data$ID_MUESTRAL[i] >= 10) {
      if (Data$ID_MUESTRAL[i] < 100) {
        aux[i] <- paste0("000", Data$ID_MUESTRAL[i]) 
      } else if (Data$ID_MUESTRAL[i] >= 100) {
        if (Data$ID_MUESTRAL[i] < 1000) {
          aux[i] <- paste0("00", Data$ID_MUESTRAL[i])
        } else if (Data$ID_MUESTRAL[i] >= 1000) {
          if (Data$ID_MUESTRAL[i] < 10000) {
            aux[i] <- paste0("0", Data$ID_MUESTRAL[i])
          } else {aux[i] <- as.character(Data$ID_MUESTRAL[i])
          }  
        } 
      }
    }
  }
    
  Data["ID_MUESTRAL"] <- aux
  
  Data["ID_MANZANA"] <- aux
    
  return(Data)
}

# Se realiza el punto de inicio:
PUNTO_INICIO <- function(n){
  aux1 <- sample(x = 1:15, size = n, replace = T) 
  aux2 <- sample(x = c('A', 'B', 'C', 'D'), size = n, replace = T)
  return(paste(aux2, aux1, sep = ""))
}