seleccionar_puntos<-function(Data,N,EncuestasxPunto) {

  #Se crean las variables de limites  
Data["limite_inferior"]<-""
Data["limite_superior"]<-""
Data[1,"limite_inferior"]<-1
Data[1,"limite_superior"]<-Data[1,"POBLACION"]

Data$limite_inferior<-as.numeric(Data$limite_inferior)
Data$limite_superior<-as.numeric(Data$limite_superior)

for (i in 2:length(Data$limite_inferior)) {
  Data$limite_inferior[i]<-Data$limite_superior[i-1]+1
  Data$limite_superior[i]<-Data$limite_inferior[i]+Data$POBLACION[i]
}

  #Se crea el intervalo 
intervalo<-sum(Data$POBLACION)/N
aleatorio<-runif(1)*intervalo

  #Se crean los indices

seleccion<-numeric(N)

for (i in 1:N){
  if (i==1) {
    seleccion[i]<-aleatorio
  } else
  {
    seleccion[i]<-seleccion[i-1]+intervalo
  }
}

indice1<-c(1:length(Data$MUNICIPIO))
indice2<-numeric(length(Data$MUNICIPIO))

for (i in 1:length(Data$MUNICIPIO)) {
  if(i==1){
    indice2[i]<-sum(indice1)
  } else
  {
    indice2[i]<-indice2[i-1]-indice1[i-1]
  }
  
}

#se hace la siguiente validación de indices--DEBEN SER IGUALES--
indice1[length(Data$MUNICIPIO)]
indice2[length(Data$MUNICIPIO)]

Data$indice1<-indice1
Data$indice2<-indice2

indice3<-c(1:N)

for (e in seq_along(seleccion)) {
  indice3[e] <- sum(Data$indice1[Data$limite_superior > seleccion[e]])
}


indice3

indice<-as.data.frame(indice3)

seleccionados<-merge(Data,indice,by.x="indice2",by.y="indice3")

seleccionados$PUNTOS_MUESTRALES<-1
seleccionados$ENCUESTAS<-EncuestasxPunto

seleccionados<- seleccionados %>% select(REGION,
                                         COD_REGION,
                                         COD_DEPARTAMENTO,
                                         DEPARTAMENTO,
                                         COD_MUNICIPIO,
                                         MUNICIPIO,
                                         Zona,
                                         POBLACION,
                                         POBLACION_TOTAL,
                                         PUNTOS_MUESTRALES,
                                         ENCUESTAS,
                                         COD_TAMANO, 
                                         TAMANO)

return(seleccionados)

}