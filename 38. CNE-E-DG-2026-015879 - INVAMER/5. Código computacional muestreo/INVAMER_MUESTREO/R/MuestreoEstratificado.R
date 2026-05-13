#' MUESTREO ESTRATIFICADO
#'
#' @param datos La base de datos a muestrear. Clase: "data.frame" o "tibble".
#' @param SubMuestreo Tipo de muestreo para cada estrato. Clase: "character", solo se puede "simple" o "sistematico".
#' @param InfoMuestra La base de datos con la información del muestreo.  Clase: "data.frame" o "tibble".
#'
#' @description La siguiente función realizara un muestreo aleatorio estratificado.
#'
#' @details Es muy importante, crear la base de datos correspondiente al parámetro "InfoMuestra", puede hacerlo en Excel e
#' importar o directamente en R, con el fin de especificar cuáles son las variables que compondrán los estratos. Las columnas
#' deben tener el mismo nombre, de las variables de la base de datos a muestrear, por ejemplo:
#' con un solo estrato, tenemos "datos = Marco_Manzanas_2019" que tiene la columna "COD_MUNICIPIO".
#' Entonces "InfoMuestra" tendrá dos columnas, una "COD_MUNICIPIO" y "Cuotas". Ingresamos los estratos que queremos muestrear en
#' "COD_MUNICIPIO" y en la última columna del data.frame, "cuotas" la cantidad de elementos de cada estrato
#' (las cantidades siempre deben estar en la última columna).
#' Algo importante es que la función podrá tener hasta 4 estratos, el algoritmo identifica la cantidad de estratos
#' con la cantidad de columnas que se ingresan en "InfoMuestra".
#
#' Los resultados arrojados, son los índices que corresponden a las posiciones de la base de datos que fueron seleccionados de
#' manera aleatoria, y la muestra que es el resultado de tomar los datos ingresados, en las posiciones de los índices aleatorios.
#' "SubMuestreo" hace referencia al tipo de muestreo que se debe realizar dentro de cada estrato o combinación de estratos.
#'
#' Si, la cantidad de muestras que se solicita, supera la cantidad de registris, se generará la muestra pero solo con la cantidad de
#' datos existentes y se mostrará un mensaje de advertencia con algo de imformación relevante.
#'
#' @return Retornará una lista con 2 elementos, los índices y la muestra correspondiente.
#' @export
#'
#' @examples
#' # Cargue los datos "Marco_Manzanas_2019" desde SQL y llámelos como "data" cree un archivo llamado cuotas con las
#' # especificaciones dadas en la descripción y elija un tipo de SubMuestro.
#' # MuestreoEstratificado(datos = data, InfoMuestra = cuotas, SubMuestreo = "simple")
MuestreoEstratificado <- function(datos, InfoMuestra , SubMuestreo = "simple") {

    if (!is.data.frame(datos)) {
        stop("El parametro datos deben ser de Clase: data.frame o tibble.")
    }

    if (!is.data.frame(InfoMuestra)) {
        stop("El parametro InfoMuestra deben ser de Clase: data.frame o tibble.")
    }

    if (!is.character(SubMuestreo)) {
        stop("El parametro SubMuestreo deben ser de Clase: character")
    }

    if (!SubMuestreo %in% c("simple", "sistematico")) {
        stop("En el parametro SubMuestreo solo se aceptan los valores: simple o  sistematico")
    }


    # Se crea el marco para almacenar la muestra
    muestra <- datos[1, ]
    muestra <- muestra[-1, ]

    # Se crea la lista para alamacenar los indices del muestreo
    indices <- list()


    # Con esta variable controlo el muestro estratificado, por los estratos que necesito.
    n_estratos <- length(names(InfoMuestra)) - 1
    #-----------------------------------------------------------------------------------------------------------------------------
    # Aqui se realiza el muestreo para 1 estrato:
    #-----------------------------------------------------------------------------------------------------------------------------
    if (n_estratos == 1) {

        # Renombro la variable de estratificación como estrato1 con el objetivo de generalizar el proceso.
        datos <- datos %>% rename(estrato1 = names(InfoMuestra)[1])

        for (estrato_i in seq_along(InfoMuestra[[1]]) ) {

            # Filtro por las categorias de la varible de estratificación:
            aux <- datos %>% filter(estrato1 == InfoMuestra[[1]][estrato_i])

            # Se realiza la selección del tipo de muestreo que se realizará en cada estrato (Son las muestras ya programadas.)
            if (SubMuestreo == "simple") {
                aux2 <- MuestreoAleatorioSimple(datos = aux, TamanoMuestra = InfoMuestra[[2]][estrato_i])
            } else {
                aux2 <- MuestreoSistematico(datos = aux, TamanoMuestra = InfoMuestra[[2]][estrato_i])
            }

            # Guardo los indices
            indices[[estrato_i]] <- aux2[["indices"]]

            # Uno la muestra
            muestra <- rbind(muestra, aux2[["muestra"]] )

        }

        # recupero el nombre que utilicé:
        donde <- which("estrato1" == names(muestra))
        names(muestra)[donde] <- names(InfoMuestra)[[1]]
    #-----------------------------------------------------------------------------------------------------------------------------
    # Aqui se realiza el muestreo para 2 estratos:
    #-----------------------------------------------------------------------------------------------------------------------------
    } else if (n_estratos == 2) {

        # Renombro la variable de estratificación como estrato1 con el objetivo de generalizar el proceso.
        datos <- datos %>% rename(estrato1 = names(InfoMuestra)[1]
                                , estrato2 = names(InfoMuestra)[2]
                                 )

        for (estrato_i in seq_along(InfoMuestra[[1]]) ) {
            aux  <- datos %>% filter(estrato1 == InfoMuestra[[1]][estrato_i], estrato2 == InfoMuestra[[2]][estrato_i])

            # Se realiza la selección del tipo de muestreo que se realizará en cada estrato (Son muestras ya programadas.)
            if (SubMuestreo == "simple") {
                aux2 <- MuestreoAleatorioSimple(datos = aux, TamanoMuestra = InfoMuestra[[3]][estrato_i])
            } else {
               aux2 <- MuestreoSistematico(datos = aux, TamanoMuestra = InfoMuestra[[3]][estrato_i])
            }



            indices[[estrato_i]] <- aux2[["indices"]]

            muestra <- rbind(muestra, aux2[["muestra"]] )
        }

        # recupero el nombre que utilicé:
        donde <- which(names(muestra) %in% c("estrato1", "estrato2" ) )
        names(muestra)[donde[1]] <- names(InfoMuestra)[[1]]
        names(muestra)[donde[2]] <- names(InfoMuestra)[[2]]

    #-----------------------------------------------------------------------------------------------------------------------------
    # Aqui se realiza el muestreo para 3 estratos:
    #-----------------------------------------------------------------------------------------------------------------------------
    } else if (n_estratos == 3) {

        # Renombro la variable de estratificación como estrato1 con el objetivo de generalizar el proceso.
        datos <- datos %>% rename(  estrato1 = names(InfoMuestra)[1]
                                  , estrato2 = names(InfoMuestra)[2]
                                  , estrato3 = names(InfoMuestra)[3]
        )

        for (estrato_i in seq_along(InfoMuestra[[1]]) ) {
            aux  <- datos %>% filter(  estrato1 == InfoMuestra[[1]][estrato_i]
                                     , estrato2 == InfoMuestra[[2]][estrato_i]
                                     , estrato3 == InfoMuestra[[3]][estrato_i]
                                     )

            # Se realiza la selección del tipo de muestreo que se realizará en cada estrato (Son muestras ya programadas.)
            if (SubMuestreo == "simple") {
                aux2 <- MuestreoAleatorioSimple(datos = aux, TamanoMuestra = InfoMuestra[[4]][estrato_i])
            } else {
                aux2 <- MuestreoSistematico(datos = aux, TamanoMuestra = InfoMuestra[[4]][estrato_i])
            }



            indices[[estrato_i]] <- aux2[["indices"]]

            muestra <- rbind(muestra, aux2[["muestra"]] )
        }

        # recupero el nombre que utilicé:
        donde <- which(names(muestra) %in% c("estrato1", "estrato2", "estrato3" ) )
        names(muestra)[donde[1]] <- names(InfoMuestra)[[1]]
        names(muestra)[donde[2]] <- names(InfoMuestra)[[2]]
        names(muestra)[donde[3]] <- names(InfoMuestra)[[3]]


    #-----------------------------------------------------------------------------------------------------------------------------
    # Aqui se realiza el muestreo para 4 estratos:
    #-----------------------------------------------------------------------------------------------------------------------------
    } else if (n_estratos == 4) {

        # Renombro la variable de estratificación como estrato1 con el objetivo de generalizar el proceso.
        datos <- datos %>% rename(  estrato1 = names(InfoMuestra)[1]
                                    , estrato2 = names(InfoMuestra)[2]
                                    , estrato3 = names(InfoMuestra)[3]
                                    , estrato4 = names(InfoMuestra)[4]
        )

        for (estrato_i in seq_along(InfoMuestra[[1]]) ) {
            aux  <- datos %>% filter(  estrato1 == InfoMuestra[[1]][estrato_i]
                                       , estrato2 == InfoMuestra[[2]][estrato_i]
                                       , estrato3 == InfoMuestra[[3]][estrato_i]
                                       , estrato4 == InfoMuestra[[4]][estrato_i]
            )

            # Se realiza la selección del tipo de muestreo que se realizará en cada estrato (Son muestras ya programadas.)
            if (SubMuestreo == "simple") {
                aux2 <- MuestreoAleatorioSimple(datos = aux, TamanoMuestra = InfoMuestra[[5]][estrato_i])
            } else {
                aux2 <- MuestreoSistematico(datos = aux, TamanoMuestra = InfoMuestra[[5]][estrato_i])
            }



            indices[[estrato_i]] <- aux2[["indices"]]

            muestra <- rbind(muestra, aux2[["muestra"]] )
        }

        # recupero el nombre que utilicé:
        donde <- which(names(muestra) %in% c("estrato1", "estrato2", "estrato3", "estrato4" ) )
        names(muestra)[donde[1]] <- names(InfoMuestra)[[1]]
        names(muestra)[donde[2]] <- names(InfoMuestra)[[2]]
        names(muestra)[donde[3]] <- names(InfoMuestra)[[3]]
        names(muestra)[donde[4]] <- names(InfoMuestra)[[4]]
    }

    Lista <- list("indices" = indices, "muestra" = muestra)

    return(Lista)

}
