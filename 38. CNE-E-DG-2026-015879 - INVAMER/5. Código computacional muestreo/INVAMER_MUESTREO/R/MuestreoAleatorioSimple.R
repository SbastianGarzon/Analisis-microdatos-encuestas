#' MUESTREO ALEATORIO SIMPLE
#'
#' @param datos La base de datos. Clase: "data.frame" o "tibble".
#' @param TamanoMuestra El tamaño de la muestra que desea. Clase: "numeric".
#' @param reemplazo para muestreo con reemplazo. Por defecto "FALSE". Clase: "logical".
#'
#' @description La siguiente función realizará un muestreo aleatorio simple.
#'
#' @details El parámetro de reemplazo, se creó pensando en la posibilidad de que, en un futuro, se requiera un
#' muestreo de este tipo.
#'
#' Los resultados arrojados, son los índices que corresponden a las posiciones de la base de datos que fueron seleccionados de
#' manera aleatoria, y la muestra que es el resultado de tomar los datos ingresados, en las posiciones de los índices aleatorios.
#' "SubMuestreo" hace referencia al tipo de muestreo que se debe realizar dentro de cada estrato o combinación de estratos.
#' Si, la cantidad de muestras que se solicita, supera la cantidad de registris, se generará la muestra pero solo con la cantidad de
#' datos existentes y se mostrará un mensaje de advertencia con algo de imformación relevante.
#'
#' @return Retornará una lista con 2 elementos, los índices y la muestra correspondiente.
#' @export
#'
#' @examples
#'
#' # Tomamos una muestra aleatoria simple de la base de datos "mtcars"
#' MuestreoAleatorioSimple(datos = mtcars, TamanoMuestra = 10)
MuestreoAleatorioSimple <- function(datos, TamanoMuestra, reemplazo = FALSE) {

    if (!is.data.frame(datos)) {
        stop("El parametro datos deben ser de Clase: data.frame o tibble.")
    }

    if (!is.numeric(TamanoMuestra)) {
        stop("El parametro TamanoMuestra deben ser de Clase: numeric.")
    }


    if (TamanoMuestra > nrow(datos)) {

        warning(paste("La cantidad de registros es mayor al solicitado. Se me pide "
                      , TamanoMuestra, " registros, pero solo puedo darle ", nrow(datos)
                      ,"\n \n La muestra se generá con", nrow(datos), "datos"
        )
        )

        TamanoMuestra <- nrow(datos)
    }


    indices <- sample(
          x = 1:nrow(datos)
        , size = TamanoMuestra
        , replace = reemplazo
        )


    muestra <- datos[indices, ]

    Lista <- list("indices" = indices, "muestra" = muestra)

    return(Lista)
}
