#' MUESTREO SISTEMÁTICO
#'
#' @param datos La base de datos. clase: "data.frame" o "tibble".
#' @param TamanoMuestra El tamaño de la muestra que desea. clase: "numeric" o "integer".
#'
#' @description La siguiente función realizara un muestreo aleatorio sistemático.
#'
#' @details
#' En ocasiones se requiere ordenar los datos por alguna/s variable/s, para realizar una muestra que sea más representativa
#' a algunas características de la población como el estrato socioeconómico, es necesario que se ordene el data frame antes
#' de ingresarlo a la función. Una opción es utilizar dplyr::arrange().
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
#' # Tomamos una muestra aleatoria sistemática de la base de datos "mtcars"
#' MuestreoSistematico(datos = mtcars, 10)
MuestreoSistematico <- function(datos, TamanoMuestra) {

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


    # tamaño: cantidad de registros.
    tamano <- nrow(datos)
    # intervarlo: tamaño de los saltos en el indice de la muestra.
    intervalo <- round(tamano/TamanoMuestra)
    # aleatorio: valor aleatorio inicial.
    aleatorio <- sample(x = 1:intervalo, size = 1)

    # indices: los indices que se tomarán de la base de datos.
    indices <- cumsum(c(aleatorio, rep(intervalo, TamanoMuestra-1)) )
    # Aqui se usa el modulo "%%" para que, dé la vuelta a la secuencia de números en el lugar correspondiente:
    indices[which(indices > tamano)] <- indices[which(indices > tamano)] %% tamano

    muestra <- datos[indices, ]

    Lista <- list("indices" = indices, "muestra" = muestra)

    return(Lista)
}
