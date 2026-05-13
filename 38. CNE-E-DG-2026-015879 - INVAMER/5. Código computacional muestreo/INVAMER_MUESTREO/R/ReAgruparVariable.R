#' Reagrupamiento de variables
#'
#' @param datos La base de datos. Clase: "data.frame" o "tibble".
#' @param variables Nombre de la variable a reagrupar y nombre como se almacenará. Clase: Vector "character"
#' @param grupos Valores en los que se reagruparán los datos. Clase: "list"
#'
#' @description La siguiente función realizará un reagrupamiento de las variables con el fin de ayudar a especificar algunos
#' tipos de muestreos.
#'
#' @details
#' IMPORTANTE: el orden en que ingresa los grupos a la lista, será el orden en que se definirán los grupos,
#' es decir, el primer elemento de la lista dará como resultado un 1, el segundo un 2, etc.
#' Es importante siempre poner todas las categorías existentes en la variable de interés en la lista del parámetro "grupos"
#' esto con el fin de evitar confusiones. La variable puede ser dividida en máximo 5 grupos. El parámetro "variables" siempre debe
#' tener como primer elemento la variable a reagrupar y como segundo el nombre de la variable donde se almacenará. El algoritmo está
#' diseñado para identificar la cantidad de grupos dependiendo de la cantidad de elementos de la lista.

#'
#'
#' @return Devuelve el data frame ingresado con la nueva variable con sus respectivos respectivos subgrupos.
#' @export
#'
#' @examples
#' # Cargue los datos "Marco_Manzanas_2019" desde SQL y llámelos como "data", cree un vector con las variables que usará y
#' # cree una lista para el parámetro grupos, donde cada elemento representa el conjunto que quiere reagrupar, ejem:
#' # l <- list(c("1", "2", "3"), c("4", "5", "6"));l
#' # ReAgruparVariable(datos = data, variables = c("ESTRATO", "grupo2"), grupos = l )
ReAgruparVariable <- function(datos, variables, grupos) {


    if (length(grupos) == 2 ) {
        datos[[variables[2]]] <- if_else(datos[[variables[1]]] %in% grupos[[1]], 1, 2)
    } else if (length(grupos) == 3) {
        datos[[variables[2]]] <- if_else(datos[[variables[1]]] %in% grupos[[1]], 1,
                                 if_else(datos[[variables[1]]] %in% grupos[[2]], 2, 3)
        )
    } else if (length(grupos) == 4) {
        datos[[variables[2]]] <- if_else(datos[[variables[1]]] %in% grupos[[1]], 1,
                                 if_else(datos[[variables[1]]] %in% grupos[[2]], 2,
                                 if_else(datos[[variables[1]]] %in% grupos[[3]], 3, 4)
                                         ))
    } else if (length(grupos) == 5) {
        datos[[variables[2]]] <- if_else(datos[[variables[1]]] %in% grupos[[1]], 1,
                                 if_else(datos[[variables[1]]] %in% grupos[[2]], 2,
                                 if_else(datos[[variables[1]]] %in% grupos[[3]], 3,
                                 if_else(datos[[variables[1]]] %in% grupos[[4]], 4, 5)
                                                 )))
    }

    return(datos)

}
