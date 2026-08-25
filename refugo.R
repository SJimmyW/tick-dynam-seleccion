#' Reemplaza individuos descartados dentro de un grupo y devuelve el nuevo grupo
#'
#' `refugo` calcula cuántos individuos del grupo actual deben ser reemplazados
#' (descartes) y selecciona individuos de reposición desde la población
#' `historica` usando `selectInd` (AlphasimR). Devuelve un objeto del mismo tipo que `actual`
#' con los individuos de reemplazo + los que quedan del grupo original.
#'
#' @details
#' La función asume que `actual` tiene un slot `@misc$año` (vector numérico)
#' que contiene año de nacimiento individual.
#' El descarte se define como el valor mínimo de `grupo@misc$año`. El número de
#' individuos a reemplazar (`nrefugo`) es el número de elementos de
#' `actual@misc$año` iguales a ese mínimo; el resto (`norefugo`) serán reemplazados
#' por individuos seleccionados de `historica`.
#'
#' Si `año < ingreso`(edad, en meses, mínima de primer servicio),
#' los individuos seleccionados para reposición se obtienen de la población
#' histórica. De lo contrario, de generaciones posteriores.
#'
#' @param historica Objeto de población (por ejemplo, objeto de AlphaSimR) desde el
#'   cual se seleccionarán los individuos de reposición.
#' @param actual Objeto que representa el grupo actual (misma clase que el valor
#'   devuelto). Debe contener `@misc$año` (vector numérico).
#' @param ngrupo Integer. Tamaño total deseado del grupo tras el proceso.
#' @param sexo Character o factor. Especifica el sexo de los individuos a
#'   seleccionar para la reposición (p. ej. "M o F" usada en `selectInd`).
#' @param año Numeric. Año (o etiqueta temporal) asociado a la operación; se usa
#'   para ajustar `@misc$año` de las incorporaciones cuando `año < ingreso`.
#' @param ingreso Numeric. Año (o tiempo) de ingreso de los individuos; se usa
#'   para computar el valor a asignar a `reposicion@misc$año` cuando corresponda.
#' @param candidates (opcional) Vector o estructura de candidatos a pasar a
#'   `selectInd` (se delega a la función `selectInd`). Por defecto `NULL`.
#' @param campo (opcional) Valor que se asignará a `reposicion@misc$campo`
#'   cuando `año < ingreso`. Por defecto `NULL`.
#' @param criterio Character. Criterio usado por `selectInd` para seleccionar
#'   individuos. Por defecto \"rand\" (selección aleatoria). Depende de las
#'   opciones soportadas por la función `selectInd` de la que se haga uso.
#'
#' @return Un objeto del mismo tipo que `actual` que contiene:
#'   * los individuos seleccionados de `historica` para reponer los descartes, y
#'   * los individuos que permanecen del `actual` original (aquellos cuyo
#'     `@misc$año` no coincide con el descarte).
#'
#' @examples
#' \dontrun{
#'
#' @note
#' - La función depende de que `actual@misc$año` esté definido y sea un vector
#'   con un valor por individuo. Si ese slot no existe la función fallará.
#' - Si `año - ingreso` da un valor inesperado (p. ej. negativo), este valor se
#'   asignará tal cual a `reposicion@misc$año`. Valida `año` e `ingreso` antes
#'   de llamar a la función si esto no es deseado.
#' - `criterio` debe ser una opción válida para la función `selectInd` que se
#'   esté usando en tu entorno (por ejemplo, AlphaSimR).
#'
#' @author Tu Nombre <tu.email@dominio.com>
#' @export
#' @importFrom AlphaSimR selectInd


refugo <- function(historica = NULL, actual = NULL, candidates = NULL, 
                   ngrupo, sexo, 
                   año, ingreso, 
                   candidates_id = NULL,
                   campo = NULL, criterio = "rand") {
  
  descarte <- min((actual@misc$año))
  nrefugo <- sum(na.omit(unlist(actual@misc$año)) == descarte) # length(which(actual@misc$año == descarte) )
  norefugo <- ngrupo - nrefugo
  
  if (año < ingreso) {
    reposicion <- selectInd(historica, nrefugo,
                            use = criterio, sex = sexo,
                            candidates = candidates_id
    )
    reposicion@misc$año <- rep((año - ingreso), nrefugo)
    reposicion@misc$campo <- rep(campo, nrefugo)
    
  } else {
    
    reposicion <- selectInd(candidates, nrefugo, use = criterio, 
                            candidates = candidates_id,
                            sex = sexo)
    
  }
  
  quedan <- actual[!actual@misc$año %in% descarte]
  nuevos <- c(reposicion, quedan)
  
  return(nuevos)
}