

#' Generar opciones para BLUPF90
#'
#' Construye un vector de líneas de configuración `OPTION` para utilizar
#' con programas de la familia BLUPF90. La función permite configurar
#' dos modos de ejecución:
#'
#' \itemize{
#'   \item \code{"selection"}: orientado a la evaluación genética y
#'   obtención de soluciones/valores de cría.
#'   \item \code{"comp_var"}: orientado a la estimación de componentes
#'   de varianza mediante EM-REML.
#' }
#'
#' Las opciones comunes a ambos modos incluyen el código utilizado para
#' valores faltantes, el tamaño máximo de lectura de cadenas y distintas
#' opciones para controlar la salida de soluciones.
#'
#' En el modo \code{"comp_var"} se agregan las opciones necesarias para
#' ejecutar la estimación de componentes de varianza mediante el método
#' EM-REML y para calcular errores estándar de funciones de los componentes
#' de covarianza.
#'
#' @param modo Modo de ejecución. Puede ser \code{"selection"} para
#'   evaluación genética/obtención de valores de cría o \code{"comp_var"}
#'   para estimación de componentes de varianza. Se utiliza
#'   \code{\link[base]{match.arg}} para validar el argumento.
#' @param missing Código numérico utilizado por BLUPF90 para representar
#'   valores faltantes. Por defecto es \code{-999}.
#' @param max_string_readline Longitud máxima utilizada para la lectura de
#'   cadenas por BLUPF90. Por defecto es \code{100}.
#' @param em_reml_iter Número máximo de iteraciones del algoritmo EM-REML
#'   para la estimación de componentes de varianza. Solo se utiliza cuando
#'   \code{modo = "comp_var"}. Por defecto es \code{1000}.
#' @param h2_formula Expresión utilizada en \code{OPTION se_covar_function}
#'   para calcular el error estándar de una función de los componentes de
#'   varianza. Por defecto corresponde a una expresión para la
#'   heredabilidad:
#'   \code{"G_3_3_1_1/(G_3_3_1_1+R_1_1)"}.
#'
#' @return Un vector de caracteres con las líneas \code{OPTION} que pueden
#'   escribirse directamente en el archivo de parámetros de BLUPF90.
#'
#' @examples
#' \dontrun{
#' # Opciones para evaluación genética
#' blup_options(modo = "selection")
#'
#' # Opciones para estimar componentes de varianza
#' blup_options(
#'   modo = "comp_var",
#'   em_reml_iter = 1000
#' )
#'
#' # Estimar la heredabilidad como función de los componentes
#' blup_options(
#'   modo = "comp_var",
#'   h2_formula = "G_3_3_1_1/(G_3_3_1_1+R_1_1)"
#' )
#' }
#'
#' @export

    blup_options <- function(
    
      modo = c( "ebv", "comp_var"),
      missing = -999, # ok
      max_string_readline = 100, # ok
      em_reml_iter = 1000,
      h2_formula = "G_3_3_1_1/(G_3_3_1_1+R_1_1)" ) {
      
      modo <- match.arg(modo)
      
      opciones <- c( paste("OPTION missing", missing ), # "OPTION missing -999"
                     paste("OPTION max_string_readline",  max_string_readline), # OPTION max_string_readline 100
                     "OPTION sol se", 
                     "OPTION solution all", 
                     "OPTION solution mean", 
                     "OPTION origID",
                     "OPTION out_se_covar_function",
                     "OPTION use_yams"
                     )
  
      if (modo == "ebv") {
        
        options <- opciones 
        
        } 
      
      if (modo == "comp_var") {
          
          options <- c( opciones,
                        "OPTION method VCE",
                        paste("OPTION method EM-REML", em_reml_iter),
                        paste("OPTION se_covar_function", h2_formula) ) 
          }
      
      options
    }
    
    blup_options(modo ="ebv")
    