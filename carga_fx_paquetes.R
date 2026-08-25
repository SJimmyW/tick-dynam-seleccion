#' Cargar paquetes auxiliares y funciones del proyecto
#'
#' @description
#' `carga_fx_paquetes()` verifica e instala, si corresponde, los paquetes
#' necesarios para ejecutar la simulación, carga dichos paquetes en la sesión
#' de R y luego importa en el entorno global las funciones auxiliares
#' almacenadas en `donde_funciones`.
#'
#' @details
#' La función está pensada como paso de inicialización del proyecto. Primero
#' comprueba si los paquetes requeridos están instalados. Si `instalar = TRUE`,
#' intenta instalarlos automáticamente; si `instalar = FALSE` y falta alguno,
#' detiene la ejecución con un error informativo.
#'
#' Luego carga en memoria los scripts de funciones auxiliares mediante `source()`.
#' Estas funciones incluyen rutinas de parametrización, selección, simulación
#' de cruces, cálculo de edades, conteos de garrapatas y selección basada en BLUP.
#'
#' @param donde_funciones Character. Directorio donde se encuentran los scripts
#' de funciones auxiliares.
#' @param instalar Logical. Si `TRUE`, intenta instalar los paquetes faltantes.
#' Si `FALSE`, detiene la ejecución cuando falta alguno.
#'
#' @return Invisiblemente retorna `TRUE` si la carga fue exitosa.
#'
#' @note Las funciones auxiliares se cargan en `.GlobalEnv` mediante `source()`,
#' de modo que quedan disponibles en toda la sesión para los cálculos de la simulación.
#'
#' @examples
#' \dontrun{
#' carga_fx_paquetes("H:/Mi unidad/garra/genomas/funciones", instalar = FALSE)
#' }
#'
#' @export

  carga_fx_paquetes <- function(donde_funciones, instalar = FALSE) {
    paquetes <- c(
    "assertthat", "data.table", "AlphaSimR", "future", "future.apply",
    "mc2d", "mvtnorm" )
    
    faltan <- setdiff(paquetes, rownames(installed.packages()))
    instalar <- paquetes %in% installed.packages() # chequea con los instalados
    
    if (length(paquetes[!instalar]) > 0) {
      install.packages(paquetes[!instalar])
      message("Se instalaron los paquetes: ", paste(paquetes[!instalados],
                                                    collapse = ", "
      ))
    }
    
    lapply(paquetes, require, character.only = TRUE) # Load packages into session
    
    if (length(faltan) > 0 && !instalar) {
      stop("Faltan paquetes: ", paste(faltan, collapse = ", "))
     }
    
    invisible(lapply(paquetes, library, character.only = TRUE))
    
    funciones <- c( "parambio3.R",  "iniciales2.R",  "loggarrapatas2.R",
                    "tickdynam2.R", "log_txt2.R", "params_repro.R", "año0.R",
                    "refugo.R", "viejos.R", "cantidades.R", "edad.R",
                    "repro_sel.R", "blup_options.R", "pa_blup_sel.R",  
                    "blup_seleccion.R", "cruzamiento_sel.R", 
                    "crear_listas_replica.R", "salidas_bv.R")
    
    invisible(lapply(file.path(donde_funciones, funciones), source))
    }