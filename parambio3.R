

#' param_bio2: Generador de parámetros por mes 
#'
#' Esta función recorre todos los meses definidos en la tabla `parametros` y,
#' según la opción elegida, extrae para cada parámetro:
#'   - "min": mínimo para PFO, PEH, LL, HFR; máximo para POP e IP.
#'   - "max": máximo para PFO, PEH, LL, HFR; mínimo para POP e IP.
#'   - "media": media analítica precalculada.
#'   - "sample": muestreo aleatorio de la distribución original.
#'
#' @param mes Caracter cadena para validación. no corresponde exactamente con mes calendario
#' @param opcion Una de c("min","max","media","sample"). Define qué estadístico usar.
#' @param n Número de réplicas por mes. Si n>1 devuelve lista de listas.
#' @return Lista nombrada con un elemento por cada mes. Cada elemento es:
#'   - una lista de parámetros (si n=1),
#'   - o una lista de listas de réplicas (si n>1).
#' @PFO numeric. Muestreo de PERT. Proporción de hembras en oviposición
#' @POP numeric. Muestreo de distribución uniforme. Periodo pre-oviposición (dias)
#' @IP  numeric. Muestro distribución PERT. Periodo de Incuvación (dias).
#' @PEH numeric. Muestreo distribución PERT. Proporción de huevos que eclosionan (fertilidad de huevos)
#' @LL  numeric. Muestreo de distribución PERT. Longevidad Larval (dias).
#' @HFR numeric. Host Finding Rate. Probabilidad de que una larva en el ambiente interactue con un huésped
#' @examples
#' param_bio2("marzo", opcion="min", n=1)
#' param_bio2("enero", opcion="sample", n=3)
#' @export
#' 


  parambio2 <- function(mes = NULL, opcion = c("min", "max", "media", "sample"),
                         n = 1) {
  
  #opción <- match.arg(opcion)
  
##------------------------------------------------------------------------------
#           1)        Tabla de parámetros
##------------------------------------------------------------------------------
  
parametros <- rbindlist(list(
  # enero
  data.table(mes = "enero",  
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert", "uniform", "pert", "pert", "pert", "constant"),
             min  = c(0.9, 4.5,  21 ,  0.9,   31 ,  0.001),
             mode = c(0.95,  NA, 24.5, 0.95,  74.2, 0.001),
             max  = c(  1 , 5.5,  29 ,  1 ,   89 ,  0.001)
  ),

  # marzo 
  data.table(mes = "marzo", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("uniform", "uniform", "pert", "uniform", "pert", "constant"),
             min  = c( 0.95, 4.5, 30, 0.95,   41,  0.0066),
             mode = c( NA,   NA,  35,  NA,   73.7, 0.0066),
             max  = c( 1,    5.5, 37,  1,     83,  0.0066)
  ),
  # marzo_abril
  data.table(mes = "marzo_abril", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("uniform", "constant", "pert", "pert", "pert", "constant"),
             min  = c(0.95, 6, 50, 0.85,  48,  0.004),
             mode = c(NA,   6, 56, 0.90, 74.3, 0.004),
             max  = c(1,   6, 60, 0.95,  113, 0.004)
  ),
  # abril_mayo
  data.table(mes = "abril_mayo",       
             parámetro = c("PFO","POP","IP","PEH","LL","HFR"),
             dist = c("pert","constant","pert","pert","pert","constant"),
             min  = c( 0.9, 11, 114,  0,   1, 0.0005),
             mode = c(0.95, 11, 121, 0.1, 19, 0.0005),
             max  = c( 1,   11, 124, 0.2, 42, 0.0005)
  ),

  # junio
  data.table(mes = "junio", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert", "constant", "pert", "pert", "pert", "constant"),
             min  = c( 0.9, 17,  99,  0,   1, 0.00016),
             mode = c(0.95, 17, 105, 0.1,  8, 0.00016),
             max  = c(  1,  17, 110, 0.2, 28, 0.00016)
  ),
  # julio_agosto 
  data.table(mes = "julio_agosto", parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist =  c("pert","constant", "pert", "pert", "pert", "constant"),
             min  =  c(0.85, 15,  90, 0.25,  1, 0.00016),
             mode =  c( 0.9, 15,  98,  0.3,  8, 0.00016),
             max  =  c(0.95, 15, 101, 0.35, 28, 0.00016)
  ),
  # agosto_septiembre
  data.table(mes = "agosto_septiembre", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert", "constant", "constant", "pert", "pert", "constant"),
             min  = c( 0.9, 14, 77, 0.35, 15, 0.0003),
             mode = c(0.95, 14, 77,  0.4, 37, 0.0003),
             max  = c( 1,   14, 77, 0.45, 65, 0.0003)
  ),

  # octubre_noviembre
  data.table(mes = "octubre_noviembre", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert", "uniform", "pert", "uniform", "pert", "constant"),
             min  = c(0.75, 5,   49,     0.7,  1, 0.0013),
             mode = c( 0.8, NA, 56.1,    NA,  28, 0.0013),
             max  = c(0.85, 6,   59,     0.8, 59, 0.0013)
  ),
  
  # noviembre_diciembre
  data.table(mes = "noviembre_diciembre", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert", "uniform", "constant", "uniform", "pert", "constant"),
             min  = c( 0.9,  5, 24, 0.7, 13, 0.002),
             mode = c(0.95, NA, 24,  NA, 40, 0.002),
             max  = c( 1,    6, 24, 0.8, 63, 0.002)
  ),

  # diciembre_enero
  data.table(mes = "diciembre_enero", 
             parámetro = c("PFO", "POP", "IP", "PEH", "LL", "HFR"),
             dist = c("pert","uniform","pert","uniform","pert","constant"),
             min  = c(0.85,  5,  21,  0.7, 13, 0.002),
             mode = c( 0.9, NA, 25.1, NA,  39, 0.002),
             max  = c(0.95,  6,  29,  0.8, 60, 0.002)
  )
))

# como la distribución uniforme no tiene moda, hago la media de ambas distribuciones
  parametros[, mean := fifelse(                   # 1° if
    dist == "uniform", (min + max) / 2, 
    fifelse(                                     # 2° if
      dist == "pert", (min + 4*mode + max) / 6,
      min 
      )
    )]
## -----------------------------------------------------------------------------
#      comprobaciones (mes y opción)
##------------------------------------------------------------------------------

  if (!is.null(mes)) {
    if ( !mes %in% unique(parametros$mes)) {
      warning("Mes '", mes, "' no válido. Se usará 'enero'.")
      mes <- "enero"
    }
  }
  
  
  opciones <- c("min","max","media","sample")
  if (! opcion %in% opciones) {
    
    warning("Opción '", opcion, "' no válida. Se usará 'sample'.")
    opcion <- "sample"
  }
  

##------------------------------------------------------------------------------
#        salida por cada mes, todos los meses
##------------------------------------------------------------------------------
  draw_vals <- function(defs, n) {
    
    reps <- lapply(seq_len(n), function(i) {
      v <- vapply(seq_len(nrow(defs)), function(j) {
        dd <- defs[j]
        switch(opcion,
              
               min = switch(dd$parámetro,
                            PFO = dd$min, PEH = dd$min, LL = dd$min, HFR = dd$min,
                            POP = dd$max, IP = dd$max,
                            dd$min
               ),
               
               max = switch(dd$parámetro,
                            PFO = dd$max, PEH = dd$max, LL = dd$max, HFR = dd$max,
                            POP = dd$min, IP = dd$min,
                            dd$max
               ),
               
               media = dd$mean,
              
               sample = switch(dd$dist,
                               uniform  = runif(1, dd$min, dd$max),
                               pert     = rpert(1, min=dd$min, mode=dd$mode, max=dd$max),
                               constant = dd$min,
                               dd$min
               )
        )
      }, numeric(1))
     
      setNames(as.list(v), defs$parámetro)
    })

    if (n == 1) reps[[1]] else reps
  }
  
  if (is.null(mes)) {
    
    cuales <- unique(parametros$mes)
  } else {
    cuales <- mes
  }
  
  salida <- lapply(cuales, function(m0) {
    defs <- parametros[mes == m0]
    draw_vals(defs, n)
  })
  
  names(salida) <- cuales

  return(salida)
  }
  
  
  save(parambio2, file = "parambio2.RData")
  