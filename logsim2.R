
#' Simulación integrada de cruzamientos y dinámica de garrapatas
#'
#' Esta función realiza una simulación completa de de dinámica de infestación por garrapata
#' en contexto de heteogeneidad de susceptibilidad individual por campo,
#' aplicando diferentes diseños de familias (`MH`: Medios Hermanos, `HE`: Hermanos Enteros,
#' `SD`, sin diseño de familias). 
#'
#' @param ncampo Integer. Número de campos a simular.
#' @param h2 Numeric. Heredabilidad del carácter simulado.
#' @param pop Objeto de clase `Pop` (AlphaSimR). Población base.
#' @param nobsv Integer. Número de progenies simuladas.
#' @param tfliar Integer. Vector de dimensión 2. Tamaño de cada familia simulada.
#' @param maximo_a Numeric. Valor máximo permitido de la susceptibilidad individual (`a`).
#' @param minimo_a Numeric. Valor mínimo permitido  de la susceptibilidad individual (`a`).
#' @param size Numeric vector de longitud `nregiones`. 
#' Parámetro `size` de la binomial negativa por región.
#' @param mu Numeric vector de longitud `nregiones`. 
#' Parámetro `mu` de la binomial negativa por región.
#' @param size2 Igual que `size`, usado internamente por `tickdynam`.
#' @param mu2 Igual que `mu`, usado internamente por `tickdynam`.
#' @param infestación: en caso de definir nivel de infestación conocido
#' y fijo para todos los animales. Tal el caso dei nfestación artificial.
#' @param factor Numeric. Factor de escala de la infestación inicial por animal.
#' @param donde Character. Ruta al directorio donde guardar los resultados.
#' @param SP Simulation Parameter de AlphaSimR.
#' @param nhembra_machoHE Integer. Número de hembras por macho en cruzamiento Hermanos Enteros (`HE`).
#' @param ncampos_padreMH Integer. Número de campos desde ue cada padre tendrá progenie de
#' medios hermanos (`MH`).
#' @param nregiones Integer. Número de regiones simulados. Afecta la contaminación
#' inicial de parcela e infestación individual.
#' @param naños Integer. Número de años para la simulación del ciclo dinámico de garrapata.
#'
#' @return Una lista con los siguientes elementos:
#' \describe{
#'   \item{padresHE, padresMH, padresSD}{IDs de padres usados en cada cruzamiento.}
#'   \item{madresHE, madresMH, madresSD}{IDs de madres usadas en cada cruzamiento.}
#'   \item{crossplanMH, crossplanHE, crossplanSD}{Planes de cruzamiento por tipo.}
#'   \item{genealogiaMH, genealogiaHE, genealogiaSD}{Genealogías combinadas (`pedigree` base + progenie).}
#'   \item{garrapatasMH, garrapatasHE, garrapatasSD}{Rutas a los archivos `.rds` por campo con datos de dinámica.}
#'   \item{logarraMH, logarraHE, logarraSD}{Rutas a los archivos `.rds` con los logs de la simulación.}
#'   \item{aMH, aHE, aSD}{Listas de matrices con IDs y valores `a` por campo.}
#'   \item{conteoinicialMH, conteoinicialHE, conteoinicialSD}{Conteo inicial de garrapatas por animal.}
#' }
#'
#' @details
#' Esta función integra varios pasos:
#' - Validación de argumentos y carga de paquetes requeridos.
#' - Carga de funciones externas (`.RData`) necesarias.
#' - Generación de planes de cruzamiento mediante `crossplan3()`.
#' - Simulación de cruzas por tipo de familias (MH, HE, SD) usando `cruza3()`.
#' - Simulación de dinámica de garrapatas por campo con `tickdynam()`.
#' - Armado de genealogías combinadas (`ped` + progenie).
#'
#' Todos los resultados son almacenados en disco en la carpeta especificada en `donde`.
#'
#' @note Esta función requiere tener previamente definidos y accesibles los archivos `.RData`
#' con las funciones auxiliares: `param_bio2.RData`, `iniciales2.RData`, `loggarrapatas.RData`,
#' `crossplan3.RData`, `cruza3.RData`, `tickdynam.RData`.
#'
#' @seealso [tickdynam()], [cruza3()], [crossplan3()]
#'
#' @export


logsim2 <- function(donde_funciones, ncampo, h2, pop, SP, nobsv, tfliar, maximo_a, 
                    minimo_a, size = c(2.618199, 1.52), mu = c(6.424402, 33.48),
                    nregiones = 2, infestacion = NULL,
                    opcion_param, donde,  vez = NULL,   nhembra_machoHE = 3,
                    niterMH = 100, niterHE = 500, ncampos_padreMH = 2, naños,
                    factor, replica) {
  
  assertthat::assert_that(dir.exists(donde_funciones), 
                          is.numeric(ncampo), ncampo > 0,
                          length(size) == nregiones,
                          length(mu) == nregiones,
                          msg = "Revisa tus argumentos")
  
  direccion <- normalizePath(donde, winslash = "\\")
  
  paquetes <- c(  "mc2d", "mvtnorm", "dplyr", "tidyr", "purrr", "furrr",
                  "AlphaSimR", "R6", "data.table", "future.apply", "future")# necesarios

  instalados <- paquetes %in% installed.packages() # chequea con los instalados
  
  if (length(paquetes[!instalados]) > 0) {
    install.packages(paquetes[!instalados])
    message("Se instalaron los paquetes: ", paste(paquetes[!instalados],
                                                  collapse = ", "
    ))
  }
  
  lapply(paquetes, require, character.only = TRUE) # Load packages into session
  
  funciones <- c("parambio3.R", "iniciales2.R",
                 "loggarrapatas2.R", "crossplanHE.R",
                 "crossplanMH.R", "crossplan3.R", "cruza4.R",
                 "tickdynam2.R")
  
  for (func in funciones) {
 
    rdatafile <- file.path(donde_funciones, func)
    
    if (file.exists(rdatafile)) {
      message("Sourcing (script): ", func)
      source(rdatafile, local = .GlobalEnv)
    } else {
      message("No se encontró la función .R ")
    }
  }
  
  print("multisession")
  num_cores <- availableCores()  
  plan(multisession, workers = max(1, num_cores - 1))
  options(future.globals.maxSize = 2 * 1024^3)  # Hasta 2 GB
##'------------------------------------------------------------------------##'
##'                  plan de cruza                                         ##
##'------------------------------------------------------------------------##'
##'
  
  rty <- crossplan3(nobsv = nobsv, tfliar = tfliar, h2 = h2, 
                    pop, ncampo, nhembra_machoHE = 3, 
                    niterMH = niterMH, niterHE = niterHE,
                    ncampos_padreMH = ncampos_padreMH, nregiones = nregiones)
  
##'------------------------------------------------------------------------##'
##'              cruzamientos, uno por escenario de flias.                 ##
##'------------------------------------------------------------------------##' 
##'

  MH <- cruza3(ncampo = ncampo, plan = rty$crossplanMH,
               pop = pop, tipo = "MH", h2 = h2, SP = SP,
               min_a = minimo_a, max_a = maximo_a)
  
  print("Terminó MH")
  HE <- cruza3(ncampo = ncampo, plan = rty$crossplanHE,
               pop = pop, tipo = "HE", h2 = h2, SP,
               min_a = minimo_a, max_a = maximo_a )
  
  print("Terminó HE")
  SD <- cruza3(ncampo = ncampo, plan = rty$crossplanSD,
               pop = pop, tipo = "SD", h2 = h2, SP,
               min_a = minimo_a, max_a = maximo_a )
  gc()
  print("Terminó SD")
  
##'------------------------------------------------------------------------##'
##'                   tick dynamic model                                   ##
##'               uno por escenario de flias.                              ##
##'------------------------------------------------------------------------##' 
##' 

  print("Empieza tick dynamic model")
  plan(sequential)                                          # paralelización 
  tickMH <- tickdynam(ncampo, genea = MH$genea_progenie, 
                      min_a = minimo_a, max_a = maximo_a,
                      naños = naños, h2 = h2,
                      size = size, mu = mu,
                      prefijo = "MH",  
                      factor = factor, opcion_param = opcion_param,
                      pop = pop, donde = direccion,
                      replica = replica) 
  
  tickHE <- tickdynam(ncampo, genea = HE$genea_progenie, 
                      min_a = minimo_a, max_a = maximo_a,
                      naños = naños, h2 = h2,  
                      size = size, mu = mu,
                      prefijo = "HE",   
                      opcion_param = opcion_param, 
                      factor = factor, donde = direccion, 
                      pop = pop, replica = replica) 

   gc()
  tickSD <- tickdynam(ncampo, genea = SD$genea_progenie, 
                      min_a = minimo_a, max_a = maximo_a,
                      naños = naños, h2 = h2,  
                      size = size, mu = mu, 
                      prefijo = "SD",  
                      opcion_param = opcion_param, 
                      factor = factor, donde = direccion,
                      pop = pop, replica = replica) 
  
 
  gc()
  
  ped <- as.data.table(SP$pedigree[,1:2], keep.rownames = "ID")
  names(ped) <- c("id", "padre", "madre")
  
  pedMH <- rbindlist(list(ped, MH$genea_progenie[,1:3]))

  pedHE <- rbindlist(list(ped, HE$genea_progenie[,1:3]))

  pedSD <- rbindlist(list(ped, SD$genea_progenie[,1:3]))

  return(list(
    padresHE = rty$idpadresHE,
    padresMH = rty$idpadresMH,
    padresSD = rty$idpadresSD,
    
    madresHE = rty$idmadresHE,
    madresMH = rty$idmadresMH,
    madresSD = rty$idmadresSD,
    
    crossplanMH = rty$crossplanMH,
    crossplanHE = rty$crossplanHE,
    crossplanSD = rty$crossplanSD,
    
    #progenie = listprogenie, # separada por campo en una gran lista
    
    genealogiaMH =  pedMH,
    genealogiaHE =  pedHE,
    genealogiaSD =  pedSD,
    
    garrapatasMH = tickMH$medias,             # promedio infestación diario por campo
    garrapatasHE = tickHE$medias,             # promedio infestación diario por campo
    garrapatasSD = tickSD$medias,             # promedio infestación diario por campo
    
    logarraMH = tickMH$logtabla,               # conteo cada 30 días
    logarraHE = tickHE$logtabla,               # conteo cada 30 días
    logarraSD = tickSD$logtabla,               # conteo cada 30 días
    
    directoMH = tickMH$directo,                # conteo directo
    directoHE = tickHE$directo,               # infestación individual diaria
    directoSD = tickSD$directo,               # infestación individual diaria
    
    aMH = tickMH$ases,
    aHE = tickHE$ases,
    aSD = tickSD$ases,
    
    conteoinicialMH = tickMH$inicial_garras,
    conteoinicialHE = tickHE$inicial_garras,
    conteoinicialSD = tickSD$inicial_garras
  ))
}

save(logsim2, file = "logsim2.RData")


