
#' Simulación de dinámica de garrapatas por campo
#'
#' Esta función simula la dinámica poblacional de garrapatas para múltiples campos,
#' ajustando la distribución inicial de acuerdo al valor genético o fenotípico de cada animal
#' y controlando el riesgo dentro de un rango específico.
#' Los resultados se guardan por campo como archivos `.rds` en una ruta especificada.
#'
#' @param ncampo Integer. Número de campos a simular.
#' @param genea Data.table. Genealogia enrriquecidad de los individuos simulados
#'              Columnas: `id`, `campo`, `region`, y `fenotipo individual`.
#' @param min_a Numeric. Umbral mínimo del parámetro `a` de susceptibilidad individual.
#' @param max_a Numeric. Umbral mínimo del parámetro `a` de susceptibilidad individual.
#' @param size Numeric vector de longitud `region`. 
#' Parámetro `size` de la distribución binomial negativa para cada región.
#' @param mu Numeric vector de longitud `region`. 
#' Parámetro `mu` de la distribución binomial negativa para cada región.
#' @param infestación: en caso de definir nivel de infestación conocido
#' y fijo para todos los animales. Tal el caso dei nfestación artificial.
#' @param prefijo Character. Texto opcional a incluir en el nombre de los archivos generados.
#' @param vez Integer. Identificador único de la corrida 
#' (usado como parte del nombre del archivo).
#' @param factor Numeric. Factor de escalamiento para ajustar la cantidad inicial de garrapatas.
#'@param naños Integer. Número de años para la simulación del ciclo dinámico de garrapata.
#'
#' @return Una lista con cuatro elementos:
#' \describe{
#'   \item{medias} {Lista de rutas a archivos `.rds` que contienen los promedios,
#'    por campo, de infestación diaria.}
#'   \item{feno_directo}{Lista de rutas a archivos `.rds` que contienen tablas, por campo 
#'   e id, con la dinámica poblacional simulada.}
#'   \item{logtabla}{Lista de rutas a archivos `.rds` que contienen tablas con 
#'   infestación individual, cada 30 días a partir del día 0.}
#'   \item{ases}{Lista de matrices con ID y valor `a` (susceptibilidad individual) de los animales simulados por campo.}
#'   \item{inicial_garras}{Lista de data.tables con la infestación inicial simulada por animal, por campo.}
#'
#' @details
#' El cálculo de la susceptbilidad indvidual (`a`) depende del valor de heredabilidad `h2` del carácter.
#' #' Este índice se transforma exponencialmente con coeficientes ajustados empíricamente.
#'
#' Las dinámicas se simulan usando funciones auxiliares: 
#'    `iniciales2()` para calcular condiciones iniciales
#'    `loggarrapatas()` para modelar el ciclo de vida diario de la población de garrapatas.
#'
#' Cada simulación por campo se ejecuta en paralelo usando `future_lapply()`. Los resultados se guardan directamente en disco.
#'
#' @examples
#' \dontrun{
#' resultado <- tickdynam(ncampo = 4, genea = genea_dt,
#'                        min_a = 0.01, max_a = 0.95,
#'                        size = c(2.6, 1.5), mu = c(6.4, 33.5),
#'                        prefijo = "_sim1_", vez = 42, factor = 1) #' }
#'
#' @seealso [iniciales2()], [loggarrapatas()]
#' 
#' @export

  tickdynam <- function(ncampo, genea, min_a, max_a, a_tipo = "pheno",
                        naños = 1, h2,
                      size = c(2.618199, 1.52), mu = c(6.424402, 33.48), 
                      infestación = NULL, factor = 1, prefijo = NULL, 
                      opcion_param, donde, replica, año, meanP, naños_seleccion, #pop,
                      max_animales = NUL){

  dias <- c(38, 59, 86, 127, 157, 198, 253, 304, 331, 365)
  mediaP <- meanP #mean(genea$feno)# mean(pop@pheno) 
  
  medias <- vector("list", ncampo)
  logtabla <- vector("list", ncampo)
  feno_directo <- vector("list", ncampo)
  ases <- vector("list", ncampo)
  inicial_garras <- vector("list", ncampo)
  
  salida <- future_lapply(1:ncampo, function(garra) {# es más lento. Un bucle/campo asegura que la dinámica poblacional de garrapara sea propia
    tryCatch({
      campo <- garra
      genea_garra <- genea[campo == garra]
      region <- genea_garra$region[1]
      id <- genea_garra$id  
      
      if ( a_tipo == "pheno" ) {
        
        x <- genea_garra$feno_alphasim
        
      } else if (a_tipo == "tbv") { 
        
        x <- genea_garra$bv
        
      }
      
##------------------------------------------------------------------------------
##    parámetro de susceptbilidad individual  (`a`)
##    como proporción, toma un rango acotado de valores, de ahi el filtrado
##------------------------------------------------------------------------------      
      a <- switch(as.character(h2),
                  "0.1" = {exp(0.22 * (x - mediaP) - 2.073)},
                  # "0.1" = exp(0.223 * (x - mediaP) - 2.07), # nuevo
                 
                   # "0.3" = exp(0.375 * (x - mediaP) - 2.13), 
                 # "0.3" = exp(0.145 * (x - mediaP) - 0.585),
                "0.3" = {if(a_tipo == "tbv"){ 
                  exp(0.71 * x - 2.05)
                } else if (a_tipo == "pheno"){
                  
                  exp(0.375 * (x - mediaP) - 2.13) # exp(0.73 * (x - mediaP) - 1.9)
                  
                  }},

                 # "0.5" = exp(0.185 * (x - mediaP) - 0.575),
                  "0.5" = exp(0.505 * (x - mediaP) - 2.083),
                  
                  stop("Valor de h2 no se encuentra dentro de lo parametrizado")
      )
      
      quedan <- which(a >= min_a & a <= max_a)
      a  <-  a[quedan]
      id <- id[quedan]
      
      if (length(a) > (max_animales/ ncampo)) {
        
        quedan <- sample(1:(max_animales / ncampo))
        a <- a[quedan]
        id <- id[quedan]
        
      }
      
      n <- length(a)
      
      ases[[garra]] <- data.table::data.table(
        id = id,
        a = a)# cbind(id, a)
      
##------------------------------------------------------------------------------
##                        infestación individual 
##------------------------------------------------------------------------------     
      
      if( is.null(infestación) ) {
        Tticks <- ceiling(rnbinom(n = n, 
                                  size = size[region],
                                  mu = mu[region] ) * factor) 
      }
      
      inicial_garras[[garra]] <- data.table(campo = rep(garra, n),
                                            region = rep(region, n),
                                            inicial = Tticks) 
      
      inic <- iniciales2(
        Tticks0 = Tticks, 
        n = n, 
        size = size[ region ],
        mu = mu[ region ], 
        mes = "enero", 
        opcion_param = opcion_param )
       
##------------------------------------------------------------------------------
##            simulación del ciclo biológico 
##------------------------------------------------------------------------------
      
      garrapata <- loggarrapatas(
        dias, parambio, 
        c_prim_ver, 
        c_oto_inv,
        opcion_param = opcion_param, 
        Tticks, 
        Nfemd_1 = inic$Nfemd_1,
        Teggsd_1 = inic$Teggsd_1, 
        Neggshd_1 = inic$Neggshd_1,
        Nlhostd_1 = inic$Nlhostd_1, 
        n,  
        a, 
        campo = campo, 
        id,
        naños = naños, 
        region = region,
        replica = replica,
        año = año,
        naños_seleccion = naños_seleccion
      )
      
     # cat(" tabla campo", garra, "\n")
##------------------------------------------------------------------------------
##    Genera un nombre de archivo temporal para cada campo y guarda en memoria
##------------------------------------------------------------------------------
      temp_file <- file.path(donde, paste0("garrapatas_rep_", replica, prefijo, 
                                            "campo_", garra,"año_", año, ".rds")) 
      
      saveRDS(garrapata$medias, file = temp_file)                        # promedio de conteos
     
      temp_fileB <- file.path(donde, paste0("loggarrapatas_", replica, prefijo, 
                                            "campo_", garra, "año_", año, ".rds")) 
      saveRDS(garrapata$logtabla, file = temp_fileB)                    # un conteo cada 30 días 
      
      
      if( año == naños_seleccion ){
        
        temp_fileC <- file.path(donde, paste0("conteo_directo_", replica, prefijo, 
                                              "campo_", garra, "año_", año, ".rds")) 
        saveRDS(garrapata$directo, file = temp_fileC)                    # infestación diaria por animal
        
      } 
##------------------------------------------------------------------------------
##              Guarda la tabla en un archivo temporal
##------------------------------------------------------------------------------
  
      list(temp_file = temp_file,        # promedio diario
           temp_fileB = temp_fileB,     # conteo log individual
           if( año == naños_seleccion ) temp_fileC = temp_fileC,     # recuento directo individual diario
           ases = ases[[garra]], 
           inicial_garras = inicial_garras[[garra]])
      
    }, error = function(e) {
      # Este bloque SOLO se ejecuta si ocurre un error, para identificarlo
      message(sprintf("Error en campo %d: %s", garra, e$message))
      
      if (!is.null(e$call)) {
        message(" Llamada fallida:\n  ", deparse(e$call))
      }
      
      message(" Traceback:")
      tb <- sys.calls()
      
      tb_show <- tail(tb, 10)
      for (i in seq_along(tb_show)) {
        message(sprintf("  %2d: %s", i, deparse(tb_show[[i]])))
      }
      
      list(
        success = FALSE,
        campo   = garra,
        message = e$message,
        call    = e$call,
        traceback = tb_show
      )
      
    })
  }, future.seed = TRUE, 
  future.globals =TRUE,

 # future.globals = c("genea", "size", "mu", "factor", "donde",
  #                 "replica", "prefijo", "iniciales2", "loggarrapatas",
   #                "naños" ),
  future.packages = "data.table" )
  
  for (garra in 1:ncampo) {
    medias[[garra]] <- salida[[garra]]$temp_file
    logtabla[[garra]] <- salida[[garra]]$temp_fileB
    if( año == naños_seleccion) feno_directo[[garra]] <- salida[[garra]]$temp_fileC
    inicial_garras[[garra]] <- salida[[garra]]$inicial_garras
    
    ases[[garra]] <- salida[[garra]]$ases
    
  }
  
  return(list(medias = medias,
              logtabla = logtabla,
              if( año == naños_seleccion ) feno_directo = feno_directo,
              ases = ases, 
              inicial_garras = inicial_garras))
  
  }
  
 # save(tickdynam, file = "tickdynam.RData")
  