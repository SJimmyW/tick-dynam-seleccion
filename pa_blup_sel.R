#' Run BLUPf90 and Parse EBV Solutions
#'
#' @description
#' `pablup2()` writes a BLUPF90 instruction file, runs RENUMF90 to generate
#' `renf90.par`, executes BLUPF90, and returns the phenotype table merged with
#' the estimated breeding values (EBV).
#'
#' @details
#' The function is designed for a BLUPF90 workflow in which:
#' \itemize{
#'   \item RENUMF90 reads the instruction file and generates `renf90.par`;
#'   \item BLUPF90 solves the mixed model equations;
#'   \item `solutions.orig` is parsed and merged with the phenotype table.
#' }
#'
#' The sparse PCG solver is controlled with `OPTION num_threads_pcg`.
#'
#' @param donde (where) Character. Working directory where input files are 
#' located and output files are written.
#' @param cuando (when) Character. Trait/record selector used downstream in 
#' phenotype parsing.Used to id the file.
#' @param traits Integer vector. Trait positions in the data file.
#' @param efectos (effects). List of character vectors describing the 
#' model effects in BLUPF90/RENUMF90 syntax.
#' @param file_ped Character. Pedigree file used by BLUPF90.
#' @param file_feno Character. Phenotype file used by BLUPF90.
#' @param var_residual Numeric scalar. Residual variance.
#' @param iter Integer. Replica index used to name intermediate files.
#' @param año Integer. Year index used to name intermediate files.
#' @return A `data.table` with phenotype records merged with EBV solutions.
#' @estima options: "ebv" o "comp_var". Used for the blup_options function to
#' create OPTIONS.
#' 
#' @examples
#' \dontrun{
#' sol <- pablup2(
#'   donde = "H:/Mi unidad/garra/exp sel",
#'   file_ped = "ped_replica_1_year_0.txt",
#'   file_feno = "feno_acum_rep_1_year_0.txt",
#'   iter = 1,
#'   año = 0
#' )
#' }
#' @export

pablup2 <- function( donde, cuando = "14;17", traits = c( 4), 
                     efectos = list(c(2, "cross alpha"), c(3, "cross alpha"),
                                    c(1, "cross alpha")),
                     file_ped,
                     file_feno, 
                     base_feno,
                     var_residual = residual_variance,
                     covariance,
                     options,
                     iter, año
                     ) {

  wd0 <- getwd()
  setwd(donde)
  
  on.exit(setwd(wd0))

  keyword <- "* END  job"   # para esperar a que se ejecute secuencialmente
  keyword2<- "* FINISHED"

##------------------------------------------------------------------------------
## función auxiliar, tiempo de espera para leer salidad de consola
##------------------------------------------------------------------------------
  espera <- function(cmd, keyword, intervalo = 2, timeout = 60) {
    inicio <- Sys.time()
    repeat {
      salida <- tryCatch(system(cmd, intern = TRUE, wait = TRUE),
                         error = function(e) return(character(0)))
      
      ultima_linea <- tail(salida, 1)
      cat("📌 Última línea actual:\n", ultima_linea, "\n")
      if (grepl(keyword, ultima_linea)) {
        return(salida)
      }
      if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) > timeout) {
        stop("Timeout: No se encontró la palabra clave.")
        Sys.sleep(intervalo)
      }
      
    }
  }
  
##------------------------------------------------------------------------------
  ##  arma archivos de parámetros   
##------------------------------------------------------------------------------
     
  param_blupf90 <- function(nombre_archivo, ruta_datos, traits,
                            campovacio, pesos,
                            var_residual, covariance, efectos, genea) {
    parametros <- c(
      "DATAFILE", 
      ruta_datos,
      "TRAITS",
      paste(traits, collapse = " "),
      "FIELDS_PASSED",
      ifelse(campovacio == "", "", campovacio),
      "WEIGHT(S)",
      ifelse(pesos == "", "", pesos),
      "RESIDUAL_VARIANCE",
      var_residual,
      unlist(lapply(efectos, function(e) c("EFFECT", paste(e, collapse = " ")))),
      "RANDOM",
      "animal",
      "FILE",
      genea,
      "FILE_POS",
      "1 2 3 0 0",  # Ajustar según corresponda
      "PED_DEPTH",
      20,
      "(CO)VARIANCES",
      covariance,
      blup_options( options )
      #"OPTION missing -999",# sumado "OPTION max_string_readline 100", # sumado #"OPTION method VCE",
      #"OPTION method EM-REML 1000", "OPTION sol se", # sumado "OPTION solution all", # sumado
      #"OPTION solution mean", # sumado "OPTION origID", # sumado "OPTION out_se_covar_function" # sumado
    )
    
    # Escribir el archivo
    writeLines(parametros, con = nombre_archivo)
    
  }
  
##------------------------------------------------------------------------------
##       corre de a uno, almacena en tabla   
##------------------------------------------------------------------------------
  
    file_par <- paste0("VCE_par", iter, año ,".txt")
    archivo_parametros <- file.path(donde,  file_par) # 
    nombre <- file_feno
    ped <- file_ped  
    
    param_blupf90 (
      nombre_archivo = archivo_parametros,
      ruta_datos = nombre,
      traits = traits,
      campovacio = "",##### campos
      pesos = "",
      var_residual = var_residual,
      covariance = covariance,
      efectos = efectos, # campo , región , ID,
      genea = ped
    )
    
    cat(paste("generando archivo de parámetros ", iter) )
    
    system("cmd.exe")
    cmd <- sprintf('"%s\\renumf90 (1).exe" %s', donde,
                   file_par)
    
    salida <- espera(cmd, keyword, intervalo = 2, timeout = 60)
    
    cmd2 <- sprintf('cmd.exe /c cd /d "%s" && blupf90+.exe renf90.par', 
                   normalizePath(donde, winslash = "\\"))
    
   salida2 <- espera(cmd2, keyword2, intervalo = 2, timeout = 60)
   
   soluciones <- read.table("solutions.orig", header = T , fill = TRUE) 
   efectos <- unique(soluciones$effect)
  
   soluciones <- soluciones[ soluciones$effect == max(efectos), 4:6 ]
   soluciones[,2:3] <- lapply(soluciones[,2:3], as.numeric)
   
   colnames(soluciones) <- c("id", "ebv", "se")
   soluciones <- soluciones[order(soluciones$ebv, decreasing = TRUE), ]
   
   sol <- merge(
     base_feno,
     soluciones,
     by = "id",
     all.x = TRUE
   )
   
   #setnames(sol, "valor", "infestacion")

 
   return(sol)

}

