#' Simulate Tick Counts and Log-transform for Multiple Days and Years
#'
#' @description
#' `loggarrapatas` calcula dinámicamente recuentos y estadísticas de garrapatas por día,
#' generando dos salidas:
#' - `tabla`: matriz con columnas diarias y promedios de población de garrapatas.
#' - `logtabla`: matriz con recuentos log-transformados espaciados cada 30 días.
#
#' @param dias Integer vector. Días del año en que cambia el periodo de tiempo
#'  en que se divide cada fase del ciclo vital de la garrapata.
#' @param parambio Lista. Parámetros biológicos estacionales del ciclo biológico
#' del parásito. Almacenados en la función `param_bio2()`.
#' @param c_prim_ver Numeric. Constante. Exponente c para primavera/verano.
#' @param c_oto_inv Numeric. Constante. Exponente c para otoño/invierno.
#' @param Tticks Numeric. Infestación indvidual.
#' @param Nfemd_1 Numeric. Número de hembras adultas sobre el animal en el dia previo.
#' @param Teggsd_1 Numeric. Número de Huevos en el ambiente el día anterior.
#' @param Neggshd_1 Numeric. Número de Huevos  que eclosionaron el día anterior.
#' @param Nlhostd_1 Numeric. Número de larvas que encontraron huésped el día anterior.
#' @param n Integer. Número de individuos simulados.
#' @param a Numeric. Variable a. Parámetro de susceptibilidad individual.
#' @param campo Character. Identificador de campo.
#' @param id Character. Vector de identificadores individual para `logtabla`.
#' @param naños Integer. Número de años de simulación.
#' @param region Character. Región simulada.
#'
#' @return Lista con dos elementos:
#'   - `tabla`: matriz numérica con filas = 365*naños y columnas:
#'      "dia (1:365)", "campo", "region", "log(Nlhost)",
#'      "Media Total garrapatas/animal/día", "log(Tticks)-LogTicks", "Nlarva".
'   - `logtabla`: matriz con recuentos log-transformados cada 30 días, columnas: `campo`, `region`, y `id`.
#'
#' @details
#' Para cada `dia` desde 1 hasta 365*naños:
#' 1. Reinicia `param` si inicia un nuevo año (cada 365 días).
#' 2. Selecciona exponentes `c` según estación (día ≤ 81 o ≥265).
#' 3. Obtiene parámetros estacionales `PFO`, `POP`, `IP`, `PEH`, `LL`, `HFR`.
#' 4. Calcula:
#'    - `Nfem`, `Nfeov`, `Neggs`, `Teggs`, `Neggsh`, `Nlarva`, `Nlhost`.
#'    - `Ntick`, `logNtick`, `difTicks`, actualización de `Tticks`.
#' 5. Llena `tabla` con valores diarios.
#' 6. Cada 30 días, guarda en `tabla2` la fila `logNtick` bajo `logtabla`.
#'
#' @examples
#' \dontrun{
#' res <- loggarrapatas(
#'   dias = 1:365,
#'   parambio = param_bio2(),
#'   c_prim_ver = 1.2,
#'   c_oto_inv = 0.8,
#'   Tticks = 100,
#'   Nfemd_1 = 50,
#'   Teggsd_1 = 200,
#'   Neggshd_1 = 150,
#'   Nlhostd_1 = 80,
#'   n = 10,
#'   a = 0.5,
#'   campo = "Campo1",
#'   id = paste0("rep", 1:10),
#'   naños = 2,
#'   region = "RegiónA"
#' )
#' }
#'
#' @export


loggarrapatas <- function(dias, parambio, opcion_param = "sample", 
                          c_prim_ver, c_oto_inv,
                          Tticks, Nfemd_1, Teggsd_1, Neggshd_1,
                          Nlhostd_1, n, a, campo, id, naños = 1, 
                          region, replica, año, naños_seleccion) {
  n <- length(id)
  ndias <- 365 * naños
  tabla <- matrix(NA, nrow = ndias, ncol = 7 ) # medias
  tabla2 <- matrix(NA, nrow = ndias, ncol = (n + 3)) # conteo log
  lista_diaria <- vector("list", ndias)
  # tabla3 <- matrix(NA, nrow = n, ncol = (365 * naños + 4) ) # conteo directo
  
  colnames(tabla) <- c(
    "dia (1:365)", "campo", "region", "replica", "año",
    "Media Total garrapatas/animal/día", "Nlarva")
  
  colnames(tabla2) <- c("campo", "region", "replica", id)
  
  fechas <- seq.Date(from = as.Date("2025-01-24"), to = as.Date("2026-01-23"), by = "day")
  mes_dia <- format(fechas, "%d %b")
  
# colnames(tabla3) <- c("id", "campo", "region", "replica", rep(mes_dia, naños))
                        
  row.names(tabla) <- rep(mes_dia, naños)
  row.names(tabla2) <- rep(mes_dia, naños)
 # row.names(tabla3) <- id
  
 # tabla3[1:n, 1] <- id ; tabla3[1:n, 2] <- campo  ; tabla3[1:n, 3] <- unlist(region[1])
  #tabla3[1:n, 4] <- replica
  
  # cambio en el período de tiempo anual de los parámetrso biológicos
  dias_mas_anos <- unlist(lapply(0:(naños - 1), function(n) dias + 365 * n)) 
  
  param <- parambio2(opcion = opcion_param, n = 1)
  a <- as.numeric(a)
  fila <- 30
  
  for (dia in 1:(365 * naños)) {
    if ((dia - 1) %% 365 == 0) {
      param <- parambio2(opcion = opcion_param, n=1)
      cat("\n\n")
      cat("Actualizando parambio para el año", (dia - 1) / (365 + 1), "\n")
      cat("\n\n")
      print(param$enero$PFO)
      cat("\n\n")
    } # reinicia parámetros si es más de un año
    
    dia_anual <- dia %% 365 # tiene sentido si más de un año
    # if (dia_anual == 0) dia_anual <- 365 # Asegura que el último día del año sea 365
## -----------------------------------------------------------------------------
##             constante c. Podría variar a lo laargo del año
## -----------------------------------------------------------------------------    
    if (dia_anual <= 81 || dia_anual >= 265) {
      c <- c_prim_ver
    } else {
      c <- c_oto_inv
    }

## -----------------------------------------------------------------------------
##  parámetros biológicos del ciclo vital de la garrapara. Variable por períiodo de tiempo
## -----------------------------------------------------------------------------     
    periodo <- min(which(dias_mas_anos >= dia_anual))
    
    PFO <- param[[periodo]]$PFO 
    POP <- param[[periodo]]$POP 
    IP <- param[[periodo]]$IP 
    PEH <- param[[periodo]]$PEH 
    LL <- param[[periodo]]$LL 
    HFR <- param[[periodo]]$HFR 
    
## -----------------------------------------------------------------------------
##                  ciclo propiamente dicho
## -----------------------------------------------------------------------------  
    
    Nfem <- pmax(0, Tticks / 23) # original
    Nfeov <- pmax(0, ((Nfem * PFO) / POP) + Nfemd_1 - (Nfemd_1 / POP)) # (((Nfem * PFO - Nfemd_1) / POP) + Nfemd_1)   #
    Nfeovq <- pmax(0, sum(Nfeov, na.rm = T)) # combino en un escalar, operaciones al vicio)
    
    Neggs <- trunc(mean(runif(Nfeovq + 1, min = 2000, max = 3000))) # Neggs <- round(runif(1, 2000, 3000))
    Teggs <- pmax(0, (Nfeovq * Neggs * PEH + Teggsd_1 - Neggshd_1)) # combiné dos términos
    Neggsh <- pmax(0, (Teggs * PEH) / IP)
    
    Nlarva <- pmax(0, (Neggshd_1 + Neggsh) * (1 - (1 / LL)) - Nlhostd_1) # creo haber simplificado
    Nlarva <- matrix(Nlarva / n, nrow = 1, ncol = n) # divido el pool de larvas
    Nlhost <- pmax(0, Nlarva * HFR) # N° que sobreviven al huésped
    
    Ntick <- pmax(0, a * (Nlhost^c))
    logNtick <- log(a) + c * log(Nlhost)
    difTicks <- log(Ntick) - logNtick
    Tticks <- pmax(0, Tticks + Ntick - Nfeov)
    # Tticks <- ifelse((Tticks + Ntick - Nfeov)<0, 0, (Tticks + Ntick - Nfeov)) # al pool se le suman las nuevas y restan las adultparambio
    
## -----------------------------------------------------------------------------
##            almacenamiento tablas
## -----------------------------------------------------------------------------     
   # cat(" armando tabla dia", dia, "\n")
    Nfemd_1 <- Nfem
    Teggsd_1 <- Teggs
    Neggshd_1 <- Neggsh
    Nlhostd_1 <- Nlhost
    
    tabla[dia, 1] <- dia
    tabla[dia, 2] <- campo
    tabla[dia, 3] <- unlist(region[1])
    tabla[dia, 4] <- replica
    tabla[dia, 5] <- año
    tabla[dia, 6] <- (mean(Tticks)) # Tticks
    tabla[dia, 7] <- ceiling(mean(Nlarva)) # Tticks
    #tabla[dia, 8:ncol(tabla)] <- ceiling(Tticks)
    
    
    if (dia %% 30 == 0) { # guardo cada 30 días exclusivamente
      
      tabla2[fila, 1] <- campo                #tabla2[dia, 1] <- campo
      tabla2[fila, 2] <- unlist(region[1])    #tabla2[dia, 2] <- unlist(region[1])
      tabla2[fila, 3] <- replica
      tabla2[fila, 4:ncol(tabla2)] <- Tticks  #tabla2[dia, 3:ncol(tabla2)] <- logNtick
      fila <- fila + 30
    }
     
     # tabla3[1:n, (4 + dia)] <- 
     if(año == naños_seleccion) {
       lista_diaria[[dia]] <- data.table( id = id,
                                          campo = campo,
                                          region = region,
                                          replica = iter,
                                          año = año,
                                          dia = dia,
                                          garrapatas = Tticks)
       
     }
    tabla3 <- rbindlist(lista_diaria)
    }
  
  return(list(
    logtabla = na.omit(tabla2),
    if(año == naños_seleccion) directo = tabla3,
    medias = tabla
  ))
}

save(loggarrapatas, file = "loggarrapatas.RData")
