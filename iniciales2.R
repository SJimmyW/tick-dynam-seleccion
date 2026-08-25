
#' Inicialización de parámetros de infestación al momento 0 de la simulación 
#'
#' Calcula los valores iniciales de hembras, huevos, larvas en ambiente y en huésped
#' según parámetros biológicos del mes y proporciones iniciales.
#'
#' @param Tticks0 Número inicial de garrapatas (ticks). Infestación inicial. Default: 644
#' @param Nfem0 Número inicial de hembras sobre el animal. Default: 27
#' @param Teggs0 Número inicial de huevos en el ambiente. Default: 170000
#' @param Neggsh0 Número inicial de huevos eclosionados el día 0. Default: 6000
#' @param Nlarva0 Número inicial de larvas en el ambiente. Default: 230000
#' @param mes Mes para tomar parámetros biológicos (ej. `"enero"`). Default: `"enero"`
#' @param n Número de individuos simulados (si n > 1, escala los parámetros). Default: NULL
#' @param size Parámetro `size` de la distribución negativa binomial. Default: 0.9600019
#' @param mu Parámetro `mu` de la distribución negativa binomial. Default: 9.6250294
#' @param mu y @param size  son utilizados para asignar una infestación inicial única a cada individuo
#' @return Lista con: `Nfemd_1`, `Teggsd_1`, `Neggshd_1`, `Nlhostd_1`, y `parámetros` utilizados.
#' @param opcion_param Una de c("min","max","media","sample"). Define qué estadístico usar en el
#' muestreo de la distribución de probabilidad de los parámetros biológicos.
#' @export
#' opcion_param
#' 
iniciales2 <- function(Tticks0 = 644, Nfem0 = 27,
                       Teggs0 = 170000, Neggsh0 = 6000,
                       Nlarva0 = 230000, mes = NULL, n = NULL,
                       size = 0.9600019, mu = 9.6250294,
                       opcion_param = NULL) {
  
  par <- parambio2(mes, opcion = opcion_param )
  
  
  if (!is.null(n) && n > 1) {

    sumaticks <- sum(Tticks0)
    
    Nfem0 <- ceiling(0.04192547 * Tticks0) # proporcional a 27 Nfem0 / 644Ttikcs0 del excel
    
    Teggs0 <- ceiling(263.9752 * sumaticks) # proporcional a 170000 Teggs0 / 644 Ttiks0
    
    Neggsh0 <- ceiling(9.31677 * sumaticks) # proporcional a 6000 Neggs0 / 644 Ttiks0
    
    Nlarva0 <- ceiling(357.1429 * sumaticks)
  } else {
    Nfem <- Tticks0 / 23
    Nfem0 <- Nfem0
    Teggs0 <- Teggs0
    Neggsh0 <- Neggsh0
    Nlarva0 <- Nlarva0
  }
 
  HFR <-  as.numeric(unlist(par))[6] #  Tomo el valor correspondiente a 24 de enero
  Nlhost0 <- Nlarva0 * HFR # N° de larvas en huéspedes
  Nfemd_1 <- Nfem0
  Teggsd_1 <- Teggs0
  Neggshd_1 <- Neggsh0
  Nlhostd_1 <- Nlhost0
  
  return(list(
    Nfemd_1 = Nfemd_1,
    Teggsd_1 = Teggsd_1,
    Neggshd_1 = Neggshd_1,
    Nlhostd_1 = Nlhost0,
    parámetros = par
  ))
}

save(iniciales2, file = "iniciales2.RData")