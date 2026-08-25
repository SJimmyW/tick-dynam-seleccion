

log_txt2 <- function(lista, cuando) {
  
  tablas <- lapply(lista, readRDS)
  
  sublista <- rbindlist(lapply(tablas, function(j) {
    nid <- ncol(j)
    
    data.table(
      campo  = rep(j[1, 1], nrow(j) * (nid - 3)),
      region = rep(j[1, 2], nrow(j) * (nid - 3)),
      dia    = rep(1:nrow(j), each = nid - 3),
      id     = rep(colnames(j)[4:nid], times = nrow(j)),
      valor  = as.vector(as.matrix(j[, 4:nid]))
    )
  }), fill = TRUE)
  
  sublista <- sublista[!is.na(valor)]
  sublista[, valor := log(valor)]
  
  loga <- sublista[dia %in% cuando]
  
  conteo_log <- loga[, .(valor = mean(valor, na.rm = TRUE)),
                     by = .(id, campo, region)]
  
  return(conteo_log)
}

