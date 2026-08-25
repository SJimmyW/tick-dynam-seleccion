

  año0 <- function(idp, idm, ncampos, nmadres_campo,
                   npadres_campo, npadres){
  
  madres0 <- sample( idm, sum(nmadres_campo) , replace = FALSE)
  madres <- split(madres0, sample(rep(1:ncampos, length.out = length(madres0))))
  idm_restantes <- setdiff( idm, madres0)
  
  padres <- vector("list", ncampos)
  for(camp in seq_len(ncampos)){
    padres[[camp]] <- sample(idp, npadres_campo[camp], replace = T)
  }
  
  idp_restantes <- setdiff(idp, unique(unlist(padres)))
  list(
    madres = madres,
    padres = padres,
    idm_restantes = idm_restantes,
    idp_restantes = idp_restantes
  )
  }
  
  