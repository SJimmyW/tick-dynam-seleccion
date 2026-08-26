
  params_repro <- function(
    edad_serv = 18,
    npartos = 5,
    pdest = 1,
    nmadres_campo = 100,
    ncampos = 1,
    reposicion_madres = 20,
    porcentaje_padres = 3.5,
    reposicion_padres = 25,
    edad_serv_padres = 24,
    criterio_hembras = "rand",
    criterio_machos = "rand"
    ){
  
  if (is.null(criterio_hembras)) criterio_hembras <- rep("rand", ncampos)
  if (is.null(criterio_machos))  criterio_machos  <- rep("rand", ncampos)
  
  repro_H <- list(
    edad_serv = edad_serv,
    npartos   = npartos,
    pdest     = pdest
    )
  
  ingresohembras <- ceiling(repro_H$edad_serv / 12)
  
  nmadrescampo <- rep(nmadres_campo, ncampos)
  nmadres <- sum(nmadrescampo)
  nprogenie <- ceiling(nmadrescampo * repro_H$pdest)
  
  pmadres <- c( reposicion_madres / 100,
               0.2, 0.2, 0.2, 0.2) #  0.2037599, 0.2007277, 0.1958763, 0.189812) 
                
  repro_M <- list( porcentaje_padres   = porcentaje_padres,
                   reposicion_padres   = reposicion_padres,
                   edad_serv_padres    = edad_serv_padres
                   )
  
  ingresomachos <- ceiling(repro_M$edad_serv_padres / 12)
  maximo <- 100 / repro_M$reposicion_padres
  
  npadrescampo <- ceiling(nmadrescampo * (repro_M$porcentaje_padres / 100))
  npadres <- sum(npadrescampo)
  
  n_grupos_padres <- ceiling(100 / repro_M$reposicion_padres)
  ppadres <- rep(repro_M$reposicion_padres / 100, n_grupos_padres)
  
  list( H = repro_H,
        M = repro_M,#
        ingresohembras = ingresohembras,#
        ingresomachos = ingresomachos,#
        nmadrescampo = nmadrescampo,#
        nmadres = nmadres,#
        nprogenie = nprogenie,#
        npadrescampo = npadrescampo,#
        npadres = npadres,#
        maximo = maximo,#
        pmadres = pmadres,#
        ppadres = ppadres#,#
        #criterio_hembras = criterio_hembras,#
      #  criterio_machos = criterio_machos#
        )
  }
  
  param_repro <- params_repro( edad_serv = 18,
                               npartos = 5,
                               pdest = 1,
                               nmadres_campo = 100,
                               ncampos = 1,
                               reposicion_madres = 20,
                               porcentaje_padres = 3.5,
                               reposicion_padres = 25,
                               edad_serv_padres = 24,
                               criterio_hembras = "rand",
                               criterio_machos = "rand"
                               )
  
