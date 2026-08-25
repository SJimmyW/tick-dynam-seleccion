
#' Simulate recurrent selection and tick infestation in cattle
#'
#' Simulates a multi-year cattle breeding program combining reproduction,
#' parent replacement, progeny generation, tick infestation simulation,
#' phenotype recording, EBV estimation, and genetic selection.
#'
#' The function distributes animals among fields, generates progeny using
#' AlphaSimR, simulates tick infestation with the dynamic tick model, and
#' estimates EBVs to select replacement males and females for subsequent
#' breeding years.
#'
#' @param ncampos Integer. Number of fields used in the simulation.
#' @param h2 Numeric. Heritability used to simulate progeny phenotypes.
#' @param n_replicas Integer. Number of simulation replicates.
#' @param iter Integer. Identifier of the current simulation replicate.
#' @param naños Integer. Number of years considered in the tick dynamic model.
#' @param naños_seleccion Integer. Number of years over which recurrent
#'   selection is performed.
#' @param donde Character string. Directory where simulation output files are stored.
#' @param pob AlphaSimR population object containing the initial population.
#' @param SP AlphaSimR `SimParam`.
#' @param edad_serv Numeric. Age at first service of females, in months.
#' @param edad_serv_padres Numeric. Age of parents at service, in months.
#' @param npartos Integer. Number of parities considered for females.
#' @param pdest Numeric. Proportion born progeny that survives until weaning.
#' @param nmadres_campo Numeric. Number of mothers assigned to each field.
#' @param reposicion_madres Integer. Proportion of replacement females.
#' @param reposicion_padres Integer. Proportion of replacement males.
#' @param porcentaje_padres Numeric. Percentage of males used as parents.
#' @param criterio_hembras Character string. Criterion used to select females.
#' @param criterio_machos Character string. Criterion used to select males.
#' @param nregiones Integer. Number of regions represented in the simulation.
#' @param minimo_a Numeric. Minimum value of the infestation parameter `a`.
#' @param maximo_a Numeric. Maximum value of the infestation parameter `a`.
#' @param a_tipo Character string. Determines the genetic information used
#'   to calculate the infestation parameter. The default is `"pheno"`,
#'   which uses the simulated phenotype. If a different value is supplied, the true breeding value is used.
#' @param size Numeric vector. Region-specific size parameters used by the tick dynamic model.
#' @param mu Numeric vector. Region-specific mean parameters used by the tick dynamic model.
#' @param factor Numeric. Scaling factor used by the tick dynamic model.
#' @param opcion_param Parameterization option passed to the tick dynamic model.
#' @param crit_sel Selection criterion used for EBV-based selection ("pheno", ebv, "mix").
#' @param direccion_sel Character string. Direction of selection. For example,
#'   `"neg"` indicates selection toward lower EBVs.
#' @param meanP Numeric. Phenotypic mean used by the infestation model.
#' @param max_animales Integer. Maximum number of animals allowed in the simulation.
#' @param cuando Character string. Range of months used to construct the
#'   annual phenotype data, specified as two values separated by `";"`.
#'
#' @details
#' The simulation is performed sequentially across breeding years.
#' In each year, mothers and fathers are selected or replaced according to
#' the reproductive parameters. Progeny are generated using `randCross2()`
#' from AlphaSimR and their phenotypes are simulated using `setPheno()`.
#'
#' Before the annual simulation loop, the additive genetic variance (`vara`)
#' and residual variance (`vare`) of the historic population are calculated
#' once from `pob`. The additive genetic variance is obtained with
#' `varA(pob)`, whereas the residual variance is calculated as
#' `varP(pob) - varA(pob)`. These components are then kept constant throughout
#' the simulation and are used as a `covariance` and `residual_variance` 
#' in blup stimation.
#'
#' The progeny are assigned to fields and regions, and tick infestation is
#' simulated using `tickdynam()`. The argument `a_tipo` determines which
#' individual information is used in the infestation model.
#'
#' After the tick infestation phenotype is generated, EBVs are estimated
#' Selected males and females are retained as replacement parents for the 
#' following breeding year.
#' 
#' #' The BLUPF90 parameter file is configured according to the statistical
#' model fitted in the simulation. In the current implementation:
#'    
#'              2,   # field,
#'              3,  # región,
#'              6, # sex, 
#'              1 # id and
#'              
#' The trait analyzed is specified in the `TRAITS` statement and 
#' corresponds to the fourth column of the phenotype file.
#' Thus, the model includes field, region, and sex as fixed effects and
#' animal as the random additive genetic effect.
#' 
#' Intermediate populations are stored as RDS files during the simulation
#' to manage memory usage across years and fields.
#'
#' @return A list containing:
#' \describe{
#'   \item{media_garrapatas}{ Mean tick infestation by field and year. }
#'   \item{conteoinicial}{ Initial tick counts generated during the simulation. }
#'   \item{ebv}{ Final data set containing the EBV results and associated infestation
#'     information. }
#'   \item{fenotipos}{ Accumulated phenotype data generated during the simulation. }
#'   \item{conteo_ultimo_año}{ Tick infestation output corresponding to the final simulated year. }
#'   \item{genea}{Accumulated genealogical, phenotypic, and genetic information for
#'     the simulated progeny. } }
#'
#' @seealso
#' \code{\link{params_repro}}, \code{\link{tickdynam}}, \code{\link{blup_seleccion}}
#'
#' @export#'
#' 

  cruzamiento_sel <- function( ncampos = 1, 
                               h2= 0.3,
                               n_replicas , iter, 
                               naños = 2, naños_seleccion = 10,
                               donde,
                               pob, SP,
                               edad_serv = 18, edad_serv_padres = 24,
                               npartos = 5, pdest = 1, nmadres_campo = 100/2,
                               reposicion_madres = 20, reposicion_padres = 25,
                               porcentaje_padres = 3.5,
                              # criterio_hembras = "rand", 
                              # criterio_machos = "rand",
                               nregiones,  minimo_a, maximo_a, a_tipo = "pheno",
                               size, mu, factor,
                               opcion_param,
                               crit_sel = crit_sel, direccion_sel = "neg",
                               efectos,
                               options,
                               meanP,
                               max_animales = 10000,
                               cuando
                               )
                               {
                  
    assertthat::assert_that(dir.exists(donde_funciones), 
                            is.numeric(ncampos), ncampos > 0,
                            length(size) == nregiones,
                            length(mu) == nregiones,
                            msg = "Revisa tus argumentos")
    direccion <- normalizePath(donde, winslash = "\\")
    
    print("multisession")
    num_cores <- availableCores()  
    plan(multisession, workers = max(1, num_cores - 1))
    options(future.globals.maxSize = 2 * 1024^3)  # Hasta 2 GB
    
    param_repro <- params_repro( edad_serv = edad_serv,
                                 edad_serv_padres = edad_serv_padres,
                                 npartos = npartos,
                                 pdest = pdest,
                                 nmadres_campo = nmadres_campo,
                                 ncampos = ncampos,
                                 reposicion_madres = reposicion_madres,
                                 reposicion_padres = reposicion_padres,
                                 porcentaje_padres = porcentaje_padres
                                 )
    
##'-------------------------------------------------------------------------##'
##'                 listas que almacenan salidas                            ##'
##'-------------------------------------------------------------------------##'
    
    pats <- vector("list", length = ncampos)      # los padres de cada campo cada año
    geneas <- vector("list",  ncampos )           # acumula por campo. Se sobrescribe  cada año
    names(geneas) <- paste0("campo_", seq_len(ncampos))
   
    fenotipos <- vector("list", naños_seleccion + 1)      # se crea en cada réplica
    conteoinicial <-  vector("list", naños_seleccion + 1) # se crea acá solamente. Return
    names(conteoinicial) <- paste0("año_", 0:naños_seleccion)
    
    media_garra <- vector("list", naños_seleccion + 1)   # se crea acá. Return
    names(media_garra) <-  paste0("año_", 0:naños_seleccion)
    
    descarte <- vector("list", ncampos)     # reutilizable
    
    region <- sample(1:nregiones, ncampos, replace = T)
    
    candidatos <- vector("list", length = ncampos) ## candidatas <- vector("list", length = ncampos) 
    vaq <- vector("list", ncampos)
   
    sex <- c(rep("M", ceiling(param_repro$npadres * 1 * 1.3)),
             rep("F", (nInd(pob) - floor((floor(param_repro$npadres * 1 * 1.3))) )))
    pob@sex <- sample(sex, nInd(pob), replace = F)
    
    candidatas_H <- pob[pob@sex == "F"]@id    #pop_H <- pob[pob@sex == "F"]
    candidatos_M <- pob[pob@sex == "M"]@id
    
    
    vara <- varA(pob)
    vare <- varP(pob) - vara
    
    base_feno <- data.table( ) #    base_ebv <- data.table()
    geneacompleta <- data.table()
   
    for ( año in 0:naños_seleccion ){ 
      
      toritos_file <- paste0(temp_dir, "/toritos_", "año", 
                             año - param_repro$ingresomachos, ".rds")
      if ( año >= param_repro$ingresomachos ) toritos <- readRDS(toritos_file)
      
      #criterio_H <- param_repro$criterio_hembras
     # criterio_M <- param_repro$criterio_machos
      
      if ( año == 0) {
        cero <- año0(idp = candidatos_M,
                     idm = candidatas_H,
                     ncampos = ncampos,
                     nmadres_campo = param_repro$nmadrescampo,
                     npadres_campo = param_repro$npadrescampo, 
                     npadres = param_repro$npadres)
        }
      
      for (campo in 1:ncampos) {

##'------------------------------------------------------------------------##'
##'                Recambio vegetativo                                    ##
#'------------------------------------------------------------------------##' 
##'   
        
        if (año == 0) {
          
         mamis <- pob[pob@id %in% cero$madres[[ campo ]]] #selectInd(pob, param_repro$nmadrescampo[[ campo ]], sex = "F", use = criterio_H, candidates = cero$madres[[ campo ]] )
##' las madres le asigna edad y campo
##' 
          n <- cantidades(nIndv = param_repro$nmadrescampo[[ campo ]], 
                          prop = param_repro$pmadres)
          mamis@misc$año <- edad(npartos = param_repro$H$npartos, 
                                 cantidad = n,
                                 edadingreso = param_repro$ingresohembras
                                 )
          
          mamis@misc$campo <- rep(campo, param_repro$nmadrescampo[[ campo ]])
          candidatas_H <- cero$idm_restantes
          
##' guardo como archivo temporario
##' 
          saveRDS(mamis, file.path(local_temp_dir[campo], "mamis.rds"))
          

     #  cat("madres año ", año, " campo", campo, "\n")
          

          n <- cantidades(nIndv = param_repro$npadrescampo[[campo]], 
                          prop = param_repro$ppadres)
          
          padres <- viejos( # machos <- viejos(
            pob = pob, 
            nIndv = param_repro$npadrescampo[[campo]],
            ppadres = param_repro$ppadres,
            #criterio = criterio_machos,
            sexo = "M", maximo = param_repro$maximo, ncampos = ncampos,
            año = año, ingreso = param_repro$ingresomachos
            )
          
          pats[[campo]] <- padres
          
        } else {
          
          year <- paste0("año", año - param_repro$ingresohembras)
          prog <- paste0(temp_dir, "/prog_campo_", campo, year,".rds")
          mums <- readRDS(file.path( local_temp_dir[campo], "mamis.rds" )) 
          
          if( año >= param_repro$ingresohembras ) { mamitas <- readRDS(prog)
            #cuantas <- sum(mums@misc$año == min(mums@misc$año))
             #  hoy <- año - param_repro$ingresohembras #  repo <- paste0(hoy, "_")
            #campo_i <- campo
            #candidatas <- bv$top_idhembra[ campo == campo_i] #  ebv[[hoy + 1]][campo == campo_i & sexo == "F" & año == repo][order(-ebv)][1:cuantas]
          }
          
          mamis <- refugo(
            historica = pob[pob@sex == "F"], 
            actual = mums,
            candidates = mamitas,
            ngrupo = param_repro$nmadrescampo[[campo]],
            sexo = "F", año = año,
            ingreso = param_repro$ingresohembras,
            #criterio = criterio_H,
            campo = campo
          )

          saveRDS(mamis, file.path(local_temp_dir[campo], "mamis.rds"))
          
          pats[[campo]] <- refugo(
            historica = pob[pob@sex == "M"],
            actual = pats[[campo]],
            candidates = toritos,
            ngrupo = param_repro$npadrescampo[campo],
            sexo = "M", año = año,
            ingreso = param_repro$ingresomachos,
           # candidates_id = bv$top_idmacho, # sample(toritos@id, param_repro$npadrescampo[[campo]]),
            #criterio = criterio_M,
            campo = campo
          )
          
        } # bucle reposición 
        
 ##'------------------------------------------------------------------------##'
##'                           cruza                                        ##
##'                                                                        ##
##'------------------------------------------------------------------------##' 
##'    
        crias <- repro_sel(fems = mamis,
                           mals = pats[[ campo ]], 
                           nCross = param_repro$nprogenie[[ campo ]], 
                           nProgeny = 1, field = campo, reg = region,
                           year = año, h2 = h2, simParam = SP )
        
        a_descarte <- sort(unique(mamis@misc$año))[ param_repro$ingresohembras ]
        descarte[[ campo ]] <- sum(mamis@misc$año == a_descarte) 

       
        geneas[[ paste0( "campo_",  campo) ]] <- crias$genealogy

        candidatos[[ campo ]] <- crias$progeny[ crias$progeny@sex == "M" ]
        vaq[[ campo ]] <- crias$progeny[ crias$progeny@sex == "F" ]
          
        rm(mamis, crias )

      } # bucle por campo 
      
      gc()
      genea <- rbindlist(geneas, use.names = TRUE, fill = TRUE) 
      
      ped_completo <- as.data.table(SP$pedigree[ ,1:2 ], keep.rownames = "ID")

      name_ped <- paste0( "ped_replica_", iter, "_year_", año,".txt")
      ruta2 <-  file.path(paste0( "H:\\Mi unidad\\garra\\exp sel\\",
                                  name_ped))
      fwrite( ped_completo, file = ruta2, sep = " ", quote = FALSE, 
              col.names = F)
      rm(ped_completo)
      
      print("Empieza tick dynamic model")
      
##'------------------------------------------------------------------------##'
##'                     tick dynamic model                                 ##
##'                                                                        ##
##'------------------------------------------------------------------------##' 
##'      
      tick <- tickdynam(ncampo = ncampos, 
                        genea = genea, 
                        min_a = minimo_a,
                        max_a = maximo_a,
                        a_tipo = a_tipo,
                        naños = naños,
                        h2 = h2,
                        size = size,
                        mu = mu,
                        prefijo = "MH",  
                        factor = factor, 
                        opcion_param = opcion_param,
                        año =  año, 
                        donde = direccion,
                        replica = iter,
                        meanP = meanP,
                        max_animales = max_animales,
                        naños_seleccion = naños_seleccion) 
      

      fenotipos[[ año + 1 ]] <- tick$logtabla
      conteoinicial[[ paste0( "año_", año) ]] <- tick$inicial_garras
      media_garra[[ paste0( "año_", año) ]] <- tick$medias
      name_feno <- paste0( "feno_acum_rep_", iter, "_year_", año, ".txt" )
      
      cuando2 <- as.numeric(strsplit(cuando, ";")[[1]])
      cuando_vec <- cuando2[1]:cuando2[2]
      feno_anual <- log_txt2( lista = fenotipos[[ año + 1 ]],
                              cuando = cuando_vec )
      feno_anual[ , año := paste0(año,"_") ]
      setkey(feno_anual, id)
      setkey(genea, id)
      
      feno_anual[genea, sexo := i.sexo, on = "id"]
      feno_anual[genea, tbv := i.bv, on = "id"]
      feno_anual[genea, feno_alphasim := i.feno_alphasim, on = "id"]
      
      ases <- rbindlist(tick$ases)
      setkey(ases, id)
      feno_anual[ases, a := i.a]
      feno_anual[ , inf_media := (sum(valor) - valor) / (.N - 1),
                 by = .(campo, año) ]
      
      setnames(feno_anual, "valor", "conteo")
      
      base_feno <- rbindlist( list( base_feno, feno_anual ), 
                              use.names = TRUE, fill = TRUE)
      ruta_feno_acum <- file.path( "H:/Mi unidad/garra/exp sel", name_feno )
      fwrite( base_feno, file = ruta_feno_acum, sep = " ", 
              quote = FALSE, col.names = FALSE )
      
      
      setnames(genea, "bv", "tbv")
      geneacompleta <- rbindlist( list( geneacompleta, genea),
                                        use.names = TRUE, fill = TRUE) 
      
      rm(ases) 
      
##'------------------------------------------------------------------------##'
##'                     ebv                                                ##
##'                                                                        ##
##'------------------------------------------------------------------------##' 
##'     
 
        bv <- blup_seleccion( archivo_feno  = name_feno,
                              archivo_ped   = name_ped,
                              base_feno,
                              cuando        = cuando,
                              genea         = genea,
                              param_repro   = param_repro,
                              anio          = año,
                              iter          = iter,
                              descarte      = descarte,
                              crit_sel      = crit_sel,
                              direccion_sel = direccion_sel,
                              file_fenotipo = feno_anual,
                              covariance = vara,
                              residual_variance = vare,
                              options = options
                              
                              )
        
      rm(feno_anual)  
      machitos <- mergePops( candidatos )
      
      toritos <- machitos[ machitos@id %in% bv$top_idmacho ]
      toritos_file <- paste0( temp_dir, "/toritos_", "año", año, ".rds" )
      saveRDS( toritos, toritos_file )
      
      for( i in 1:ncampos) {
        
        prog <- paste0( temp_dir, "/prog_campo_", i, "año", año, ".rds" )
        print( prog )
        id_campo <- bv$top_idhembra[ campo == i, id ]
        
        hem <- vaq[[ i ]][ vaq[[ i ]]@id %in% id_campo ]
        
        saveRDS( hem, prog )
        
      }
      
      cat(paste0( "terminó año ", año, " replica ", iter, "\n") )
      
      
      } # bucle año de selección
    

    return( list(
   
     media_garrapatas = media_garra,             # promedio infestación diario por campo
     conteoinicial = conteoinicial,
     
    # a = ases,

     ebv = bv$todo, #base_ebv,
     fenotipos  = base_feno,
     conteo_ultimo_año = tick$feno_directo,
     genea = geneacompleta
    
     )
     ) 
    
 } # bucle réplica
  
  