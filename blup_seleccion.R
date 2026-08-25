
#' BLUP-based or Phenotypic Selection of Breeding Animals
#'
#' Performs within-generation selection of sires and dams using estimated
#' breeding values (EBV), phenotypic records, or random selection.
#'
#' Male candidates are selected globally from the candidate males in the
#' current generation, whereas female candidates are selected within field
#' (`campo`). The number of females retained in each field is determined by
#' `descarte`.
#'
#' The function supports selection based on EBV, phenotypes, mixed criteria
#' in which males and females use different criteria, and random selection.
#' Selection can be oriented toward increasing or decreasing the trait.
#'
#' @param archivo_feno Character. Path to the phenotype file used by BLUPF90.
#'
#' @param archivo_ped Character. Path to the pedigree file used by BLUPF90.
#'
#' @param base_feno Data frame or `data.table` containing the phenotype
#' information of all simulated animals. The columns
#' corresponding to `conteo` (phenotype), `campo` (field), `region`, `sexo` (sex),
#' and `id` are identified by name using `match()`, rather than by fixed column
#' positions.
#'
#' @param cuando Character.Period of records included in the BLUP evaluation.
#'
#' @param genea `data.table` containing individual information. Must include
#' columns `id`, `sexo` (sex), `campo` (field), and `año` (year). Sex is 
#' used to define male and female candidates.
#'
#' @param param_repro List containing reproductive parameters. Must include
#'  `npadres`, the number of sires retained.
#'
#' @param anio Integer. Year under evaluation.
#'
#' @param iter Integer. Iteration number, used to identify files.
#'
#' @param descarte Numeric vector giving the number of females retained
#'   within each field and year.
#'
#' @param crit_sel Character. Selection criterion. Available options are:
#'   \describe{
#'     \item{"ebv"}{Selection of both sexes based on estimated breeding
#'     values (EBV).}
#'     \item{"pheno"}{Selection of both sexes based on the observed
#'     phenotype.}
#'     \item{"phenoH_sampleM"}{Females are selected based on phenotype,
#'     whereas males are selected randomly.}
#'     \item{"phenoH_ebvM"}{Females are selected based on phenotype,
#'     whereas males are selected based on EBV.}
#'     \item{"random"}{Both males and females are selected randomly.
#'     Males are sampled from the global male pool, whereas females are
#'     sampled independently within field.}
#'   }
#'
#' @param direccion_sel Character. Desired direction of selection:
#'   \describe{
#'     \item{"pos"}{Select animals with the largest EBV or phenotypic values.}
#'     \item{"neg"}{Select animals with the smallest EBV or phenotypic values.}
#'   }
#'
#' @param file_fenotipo `data.table` containing phenotypic records used for
#'   phenotypic selection. It must contain columns `id`, `campo`, and`conteo`.
#'
#' @param options Character vector containing options passed to BLUPF90.
#'
#' @param covariance Covariance specification. 
#'
#' @param residual_variance Residual variance.
#'
#' @details
#' Before selection, the function performs a BLUP evaluation using
#' `pablup2()`. The response trait and positions of `campo`, `region`, `sexo`,
#'  and `id` are identified automatically by their column names. This avoids 
#'  depending on fixed column positions in `base_feno`. 
#'
#' The BLUP model includes `campo` (field), `region`, `sexo` (sex), and `id` as
#' cross-classified effects.
#'
#' For phenotypic selection, the observed value is transformed as:
#'
#' \deqn{\lambda = \exp(conteo)}
#'
#' The transformed phenotype is stored as `carga` and used to rank candidate.
#'
#' For EBV selection, animals are ranked according to the EBV returned by
#' `pablup2()`. The ranking direction is controlled by `direccion_sel`.
#'
#' Male candidates are selected globally from all males in the current
#' generation. Female candidates are selected independently within each
#' `campo`.
#'
#' Under `crit_sel = "phenoH_sampleM"`, males are sampled randomly from the
#' global male candidate pool, while females are selected according to their
#' phenotype within field.
#'
#' Under `crit_sel = "phenoH_ebvM"`, males are selected using EBV, while
#' females are selected using their phenotype within field.
#'
#' Under `crit_sel = "random"`, males are sampled randomly from the global
#' male candidate pool and females are sampled randomly within each field.
#'
#' The function performs selection only; the subsequent mating and
#' reproduction steps are handled separately.
#'
#' @return A list with three elements:
#'
#' \describe{
#'   \item{todo}{
#'   Complete table returned by `pablup2()` containing the BLUP results. }
#'   \item{top_idmacho}{
#'   IDs of the selected males. }
#'   \item{top_idhembra}{
#'   `data.table` containing the IDs and fields of the selected females. }
#' }

#'
#' @author SJWatson
#'
#' @examples
#' \dontrun{
#' sel <- blup_seleccion(
#'   archivo_feno = "phenotypes.txt",
#'   archivo_ped = "pedigree.txt",
#'   base_feno = base_feno,
#'   cuando = cuando,
#'   genea = genea,
#'   param_repro = list(npadres = 20),
#'   anio = 10,
#'   iter = 1000,
#'   descarte = c(30, 30, 30),
#'   crit_sel = "ebv",
#'   direccion_sel = "neg",
#'   options = options,
#'   covariance = covariance,
#'   residual_variance = residual_variance
#' )
#'
#' sel$top_idmacho
#' sel$top_idhembra
#' }
#'
#' @export


blup_seleccion <- function (archivo_feno,
                            archivo_ped,
                            base_feno,
                            cuando,
                            genea,
                            param_repro,
                            anio,
                            iter,
                            descarte,
                            crit_sel = c("pheno", "ebv", "phenoH_sampleM",
                                         "phenoH_ebvM", "random"),
                            direccion_sel = "neg",
                            file_fenotipo = NULL,
                            options,
                            covariance,
                            residual_variance) {
  
  id_p <- genea[ año == anio & sexo == "M", id ]
  id_m <- genea[ año == anio & sexo == "F", .(id, campo)]

# Factor used to transform the selection direction.
#
# pos: order(-conteo) -> valores grandes primero
# neg: order( conteo) -> valores pequeños primero  
  
  factor_ebv <- switch(direccion_sel, "pos" = -1, "neg" = 1)
  file_fenotipo[ , carga := exp(conteo)]
  
  descarte <- unlist(descarte)# + 1
  año_actual <- paste0(anio, "_")

  res <- pablup2(donde = file.path("H:\\Mi unidad\\garra\\exp sel\\"),
                 cuando = cuando,
                 traits = match("conteo", names(base_feno)), #c( 4), 
                 efectos = list(c(match("campo", names(base_feno)), "cross alpha"),   # campo
                                c(match("region", names(base_feno)), "cross alpha"),  # región
    #                            c(match("sexo", names(base_feno)), "cross alpha"),  # sexo
                                c(match("año", names(base_feno)), "cross alpha"),   # año
                                c(match("id", names(base_feno)), "cross alpha") ), # id
                 file_ped = archivo_ped,
                 file_feno = archivo_feno, 
                 base_feno,
                 var_residual = residual_variance,
                 covariance = covariance,
                 iter = iter, año = anio,
                 options = options)

  if( crit_sel == "ebv") { # selección ebv, en ambos sexos. 
    
    tp_idp_macho <- res[ año == año_actual & id %in% id_p
    ][order( ebv * factor_ebv ), id
    ][1: param_repro$npadres]
    
    top_idm_hembra <- res[año == año_actual & id %in% id_m$id
    ][order(campo, ebv * factor_ebv )
    ][  , .SD[seq_len(min(.N, descarte[.BY$campo]))],
        by = campo ][ , .(id, campo) ]
    
  } else if  ( crit_sel == "phenoH_sampleM") { # selección fenotipo hembras, random males.
    
   # tp_idp_macho <- file_fenotipo [ id %in% id_p ][  sample(.N, size = min (.N, param_repro$npadres)), id] #[ order(carga * factor_ebv ), id] [1: param_repro$npadres]
    tp_idp_macho <- sample( id_p, 
                            size = min(length(id_p), param_repro$npadres) )
    
    top_idm_hembra <- file_fenotipo [ id %in% id_m$id
    ][order(campo, carga * factor_ebv ), .(id, campo)
    ][ , .SD[seq_len(min(.N, descarte[.BY$campo]))],
       by = campo ] [ , .(id, campo) ]
    
  } else if ( crit_sel == "pheno") { # selección fenotipos en ambos sexos.
    
    tp_idp_macho <- file_fenotipo [ id %in% id_p ][
      order( carga * factor_ebv ), id
    ][1: param_repro$npadres]
    
    
    top_idm_hembra <- file_fenotipo [ id %in% id_m$id
    ][order(campo, carga * factor_ebv ), .(id, campo)
    ][ , .SD[seq_len(min(.N, descarte[.BY$campo]))],
       by = campo ]
    
  } else if ( crit_sel == "phenoH_ebvM" ) { # phenotype females, ebv males
    
    tp_idp_macho <- res[ año == año_actual & id %in% id_p
    ][order( ebv * factor_ebv ), id
    ][1: param_repro$npadres]
    
    top_idm_hembra <- file_fenotipo [ id %in% id_m$id
    ][order(campo, carga * factor_ebv ), .(id, campo)
    ][ , .SD[seq_len(min(.N, descarte[.BY$campo]))],
       by = campo ]
    
   } else if (crit_sel == "random") { # random both sexes
    
     tp_idp_macho <- sample( id_p, 
                             size = min(length(id_p), param_repro$npadres) )
     #tp_idp_macho <- file_fenotipo [ id %in% id_p ][  sample(.N, size = min (.N, param_repro$npadres)), id]
     
     top_idm_hembra <- id_m[ , 
                             .SD[sample(.N, min(.N, descarte[.BY$campo]))],
                             by = campo ]
   
  }
  
  
  return(list(
    todo = res,#asd,
    top_idmacho = tp_idp_macho,
    top_idhembra = top_idm_hembra
  ))
  
  
}
