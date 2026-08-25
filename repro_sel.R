
#' Simulate Progeny by Random Crosses
#'
#' Generates progeny from a specified group of females and males using
#' random crosses in AlphaSimR. Phenotypes are simulated according to the
#' specified heritability.
#'
#' In addition to the AlphaSimR progeny population, the function generates
#' a `data.table` containing individual identification, sex, simulated
#'  breeding value and phenotype, field, region, and year.
#'
#' @param fems AlphaSimR population containing the females used as dams.
#' @param mals AlphaSimR population containing the males used as sires.
#' @param nCross Integer. Number of crosses to generate between the supplied
#'   females and males.
#' @param nProgeny Integer. Number of progeny generated per cross.
#' @param field Integer or character identifying the field (`campo`) where
#'  the progeny is born.
#' @param h2 Numeric. Heritability used by `setPheno()` to simulate the
#'   phenotypic values of the progeny.
#' @param simParamP AlphaSimR `SimParam` object containing the simulation
#'   parameters used for crossing and phenotypic simulation.
#'
#' @details
#'
#' @return A list with two elements:
#'
#' \describe{
#'   \item{genealogy}{
#'   A `data.table` containing the simulated genealogy and individual
#'   information, including `id`, `sexo`, `bv`, `feno_alphasim`, `campo`,
#'    `region`, and `año`. }
#'   \item{progeny}{
#'   An AlphaSimR population object containing the simulated progeny.
#'   }
#' }
#'
#' @seealso
#' [AlphaSimR::randCross2()], [AlphaSimR::setPheno()],
#' [AlphaSimR::bv()]
#'
#' @author
#' SJWatson
#'
#' @examples
#' \dontrun{
#' resultado <- repro_sel( fems = madres, mals = padres, nCross = 100, nProgeny = 1,
#'   field = 1, h2 = 0.30, simParamP = SP )
#'
#' resultado$progeny
#' resultado$genealogy
#' }
#'
#' @export


  repro_sel <- function(fems, mals, nCross, nProgeny = 1,
                        field, year, h2, reg, simParamP ) {
  
    crias <- randCross2( females = fems,
                         males = mals,
                         nCrosses = nCross,
                         nProgeny = 1,
                         simParam = SP
    )
    
    crias <- setPheno( crias, h2 = h2, simParam = SP )
    ncrias <- nInd( crias )
    crias@misc$año <- rep( year, ncrias )
    crias@misc$campo <- rep( field, ncrias )
    crias@sex <- sample( c("F", "M"), nInd( crias ), replace = T )
    
    gen <- data.table( id = crias@id,
                       sexo = crias@sex,
                       bv = as.numeric(bv( crias )),
                       feno_alphasim = as.numeric( crias@pheno ),
                       campo = field,
                       region = reg[ field ],
                       año = year
    )
    
    return( list( genealogy = gen,
                  progeny = crias ))
  }
  