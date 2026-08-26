
#' Process cattle tick counts from RDS files
#'
#' Reads RDS files containing daily cattle tick counts, converts the data
#' from wide to long format, and applies a transformation or summary
#' according to the selected model.
#'
#' Three processing options are available through the `modelo` argument:
#' \itemize{
#'   \item `"log"`: calculates the mean for each animal and applies the
#'    natural logarithm.
#'   \item `"repeated"`: retaining the repeated observations for each animal.
#'   \item `"score"`: converts the tick counts into a 0--5 tick-resistance
#'         score based on the BREEDPLAN classification.
#' }
#'
#' For `modelo = "score"`, the original BREEDPLAN thresholds are doubled
#' because the tick count represents the combined count from both sides
#' of the animal, whereas the original scoring system is defined per side.
#'
#' @param lista (list). List containing paths to the `.rds` files
#'   containing the tick-count tables.
#' @param cuando (when). Numeric vector specifying the days to retain for analysis.
#'   Values correspond to the row numbers representing days in the
#'   original tables. The original table records data every 30 days from the
#'   start of simulation.
#' @param modelo (model). Character string specifying the processing method.
#'   Must be one of `"log"`, `"repeated"`, or `"score"`. Defaults to `"log"`.
#'
#' @return A `data.table` whose structure depends on the selected `modelo`:
#'   \describe{
#'     \item{log}{
#'       Contains `id`, `campo` (field), `region`, and `valor`, where `valor`
#'       is the natural logarithm of the mean selected tick counts.
#'     }
#'     \item{repeated}{
#'       Contains `id`, `campo` (field), `region`, `dia` (day), and `valor`, 
#'       retaining each repeated observation.
#'     }
#'     \item{score}{
#'       Contains the original observations together with a `score`
#'       column ranging from 0 to 5.
#'     }
#'   }
#'
#' @details
#' Each RDS file is expected to contain a table with the following structure:
#' \itemize{
#'   \item The first column contains the field (`campo`) identifier.
#'   \item The second column contains the region (`region`) identifier.
#'   \item The third column contains auxiliary information and is not used.
#'   \item Columns four onward contain animal identifiers.
#'   \item Each row represents one day of tick-count recording. It has naños*365
#'   days rows (nyears*365).
#' }
#'
#' The tables are converted from wide to long format before processing.
#' Missing values are removed before applying any transformation.
#'
#' For `modelo = "score"`, the following classification is used:
#' \describe{
#'   \item{0}{Clean: no observable cattle ticks.}
#'   \item{1}{Very High Resistance: 20 or fewer ticks.}
#'   \item{2}{High Resistance: 21--60 ticks.}
#'   \item{3}{Average Resistance: 61--160 ticks.}
#'   \item{4}{Low Resistance: 161--300 ticks.}
#'   \item{5}{Very Low Resistance: more than 300 ticks.}
#' }
#'
#' These thresholds correspond to twice the original BREEDPLAN thresholds
#' (0, 10, 30, 80, and 150 ticks), because the count used here represents
#' the combined number of ticks recorded on both sides of the animal.
#'
#' @references
#' BREEDPLAN. Recording Tick Scores with Video.
#' \url{https://breedplan.une.edu.au/recording-performance/recording-tick-scores-with-video/}
#'
#' @examples
#' \dontrun{
#' files <- c(
#'   "campo_1.rds",
#'   "campo_2.rds",
#'   "campo_3.rds"
#' )
#'
#' # Mean log-transformed tick count
#' result_log <- log_txt2(
#'   lista = files,
#'   cuando = 1:4,
#'   modelo = "log"
#' )
#'
#' # Retain repeated observations
#' result_repeated <- log_txt2(
#'   lista = files,
#'   cuando = 1:4,
#'   modelo = "repeated"
#' )
#'
#' # Convert tick counts to resistance scores
#' result_score <- log_txt2(
#'   lista = files,
#'   cuando = 1:4,
#'   modelo = "score"
#' )
#' }
#'
#' @importFrom data.table data.table rbindlist
#' @export

  log_txt2 <- function(lista, cuando, modelo = "log") {
    
    tablas <- lapply(lista, readRDS)
    
    sublista <- rbindlist(lapply(tablas, function(j) {
      
      nid <- ncol(j)
      
      data.table( campo  = rep(j[1, 1], nrow(j) * (nid - 3)),
                  region = rep(j[1, 2], nrow(j) * (nid - 3)),
                  dia    = rep(1:nrow(j), each = nid - 3),
                  id     = rep(colnames(j)[4:nid], times = nrow(j)),
                  valor  = as.vector(as.matrix(j[, 4:nid]))
                  )
      }), fill = TRUE)
    
    sublista <- sublista[!is.na(valor)]
     
    loga <- sublista[dia %in% cuando]
  
    if (modelo == "log") {
      
      conteo_log <- loga[, .(valor = log(mean(valor, na.rm = TRUE))),
                         by = .(id, campo, region)]   
      
    } else if ( modelo == "repeated"  ) {
      
      conteo_log <- loga[, .(valor ),
                         by = .(id, campo, region)]
      
    } else if ( modelo == "score" ) { # breedplan (https://breedplan.une.edu.au/recording-performance/recording-tick-scores-with-video/)
                                     # because is for each side and the model is for animal, the transformation
                                     # considers the double of the original score 
      
      conteo_log <- loga[, score := fcase(
        valor == 0, 0,                   # Clean
        valor <= 20, 1,     # 10, 1,    #  Very High Resistance
        valor <= 60, 2,    # 30, 2,    #   High Resistance
        valor <= 160, 3,  #80, 3,     #    Average Resistance
        valor <= 300, 4, #150, 4,    #     Low Resistance
        valor > 300, 5, #150, 5     #      Very low Resistance
      )]
      
    }
   
  
     return(conteo_log)
    
    }

