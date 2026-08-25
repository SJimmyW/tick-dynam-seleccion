#' Selecciona y etiqueta reproductores machos según año y reglas de inicialización
#'
#' `viejos` selecciona un conjunto de individuos que actuarán como padres
#' según el `año` de la simulación y la regla aplicada:
#' - Si `año == 0`: inicializa seleccionando `nIndv` individuos desde `pob` y
#'   asigna el vector `@misc$año` con valores negativos por cohorte etaria
#'   usando la proporción `ppadres`.
#' - Si `año >= ingreso`: lee un archivo de toros preguardado en disco y
#'   selecciona una fracción de ellos como padres.
#' - En otros casos devuelve el objeto global `padres` (asumido disponible).
#'
#' @details
#' Cuando `año == 0`, la función toma individuos de población histórica
#' y  reparte los `nIndv` entre `maximo` grupos  etarios según las 
#' proporciones almacenadas en la variable global `ppadres`.
#' El primer grupo absorbe el restante para ajustar redondeos. Los valores
#' asignados a `@misc$año` son negativos (`-1, -2, ...`) para indicar cohortes
#' fundacionales. Cuando `año >= ingreso` la función busca un RDS y selecciona
#' padres entre los candidatos nacidos @ingreso años previos.
#'
#' @param pob Objeto de población (por ejemplo, objeto de AlphaSimR) desde el
#'   cual se seleccionarán individuos en la inicialización (`año == 0`).
#' @param nIndv Integer. Número de individuos a seleccionar como padres.
#' @param criterio Character. Criterio usado por `selectInd` para la selección
#'   (por defecto `"rand"`). Depende de las opciones soportadas por la función
#'   `selectInd`.
#' @param sexo Character o factor. Especifica el sexo de los individuos a
#'   seleccionar (p. ej. `"M"`/`"F"` u otra convención aceptada por `selectInd`).
#' @param maximo Integer. Número máximo de años que permanecerá una padre 
#' @param ncampos Integer. (No usado explícitamente en la función actual, se
#'   mantiene por compatibilidad/uso futuro).
#' @param año Numeric. Año (o paso temporal) actual de la simulación.
#' @param ingreso Numeric. Edad, en meses, que ingresa el reproductor al sisrema.

#'
#' @return Un objeto (generalmente del mismo tipo que `pob` o el objeto leído)
#'   con los individuos seleccionados para ser padres. En la rama `año == 0`
#'   se modifica `@misc$año` para etiquetar cohortes.
#'
#' @examples
#' \dontrun{
#' # Requiere que existan selectInd y las variables globales usadas:
#' # ppadres: vector de proporciones (longitud = maximo)
#' # temp_dir: directorio donde se guardan/leen los archivos toros*.rds
#' # padres: objeto disponible en el entorno para el caso else
#'
#' temp_dir <- tempdir()
#'
#' @note
#' - La función utiliza variables globales fuera de su firma: `ppadres`,
#'   `temp_dir` y `padres`. Estas deben estar definidas en el entorno donde se
#'   ejecute la función. Considera pasar estas variables como argumentos para
#'   eliminar dependencias implícitas y facilitar el empaquetado.
#' - La función realiza lectura de disco mediante `readRDS()` cuando
#'   `año >= ingreso`. Si el fichero esperado no existe la llamada fallará.
#' - `nIndv * 0.85` se usa en la rama `año >= ingreso`; valida que el resultado
#'   sea un entero apropiado para `selectInd`.
#'
#' @seealso \code{\link[AlphaSimR]{selectInd}} para la selección de individuos.
#' @author Tu Nombre <tu.email@dominio.com>
#' @export
#' @importFrom AlphaSimR selectInd

viejos <- function(pob, nIndv, criterio = "rand", sexo, maximo,
                   ncampos, año, ingreso , ppadres) {
##'  si año <= 0, toma machos de pobación histórica
  if (año == 0) {
    papin <- selectInd(pob, nInd = nIndv , use = criterio,
                       sex = sexo)
    
    papin@misc$año <- unlist(lapply(1:maximo, function(papis) {
      n <- floor(nIndv * ppadres[papis])                # Cantidad por grupo etario
      if (papis == 1) {
        faltan <- nIndv - sum(floor(nIndv * ppadres))  # Ajustar individuos restantes
        n <- n + faltan
      }
      rep(-papis, n)
      
    }))
    
    cat(" inicializa machos ", año, "\n")
  } else if (año >= ingreso) {
    
    bliain <- año + 1 - ingreso
    cualenque <- paste0(temp_dir, "/toros", bliain, ".rds")
    toros <- readRDS(cualenque)
    papin <- selectInd(toros,
                       nInd = nIndv * 0.85,
                       use = criterio, sex = sexo
    )
    # papin <- selectInd(pob2[[cuales]], nInd = nIndv*0.85, use = criterio, sex = sexo)
    cat(" machos comunes año ", año, "\n")
  } else {
    papin = padres
  }
  
  return(papin)
}
