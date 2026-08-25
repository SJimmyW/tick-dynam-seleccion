  salidas_bv<- function(listmedia, listaconteoinicial,
                        listaebv, listafenotipos,#listagenea, listaases,
                        donde, prefijo = "seleccion",
                        leer_media_rds = TRUE,
                        guardar_csv = TRUE) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  library(data.table)
  
  dir.create(donde, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(donde, paste0(prefijo, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  
  anio_num <- function(x, i) {
    z <- suppressWarnings(as.integer(gsub("[^0-9-]", "", x)))
    ifelse(is.na(z), i - 1L, z)
  }
  
  a_dt <- function(obj, leer_rds = FALSE) {
    if (is.null(obj)) return(NULL)
    
    if (leer_rds && is.character(obj) && length(obj) == 1L) {
      if (!file.exists(obj)) return(data.table(ruta_origen = obj, error = "archivo no existe"))
      z <- tryCatch(readRDS(obj), error = function(e) e)
      if (inherits(z, "error")) {
        return(data.table(ruta_origen = obj, error = conditionMessage(z)))
      }
      dt <- as.data.table(z)
      dt[, ruta_origen := obj]
      return(dt)
    }
    
    if (is.list(obj) && !is.data.frame(obj) && !data.table::is.data.table(obj) && length(obj) == 1L) {
      obj <- obj[[1]]
    }
    
    as.data.table(obj)
  }
  
  apilar_anio_campo <- function(x, nombre, leer_rds = FALSE) {
    rbindlist(lapply(seq_along(x), function(rep) {
      xr <- x[[rep]]
      if (is.null(xr)) return(NULL)
      
      rep_name <- names(x)[rep]
      if (is.null(rep_name) || is.na(rep_name) || rep_name == "") rep_name <- paste0("replica_", rep)
      
      rbindlist(lapply(seq_along(xr), function(i) {
        anio_label <- names(xr)[i]
        if (is.null(anio_label) || is.na(anio_label) || anio_label == "") {
          anio_label <- paste0("anio_", i - 1L)
        }
        
        rbindlist(lapply(seq_along(xr[[i]]), function(campo_idx) {
          dt <- a_dt(xr[[i]][[campo_idx]], leer_rds = leer_rds)
          if (is.null(dt) || !nrow(dt)) return(NULL)
          
          if (nombre == "ases" && all(c("V1", "V2") %in% names(dt))) {
            setnames(dt, c("V1", "V2"), c("id", "a"))
          }
          
          dt[, `:=`(
            replica = rep_name,
            iter = rep,
            anio_label = anio_label,
            anio_num = anio_num(anio_label, i),
            campo_idx = campo_idx
          )]
          dt
        }), use.names = TRUE, fill = TRUE)
      }), use.names = TRUE, fill = TRUE)
    }), use.names = TRUE, fill = TRUE)
  }
  
  apilar_simple <- function(x) {
    rbindlist(lapply(seq_along(x), function(rep) {
      dt <- a_dt(x[[rep]])
      if (is.null(dt) || !nrow(dt)) return(NULL)
      rep_name <- names(x)[rep]
      if (is.null(rep_name) || is.na(rep_name) || rep_name == "") rep_name <- paste0("replica_", rep)
      dt[, `:=`(replica = rep_name, iter = rep)]
      dt
    }), use.names = TRUE, fill = TRUE)
  }
  
  tablas <- list(
    media_garrapatas = apilar_anio_campo(listmedia, "media_garrapatas", leer_media_rds),
    # ases = apilar_anio_campo(listaases, "ases"),
    conteoinicial = apilar_anio_campo(listaconteoinicial, "conteoinicial"),
    ebv = apilar_simple(listaebv),
    #genealogia = apilar_simple(listagenea),
    fenotipos = apilar_simple(listafenotipos)
  )
  
  saveRDS(list(
    listmedia = listmedia,
    #listaases = listaases,
    listaconteoinicial = listaconteoinicial,
    listaebv = listaebv,
    listafenotipos = listafenotipos
    #listagenea = listagenea
  ), file.path(out, "listas_master_originales.rds"), compress = "xz")
  
  saveRDS(tablas, file.path(out, "tablas_apiladas.rds"), compress = "xz")
  
  for (nm in names(tablas)) {
    if (!is.null(tablas[[nm]]) && nrow(tablas[[nm]]) > 0L) {
      saveRDS(tablas[[nm]], file.path(out, paste0(nm, ".rds")), compress = "xz")
      if (guardar_csv) fwrite(tablas[[nm]], file.path(out, paste0(nm, ".csv")))
    }
  }
  
  invisible(list(directorio = out, tablas = tablas))
}



  