
    crear_listas_replica <- function(n_replicas,
                                     prefijo ){
      
      crear_lista <- function(n, prefijo = NULL) {
        
        x <- vector("list", n)
        
        if (!is.null(prefijo))
          names(x) <- paste0(prefijo, seq_len(n))
        
        x
      }
      
      list( listmedia       = crear_lista(n_replicas, prefijo),
            listaases       = crear_lista(n_replicas, prefijo),
            listagenea      = crear_lista(n_replicas, prefijo),
            listaebv        = crear_lista(n_replicas, prefijo),
            listafenotipos  = crear_lista(n_replicas, prefijo),
            lista_conteo_directo  = crear_lista(n_replicas, prefijo),
            listaconteoinicial    = crear_lista(n_replicas, prefijo) 
            )
    }
    
   
    
    
    