
edad <- function(npartos = npartos, cantidad = n,
                 edadingreso ) {
  age <- unlist(lapply(1:npartos, function(mums) {
    num <- cantidad[mums]
    rep(-(mums + edadingreso), num)
  }))
  
  return(age)
}