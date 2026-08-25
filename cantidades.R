cantidades <- function(nIndv, prop) {
  n <- floor(nIndv * prop) # vector de cantidad por grupo etario
  subtotal <- sum(n)
  faltan <- nIndv - subtotal
  
  n[1] <- n[1] + faltan
  
  return(n)
}