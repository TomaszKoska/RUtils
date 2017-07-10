
buildFormula <- function(y,x){
  #returns the formula using y and vector of xs
  x<-x[x!=y]
  x.str <- paste(x,collapse="+")
  final.str <- paste(c(y,x.str),collapse="~")
  final <- as.formula(final.str)

  return(final)
}


pMiss <- function(x){sum(is.na(x))} # stworzenie funkcji
