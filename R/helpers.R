
buildFormula <- function(y,x){
  #returns the formula using y and vector of xs
  x<-x[x!=y]
  x.str <- paste(x,collapse="+")
  final.str <- paste(c(y,x.str),collapse="~")
  final <- as.formula(final.str)

  return(final)
}


pMiss <- function(x){sum(is.na(x))} # stworzenie funkcji


toDummies <- function(df,oldVar){
  tmp <- rep(0,nrow(df))
  output<-as.data.frame(tmp)

  for(lvl in levels(df[,oldVar])){
    newVar <- paste(c(oldVar,lvl),collapse = "_")
    output[,newVar] <- "no"
    output[df[,oldVar]==lvl,newVar] <- "yes"
    output[,newVar] <- as.factor(output[,newVar])
  }
  output$tmp <- NULL
  return(output)
}

toDummiesForTwo <- function(df,oldVar,oldVar2){
  tmp <- rep(0,nrow(df))
  output<-as.data.frame(tmp)

  for(lvl in levels(df[,oldVar])){
    newVar <- paste(c(oldVar,lvl),collapse = "_")
    output[,newVar] <- "no"
    output[!is.na(df[,oldVar]) & (df[,oldVar]==lvl | df[,oldVar2]==lvl),newVar] <- "yes"
    output[,newVar] <- as.factor(output[,newVar])
  }
  output$tmp <- NULL
  return(output)
}

square <- function(x){
  x^2
}
cube <- function(x){
  x^3
}
