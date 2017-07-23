outDropEdges  <- function(fo,lowerP = 0.25, upperP = 0.75){
  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }
  lower <- quantile(fo$train[,n],lowerP)
  upper <-quantile(fo$train[,n],upperP)

  fo$train <- fo$train[fo$train[,n]>=lower,]
  fo$train <- fo$train[fo$train[,n]<=upper,]


  fo
}
