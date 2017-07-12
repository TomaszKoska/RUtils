
redRemoveSelected <- function(fo, namesToRemove = c()){
  require(caret)

  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }


 for(n in namesToRemove){
   if(n %in% names(fo$trainFull) && n != fo$yName){
     fo$train[,n] <- NULL
     fo$trainFull[,n] <- NULL
     fo$forecast[,n] <- NULL
   }
 }
  fo$description <- append(fo$description,"And after that we deleted some manually chosen variables!")

  fo
}
