fullPredictExp <- function(fo, caretModel,forecastOutputFile = "outputForecast.csv", fullOutputFile = "outputFull.csv", modelFile="model.RData"){
  require(caret)
  if(class(fo) != "ForecastingObject"){
    warning("This function should get ForecastingObject as parameter. Please use buildForecastingObject.")
    return(NULL)
  }

  predictionTrainFull <- exp(predict(object = caretModel, newdata = fo$trainFull))
  predictionForecast <- exp(predict(object = caretModel, newdata = fo$forecast))

  predictionAll <- append(predictionTrainFull,predictionForecast)
  names(predictionAll) <- fo$yName
  names(predictionForecast) <- fo$yName

  write.csv(predictionAll,fullOutputFile, row.names=FALSE)
  write.csv(predictionForecast,forecastOutputFile, row.names=FALSE)
  save(caretModel,file=modelFile)

}
