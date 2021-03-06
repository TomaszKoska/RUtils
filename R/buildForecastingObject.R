
buildForecastingObject <- function(dfTrain, dfForecast, yName, initialDescription = c("So we took the data...")){
#first two arguments can be string paths or dataframes (which is recommended)

  if(class(dfTrain)=="character"){
    #treat dfTrain as a path
    dfTrain <- read.csv(dfTrain)
  }
  if(class(dfForecast)=="character"){
    #treat dfForecast as a path
    dfForecast <- read.csv(dfForecast)
  }

  if(!(yName %in% colnames(dfForecast))){
    dfForecast[,yName] <- rep(NA,nrow(dfForecast))
  }


  output <- list()
  output$train <- dfTrain
  output$forecast <- dfForecast
  output$trainFull <- dfTrain
  output$yName <- yName
  output$description <- initialDescription
  class(output) <- "ForecastingObject"
  output
}


