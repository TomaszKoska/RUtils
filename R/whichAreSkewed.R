whichAreSkewed <- function(df,maxSkewness=0.75,choosenVariables=NULL){
require(e1071)

  x<-sapply(df, class)

  if(is.null(choosenVariables)){
    choosenVariables<-names(x[x=="numeric"|x=="integer"])
  }

  #jeszcze sprawdzenie czy choosen są numericami
  notNumerics <- x[x!="numeric" & x !="integer"]
  choosenVariables<-setdiff(choosenVariables,notNumerics)

  #czy są w ogóle jakieś choosen?
  finalVec <- c()
  finalNames <- c()
  if(length(choosenVariables)>0){
    for(n in choosenVariables){
      finalNames <-append(finalNames,n)
      finalVec<-append(finalVec,skewness(df[,n]))
    }
  }
  output<-data.frame(name=finalNames,skewness=finalVec)
  output[abs(finalVec)>=maxSkewness,]

}
