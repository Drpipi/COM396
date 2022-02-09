library(TTR)
maxRows <- 3100
stoploss<- NA

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero

  if (store$iter > params$kdjLookback) {
    
    startIndex <-  store$iter - params$kdjLookback
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      
      KD <-last(stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD, nSlowD = params$nSlowD, warning = FALSE))
      
      Jline <- 3*KD[1]- 2*KD[2] 
      
      if (Jline < 0.2 && !is.na(Jline)) {
        pos[params$series[i]] <- -1
      }

      else if (Jline > 0.80 && !is.na(Jline)) {
        pos[params$series[i]] <- 1
      }else{
        pos[params$series[i]] <- 0
      }
      
    }
  }
  
  if (store$iter > params$macdLookback) {
    
    startIndex <-  store$iter - params$macdLookback
    
    for (i in 1:length(params$series)) {
      
      macd <- last(MACD(store$cl[startIndex:store$iter,i], percent=TRUE))
      

      if (macd[,"signal"] > macd[,"macd"]) {
        pos[params$series[i]] <- -1
      } 
    }
  }
  pos <- pos #check the position sizes
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}