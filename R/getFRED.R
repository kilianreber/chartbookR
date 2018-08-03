getFRED <- function(tickers, names){

  #Load packages
  library(readxl)
  library(quantmod)
  library(zoo)
  library(xts)

  ####### DOWNLOAD FRED DATA BASED ON INPUTS #######
  ##################################################

    #Download FRED data and merge
  fred <- lapply(tickers, function(sym) {na.omit(getSymbols(sym, src="FRED", auto.assign=FALSE, return.class = "zoo"))})
  fred <- Reduce(merge, fred)
  n <- ncol(fred)

  #Replace NAs with previous values in columns 2:n, subset by values in first column
  fred[,2:n] <- na.locf(fred)[,2:n]
  #http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
  fred <-subset(fred, (!is.na(fred[,1])))
  df <- fred

  #Adjust column headers
  colnames(df) <- names

  #Return results
  return(df)

}
