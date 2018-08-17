####### DOWNLOAD FRED DATA BASED ON INPUTS #######
##################################################

getFRED <- function(start, tickers, names){

  #Turn off warnings
  options(warn=-1)
  
  #Load packages
  library(quantmod)
  library(zoo)
  
  #Set missing values
  if (missing(start))      {start      <- "01/01/1666" }
  
  #Prepare data
  # Add flipTime
  if (start!="01/01/1666") {start <- as.Date(start, "%d/%m/%Y")}
  
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

  #Subset data according to start date provided
  if (start!="01/01/1666") {df <- subset(df, index(df)>=start)}
  
  #Return results
  return(df)

}
