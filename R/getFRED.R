####### DOWNLOAD FRED DATA BASED ON INPUTS #######
##################################################

#' FRED data as zoo object
#' 
#' Returns FRED data as zoo object; it relies on the quantmod::getSymbols function but simplifies download and output
#' 
#' @import zoo
#' @import lubridate
#' @import quantmod
#' @import bsts
#' 
#' @param tickers character vector of FRED ticker(s)
#' @param names optional character vector of column name(s) for zoo object; default uses 'tickers' vector
#' @param start optional start date for data download; format is 'dd/mm/yyyy'; default is Sys.Date() - 3*365
#' @param end optional end date for data download; format is 'dd/mm/yyyy'; default is Sys.Date() -1
#' @param time optional string to specify start date; options are 'D' (Days), 'W' (Weeks), 'M' (Months), 'Q' (Quarters), 'Y' (Years), or 'YTD' (Year-to-Date), e.g. '3M', '4Q', '5Y', 'YTD'; default is none
#' @param na optional boolean to replace NAs with the last observation if set to FALSE; default is TRUE
#' 
#' @return returns a zoo object with the downloaded FRED data
#' 
#' @export
#' 
#' @examples
#' zoo <- getFRED(tickers=c('UNRATE', 'NROU'), names=c('US Unemployment', 'Natural Unemployment'))
#' zoo <- getFRED(tickers=c('UNRATE', 'NROU'), names=c('US Unemployment', 'Natural Unemployment'), time='15Y', na=FALSE)
#' zoo <- getFRED(tickers=c("T10YIE", "T5YIE"), names=c("10Y Breakeven", "5Y Breakeven"), start='01/01/2015')
#' zoo <- getFRED(tickers=c("CPIAUCSL", "PCEPILFE"), names=c("CPI", "Core PCE"), start='01/01/2000', end='01/01/2018')

getFRED <- function(tickers, names, start, end, time, na){

  #Turn off warnings
  options(warn=-1)
  suppressPackageStartupMessages(library(quantmod))
  
  #Calculate last day of previous year
  last_day_prev_year <- function(x) floor_date(x, "year") - days(1)
  start_ytd <- last_day_prev_year(Sys.Date())
  
  #Set defaults
  if(missing(names))         {names          <- tickers        }
  if(length(tickers)==1)     {one_tickr_fix  <- TRUE} else {one_tickr_fix <- FALSE}
  if(length(tickers)==1)     {tickers        <- rep(tickers, 2 )
                              names          <- rep(names, 2)  }
  if(missing(time))          {time           <- "none"         }
                              tf             <- "none"
  if(time!="none")           {tf             <- gsub('[[:digit:]]+', '', time)}
  if(missing(start))         {start          <- "01/01/1666"   }
  if(missing(end))           {end            <- "01/01/1666"   }
  if(time=="ytd") {time <- "YTD"}
  if(time=="YTD")            {start          <- start_ytd      }
  if(time!="none" & time!="YTD" & tf=="Y") {start <- as.Date(Sys.Date()-(365*as.numeric((gsub("Y", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="Q") {start <- as.Date(Sys.Date()-(30*3*as.numeric((gsub("Q", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="M") {start <- as.Date(Sys.Date()-(30*as.numeric((gsub("M", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="W") {start <- as.Date(Sys.Date()-(7*as.numeric((gsub("W", "", time)))))}                                                        
  if(time!="none" & time!="YTD" & tf=="D") {start <- as.Date(Sys.Date()-(as.numeric((gsub("D", "", time)))))}
  if(missing(na))            {na             <- TRUE           }
  
  #Prepare data
  if (start!="01/01/1666") {start <- as.Date(start, "%d/%m/%Y")}
  if (end!="01/01/1666")   {end   <- as.Date(end, "%d/%m/%Y")  }
  
  #Download FRED data and merge
  fred <- lapply(tickers, function(sym) {na.omit(getSymbols(sym, src="FRED", auto.assign=FALSE, return.class = "zoo"))})
  fred <- Reduce(merge, fred)
  n <- ncol(fred)

  #Replace NAs with previous values in columns 2:n, subset by values in first column
  fred[,2:n] <- na.locf(fred)[,2:n]
  fred <-subset(fred, (!is.na(fred[,1])))
  df <- fred

  #Adjust column headers
  colnames(df) <- names

  #Subset data according to start and end dates provided
  if (start!="01/01/1666") {df <- subset(df, index(df)>=start)}
  if (end!="01/01/1666")   {df <- subset(df, index(df)<=end)  }
  
  #Replace intermittent NAs (optional)
  if (na==FALSE){
    min <- sapply(df, function(col) min(which(!is.na(col))))
    max <- sapply(df, function(col) max(which(!is.na(col))))
    for(i in 1:ncol(df)) {df[min[i]:max[i],i] <- na.locf(df[min[i]:max[i],i])}
  }
  
  #Replace preceding NAs (default)
  if (na==FALSE) {df <- na.locf(df)}
  
  #Remove NAs in front & end
  df <- na.trim(df, sides="both", is.na="all")
  
  #Apply one_ticker_fix if necessary
  if (one_tickr_fix==TRUE) {df <- df[,1]}
  
  #Return results
  return(df)

}
