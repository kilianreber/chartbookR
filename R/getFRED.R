####### DOWNLOAD FRED DATA BASED ON INPUTS #######
##################################################

getFRED <- function(tickers, names, time, start, end, na){

  #Turn off warnings
  options(warn=-1)
  
  #Load packages
  library(zoo)
  library(lubridate)
  
  ### TO DELETE IF NOT NEEDED!
  #library(quantmod)
  #library(bsts)
  
  #Calculate last day of previous year
  last_day_prev_year <- function(x) floor_date(x, "year") - days(1)
  start_ytd <- last_day_prev_year(Sys.Date())
  
  #Set defaults
  if(missing(start))       {start     <- "01/01/1666"   }
  if(missing(end))         {end       <- "01/01/1666"   }
  if(missing(time))        {time      <- "none"         }
  if(time=="ytd")          {start     <- start_ytd      }
  if(time!="none" & time!="ytd") {start <- as.Date(Sys.Date()-(365*as.numeric((gsub("Y", "", time)))))}
  if(missing(na))          {na <- TRUE                  }
  
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
  return(df)
  
  
  #Return results
  return(df)

}
