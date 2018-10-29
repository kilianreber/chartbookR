####### DOWNLOAD BBG DATA BASED ON INPUTS ########
##################################################

#' Bloomberg data as zoo object
#' 
#' Returns Bloomberg data as zoo object; it relies on Rblpapi package but simplifies download and output
#' 
#' @import zoo
#' @import Rblpapi
#' @import lubridate
#' 
#' @param tickers character vector of Bloomberg ticker(s)
#' @param field optional character vector of Bloomberg field(s); default is 'PX_LAST'
#' @param names optional character vector of column name(s) for zoo object; default uses 'tickers' vector
#' @param start optional start date for data download; default is Sys.Date() - 3*365
#' @param end optional end date for data download; default is Sys.Date() -1
#' @param freq optional frequency for data download; options are 'DAILY', 'WEEKLY', 'MONTHLY', 'QUARTERLY', 'YEARLY'; default is 'MONTHLY'
#' @param time optional string to specify start date; options are 'D' (Days), 'W' (Weeks), 'M' (Months), 'Q' (Quarters), 'Y' (Years), or 'YTD' (Year-to-Date), e.g. '3M', '4Q', '5Y', 'YTD'; default is none
#' @param na optional boolean to replace NAs with the last observation if set to FALSE; default is TRUE
#' 
#' @return returns a zoo object with the downloaded Bloomberg data
#' 
#' @export
#' 
#' @examples
#' zoo <- getBBG(tickers='VIX Index', time='YTD')
#' zoo <- getBBG(tickers='VIX Index', names='VIX', freq='D', time='30Y', na=FALSE)
#' zoo <- getBBG(tickers=c('CPI YOY Index', 'PPI YOY Index'), names=c('CPI', 'PPI'), start='2000-01-01')
#' zoo <- getBBG(tickers=c('CPI YOY Index', 'PPI YOY Index'), names=c('CPI', 'PPI'), start='2000-01-01', end='2018-01-01')
#' zoo <- getBBG(tickers=c('NAPMPMI', 'MPMIEZMA', 'MPMIEMMA'), names=c('United States (ISM)', 'Eurozone', 'Emerging Markets'), time='3Y')

getBBG <- function(tickers, field, names, start, end, time, freq, na){

  #Turn off warnings
  options(warn=-1)

  #Open Bloomberg connection
  blpConnect()

  #Calculate last day of previous year
  last_day_prev_year <- function(x) floor_date(x, "year") - days(1)
  start_ytd <- last_day_prev_year(Sys.Date())

  #Set defaults
  if(missing(names))         {names          <- tickers                   }
  if(length(tickers)==1)     {one_tickr_fix  <- TRUE} else {one_tickr_fix <- FALSE}
  if(length(tickers)==1)     {tickers        <- rep(tickers, 2 )
                              names          <- rep(names, 2)             }
  if(missing(freq))          {freq           <- "MONTHLY"                 }
  if(missing(field))         {field          <- "PX_LAST"                 }
  if(missing(time))          {time           <- "none"                    }
                              tf             <- "none"
  if(time!="none")           {tf             <- gsub('[[:digit:]]+', '', time)}
  if(missing(start))         {start <- (LastDayInMonth(Sys.Date())-3*365)}
  if(missing(end))           {end            <- as.Date(Sys.Date()-1)     }
  if(time=="ytd") {time <- "YTD"}
  if(time=="YTD")            {start          <- start_ytd                 }
  if(time!="none" & time!="YTD" & tf=="Y") {start <- as.Date(Sys.Date()-(365*as.numeric((gsub("Y", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="Q") {start <- as.Date(Sys.Date()-(30*3*as.numeric((gsub("Q", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="M") {start <- as.Date(Sys.Date()-(30*as.numeric((gsub("M", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="W") {start <- as.Date(Sys.Date()-(7*as.numeric((gsub("W", "", time)))))}                          
  if(time!="none" & time!="YTD" & tf=="D") {start <- as.Date(Sys.Date()-(as.numeric((gsub("D", "", time)))))}
  if(missing(na))            {na             <- TRUE                      }

  start <- as.Date(start)
  end   <- as.Date(end)
                              
  # Complete tickers list
  for (i in 1:length(tickers))
  {if (length(unlist(strsplit(tickers[i], " "))) == 1) {tickers[i] <- paste(tickers[i], " Index")} else {next}}

  #Create Tickers vs. Names Database
  db <- as.data.frame(cbind(gsub(" |%" ,".",tickers), names))
  colnames(db)[1] <- "tickers"
  db$tickers <- paste(db$tickers, field, sep=".")

  #Configure Bloomberg download
  option.fields <- c("periodicitySelection",
                     "nonTradingDayFillOption",
                     "nonTradingDayFillMethod",
                     "periodicityAdjustment",
                     "adjustmentFollowDPDF",
                     "pricingOption",
                     "currency")

  option.values <- c( freq,
                      "NON_TRADING_WEEKDAYS",
                      "NIL_VALUE",
                      "CALENDAR",
                      "TRUE",
                      "PRICING_OPTION_PRICE",
                      "USD")

  bbg.options <- structure(option.values, names = option.fields)

  #Download Bloomberg time series data
  bbg <- bdh(securities = tickers,
             fields = field,
             start.date = start,
             end.date = end,
             options = bbg.options,
             overrides = NULL)

  #Make all lists the same length
  m = 0
  for(i in 1:length(tickers)){
    l = lengths(bbg[[i]][1])
    if (l>m){
      m=l
      lc = i
    }
  }

  for(i in 1:length(tickers)){
    if(i==lc){
      next
    }
    else {
      #bbg[[i]][m,2] <- NA
      bbg[[i]][m,1] <- bbg[[lc]][m,1]
    }
  }


  #Transform to data frame
  bbg_trans <- as.data.frame(bbg)

  #Delete excess dates
  cols <- ncol(bbg_trans)
  keep <- c(1,seq(2, cols, by=2))
  bbg_trans <- bbg_trans[keep]

  #Clean and replace column names
  colnames(bbg_trans) <- db$names[match(colnames(bbg_trans), db$tickers)]
  colnames(bbg_trans)[1] <- "Dates"

  #Output dataframe
  bbg_trans <- read.zoo(bbg_trans, format = "%Y-%m-%d")

  #Replace intermittent NAs (optional)
  if (na==FALSE){
  min <- sapply(bbg_trans, function(col) min(which(!is.na(col))))
  max <- sapply(bbg_trans, function(col) max(which(!is.na(col))))
  for(i in 1:ncol(bbg_trans)) {bbg_trans[min[i]:max[i],i] <- na.locf(bbg_trans[min[i]:max[i],i])}
  }

  #Replace preceding NAs (default)
  if (na==FALSE) {bbg_trans <- na.locf(bbg_trans)}
  
  #Remove NAs in front & end
  bbg_trans <- na.trim(bbg_trans, sides="both", is.na="all")
  
  #Apply one_ticker_fix if necessary
  if (one_tickr_fix==TRUE) {bbg_trans <- bbg_trans[,1]}
  
  #Return results
  return(bbg_trans)

}
