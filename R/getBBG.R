getBBG <- function(tickers, field, names, time, start, end, freq, na){

  #Turn off warnings
  options(warn=-1)
  
  #Load libraries
  library(Rblpapi)
  library(bsts)
  library(lubridate)

  #Open Bloomberg connection
  blpConnect()

  #Calculate last day of previous year
  last_day_prev_year <- function(x) floor_date(x, "year") - days(1)
  start_ytd <- last_day_prev_year(Sys.Date())

  #Set defaults
  if(missing(freq))  {freq <- "MONTHLY"}
  if(missing(field)) {field <- "PX_LAST"}
  if(missing(time))  {time <- "none"}
  if(missing(start)) {start <- (LastDayInMonth(Sys.Date())-3*365)}
  if(missing(end))   {end <- as.Date(Sys.Date()-1)}
  if(time=="ytd")    {start <- start_ytd}
  if(time!="none" & time!="ytd") {start <- as.Date(Sys.Date()-(365*as.numeric((gsub("Y", "", time)))))}
  if(missing(na))    {na <- TRUE}

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
      bbg[[i]][m,2] <- NA
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
  #colnames(bbg_trans) <- sub('\\..*', '', colnames(bbg_trans))
  colnames(bbg_trans) <- db$names[match(colnames(bbg_trans), db$tickers)]
  colnames(bbg_trans)[1] <- "Dates"

  #Sort output alphabetically by column name
  #bbg_trans <- bbg_trans[,order(colnames(bbg_trans))]

  #Output dataframe
  bbg_trans <- read.zoo(bbg_trans, format = "%Y-%m-%d")

  #Replace intermittent NAs (optional)
  if (na==FALSE){
  min <- sapply(bbg_trans, function(col) min(which(!is.na(col))))
  max <- sapply(bbg_trans, function(col) max(which(!is.na(col))))
  for(i in 1:ncol(bbg_trans)) {bbg_trans[min[i]:max[i],i] <- na.locf(bbg_trans[min[i]:max[i],i])}
  }

  if (na==FALSE) {bbg_trans <- na.locf(bbg_trans)} # TO GET RID OF NAs IN FRONT
  return(bbg_trans)

}
