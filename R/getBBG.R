####### DOWNLOAD BBG DATA BASED ON INPUTS ########
##################################################

#' Bloomberg data as zoo object
#' 
#' Returns Bloomberg data as zoo object; it relies on Rblpapi package but simplifies download and output
#' 
#' @import zoo
#' @import Rblpapi
#' @import lubridate
#' @import xts
#' 
#' @param tickers character vector of Bloomberg ticker(s)
#' @param field optional character vector of Bloomberg field(s)
#' @param names optional character vector of column name(s) for zoo object; default uses 'tickers' vector
#' @param start optional start date for data download; format is 'dd/mm/yyyy'
#' @param end optional end date for data download; format is 'dd/mm/yyyy'
#' @param freq optional frequency for data download; options are 'DAILY', 'WEEKLY', 'MONTHLY', 'QUARTERLY', 'YEARLY'; default is 'MONTHLY'
#' @param time optional string to specify start date; options are 'D' (Days), 'W' (Weeks), 'M' (Months), 'Q' (Quarters), 'Y' (Years), or 'YTD' (Year-to-Date), e.g. '3M', '4Q', '5Y', 'YTD'
#' @param na optional boolean to replace NAs with the last observation if set to FALSE
#' @param last optional boolean to toggle appending of the latest data points
#' @param fut_dt optional boolean to toggle deletion of data points that lie in the future
#' 
#' @return returns a zoo object with the downloaded Bloomberg data
#' 
#' @export
#' 
#' @examples
#' zoo <- getBBG(tickers = 'VIX Index', time = 'YTD')
#' zoo <- getBBG(tickers = 'VIX Index', names = 'VIX', freq = 'DAILY', time = '30Y', na = FALSE)
#' zoo <- getBBG(tickers = c('CPI YOY Index', 'PPI YOY Index'), names = c('CPI', 'PPI'), start = '01/01/2000')
#' zoo <- getBBG(tickers = c('CPI YOY Index', 'PPI YOY Index'), names = c('CPI', 'PPI'), start = '01/01/2000', end = '01/01/2018')
#' zoo <- getBBG(tickers = c('NAPMPMI', 'MPMIEZMA', 'MPMIEMMA'), names = c('United States (ISM)', 'Eurozone', 'Emerging Markets'), time = '3Y')

getBBG <- function(tickers, field = "PX_LAST", names, start = (LastDayInMonth(Sys.Date())-3*365), end = as.Date(Sys.Date()-1), time, freq = "MONTHLY", na = TRUE, last = FALSE, fut_dt = FALSE){

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
  if(length(tickers)==1)     {tickers        <- c(tickers, "CO1 Comdty"   )
                              names          <- c(names, "Oil")           }
  if(missing(time))          {time           <- "none"                    }
                              tf             <- "none"
  if(time!="none")           {tf             <- gsub('[[:digit:]]+', '', time)}
  if(time=="ytd") {time <- "YTD"}
  if(time=="YTD")            {start          <- start_ytd                 }
  if(time!="none" & time!="YTD" & tf=="Y") {start <- as.Date(Sys.Date()-(365*as.numeric((gsub("Y", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="Q") {start <- as.Date(Sys.Date()-(30*3*as.numeric((gsub("Q", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="M") {start <- as.Date(Sys.Date()-(30*as.numeric((gsub("M", "", time)))))}
  if(time!="none" & time!="YTD" & tf=="W") {start <- as.Date(Sys.Date()-(7*as.numeric((gsub("W", "", time)))))}                          
  if(time!="none" & time!="YTD" & tf=="D") {start <- as.Date(Sys.Date()-(as.numeric((gsub("D", "", time)))))}
  
  start <- as.Date(start, "%d/%m/%Y")
  end   <- as.Date(end, "%d/%m/%Y")
                              
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
                     "pricingOption")

  option.values <- c( freq,
                      "NON_TRADING_WEEKDAYS",
                      "NIL_VALUE",
                      "CALENDAR",
                      "TRUE",
                      "PRICING_OPTION_PRICE")

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

  #Sort data according to names vector
  bbg_ordering <- c("Dates", names)
  bbg_trans <- bbg_trans[, bbg_ordering]
  
  #Output zoo object
  bbg_trans <- read.zoo(bbg_trans, format = "%Y-%m-%d")

  #Add last data points
  if (last == TRUE){

    #Download and arrange data
    bbg_last_dl  <- bdp(securities = tickers, fields = field, overrides = NULL)
    bbg_last_dt  <- bdp(securities = tickers, fields = "LAST_UPDATE_DT", overrides = NULL)
    bbg_last     <- cbind(bbg_last_dt, bbg_last_dl)
    bbg_last[,1] <- format(as.Date(bbg_last[,1]), "%Y-%m-%d")

    #Organize and clean data
    bbg_last$Variable <- row.names(bbg_last)
    rownames(bbg_last) <- c()
    colnames(bbg_last) <- c("Dates", "Level", "Variable")

    #Reshape to Wide Format
    bbg_last <- reshape(bbg_last, idvar = "Dates", timevar = "Variable", direction = "wide")
    
    #Create db2 & replace column names
    db$tickers2 <- paste0("Level.", tickers)
    colnames(bbg_last) <- db$names[match(colnames(bbg_last), db$tickers2)]
    colnames(bbg_last)[1] <- "Dates"
    
    #Convert to zoo object, merge, remove duplicates
    zoo_last  <- read.zoo(bbg_last, format = "%Y-%m-%d")
    comb      <- c(index(bbg_trans), index(zoo_last))
    kill      <- comb[duplicated(comb)]
    if (length(kill > 0)) {
    zoo_last  <- zoo_last[!(index(zoo_last) %in% kill),]
    }
    if (length(zoo_last > 0)) {
    bbg_trans <- rbind(bbg_trans, zoo_last)
    }
  }
  
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
  
  #Remove looking-ahead data
  if (fut_dt == FALSE){
  bbg_trans <- bbg_trans[!(index(bbg_trans) > Sys.Date()),]
  }
  
  #Apply one_ticker_fix if necessary
  if (one_tickr_fix==TRUE) {bbg_trans <- bbg_trans[,1]}
  
  #Return results
  return(bbg_trans)
}
