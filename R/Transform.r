################# FCT TRANSFORM DATA #################
#######################################################

#' Transformation of zoo object
#' 
#' Adds new columns to zoo object with transformed data; applies for all columns in 'data'
#' 
#' @import zoo
#' @import xts
#' @import stringr
#' @import quantmod
#' @import scales
#' @import roll
#' 
#' @param data specification of zoo object
#' @param start optional start date to trim 'data'; format is 'dd/mm/yyyy'
#' @param end optional end date to trim 'data'; format is 'dd/mm/yyyy'
#' @param chg optional string to calculate periodical changes; options are 'YoY' (Year-on-Year), 'QoQ' (Quarter-on-Quarter), 'MoM' (Month-on-Month), 'WoW' (Week-on-Week), 'DoD' (Day-on-Day)
#' @param chg_type optional string to specify calculation of changes; options are 'perc' (percentage change) or 'delta' (difference)
#' @param pma optional integer to specify number of data points for calculation of period-moving-averages
#' @param pms optional integer to specify number of data points for calculation of period-moving-sums
#' @param lag optional integer to lag the data by a number of periods; use negative integer for lead
#' @param rebase optional boolean to rebase the data to 100 at first observation
#' @param Z optional integer (or character) to specify the rolling number of periods over which the data is normalized; use 'all' to compute z-scores over all available periods
#' 
#' @return returns a zoo object with original and transformed data
#' 
#' @export
#' 
#' @examples
#' 
#' Note that these examples require corresponding zoo objects to work properly
#' 
#' zoo <- Transform(data = zoo, chg = "YoY", chg_type = 'perc')
#' zoo <- Transform(data = zoo, chg = "MoM", chg_type = "delta")
#' zoo <- Transform(data = zoo, start = "01/01/2010", pma = 12)
#' zoo <- Transform(data = zoo, pma = 3)
#' zoo <- Transform(data = zoo, pms = 6)
#' zoo <- Transform(data = zoo, lag = 3)
#' zoo <- Transform(data = zoo, lag = -3)
#' zoo <- Transform(data = zoo, rebase = TRUE)
#' zoo <- Transform(data = zoo, Z = 12)
#' zoo <- Transform(data = zoo, Z = all)


#Define function
Transform <- function(data, start = "01/01/1666", end = "01/01/1666", chg = "none", chg_type = "perc", pma = "none", pms = "none", lag = "none", rebase = "FALSE", Z = "none"){

#Turn off warnings
options(warn=-1)
  
#Prepare data
periodicity <- periodicity(data)$scale
MovPer <- substring(periodicity(data)$scale, 1, 1)
if (start!="01/01/1666") {start <- as.Date(start, "%d/%m/%Y")}
if (end!="01/01/1666")   {end <- as.Date(end, "%d/%m/%Y")}
delta <- intToUtf8(916)
if (chg!="none" & chg!="Overall") {chg <- gsub("O", "o", chg)}

#Fix data if only one column
if(is.null(nrow(data))) {one_row_fix <- "YES"} else {one_row_fix <- "NO"}
if(one_row_fix=="YES") {data <- cbind(data, data)}
if(one_row_fix=="YES") {colnames(data) <- c("Series", "Series2")}

#Apply lags
if (lag!="none") {lag <- lag*-1}
if (lag!="none") {temp <- stats::lag(data, lag, na.pad = TRUE)

#Rename temp & merge original data with transformed data
colnames(temp) <- paste(colnames(temp), ", lag", lag*-1, MovPer, sep="")
data <- cbind(data, temp)
}

#Apply transformation
if(chg!="none") {
  if (chg!="Overall" & chg_type == "perc") {
    if (periodicity == "monthly") {
        if (chg == "YoY") {
        temp <- 100 * ((data / stats::lag(data, k = -12)) - 1)
      } else if (chg == "QoQ") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (chg == "MoM") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "weekly") {
        if (chg == "YoY") {
        temp <- 100 * ((data / stats::lag(data, k = -52)) - 1)
      } else if (chg == "QoQ") {
        temp <- 100 * ((data / stats::lag(data, k = -16)) - 1)
      } else if (chg == "MoM") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (chg == "WoW") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "daily") {
        if (chg == "YoY") {
        temp <- 100 * ((data / stats::lag(data, k = -252)) - 1)
      } else if (chg == "QoQ") {
        temp <- 100 * ((data / stats::lag(data, k = -80)) - 1)
      } else if (chg == "MoM") {
        temp <- 100 * ((data / stats::lag(data, k = -20)) - 1)
      } else if (chg == "WoW") {
        temp <- 100 * ((data / stats::lag(data, k = -7)) - 1)
      } else if (chg == "DoD") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "quarterly") {
        if (chg == "YoY") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (chg == "QoQ") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "yearly") {
      temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", chg, " %", sep = "")
    data <- cbind(data, temp)
    if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
    
  }
  if (chg!="Overall" & chg_type == "delta") {
    if (periodicity == "monthly") {
      if (chg == "YoY") {
        temp <- (data - stats::lag(data, k = -12))
      } else if (chg == "QoQ") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (chg == "MoM") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "weekly") {
      if (chg == "YoY") {
        temp <- (data - stats::lag(data, k = -52))
      } else if (chg == "QoQ") {
        temp <- (data - stats::lag(data, k = -16))
      } else if (chg == "MoM") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (chg == "WoW") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "daily") {
      if (chg == "YOY") {
        temp <- (data - stats::lag(data, k = -252))
      } else if (chg == "QoQ") {
        temp <- (data - stats::lag(data, k = -80))
      } else if (chg == "MoM") {
        temp <- (data - stats::lag(data, k = -20))
      } else if (chg == "WoW") {
        temp <- (data - stats::lag(data, k = -7))
      } else if (chg == "DoD") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "quarterly") {
      if (chg == "YoY") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (chg == "QoQ") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "yearly") {
      temp <- (data - stats::lag(data, k = -1))


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", chg, " ", delta, sep = "")
    data <- cbind(data, temp)
    if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
  }

}

# Apply OVerall delta transformation
if(chg=="Overall" & chg_type=="delta") {
  temp <- sweep(data, MARGIN=2, STATS=coredata(data[1]))
  colnames(temp) <- paste(colnames(temp), ", ", delta, " Chg", sep = "")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
  }

# Apply OVerall percent transformation
if(chg=="Overall" & chg_type=="perc") {
  temp <- 100*(sweep(data, MARGIN=2, FUN="/", STATS=coredata(data[1]))-1)
  colnames(temp) <- paste(colnames(temp), ", % Chg", sep = "")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
}

#Apply period-moving-averages
if (pma!="none"){
  temp <- rollapply(data, pma, mean, align='right')
  colnames(temp) <- paste(colnames(temp), ", ", pma, MovPer, "ma", sep="")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
}

#Apply period-moving-sums
if (pms!="none"){
  temp <- rollapply(data, pms, sum, align='right')
  colnames(temp) <- paste(colnames(temp), ", ", pms, MovPer, "ms", sep="")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
}

#Subset data according to start date provided
if (start!="01/01/1666") {data <- subset(data, index(data)>=start)}

#Subset data according to end date provided
if (end!="01/01/1666") {data <- subset(data, index(data)<end)}

#Apply rebasing
if (rebase==TRUE) {
  temp <- na.locf(data, fromLast = TRUE)
  temp <- zoo(apply(temp, 2, function(x)  100*x/x[1]),  order.by=index(temp[,1]))
  temp <- ifelse(is.na(data), NA, temp)
  temp <- as.zoo(temp, order.by = index(data))
  colnames(temp) <- paste(colnames(temp), ", rebased", sep="")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
  }

#Apply Z-Scores
if (Z!="none" & Z!="all") {
  mean <- roll_mean(data, Z)
  sd <- roll_sd(data, Z)
  temp <- (data - mean)/sd
  colnames(temp) <- paste(colnames(temp), ", ", Z, MovPer, "-Z", sep="")
  data <- cbind(data, temp)
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
  }

if (Z=="all") {
  temp <- scale(data)
  colnames(temp) <- paste(colnames(temp), ", ", "Z-Scr", sep="")
  data <- cbind(data, temp) 
  if (one_row_fix=="YES") {data <- data[,c(-2, -4)]}
  }

return(data)

}