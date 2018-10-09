################# FCT TRANSFORM DATA #################
#######################################################

#' Calculate transformation of zoo object
#' 
#' Transforms a zoo object and appends results
#' 
#' @import zoo
#' @import stringr
#' @import quantmod
#' @import scales
#' @import roll
#' 
#' @param data specification of zoo object
#' @param start optional start date to trim 'data'
#' @param chg optional string to calculate periodical changes; options are 'YOY' (Year-on-Year), 'QOQ' (Quarter-on-Quarter), 'MOM' (Month-on-Month); is appended for all columns
#' @param chg_type optional string to specify calculation of changes; options are 'perc' (percentage change) or 'delta' (absolute change); default is 'perc'
#' @param pma optional integer to specify calculation of period-moving-averages; is appended for all columns
#' @param pms optional integer to specify calculation of period-moving-sums; is appended for all columns
#' @param lag optional integer to lag the data; is appended for all columns
#' @param lead optional integer to lead the data; is appended for all columns
#' @param rebase optional boolean to rebase the data to 100 at first observation; is appended for all columns
#' @param Z optional integer to specify the rolling number of periods over which the data is normalized; is appended for all columns
#' 
#' @return returns a zoo object with transformations appended
#' 
#' @export

#Define function
Transform <- function(data, start, chg, chg_type, pma, pms, lag, lead, rebase, Z){

#Turn off warnings
options(warn=-1)
  
#(Un)load libraries
unloadNamespace("dplyr")
library(zoo)
#library(xts)
library(stringr)
library(quantmod)
library(scales)
library(roll)  
  
#Set missing values
if (missing(start))      {start      <- "01/01/1666" }
if (missing(chg))        {chg        <- "none"       }
if (missing(chg_type))   {chg_type   <- "perc"       }
if (missing(pma))        {pma        <- "none"       }
if (missing(pms))        {pms        <- "none"       }
if (missing(lag))        {lag        <- "none"       }
if (missing(lead))       {lead       <- "none"       }
if (missing(rebase))     {rebase     <- FALSE        }
if (missing(Z))          {Z          <- "none"       }

#Prepare data
periodicity <- periodicity(data)$scale
MovPer <- substring(periodicity(data)$scale, 1, 1)
if (start!="01/01/1666") {start <- as.Date(start, "%d/%m/%Y")}

#Apply lags
if (lag!="none") {lag <- lag*-1}
if (lag!="none") {temp <- stats::lag(data, lag, na.pad = TRUE)

#Rename temp & merge original data with transformed data
colnames(temp) <- paste(colnames(temp), ", lag", lag*-1, MovPer, sep="")
data <- cbind(data, temp)
}

#Apply transformation
if(chg!="none") {
  if (chg_type == "perc") {
    if (periodicity == "monthly") {
      if (chg == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -12)) - 1)
      } else if (chg == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (chg == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "weekly") {
      if (chg == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -52)) - 1)
      } else if (chg == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -16)) - 1)
      } else if (chg == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      }

    } else if (periodicity == "daily") {
      if (chg == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -252)) - 1)
      } else if (chg == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -80)) - 1)
      } else if (chg == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -20)) - 1)
      }

    } else if (periodicity == "quarterly") {
      if (chg == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (chg == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "yearly") {
      temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", chg, "%", sep = "")
    data <- cbind(data, temp)
  }
  if (chg_type == "delta") {
    if (periodicity == "monthly") {
      if (chg == "YOY") {
        temp <- (data - stats::lag(data, k = -12))
      } else if (chg == "QOQ") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (chg == "MOM") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "weekly") {
      if (chg == "YOY") {
        temp <- (data - stats::lag(data, k = -52))
      } else if (chg == "QOQ") {
        temp <- (data - stats::lag(data, k = -16))
      } else if (chg == "MOM") {
        temp <- (data - stats::lag(data, k = -4))
      }

    } else if (periodicity == "daily") {
      if (chg == "YOY") {
        temp <- (data - stats::lag(data, k = -252))
      } else if (chg == "QOQ") {
        temp <- (data - stats::lag(data, k = -80))
      } else if (chg == "MOM") {
        temp <- (data - stats::lag(data, k = -20))
      }

    } else if (periodicity == "quarterly") {
      if (chg == "YOY") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (chg == "QOQ") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "yearly") {
      temp <- (data - stats::lag(data, k = -1))


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", chg, "-D", sep = "")
    data <- cbind(data, temp)
  }

}

# Apply net transformation
  if(chg_type=="net") {
  temp <- sweep(data,2,coredata(data[1]))
  colnames(temp) <- paste(colnames(temp), ", net chg", sep = "")
  data <- cbind(data, temp)
  }

#Apply period-moving-averages
if (pma!="none"){
  temp <- rollapply(data, pma, mean, align='right')
  colnames(temp) <- paste(colnames(temp), ", ", pma, MovPer, "ma", sep="")
  data <- cbind(data, temp)
}

#Apply period-moving-sums
if (pms!="none"){
  temp <- rollapply(data, pms, sum, align='right')
  colnames(temp) <- paste(colnames(temp), ", ", pms, MovPer, "ms", sep="")
  data <- cbind(data, temp)
}

#Subset data according to start date provided
if (start!="01/01/1666") {data <- subset(data, index(data)>=start)}

#Apply rebasing
if (rebase==TRUE) {
  temp <- na.locf(data, fromLast = TRUE)
  temp <- zoo(apply(temp, 2, function(x)  100*x/x[1]),  order.by=index(temp[,1]))
  temp <- ifelse(is.na(data), NA, temp)
  temp <- as.zoo(temp, order.by = index(data))
  colnames(temp) <- paste(colnames(temp), ", rebased", sep="")
  data <- cbind(data, temp)
  }

#Apply Z-Scores
if (Z!="none") {
  mean <- roll_mean(data, Z)
  sd <- roll_sd(data, Z)
  temp <- (data - mean)/sd
  colnames(temp) <- paste(colnames(temp), ", ", Z, MovPer, "-Z", sep="")
  data <- cbind(data, temp)

}

return(data)

}
