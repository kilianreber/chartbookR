################# FCT TRANSFORM DATA #################
#######################################################

#Turn off warnings
options(warn=-1)

#(Un)load libraries
unloadNamespace("dplyr")
library(zoo)
library(xts)
library(stringr)
library(quantmod)
library(scales)
library(roll)

#Define function
Transform <- function(data, start, tf, typ, pma, pms, lag, lead, rebase, Z){

#Set missing values
if (missing(start))      {start      <- "01/01/1666" }
if (missing(tf))         {tf         <- "none"       }
if (missing(typ))        {typ        <- "perc"       }
if (missing(pma))        {pma        <- "none"       }
if (missing(pms))        {pms        <- "none"       }
if (missing(lag))        {lag        <- "none"       }
if (missing(lead))       {lead       <- "none"       }
if (missing(rebase))     {rebase     <- FALSE        }
if (missing(Z))          {Z          <- "none"       }

#Typ can take on 'perc', 'delta' or 'net'

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
if(tf!="none") {
  if (typ == "perc") {
    if (periodicity == "monthly") {
      if (tf == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -12)) - 1)
      } else if (tf == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (tf == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "weekly") {
      if (tf == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -52)) - 1)
      } else if (tf == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -16)) - 1)
      } else if (tf == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      }

    } else if (periodicity == "daily") {
      if (tf == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -252)) - 1)
      } else if (tf == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -80)) - 1)
      } else if (tf == "MOM") {
        temp <- 100 * ((data / stats::lag(data, k = -20)) - 1)
      }

    } else if (periodicity == "quarterly") {
      if (tf == "YOY") {
        temp <- 100 * ((data / stats::lag(data, k = -4)) - 1)
      } else if (tf == "QOQ") {
        temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)
      }

    } else if (periodicity == "yearly") {
      temp <- 100 * ((data / stats::lag(data, k = -1)) - 1)


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", tf, "%", sep = "")
    data <- cbind(data, temp)
  }
  if (typ == "delta") {
    if (periodicity == "monthly") {
      if (tf == "YOY") {
        temp <- (data - stats::lag(data, k = -12))
      } else if (tf == "QOQ") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (tf == "MOM") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "weekly") {
      if (tf == "YOY") {
        temp <- (data - stats::lag(data, k = -52))
      } else if (tf == "QOQ") {
        temp <- (data - stats::lag(data, k = -16))
      } else if (tf == "MOM") {
        temp <- (data - stats::lag(data, k = -4))
      }

    } else if (periodicity == "daily") {
      if (tf == "YOY") {
        temp <- (data - stats::lag(data, k = -252))
      } else if (tf == "QOQ") {
        temp <- (data - stats::lag(data, k = -80))
      } else if (tf == "MOM") {
        temp <- (data - stats::lag(data, k = -20))
      }

    } else if (periodicity == "quarterly") {
      if (tf == "YOY") {
        temp <- (data - stats::lag(data, k = -4))
      } else if (tf == "QOQ") {
        temp <- (data - stats::lag(data, k = -1))
      }

    } else if (periodicity == "yearly") {
      temp <- (data - stats::lag(data, k = -1))


    } else{
      print("Error! Data has no valid frequency!")
    }

    #Rename & merge with original
    colnames(temp) <- paste(colnames(temp), ", ", tf, "-D", sep = "")
    data <- cbind(data, temp)
  }

}

# Apply net transformation
  if(typ=="net") {
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
