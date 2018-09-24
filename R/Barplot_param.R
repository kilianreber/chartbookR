# FUNCTION TO CREATE TICKS AND LABELS FOR R BARPLOTS

Barplot_param <- function(data1, stacked, dt_format, type){

#Prepare variables & set defaults
data1 <- as.zoo(data1)
if (missing(stacked)) {stacked <- TRUE}
if (is.null(ncol(data1))) {length_d1 <- 1} else {length_d1 <- ncol(data1)}

interval_type <- gsub('[[:digit:]]+', '', dt_format[1])
interval_type <- str_replace_all(string=interval_type, pattern=" ", repl="")
interval_type <- substring(interval_type, 1, 1)
interval <- as.numeric(gsub("[^0-9]", "", dt_format[1]))

interval_type <- gsub("y", "Y", interval_type)
interval_type <- gsub("m", "M", interval_type)

if(dt_format[2]=="%m-%Y" & interval_type=="Y") {interval <- interval*12}
if(dt_format[2]=="%b-%Y" & interval_type=="Y") {interval <- interval*12}

#Create date vectors
index_y <- as.Date(index(data1))
index_z <- as.Date(index(data1))
if (type=="B" & stacked==FALSE) {index_y <- rep(index_y, times=length_d1)}

### Logical vector with TRUE for each new interval of specified date
index_y <- as.character(sort(index_y), dt_format[2])
index_u <- !duplicated(index_y)

### Create date labels
date_labels <- as.Date(index_z[index_u])

### Index for tick marks for of start of new year for tick marks
at_tick <- which(index_u)
at_tick <- at_tick[seq(1, length(at_tick), interval)]

### Labels for date intervals
labels <- index_y[index_u]
labels <- labels[seq(1, length(labels), interval)]
date_labels <- date_labels[seq(1, length(date_labels), interval)]
#if (dt_format[2]=="%Y" & ((at_tick[2] - at_tick[1])/length_d1 <12)) {labels[1] <- ""}

### Combine output into one vector
if (type=="L") {bp_param <- data.frame(date_labels)}
if (type=="B") {bp_param <- data.frame(at_tick, labels)}

return(bp_param)

}