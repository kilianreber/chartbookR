# FUNCTION TO CREATE TICKS AND LABELS FOR R BARPLOTS

Barplot_param <- function(data, d1, stacked, dt_format){

#Prepare variables & set defaults
interval_type <- gsub('[[:digit:]]+', '', dt_format[1])
interval_type <- str_replace_all(string=interval_type, pattern=" ", repl="")
interval_type <- substring(interval_type, 1, 1)
interval <- as.numeric(gsub("[^0-9]", "", dt_format[1]))

interval <- gsub(interval, "y", "Y")
interval <- gsub(interval, "m", "M")

if(dt_format[2]=="%m-%Y" & interval_type=="Y") {interval <- interval*12}
if(dt_format[2]=="%b-%Y" & interval_type=="Y") {interval <- interval*12}
if (dt_format[2]=="%Y") {interval <- 1}

if (missing(stacked)) {stacked <- TRUE}

#Create date vector
index_y <- as.Date(index(data))
if (stacked==FALSE) {index_y <- rep(index_y, times=length(d1))}

### Logical vector with TRUE for each new interval of specified date
index_y <- as.character(sort(index_y), dt_format[2])
index_u <- !duplicated(index_y)

### Index for tick marks for of start of new year for tick marks
at_tick <- which(index_u)
at_tick <- at_tick[seq(1, length(at_tick), interval)]

### Labels for date intervals
labels <- index_y[index_u]
labels <- labels[seq(1, length(labels), interval)]
if (dt_format[2]=="%Y" & ((at_tick[2] - at_tick[1])/length(d1)<11)) {labels[1] <- ""}

### Combine output into one vector
bp_param <- rbind(at_tick, labels)
}