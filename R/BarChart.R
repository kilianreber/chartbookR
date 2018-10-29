########## CREATE CUSTOM BAR/LINE PLOT ###########
##################################################

#' Create custom bar(s)/line(s) plot
#' 
#' Create custom bar(s)/line(s) plot based on user inputs
#'
#' @import zoo
#' @import quantmod
#' @import stringr
#' @import MALDIquant
#'   
#' @param data specification of zoo dataset to use for plot
#' @param inception optional boolean to show inception date when set to 'TRUE'; default is FALSE
#' @param title optional character to add chart title
#' @param no optional integer to add chart number
#' @param stacked optional boolean to unstack bars when set to FALSE; default is TRUE
#' @param space optional integer to specify space between bars and data groups, e.g. space=c(0.5, 1); default is space=c(0.1, 0.25)
#' @param d1 optional integer vector to specify which columns to plot on primary y-axis
#' @param d2 optional integer vector to specify which columns to plot on secondary y-axis
#' @param y1 optional character to specify description of primary y-axis
#' @param y2 optional character to specify description of secondary y-axis
#' @param y1_def optional number vector to specify start, end, and intervals of primary y-axis, e.g. c(0, 10, 2)
#' @param y2_def optional number vector to specify start, end, and intervals of secondary y-axis, e.g. c(0, 10, 2)
#' @param y1_rev optional boolean to invert secondary y-axis; y2_def needs to be supplied
#' @param fn optional character to add footnote to plot
#' @param optional character to place position of legend; options are 'topleft', 'center', 'topright', 'left', 'center', 'right', 'bottomleft', 'bottom', 'bottomright'
#' @param grid optional boolean to show grid when set to TRUE
#' @param rec optional boolean to shade recessions when set to TRUE
#' @param dt_format optional character vector to specify date interval and date format of x-axis, e..g dt_format=c('3 months','\%b-\%Y')
#' @param h optional argument to specify horizontal line at specific height
#' @param v optional date to specify vertical date line, e.g. v='31/12/2012'
#' 
#' @return plots R base plot
#' 
#' @export
#' 
#' @examples
#' 
#' BarChart(data=zoo, d1=1:3, leg="topleft", fn="This is a footnote.", stacked=TRUE, space=0)
#' BarChart(data=zoo, d1=1:3, leg="topleft", fn="This is a footnote.", stacked=FALSE, space=c(0.5, 1))
#' BarChart(data=zoo, title="Example Chart", no="1.1", d1=1:3, y1="Index", leg="topleft", dt_format=c("5 years", "%Y"), v="01/01/2018")
#' BarChart(data=zoo, title="Example Chart", d1=1:3, y1="Index", leg="topleft", grid=TRUE, h=0)
#' BarChart(data=zoo, title="Example Chart", d1=4:5, d2=6, y1="in %", y2="in USD mln", rec=TRUE)
#' BarChart(data=zoo, title="Example Chart", d1=1, d2=2, y1="Unemployment (%)", y2="Budget Balance (%GDP)", y1_def=c(0, 10, 2), y2_def=c(-10,2,2), y2_rev=TRUE, leg="top", rec=TRUE)

BarChart <- function(data, inception, title, no, stacked, space, d1, d2, y1, y2, y1_def, y2_def, y2_rev, fn, leg, grid, rec, dt_format, h, v) {

#Turn off warnings
options(warn=-1)

#Set default values
if (missing(title))       {title   <- ""    }
if (missing(d1))          {d1 <- c(1:ncol(data))}
if (missing(stacked))     {stacked <- TRUE  }
if (missing(y1))          {y1      <- ""    }
if (missing(y1_def))      {y1_def  <- "none"}
if (missing(y2_def))      {y2_def  <- "none"}
if (missing(y2))          {y2      <- ""    }
if (missing(d2))          {d2      <- "none"}
if (missing(fn))          {fn      <-  ""   }
if (missing(no))          {no      <-  ""   }
if (missing(grid))        {grid    <-  FALSE}
if (missing(rec))         {rec     <-  FALSE}
if (missing(leg))         {leg  <- "topleft"}
if (missing(h))           {h       <- "none"}
if (missing(v))           {v       <- "none"}
if (missing(y2_rev))      {y2_rev  <- FALSE }
if (missing(space) & stacked==TRUE)  {space <- 0.25}
if (missing(space) & stacked==FALSE) {space <- c(0.25, 0.1)}
if (stacked==TRUE)        {space <- space[1]}
if (stacked==FALSE &  length(space)==1) {space <- c(space[1], 1)}
if (y2_def!="none")       {ylim_input=c(y2_def[1], y2_def[2])}
if (y2_def!="none" & y2_rev==TRUE) {ylim_input=rev(range(c(y2_def[1], y2_def[2])))}
if (missing(inception)) {inception <- FALSE}
bars_width <- 1
dt_format_override <- FALSE

#Fix data when it has only one column
if (is.null(ncol(data)))  {data <- cbind(data, data)
colnames(data) <- c("Series", "Series")}

#Create data vectors and data column vectors
data1 <- as.data.frame(data[,d1])
colnames(data1) <- colnames(data)[d1]
data1 <- as.zoo(data1)

if(d2!="none")  {
  data2 <- as.data.frame(data[,d2])
  colnames(data2) <- colnames(data)[d2]
  data2 <- as.zoo(data2)
}

#Trim data block
if (d2!="none") {
  d11 <- length(d1)
  d22 <- length(d2)
  
  block <- cbind(data1, data2)
  block <- na.trim(block, sides="both", is.na="all")
  data1 <- block[,c(1:d11)]
  data2 <- block[,c((d11+1):(d11+d22))]
}

if (d2=="none") {data1 <- na.trim(data1, sides="both", is.na="all")}

#Create d1, d2, d3
if(d2!="none")  {d3 <- c(d1, d2)} else {d3 <- d1}
if(d2!="none")  {data3 <- data[,d3]} else {data3 <- data1}

#Calculate length of data1
if (is.null(ncol(data1))) {length_d1 <- 1} else {length_d1 <- ncol(data1)}

if (!missing(dt_format)){
#Override unforunate dt_format inputs by user
dt_format[1] <- gsub("y", "Y", dt_format[1])
dt_format[1] <- gsub("m", "M", dt_format[1])
years     <- gsub("Y.*","\\1",dt_format[1])
months    <- gsub("M.*","\\1",dt_format[1])

if (months==dt_format[1]) {months <- 12} else {months <- as.numeric(months)}
if (years==dt_format[1])  {years  <- 1} else  {years <- as.numeric(years)}

days <- last(index(data)) - first(index(data))
intervals <- days / (years*months*30)

if (intervals > 5.5 & dt_format[2]=="%b-%Y") {dt_format_override <- TRUE}
if (intervals > 5.5 & dt_format[2]=="%m-%Y") {dt_format_override <- TRUE}
if (intervals > 8.5 & dt_format[2]=="%Y")    {dt_format_override <- TRUE}
}

#Set dt_format defaults (if user provides none)
if (missing(dt_format) || dt_format_override==TRUE)   
  {
  day_diff <- as.numeric(tail(index(data1),1) - index(data1)[1])
  if(day_diff <= 1.5*365) 				                 {dt_format <- c("2 mon", "%b-%Y")}
  if((day_diff > 1.5*365) & (day_diff <=2*365)) 	 {dt_format <- c("3 mos", "%b-%Y")}
  if((day_diff >   2*365) & (day_diff <=3*365)) 	 {dt_format <- c("6 mos", "%b-%Y")}
  if((day_diff > 3  *365) & (day_diff <=5*365)) 	 {dt_format <- c("1 years", "%Y") }
  if((day_diff > 5  *365) & (day_diff <=12*365)) 	 {dt_format <- c("2 years", "%Y") }
  if((day_diff > 12  *365) & (day_diff <=20*365))  {dt_format <- c("3 years", "%Y") }
  if(day_diff > 20*365)                            {dt_format <- c("5 years", "%Y") }
  }

#Load recession data & reset recession indicator if necessary
nber <- NBER_Recessions
if (index(data1)[1] > as.Date(tail(nber$Rec_End, 1))) {rec <- FALSE}

#Adjust title
if (no!="") {title <- paste("Fig. ", no, ": ", title, sep="")}

#Set color Palette (only if currently standard)
if (palette()==c("#428BCE", "gray35", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667")) {palette(c("#428bce", "#595959", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667"))}
if (palette()==c("black", "red", "green3", "blue", "cyan", "magenta")) {palette(c("#428bce", "#595959", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667"))}

#Create labels and tick vectors
bp_param <- Barplot_param(inception=inception, data=data1, stacked=stacked, dt_format=dt_format, type="B")

#Create plot, first y-axis, and content
par(mar = c(5,5,5,5))

if (y1_def!="none") {bp <- barplot(data1, beside=!stacked, ylim=c(y1_def[1], y1_def[2]), space=space, ann=FALSE, xaxt = "n", yaxt="n", tck=0, las=1, lwd=3, col=1:length(d1), border=NA); title(main=title, ylab=y1)
  
  axis(side=1, at=bp[as.integer(bp_param[,1])], labels=as.character(bp_param[,2]), las=1, tck=0)
  axis(2, seq(y1_def[1], y1_def[2], y1_def[3]), las=1, tck=-0)
  title(sub=fn, font.sub=3, line = 3)

    #Add horizontal abline
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
    
    #Add vertical abline
    if(v!="none") {
    v <- as.Date(v, "%d/%m/%Y")
    v_tick <- match.closest(v, index(data1))
    if (stacked==FALSE) {v_tick <- (v_tick*((length((d1))*bars_width)+((length(d1)-1)*space[1])+space[2]))+(0.5*space[2])}
    if (stacked==TRUE)  {v_tick <- (v_tick*bars_width)*(1+space)+0.5*space}
    abline(v=v_tick, lty=1, lwd=2, col="black")}
    
    #Prepare recession shading
    if (rec!=FALSE){
      if (length_d1 >1) {data_rec <- data1} else {data_rec <- cbind(data1,data1)}

    #Load recession data & create date vectors
    rec_start <- as.Date(nber$Rec_Start)
    rec_end   <- as.Date(nber$Rec_End)

    #Create limits for rects
    mymax <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    mymin <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)
    
    if (stacked==FALSE) {
      if (mymax(data_rec) > 0) {rect_max <- mymax(data_rec)*2} else {rect_max <- mymax(data_rec)*0.5}
      if (mymin(data_rec) > 0) {rect_min <- mymin(data_rec)*0.5} else {rect_min <- mymin(data_rec)*2}
    }
    
    if (stacked==TRUE) {
      if (max(rowSums(data_rec, na.rm=TRUE)) > 0) {rect_max <- max(rowSums(data_rec, na.rm=TRUE))*2} else {rect_max <- max(rowSums(data_rec, na.rm=TRUE))*0.5}
      if (min(rowSums(data_rec, na.rm=TRUE)) > 0) {rect_min <- min(rowSums(data_rec, na.rm=TRUE))*0.5} else {rect_min <- min(rowSums(data_rec, na.rm=TRUE))*2}
    }
      
    if (rect_min >0) {rect_min <- 0}
    rec_start_dt <- min(index(data_rec))
    rec_end_dt <- max(index(data_rec))
    
    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]

    #Convert date vectors to ticks
    rec_start <- match.closest(rec_start, index(data_rec))
    rec_end <- match.closest(rec_end, index(data_rec))
    
    #Set bars_width
    if (stacked==FALSE) {bars_width <- length(d1)}
    
    #Add recession shading
    rec_start <- (rec_start*bars_width)*(1+space)
    rec_end <- (rec_end*bars_width)*(1+space)
    rect(rec_start, rect_min, rec_end, rect_max, density=NULL, border=NA, lwd=0, col= rgb(0,0,0.1, alpha=0.1))
    }

    if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=1, lwd=1, col="grey")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=1, lwd=1, col="grey")}}
  
  } else {bp <- barplot(data1, beside=!stacked, ann=FALSE, bty="n", xaxt = "n", space=space, tck=0, las=1, lwd=1, col=1:length(d1), border=NA); title(main=title, ylab=y1)
    
    axis(side=1, at=bp[as.integer(bp_param[,1])], labels=as.character(bp_param[,2]), las=1, tck=0)

    #Add horizontal abline
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
  
    #Add vertical abline
    if(v!="none") {
    v <- as.Date(v, "%d/%m/%Y")
    v_tick <- match.closest(v, index(data1))
    if (stacked==FALSE) {v_tick <- (v_tick*((length((d1))*bars_width)+((length(d1)-1)*space[1])+space[2]))+(0.5*space[2])}
    if (stacked==TRUE)  {v_tick <- (v_tick*bars_width)*(1+space)+0.5*space}
    abline(v=v_tick, lty=1, lwd=2, col="black")}

    #ADD RECESSION SHADING
    if (rec!=FALSE){
      if (length_d1 >1) {data_rec <- data1} else {data_rec <- cbind(data1,data1)}

    #Load recession data & create date vectors
    rec_start <- as.Date(nber$Rec_Start)
    rec_end   <- as.Date(nber$Rec_End)

    #Create limits for rects
    mymax <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    mymin <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)
    
    if (stacked==FALSE) {
      if (mymax(data1) > 0) {rect_max <- mymax(data1)*2} else {rect_max <- mymax(data1)*0.5}
      if (mymin(data1) > 0) {rect_min <- mymin(data1)*0.5} else {rect_min <- mymin(data1)*2}
    }
    
    if (stacked==TRUE) {
      if (max(rowSums(data_rec, na.rm=TRUE)) > 0) {rect_max <- max(rowSums(data_rec, na.rm=TRUE))*2} else {rect_max <- max(rowSums(data_rec, na.rm=TRUE))*0.5}
      if (min(rowSums(data_rec, na.rm=TRUE)) > 0) {rect_min <- min(rowSums(data_rec, na.rm=TRUE))*0.5} else {rect_min <- min(rowSums(data_rec, na.rm=TRUE))*2}
    }

    if (rect_min >0) {rect_min <- 0}
    rec_start_dt <- min(index(data_rec))
    rec_end_dt <- max(index(data_rec))
    
    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]
  
    #Convert date vectors to ticks
    rec_start <- match.closest(rec_start, index(data1))
    rec_end <- match.closest(rec_end, index(data1))
    
    #Set bars_width
    if (stacked==FALSE) {bars_width <- length(d1)}
    
    #Add recession shading
    rec_start <- (rec_start*bars_width)*(1+space)
    rec_end   <- (rec_end*bars_width)*(1+space)
    rect(rec_start, rect_min, rec_end, rect_max, density=NULL, border=NA, lwd=0, col=rgb(0,0,0.1, alpha=0.1))
    }

  title(sub=fn, font.sub=3, line = 3)

  if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=1, lwd=1, col="grey")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=1, lwd=1, col="grey")
    }}
}

#Create second y-axis, and content (if available)
if (d2!="none") {par(new = T)

  if (y2_def!="none") {plot(data2, plot.type="s", bty="n", ylim=ylim_input, col=(length(d1)+1):(length(d3)), axes=F, lwd=3, ann=FALSE)
    axis(4, seq(y2_def[1], y2_def[2], y2_def[3]), las=1, lwd=0, tck=-0)
    } else {plot(data2, plot.type="s", bty="n", col=(length(d1)+1):(length(d3)), axes=F, lwd=3, las=1, ann=FALSE, tck=-0)
    axis(4, las=1, lwd=0, tck=-0)}
    mtext(side = 4, line = 3, y2)

    legend(leg, box.lty=0, bg="#FFFFFF", legend=colnames(data3), pch=15, col=1:length(d3), ncol=1)
    } else {legend(leg, box.lty=0, bg="#FFFFFF", legend=colnames(data3), pch=15, col=1:length(d3), ncol=1)}

if (d2=="none") {box(which = "plot", bty = "l")} else {box(which = "plot", bty = "u")}

}
