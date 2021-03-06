############ CREATE CUSTOM LINE PLOT #############
##################################################

#' Create custom line(s) plot
#' 
#' Create custom line(s) plot based on user inputs
#' 
#' @import zoo
#' @import quantmod
#' @import stringr
#' @import MALDIquant
#' @import Boom
#' 
#' @param data specification of zoo dataset to use for plot
#' @param inception optional boolean to show clipped inception date when set to 'TRUE'
#' @param title optional character to add chart title
#' @param title_adj optional integer to specify title alignment (0: left-align, 1: right-align, NULL: center)
#' @param no optional integer to add chart number
#' @param l1 optional integer vector to specify which columns to plot (as lines) on primary y-axis
#' @param l2 optional integer vector to specify which columns to plot (as lines) on secondary y-axis
#' @param y1 optional character to specify description of primary y-axis
#' @param y2 optional character to specify description of secondary y-axis
#' @param y1_def optional number vector to specify start, end, and intervals of primary y-axis, e.g. c(0, 10, 2)
#' @param y2_def optional number vector to specify start, end, and intervals of secondary y-axis, e.g. c(0, 10, 2)
#' @param y2_rev optional boolean to invert secondary y-axis; y2_def needs to be supplied
#' @param fn optional character to add footnote to plot
#' @param fn_adj optional integer to specify alignment of footnote (0: left-align, 1: right-align, NULL: center)
#' @param leg optional character to specify legend position; options are 'topleft', 'center', 'topright', 'left', 'center', 'right', 'bottomleft', 'bottom', 'bottomright'
#' @param grid optional boolean to show grid when set to TRUE
#' @param rec optional boolean to shade recessions when set to TRUE
#' @param dt_format optional character vector to specify date interval and date format of x-axis, e..g dt_format=c('3 months', '\%b-\%Y')
#' @param h1 optional argument to specify horizontal line(s) at specific height
#' @param v optional date to specify vertical date line(s), e.g. v='31/12/2012'
#' 
#' @return plots R base plot
#' 
#' @export
#' 
#' @examples
#' 
#' Note that these examples require corresponding zoo objects to work properly
#' 
#' LineChart(data = zoo, l1 = 1:3, leg = "topleft", fn = "This is a footnote.")
#' LineChart(data = zoo, title = "Example Chart", no = "1.1", l1 = 1:3, y1 = "Index", leg = "topleft", dt_format = c("5 years", "%Y"), v = "01/01/2018")
#' LineChart(data = zoo, title = "Example Chart", l1 = 1:3, y1 = "Index", leg = "topleft", grid = TRUE, h = 0)
#' LineChart(data = zoo, title = "Example Chart", l1 = 4:5, l2 = 6, y1 = "in %", y2 = "in USD mln", rec = TRUE)
#' LineChart(data = zoo, title = "Example Chart", l1 = 1, l2 = 2, y1 = "Unemployment (%)", y2 = "Budget Balance (%GDP)", y1_def = c(0, 10, 2), y2_def = c(-10,2,2), y2_rev = TRUE, leg = "top", rec = TRUE)

LineChart <- function(data, inception = FALSE, title = NULL, title_adj = NULL, no = "", l1, l2 = "none", y1 = NULL, y2 = NULL, y1_def = "none", y2_def = "none", y2_rev = FALSE, fn = NULL, fn_adj = NULL, leg ="topleft", grid = FALSE, rec = FALSE, dt_format, h1 = "none", v = "none") {

#Turn off warnings
options(warn=-1)

#Set default values
if (missing(l1) &  is.null(ncol(data)))  {l1 <- 1 }
if (missing(l1) & !is.null(ncol(data)))  {l1 <- c(1:ncol(data))}
if (y2_def!="none")       {ylim_input=c(y2_def[1], y2_def[2])}
if (y2_def!="none" & y2_rev==TRUE) {ylim_input=rev(range(c(y2_def[1], y2_def[2])))}
dt_format_override <- FALSE

#Fix data when it has only one column
if (is.null(ncol(data)))  {data <- cbind(data, data)
colnames(data) <- c("Series", "Series")}

#Create data vectors and data column vectors
data1 <- as.data.frame(data[,l1])
colnames(data1) <- colnames(data)[l1]
data1 <- as.zoo(data1)

if(l2!="none")  {
  data2 <- as.data.frame(data[,l2])
  colnames(data2) <- colnames(data)[l2]
  data2 <- as.zoo(data2)
}

#Trim data block
if (l2!="none") {
  d11 <- length(l1)
  d22 <- length(l2)
  
  block <- cbind(data1, data2)
  block <- na.trim(block, sides="both", is.na="all")
  data1 <- block[,c(1:d11)]
  data2 <- block[,c((d11+1):(d11+d22))]
}

if (l2=="none") {data1 <- na.trim(data1, sides="both", is.na="all")}

#Create d1, d2, d3
if(l2!="none")  {d3 <- c(l1, l2)} else {d3 <- l1}
if(l2!="none")  {data3 <- data[,d3]} else {data3 <- data1}

if (!missing(dt_format)){

#Override unfortunate dt_format inputs by user
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
  day_diff <- as.numeric(tail(index(data),1) - index(data)[1])
  if(day_diff <= 1.5*365) 			                   {dt_format <- c("2 mos", "%b-%Y")}
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
if (palette()==c("#428BCE", "gray35", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667")) {palette(c("#4663AC", "#97A4CC", "#CEBC9A", "gray35", "#BF7057", "#ADAFB2", "#E7C667"))}
if (palette()==c("black", "red", "green3", "blue", "cyan", "magenta")) {palette(c("#4663AC", "#97A4CC", "#CEBC9A", "gray35", "#BF7057", "#ADAFB2", "#E7C667"))}

#Create labels and tick vectors
bp_param <- Barplot_param(inception=inception, data=data1, stacked=TRUE, dt_format=dt_format, type="L")

#Create plot, first y-axis, and content
par(mar = c(5,5,5,5))

if (y1_def!="none") {plot(data1, plot.type="s", ann=FALSE, bty="n", ylim=c(y1_def[1], y1_def[2]), xaxt="n", yaxt="n", tck=0, las=1, lwd=3, col=1:length(l1), border=0.1, space=0); title(main=title, adj=title_adj, line=0.75)
  title(ylab=y1)
  
  axis(side=1, at=bp_param[,1], labels=format(bp_param[,1], dt_format[2]), las=1, tck=0)
  axis(2, seq(y1_def[1], y1_def[2], y1_def[3]), las=1, tck=0)
  title(sub=fn, font.sub=3, line = 3, adj=fn_adj)

    #Prepare recession shading
    if (rec!=FALSE){

    #Load recession data & create date vectors
    rec_start <- as.Date(nber$Rec_Start)
    rec_end   <- as.Date(nber$Rec_End)

    #Create limits for rects
    mymax <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    mymin <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

    if (mymax(data1) > 0) {rect_max <- mymax(data1)*2} else {rect_max <- mymax(data1)*0.5}
    if (mymin(data1) > 0) {rect_min <- mymin(data1)*0.5} else {rect_min <- mymin(data1)*2}
    
    if(y1_def!="none")    {rect_min <- (y1_def[1] - y1_def[3])}
    if(y1_def!="none")    {rect_max <- (y1_def[2] + y1_def[3])}
    
    rec_start_dt <- min(index(data1))
    rec_end_dt <- max(index(data1))

    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]
    
    #Add recession shading
    rect(as.Date(rec_start), rect_min, as.Date(rec_end), rect_max, density=NULL, border=NA, lwd=0, col= rgb(0,0,0.1, alpha=0.15))
    }

    if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=1, lwd=1, col="lavenderblush3")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=1, lwd=1, col="lavenderblush3")}}

    # Add ablines
    if(h1!="none") {abline(h=h1, lty=1, lwd=1, col="black")}
    if(v!="none") {abline(v=as.Date(v, "%d/%m/%Y"), lty=1, lwd=1)}
  
} else {plot(data1, plot.type="s", ann=FALSE, bty="n", xaxt="n", tck=0, las=1, las=1, lwd=3, col=1:length(l1), border=0.1, space=0); title(main=title, adj=title_adj, line=0.75)
    title(ylab=y1)  
  
    axis(side=1, at=bp_param[,1], labels=format(bp_param[,1], dt_format[2]), las=1, tck=0)
  
    #ADD RECESSION SHADING
    if (rec!=FALSE){

    #Load recession data & create date vectors
    rec_start <- as.Date(nber$Rec_Start)
    rec_end   <- as.Date(nber$Rec_End)

    #Create limits for rects
    mymax <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
    mymin <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

    if (mymax(data1) > 0) {rect_max <- mymax(data1)*2} else {rect_max <- mymax(data1)*0.5}
    if (mymin(data1) > 0) {rect_min <- mymin(data1)*0.5} else {rect_min <- mymin(data1)*2}
  
    if(y1_def!="none")    {rect_min <- (y1_def[1] - y1_def[3])}
    if(y1_def!="none")    {rect_max <- (y1_def[2] + y1_def[3])}
    
    rec_start_dt <- min(index(data1))
    rec_end_dt <- max(index(data1))

    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]

    #Add recession shading
    rect(as.Date(rec_start), rect_min, as.Date(rec_end), rect_max, density=NULL, border=NA, lwd=0, col= rgb(0,0,0.1, alpha=0.15))
    }

  title(sub=fn, font.sub=3, line = 3, adj=fn_adj)

  if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=1, lwd=1, col="lavenderblush3")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=1, lwd=1, col="lavenderblush3")
    }}

  # Add ablines
  if(h1!="none") {abline(h=h1, lty=1, lwd=1, col="black")}
  if(v!="none") {abline(v=as.Date(v, "%d/%m/%Y"), lty=1, lwd=1)}
  }

#Create second y-axis, and content (if available)
if (l2!="none") {par(new = T)

  if (y2_def!="none") {plot(data2, plot.type="s", bty="n", ylim=ylim_input, col=(length(l1)+1):(length(d3)), axes=F, lwd=3, ann=FALSE)
    axis(4, seq(y2_def[1], y2_def[2], y2_def[3]), las=1, lwd=0, tck=0)
    } else {plot(data2, plot.type="s", bty="n", col=(length(l1)+1):(length(d3)), axes=F, lwd=3, las=1, ann=FALSE, tck=-0)
    axis(4, las=1, lwd=0, tck=0)}
    mtext(side = 4, line = 3, y2)

  legend(leg, box.lty=0, bg="#FFFFFF", legend=colnames(data3), pch=15, col=1:length(d3), ncol=1)
  } else {legend(leg, box.lty=0, bg="#FFFFFF", legend=colnames(data3), pch=15, col=1:length(d3), ncol=1)}

if (l2=="none") {box(which = "plot", bty = "l")} else {box(which = "plot", bty = "u")}

}
