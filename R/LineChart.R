LineChart <- function(no, title, data, d1, d2, y1, y2, y1_def, y2_def, y2_rev, fn, leg, grid, rec, dt_format, h, v) {

#Turn off warnings
options(warn=-1)
  
#Load libraries
library(xlsx)
lapply(c("quantmod", "zoo", "stringr", "MALDIquant"), library, character.only = TRUE)

#Set default values
if (missing(title))       {title   <- ""    }
if (missing(d1) &  is.null(ncol(data)))  {d1 <- 1}
if (missing(d1) & !is.null(ncol(data)))  {d1 <- c(1:ncol(data))}
if (missing(y1))          {y1 <- ""               }
if (missing(y1_def))      {y1_def  <- "none"      }
if (missing(y2_def))      {y2_def  <- "none"      }
if (missing(y2))          {y2      <- ""          }
if (missing(d2))          {d2      <- "none"      }
if (missing(fn))          {fn      <-  ""         }
if (missing(no))          {no      <-  ""         }
if (missing(grid))        {grid    <-  FALSE      }
if (missing(rec))         {rec     <-  FALSE      }
if (missing(leg))         {leg     <- "topleft"   }
if (missing(h))           {h       <- "none"      }
if (missing(v))           {v       <- "none"      }
if (missing(y2_rev))      {y2_rev  <- FALSE       }
if (y2_def!="none")       {ylim_input=c(y2_def[1], y2_def[2])}
if (y2_def!="none" & y2_rev==TRUE) {ylim_input=rev(range(c(y2_def[1], y2_def[2])))}

#Set dt_format defaults (if user provides none)
if (missing(dt_format))   
  {
  day_diff <- as.numeric(tail(index(data),1) - index(data)[1])
  if(day_diff <= 1.5*365) 				                 {dt_format <- c("3 mon", "%b-%Y")}
  if((day_diff > 1.5*365) & (day_diff <=3*365)) 	 {dt_format <- c("6 mon", "%b-%Y")}
  if((day_diff > 3  *365) & (day_diff <=5*365)) 	 {dt_format <- c("1 years", "%Y") }
  if((day_diff > 5  *365) & (day_diff <=12*365)) 	 {dt_format <- c("2 years", "%Y") }
  if((day_diff > 12  *365) & (day_diff <=20*365))  {dt_format <- c("3 years", "%Y") }
  if(day_diff > 20*365)                            {dt_format <- c("5 years", "%Y") }
  }

#Load recession data & reset recession indicator if necessary
nber <- NBER_Recessions
if (index(data)[1] > as.Date(tail(nber$Rec_End, 1))) {rec <- FALSE}

#Adjust title
if (no!="") {title <- paste("Fig. ", no, ": ", title, sep="")}

#Set color Palette (only if currently standard)
if (palette()==c("#428BCE", "gray35", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667")) {palette(c("#428bce", "#595959", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667"))}
if (palette()==c("black", "red", "green3", "blue", "cyan", "magenta")) {palette(c("#428bce", "#595959", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667"))}

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

# Getting min, max date points & date formats
# x_min_dt  <-  min(index(data1))
# x_max_dt  <-  max(index(data1))
# by_yrs    <-  dt_format[1]
# dt_format <-  dt_format[2]

### THIS IS NEW ###
#Create labels and tick vectors
bp_param <- Barplot_param(data=data1, stacked=TRUE, dt_format=dt_format)



#Create plot, first y-axis, and content
par(mar = c(5,5,5,5))

if (y1_def!="none") {plot(data1, plot.type="s", ann=FALSE, bty="n", ylim=c(y1_def[1], y1_def[2]), xaxt="n", yaxt="n", tck=0, las=1, lwd=3, col=1:length(d1), border=0.1, space=0); title(main = title, ylab = y1)
  
  axis(side=1, at=bp_param[,3], labels=format(bp_param[,3], dt_format[2]), las=1, tck=0)
  axis(2, seq(y1_def[1], y1_def[2], y1_def[3]), las=1, tck=-0)
  title(sub=fn, font.sub=3, line = 3)

    # Add ablines
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
    if(v!="none") {abline(v=as.Date(v, "%d/%m/%Y"), lty=1, lwd=1)}

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
    if (y1_def=="none") {grid(NA, ny=NULL, lty=3, lwd=1, col="#424447")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=3, lwd=1, col="#424447")}}

} else {plot(data1, plot.type="s", ann=FALSE, bty="n", xaxt="n", tck=0, las=1, las=1, lwd=3, col=1:length(d1), border=0.1, space=0); title(main=title, ylab=y1)
  
  axis(side=1, at=bp_param[,3], labels=format(bp_param[,3], dt_format[2]), las=1, tck=0)
  
    # Add ablines
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
    if(v!="none") {abline(v=as.Date(v, "%d/%m/%Y"), lty=1, lwd=1)}

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

    rec_start_dt <- min(index(data1))
    rec_end_dt <- max(index(data1))

    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]

    #Add recession shading
    rect(as.Date(rec_start), rect_min, as.Date(rec_end), rect_max, density=NULL, border=NA, lwd=0, col= rgb(0,0,0.1, alpha=0.15))
    }

  title(sub=fn, font.sub=3, line = 3)

  if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=3, lwd=1, col="#424447")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    abline(h=seq, lty=3, lwd=1, col="#424447")
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
