BarChart <- function(no, title, data, stacked, d1, d2, y1, y2, y1_def, y2_def, y2_rev, space, fn, leg, grid, rec, dt_format, h, v) {

#Turn off warnings
options(warn=-1)
  
#Load libraries
lapply(c("quantmod", "zoo", "stringr", "MALDIquant"), library, character.only = TRUE)

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
bars_width <- 1

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

#Create data vectors and data column vectors
data1 <- as.data.frame(data[,d1])
colnames(data1) <- colnames(data)[d1]
data1 <- as.zoo(data1)

if(d2!="none")  {
  data2 <- as.data.frame(data[,d2])
  colnames(data2) <- colnames(data)[d2]
  data2 <- as.zoo(data2)
}

if(d2!="none")  {d3 <- c(d1, d2)} else {d3 <- d1}
if(d2!="none")  {data3 <- data[,d3]} else {data3 <- data1}

#Create labels and tick vectors
bp_param <- Barplot_param(data=data, d1=d1, stacked=stacked, dt_format=dt_format)

#Create plot, first y-axis, and content
par(mar = c(5,5,5,5))

if (y1_def!="none") {bp <- barplot(data1, beside=!stacked, ylim=c(y1_def[1], y1_def[2]), space=space, ann=FALSE, xaxt = "n", yaxt="n", tck=0, las=1, lwd=3, col=1:length(d1), border=NA); title(main=title, ylab=y1)
  axis(side=1, at=bp[as.integer(bp_param[1,])], labels=bp_param[2,], las=1, tck=0)
  axis(2, seq(y1_def[1], y1_def[2], y1_def[3]), las=1, tck=-0)
  title(sub=fn, font.sub=3, line = 3)

    #Add horizontal abline
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
    
    #Add vertical abline
    if(v!="none") {
    v <- as.Date(v, "%d/%m/%Y")
    v_tick <- match.closest(v, index(data))
    if (stacked==FALSE) {v_tick <- (v_tick*((length((d1))*bars_width)+((length(d1)-1)*space[1])+space[2]))+(0.5*space[2])}
    if (stacked==TRUE)  {v_tick <- (v_tick*bars_width)*(1+space)+0.5*space}
    abline(v=v_tick, lty=1, lwd=2, col="black")}
    
    #Prepare recession shading
    if (rec!=FALSE){

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
      if (max(rowSums(data1, na.rm=TRUE)) > 0) {rect_max <- max(rowSums(data1, na.rm=TRUE))*2} else {rect_max <- max(rowSums(data1, na.rm=TRUE))*0.5}
      if (min(rowSums(data1, na.rm=TRUE)) > 0) {rect_min <- min(rowSums(data1, na.rm=TRUE))*0.5} else {rect_min <- min(rowSums(data1, na.rm=TRUE))*2}
    }
      
      
    rec_start_dt <- min(index(data1))
    rec_end_dt <- max(index(data1))
    
    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]

    #Convert date vectors to ticks
    rec_start <- match.closest(rec_start, index(data))
    rec_end <- match.closest(rec_end, index(data))
    
    #Set bars_width
    if (stacked==FALSE) {bars_width <- length(d1)}
    
    #Add recession shading
    rec_start <- (rec_start*bars_width)*(1+space)
    rec_end <- (rec_end*bars_width)*(1+space)
    rect(rec_start, rect_min, rec_end, rect_max, density=NULL, border=NA, lwd=0, col= rgb(0,0,0.1, alpha=0.1))
    }

    if (grid!=FALSE) {
    if (y1_def=="none") {grid(NA, ny=NULL, lty=3, lwd=1, col="#424447")}
    else {seq <- seq(y1_def[1], y1_def[2], y1_def[3])
    }}
  
  } else {bp <- barplot(data1, beside=!stacked, ann=FALSE, bty="n", xaxt = "n", space=space, tck=0, las=1, lwd=1, col=1:length(d1), border=NA); title(main=title, ylab=y1)
    axis(side=1, at=bp[as.integer(bp_param[1,])], labels=bp_param[2,], las=1, tck=0)

    #Add horizontal abline
    if(h!="none") {abline(h=h, lty=1, lwd=1, col="black")}
  
    #Add vertical abline
    if(v!="none") {
    v <- as.Date(v, "%d/%m/%Y")
    v_tick <- match.closest(v, index(data))
    if (stacked==FALSE) {v_tick <- (v_tick*((length((d1))*bars_width)+((length(d1)-1)*space[1])+space[2]))+(0.5*space[2])}
    if (stacked==TRUE)  {v_tick <- (v_tick*bars_width)*(1+space)+0.5*space}
    abline(v=v_tick, lty=1, lwd=2, col="black")}

    #ADD RECESSION SHADING
    if (rec!=FALSE){

    #Load recession data & create date vectors
    rec_start <- as.Date(nber$Rec_Start)
    rec_end   <- as.Date(nber$Rec_End)

    #Create limits for rects
    mymax <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
    mymin <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

    if (stacked==FALSE) {
      if (mymax(data1) > 0) {rect_max <- mymax(data1)*2} else {rect_max <- mymax(data1)*0.5}
      if (mymin(data1) > 0) {rect_min <- mymin(data1)*0.5} else {rect_min <- mymin(data1)*2}
    }
    
    if (stacked==TRUE) {
      if (max(rowSums(data1, na.rm=TRUE)) > 0) {rect_max <- max(rowSums(data1, na.rm=TRUE))*2} else {rect_max <- max(rowSums(data1, na.rm=TRUE))*0.5}
      if (min(rowSums(data1, na.rm=TRUE)) > 0) {rect_min <- min(rowSums(data1, na.rm=TRUE))*0.5} else {rect_min <- min(rowSums(data1, na.rm=TRUE))*2}
    }

    rec_start_dt <- min(index(data1))
    rec_end_dt <- max(index(data1))
    
    #Trim date vectors
    rec_start <- subset(rec_start, rec_start>=as.Date(rec_start_dt))
    items <- length(rec_start)
    rec_end <- rec_end[(length(rec_end)-items+1):length(rec_end)]
  
    #Convert date vectors to ticks
    rec_start <- match.closest(rec_start, index(data))
    rec_end <- match.closest(rec_end, index(data))
    
    #Set bars_width
    if (stacked==FALSE) {bars_width <- length(d1)}
    
    #Add recession shading
    rec_start <- (rec_start*bars_width)*(1+space)
    rec_end   <- (rec_end*bars_width)*(1+space)
    rect(rec_start, rect_min, rec_end, rect_max, density=NULL, border=NA, lwd=0, col=rgb(0,0,0.1, alpha=0.1))
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
