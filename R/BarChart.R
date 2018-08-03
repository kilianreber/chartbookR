BarChart <- function(no, title, data, side,d1, d2, l1, xf, y1, y2, y1_def, y2_def, date_spc, dt_format, spc, leg, lgb) {

  #Set default values
  if (missing(no))        {no      <-  ""    }
  if (missing(y1_def))    {y1_def  <- "none" }
  if (missing(l1))        {l1  <- "none"     }
  if (missing(side))      {side  <- FALSE    }
  if (missing(y2_def))    {y2_def  <- "none" }
  if (missing(d2))        {d2      <- "none" }
  if (missing(spc))       {spc     <- 1      }
  if (missing(lgb))       {lgb     <- "n"    }
  if (missing(xf))        {xf      <- "%Y"   }
  if (missing(date_spc))     {date_spc      <- 1   }
  if (missing(dt_format)) {dt_format <- c("1 years", "%m-%Y")}

  # Format for having bars side by side:
  if(side == TRUE){spc <- NULL
  date_spc <- 2}

  if(spc == 0){
  date_spc <- 0}

  #Adjust title
  if (no!="") {title <- paste("Fig. ", no, ": ", title, sep="")}

  x_min <- min(index(data))
  x_max <- max(index(data))

  # Set TS that shoud be bars:
  d1 <- data[,d1]

  # Set what TS should be line plot
  if(d2!="none")  {
    d2 <- data[,d2]
  }

  #Set color Palette
  palette(c("#428bce", "#595959", "#CEBC9A", "#BF7057", "#ADAFB2", "#E7C667"))

  if (is.null(ncol(d1))) {d1L <- 1} else {d1L <- ncol(d1)}
  if (d2!="none") {if(is.null(ncol(d2))) {d2L <- 1} else {d2L <- ncol(d2)}} else {d2L <-0}

  #Create plot, first y-axis, and content
  par(mar = c(5,5,5,5))
  if (y1_def!="none") {barplot(d1, beside = side, names.arg=format(index(data), dt_format[2]), ann=FALSE, bty="n", tck=-0, col=1:d1L, yaxt="n", ylim=c(y1_def[1], y1_def[2]), border=NA, space=spc); title(main=title, ylab=y1)
  #axis(1, at=match(seq(as.Date(x_min), x_max, dt_format[1]),index(df))*(1+date_spc),labels = format(seq(as.Date(x_min), x_max, dt_format[1]),dt_format[2]),lwd=0)
    #Code to draw x-axis labels!
    index_y <- format(index(data), dt_format[2])
    index_u <- !duplicated(index_y)
    at_tick <- which(index_u)
    labels <- index_y[index_u]
    #bp <- barplot(df[,2], xaxt = "n", yaxt="n", ylab= "y-axis")
    #axis(side = 1, at = bp[at_tick], labels = labels, tck=-0, lwd = 0)

  axis(2, seq(y1_def[1], y1_def[2], y1_def[3]), las=1, tck=-0)

  } else {barplot(d1, beside = side, names.arg=format(index(data), dt_format[2]), ann=FALSE, bty="n", las=1, tck=-0, col=1:d1L, border=NA, space=spc)
    title(main=title, ylab=y1)
    #axis(1, at=match(seq(as.Date(x_min), x_max, dt_format[1]),index(d1))*(date_spc), labels = format(seq(as.Date(x_min), x_max, dt_format[1]),dt_format[2]),lwd=0)
    }

  #Create second y-axis, and content (if available)
  if (d2!="none") {par(new = T)
    if (y2_def!="none") {plot(d2, plot.type="s", bty="n", ylim=c(y2_def[1], y2_def[2]), col=(d1L+1):(d1L+d2L), axes=F, lwd=3, ann=FALSE)
      axis(4, seq(y2_def[1], y2_def[2], y2_def[3]), las=1, tck=-0)
    } else {plot(d2, plot.type="s", bty="n", col=(d1L+1):(d1L+d2L), axes=F, lwd=3, las=1, ann=FALSE)
      axis(4, las=1, tck=-0)}
    mtext(side = 4, line = 3, y2)

    legend(leg, bty=lgb, box.lty=0, bg="#FFFFFF", legend=colnames(data)[1:(d1L+d2L)], pch=15, col=1:(d1L+d2L), ncol=1)
  } else {legend(leg, bty=lgb, box.lty=0, bg="#FFFFFF", legend=colnames(data)[1:d1L], pch=15, col=1:d1L, ncol=1)}

  if (d2=="none") {box(which = "plot", bty = "l")} else {box(which = "plot", bty = "u")}
}
