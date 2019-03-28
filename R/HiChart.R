############ PLOT HIGHCHARTER OBJECT #############
##################################################

#' Create highcharter object
#' 
#' Create highcharter object based on user inputs
#' 
#' @import highcharter
#' @import reshape2
#' @import zoo
#' @import xts
#' 
#' @param data specification of zoo dataset to use for plot
#' @param l1 optional integer vector to specify which columns to plot (as lines) on primary y-axis
#' @param l2 optional integer vector to specify which columns to plot (as lines) on secondary y-axis
#' @param c1 optional integer vector to specify which columns to plot (as columns) on primary y-axis
#' @param c2 optional integer vector to specify which columns to plot (as columns) on secondary y-axis
#' @param a1 optional integer vector to specify which columns to plot (as areas) on primary y-axis
#' @param a2 optional integer vector to specify which columns to plot (as areas) on secondary y-axis
#' @param series1 optional integer to override series name if only one series is available; default is "series1"
#' @param stacking optional integer to specify if and how bars or areas should be stacked
#' @param space optional integer to specify space between bars
#' @param title optional character to add chart title
#' @param title_adj optional integer to specify title alignment ("left", "right", "center")
#' @param y1 optional character to specify description of primary y-axis
#' @param y2 optional character to specify description of secondary y-axis
#' @param y1_def optional number vector to specify start, end, and intervals of primary y-axis, e.g. c(0, 10, 2)
#' @param y2_def optional number vector to specify start, end, and intervals of secondary y-axis, e.g. c(0, 10, 2)
#' @param y1_right optional boolean to display primary y-axis on right-hand side
#' @param y1_rev optional boolean to invert primary y-axis
#' @param y2_rev optional boolean to invert secondary y-axis
#' @param grid optional integer vector to specify line width of grid (horizontal, vertical)
#' @param lineWidth optional integer vector to specify width of lines
#' @param zoom optional boolean to show or hide zoom function
#' @param zoom_cst optional integer to specify custom zoom intervals, e.g. c("1W", "1M", "3M", "6M")
#' @param navigator optional boolean to show or hide range navigator
#' @param tooltip optional boolean to show or hide tooltip
#' @param stickyLabels optional boolean to turn on or off sticky labels
#' @param decimals optional integer to specify number of decimals shown in tooltip
#' @param size optional integer vector to specify chart output size
#' @param linePos optional integer vector to specify at which level (front, back) of the plot h1, h2, and v lines are drawn
#' @param h1 optional argument to specify horizontal line(s) at specific height on primary y-axis
#' @param h1_lab optional argument to specify horizontal line(s) labels on primary y-axis
#' @param h2 optional argument to specify horizontal line(s) at specific height on secondary y-axis
#' @param h2_lab optional argument to specify horizontal line(s) labels on secondary y-axis
#' @param v optional date to specify vertical date line(s), e.g. v = '2012-01-01'
#' @param v_lab optional argument to specify vertical date line label(s), e.g. v = '31/12/2012'
#' @param b1_from optional argument to specify start of horizontal range band(s) on primary y-axis
#' @param b1_to optional argument to specify end of horizontal range band(s) on primary y-axis
#' @param b1_lab optional argument to specify horizontal range band(s) label(s) on primary y-axis
#' @param b2_from optional argument to specify start of horizontal range band(s) on secondary y-axis
#' @param b2_to optional argument to specify end of horizontal range band(s) on secondary y-axis
#' @param b2_lab optional argument to specify horizontal range band(s) label(s) on secondary y-axis
#' @param v_from optional date(s) to specify start of vertical range band(s), e.g. v = '31/12/2012'
#' @param v_to optional date(s) to specify end of vertical range band(s), e.g. e.g. v = '31/12/2012'
#' @param vb_lab optional argument to specify label(s) of vertical range band(s)
#' @param rec optional boolean to shade recessions when set to TRUE
#' 
#' @return returns highchart object
#' 
#' @export

HiChart <- function(data, l1=NULL, l2 = NULL, a1 = NULL, a2 = NULL, c1 = NULL, c2 = NULL, series1 = NULL, stacking = NULL, space = 1.5, title = NULL, subtitle = NULL, title_adj = "left", y1 = "", y2 = "", y1_def = c(NULL, NULL, NULL), y2_def = c(NULL, NULL, NULL), y1_right = FALSE, y1_rev = FALSE, y2_rev = FALSE, grid =c(1,1), lineWidth = 3, zoom = TRUE, zoom_cst = "none", navigator = FALSE, tooltip = TRUE, stickyLabels = FALSE, decimals = 1, size = c(750, 600), linePos = 1, h1 = NULL, h1_lab = NULL, h2 = NULL, h2_lab = NULL, v = NULL, v_lab = NULL, b1_from = NULL, b1_to = NULL, b1_lab = NULL, b2_from = NULL, b2_to = NULL, b2_lab = NULL, v_from = NULL, v_to = NULL, vb_lab = NULL, rec = FALSE) {

# Default settings
if(is.null(l1) & is.null(l2) & is.null(a1) & is.null(a2) & is.null(c1) & is.null(c2)) {if(is.null(ncol(data))) {l1 <- 1} else {l1  <- 1:ncol(data) }}
if(is.null(series1))     {series1 <- "Series1"}
if(space!= 1.5) {space <- space - 0.33}

#PREPARE LINE VECTORS
  
#Create df1 and convert to Long Format
if(!is.null(l1)){
df1 <- data.frame(time = time(data[,l1]), data[,l1], check.names = FALSE)
df1 <- melt(df1, id.vars = "time")
}

#Adjust df1 variable name if necessary
if (length(l1) == 1 & !is.null(ncol(data))) {df1[2] <- colnames(data)[l1]}
if (length(l1) == 1 & is.null(ncol(data)))  {df1[2] <- series1}

#Create df2 and convert to Long Format
if(!is.null(l2)) {
  df2 <- data.frame(time=time(data[,l2]), data[,l2], check.names = FALSE)
  df2 <- melt(df2, id.vars = "time")  

#Adjust df2 variable name if necessary
    if (length(l2) == 1) {df2[2] <- colnames(data)[l2]}
    }

#Finalize df2 variable names with 'rhs' addage
if(!is.null(l2)){
  df2$variable <- paste0(df2$variable, " (rhs)", sep = "")
  }

#PREPARE AREA VECTORS

#Create df3 and convert to Long Format
if(!is.null(a1)) {
  df3 <- data.frame(time=time(data[,a1]), data[,a1], check.names = FALSE)
  df3 <- melt(df3, id.vars = "time")  
  
  #Adjust df3 variable name if necessary
  if (length(a1) == 1) {df3[2] <- colnames(data)[a1]}
}

#Create df4 and convert to Long Format
if(!is.null(a2)) {
  df4 <- data.frame(time=time(data[,a2]), data[,a2], check.names = FALSE)
  df4 <- melt(df4, id.vars = "time")  
  
  #Adjust df4 variable name if necessary
  if (length(a2) == 1) {df4[2] <- colnames(data)[a2]}
}

#Finalize df4 variable names with 'rhs' addage
if(!is.null(a2)){
  df4$variable <- paste0(df4$variable, " (rhs)", sep = "")
}

#PREPARE COLUMN VECTORS

#Create df5 and convert to Long Format
if(!is.null(c1)) {
  df5 <- data.frame(time=time(data[,c1]), data[,c1], check.names = FALSE)
  df5 <- melt(df5, id.vars = "time")  
  
  #Adjust df5 variable name if necessary
  if (length(c1) == 1) {df5[2] <- colnames(data)[c1]}
}

#Create df6 and convert to Long Format
if(!is.null(c2)) {
  df6 <- data.frame(time=time(data[,c2]), data[,c2], check.names = FALSE)
  df6 <- melt(df6, id.vars = "time")  
  
  #Adjust df6 variable name if necessary
  if (length(c2) == 1) {df6[2] <- colnames(data)[c2]}
}

#Finalize df6 variable names with 'rhs' addage
if(!is.null(c2)){
  df6$variable <- paste0(df6$variable, " (rhs)", sep = "")
}


#PREPARE CODE

#Create bold title
title <- paste("<b>", title, "</b>", sep = "")

#Grab current color palette from R
clrs <- palette()

#Get periodicity
per <- as.character(xts::periodicity(data)[6])

#Calculate custom zoom if zoom_cst is none and zoom is TRUE - for monthly data
if (zoom_cst == "none" & zoom == TRUE & per == "monthly") {
years <- as.numeric((tail(index(data),1) - index(data)[1])/365)
if (years > 0 & years <= 1)    {zoom_cst <- c("6M", "9M", "12M")          }
if (years > 1 & years <= 3)    {zoom_cst <- c("6M", "12M", "18M", "2Y")   }
if (years > 3 & years <= 5)    {zoom_cst <- c("6M", "12M", "2Y", "3Y")    }
if (years > 5 & years <= 10)   {zoom_cst <- c("6M", "1Y", "3Y", "5Y")     }
if (years > 10 & years <= 20)  {zoom_cst <- c("1Y", "3Y", "5Y", "10Y")    }
if (years > 20)                {zoom_cst <- c("3Y", "5Y", "10Y", "20Y")   }
}

#Calculate custom zoom if zoom_cst is none and zoom is TRUE - for weekly data
if (zoom_cst == "none" & zoom == TRUE & per == "weekly") {
  years <- as.numeric((tail(index(data),1) - index(data)[1])/365)
  if (years > 0 & years <= 1)    {zoom_cst <- c("3M", "6M", "9M")         }
  if (years > 1 & years <= 3)    {zoom_cst <- c("3M", "6M", "12M")        }
  if (years > 3 & years <= 5)    {zoom_cst <- c("3M", "6M", "1Y", "3Y")   }
  if (years > 5 & years <= 10)   {zoom_cst <- c("6M", "1Y", "3Y", "5Y")   }
  if (years > 10 & years <= 20)  {zoom_cst <- c("1Y", "3Y", "5Y", "10Y")  }
  if (years > 20)                {zoom_cst <- c("3Y", "5Y", "10Y", "20Y") }
}

#Calculate custom zoom if zoom_cst is none and zoom is TRUE - for daily data
if (zoom_cst == "none" & zoom == TRUE & per == "daily") {
  years <- as.numeric((tail(index(data),1) - index(data)[1])/365)
  if (years > 0 & years <= 1)    {zoom_cst <- c("1W", "1M", "3M", "6M")   }
  if (years > 1 & years <= 3)    {zoom_cst <- c("1W", "1M", "3M", "1Y")   }
  if (years > 3 & years <= 5)    {zoom_cst <- c("1M", "3M", "1Y", "3Y")   }
  if (years > 5 & years <= 10)   {zoom_cst <- c("1M", "3M", "1Y", "5Y")   }
  if (years > 10 & years <= 20)  {zoom_cst <- c("1M", "1Y", "5Y", "10Y")  }
  if (years > 20)                {zoom_cst <- c("1M", "5Y", "10Y", "20Y") }
}

#Calculate custom zoom if zoom_cst is none and zoom is TRUE - for quarterly data
if (zoom_cst == "none" & zoom == TRUE & per == "quarterly") {
  years <- as.numeric((tail(index(data),1) - index(data)[1])/365)
  if (years > 0 & years <= 2)    {zoom_cst <- NULL                          }
  if (years > 2 & years <= 3)    {zoom_cst <- c("12M", "18M", "2Y")         }
  if (years > 3 & years <= 5)    {zoom_cst <- c("12M", "18M", "2Y", "3Y")   }
  if (years > 5 & years <= 10)   {zoom_cst <- c("1Y", "3Y", "5Y")           }
  if (years > 10 & years <= 20)  {zoom_cst <- c("1Y", "3Y", "5Y", "10Y")    }
  if (years > 20)                {zoom_cst <- c("3Y", "5Y", "10Y", "20Y")   }
}

#Calculate custom zoom if zoom_cst is none and zoom is TRUE - for yearly data
if (zoom_cst == "none" & zoom == TRUE & per == "yearly") {
  years <- as.numeric((tail(index(data),1) - index(data)[1])/365)
  if (years > 0 & years <= 5)    {zoom_cst <- NULL                          }
  if (years > 5 & years <= 10)   {zoom_cst <- c("5Y", "10Y")                }
  if (years > 10 & years <= 20)  {zoom_cst <- c("5Y", "10Y", "15Y")         }
  if (years > 20)                {zoom_cst <- c("5Y", "10Y", "15Y", "20Y")  }
  }


#Create zoom range vectors
if (zoom == TRUE){
range_counts <- gsub("[^0-9.]", "",  zoom_cst)
range_counts <- range_counts[range_counts!=""]
range_types <- gsub('[[:digit:]]+', '', zoom_cst)
map = setNames(c("day", "week", "month", "year"), c("D", "W", "M", "Y"))
range_types[] <- map[unlist(range_types)]
}

#Load recession data
nber <- as.data.frame(chartbookR::NBER_Recessions)
rec_start <- as.Date(nber$Rec_Start)
rec_end <- as.Date(nber$Rec_End)

#No recessions if dates outside relevant area
if (index(data)[1] > as.Date(tail(nber$Rec_End, 1))) {rec <- FALSE}

#Subset relevant recession dates
rec_start_dt <- min(index(data))
rec_end_dt   <- max(index(data))
rec_start    <- subset(rec_start, rec_start >= as.Date(rec_start_dt))
items        <- length(rec_start)
rec_end      <- rec_end[(length(rec_end) - items + 1):length(rec_end)]

#Convert date formats
if (!is.null(v))       {v <- as.Date(v, "%d/%m/%Y")}
if (!is.null(v_from))  {v_from <- as.Date(v_from, "%d/%m/%Y")}
if (!is.null(v_to))    {v_to <- as.Date(v_to, "%d/%m/%Y")}

#Convert date vector to dates
if (!is.null(v))          {v <- datetime_to_timestamp(as.Date(v, tz="UTC"))}
if (!is.null(v_from))     {dates_open   <- datetime_to_timestamp(as.Date(v_from, tz="UTC"))}
if (!is.null(v_to))       {dates_close  <- datetime_to_timestamp(as.Date(v_to, tz="UTC"))}

if (!is.null(rec_start))  {rec_open     <- datetime_to_timestamp(as.Date(rec_start, tz="UTC"))}
if (!is.null(rec_end))    {rec_close    <- datetime_to_timestamp(as.Date(rec_end, tz="UTC"))}

#Create rec bands
if (rec == TRUE & !is.null(rec_start) & !is.null(rec_end)){
  rec_bands = list()
  for (i in 1:length(rec_open)) {
    temp  <- list(from = rec_open[i], to = rec_close[i], color="rgba(5, 0, 0, 0.1)", label = list(x = 7.5, y = 15, text = NULL))
    rec_bands[[length(rec_bands)+1]] <- temp
    i + 1}} else {rec_bands <- NULL}

#Create vBands
if (!is.null(v_from)){
xbands = list()
for (i in 1:length(dates_open)) {
  temp  <- list(from = dates_open[i], to = dates_close[i], color="rgba(100, 0, 0, 0.1)", label = list(x = 7.5, y = 15, text = vb_lab[i]))
  xbands[[length(xbands)+1]] <- temp
  i + 1}
} else {
  xbands <- NULL}

#Combine xbands and rec_bands into one
xbands_final <- c(xbands, rec_bands)

#Create y1_Bands
ybands = list()
for (i in 1:length(b1_from)) {
  temp  <- list(from = b1_from[i], to = b1_to[i], color="rgba(100, 0, 0, 0.1)", zIndex = 1, label = list(zIndex = 100, x = 20, align = "left", text = b1_lab[i]))
  ybands[[length(ybands)+1]] <- temp
  i + 1}

#Create y2_Bands
y2bands = list()
for (i in 1:length(b2_from)) {
  temp  <- list(from = b2_from[i], to = b2_to[i], color="rgba(100, 0, 0, 0.1)", zIndex = 1, label = list(zIndex = 100, x = -20, align = "right", text = b2_lab[i]))
  y2bands[[length(y2bands)+1]] <- temp
  i + 1}

#Create vLines
xlines = list()
for (i in 1:length(v)) {
  temp  <- list(color = "black", zIndex = linePos, width = 1.2, value = v[i], zIndex = 1, label = list(zIndex = 100, x = 7.5, y = 17.5, rotation = 0, text = v_lab[i]))
  xlines[[length(xlines)+1]] <- temp
  i + 1}

#Create h1_Lines
ylines = list()
for (i in 1:length(h1)) {
  temp  <- list(color = "black", zIndex = linePos, width = 1.2, value = h1[i], zIndex = 1, label = list(zIndex = 100, x = 20, align = "left", text = h1_lab[i]))
  ylines[[length(ylines)+1]] <- temp
  i + 1}

#Create h2_Lines
y2lines = list()
for (i in 1:length(h2)) {
  temp  <- list(color = "black", zIndex = linePos, width = 1.2, value = h2[i], zIndex = 1, label = list(zIndex = 100, x = -20, align = "right", text = h2_lab[i]))
  y2lines[[length(y2lines)+1]] <- temp
  i + 1}

#Create Ranges
if(zoom == TRUE){
ranges = list()
for (i in 1:length(zoom_cst)) {
  temp  <- list(type = range_types[i], count = range_counts[i], text = zoom_cst[i])
  ranges[[length(ranges)+1]] <- temp
  i + 1}
  ranges[[length(ranges)+1]] <- list(type = "ytd", text = "YTD")
  ranges[[length(ranges)+1]] <- list(type = "all", text = "All")
} else {ranges <- NULL}
  
#CREATE CHARTS
  
  #Highchart with primary y-axis
  if (is.null(l2) & is.null(a2) & is.null(c2)){
    chart <- highchart(type = "stock")
    if (!is.null(a1)) {chart <- hc_add_series(hc = chart, data = df3, type = "area", hcaes(x = time, y = value, group = variable))}
    if (!is.null(c1)) {chart <- hc_add_series(hc = chart, data = df5, type = "column", hcaes(x = time, y = value, group = variable))}
    if (!is.null(l1)) {chart <- hc_add_series(hc = chart, data = df1, type = "line", hcaes(x = time, y = value, group = variable))}
    chart <- hc_title(hc = chart, text = title, align = title_adj, style = list(color = "black"))
    chart <- hc_subtitle(hc = chart, text = subtitle, align = title_adj, style = list(color = "grey"))
    chart <- hc_add_theme(hc = chart, hc_theme_flat(colors = clrs, chart = list(backgroundColor = "white") ))
    chart <- hc_xAxis(hc = chart, title = list(text = ""), labels = list(style = list(color = "black")), tickColor="grey", lineColor = "grey", gridLineWidth = grid[2], plotLines = xlines, plotBands = xbands_final)
    chart <- hc_yAxis(hc= chart, reversed = y1_rev, min = y1_def[1], max = y1_def[2], title = list(text = y1, style = list(color = "black")), labels = list(style = list(color = "black")), tickColor="grey", lineWidth = 1, lineColor = "grey", gridLineWidth = grid[1], opposite = y1_right, plotLines = ylines, plotBands = ybands)
    chart <- hc_plotOptions(hc = chart, line = list(lineWidth = lineWidth, stickyTracking = FALSE), column = list(stickyTracking = FALSE, dataLabels = list(enabled = FALSE), pointPadding = space, stacking = stacking), area = list(stickyTracking = FALSE, dataLabels = list(enabled = FALSE), stacking = stacking), series = list(dataGrouping = list(enabled = FALSE)))
    chart <- hc_legend(hc = chart, enabled = TRUE, align = "center")
    chart <- hc_rangeSelector(hc = chart, enabled = zoom, buttons = ranges)
    chart <- hc_navigator(hc = chart, enabled = navigator)
    chart <- hc_scrollbar(hc = chart, enabled = FALSE)
    chart <- hc_tooltip(hc = chart, split = stickyLabels, shared = FALSE, enabled = tooltip, valueDecimals = decimals)
    chart <- hc_size(hc = chart, width = size[1], height = size[2])
    return(chart)
  }
  
  #Highchart with secondary y-axis  
  if (!is.null(l2) || !is.null(a2) || !is.null(c2)){
    chart <- highchart(type = "stock")
    
    if (!is.null(a1)) {chart <- hc_add_series(hc = chart, data = df3, type = "area",   hcaes(x = time, y = value, group = variable))}
    if (!is.null(a2)) {chart <- hc_add_series(hc = chart, data = df4, type = "area",   yAxis = 1, hcaes(x = time, y = value, group = variable))}
    if (!is.null(c1)) {chart <- hc_add_series(hc = chart, data = df5, type = "column", hcaes(x = time, y = value, group = variable))}
    if (!is.null(c2)) {chart <- hc_add_series(hc = chart, data = df6, type = "column", yAxis = 1, hcaes(x = time, y = value, group = variable))}
    if (!is.null(l1)) {chart <- hc_add_series(hc = chart, data = df1, type = "line",   hcaes(x = time, y = value, group = variable))}
    if (!is.null(l2)) {chart <- hc_add_series(hc = chart, data = df2, type = "line",   yAxis = 1, hcaes(x = time, y = value, group = variable))}
    
    chart <- hc_title(hc = chart, text = title, align = title_adj, style = list(color = "black"))
    chart <- hc_subtitle(hc = chart, text = subtitle, align = title_adj, style = list(color = "grey"))
    chart <- hc_add_theme(hc = chart, hc_theme_flat(colors = clrs, chart = list(backgroundColor = "white") ))
    chart <- hc_xAxis(hc = chart, title = list(text = ""), labels = list(style = list(color = "black")), tickColor="grey", lineColor = "grey", gridLineWidth = grid[2], plotLines = xlines, plotBands = xbands_final)
    chart <- hc_yAxis_multiples(hc = chart, list(title = list(text = y1, style = list(color = "black")), labels = list(style = list(color = "black")), tickColor = "grey", lineWidth = 1, lineColor = "grey", reversed = y1_rev, min = y1_def[1], max = y1_def[2], gridLineWidth = grid[1], opposite=FALSE, plotLines = ylines, plotBands = ybands), list(title = list(text = y2, style = list(color = "black")), labels = list(style = list(color = "black")), tickColor = "grey", lineWidth = 1, lineColor = "grey", gridLineWidth = 0, reversed = y2_rev, min = y2_def[1], max = y2_def[2], alignTicks = FALSE, opposite=TRUE, plotBands = y2bands, plotLines = y2lines))
    chart <- hc_plotOptions(hc = chart, line = list(lineWidth = lineWidth, stickyTracking = FALSE), column = list(stickyTracking = FALSE, dataLabels = list(enabled = FALSE), pointPadding = space, stacking = stacking), area = list(stickyTracking = FALSE, dataLabels = list(enabled = FALSE), stacking = stacking), series = list(dataGrouping = list(enabled = FALSE)))
    chart <- hc_legend(hc = chart, enabled = TRUE, align = "center")
    chart <- hc_rangeSelector(hc = chart, enabled = zoom, buttons = ranges)
    chart <- hc_navigator(hc = chart, enabled = navigator)
    chart <- hc_scrollbar(hc = chart, enabled = FALSE)
    chart <- hc_tooltip(hc = chart, split = stickyLabels, shared = FALSE, enabled = tooltip, valueDecimals = decimals)
    chart <- hc_size(hc = chart, width = size[1], height = size[2])
    return(chart)
  }
}