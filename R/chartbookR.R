#' Creation of chartbooks
#' 
#' chartbookR: Offers convenient download and transformation of Bloomberg and FRED (St. Louis Fed) data, plotting of customized charts, and saving to Powerpoint.
#'
#' @section Details:
#'The chartbookR package consists of four types of functions for chartbook creation:
#' 
#' \itemize{
#' \item Data download: \code{\link[chartbookR]{getBBG}} and \code{\link{getFRED}}
#' \item Data transformation: \code{\link{Transform}}
#' \item Customized plotting: \code{\link{LineChart}} and \code{\link{BarChart}}
#' \item Exporting to Powerpoint: \code{\link{makePPT}}
#' }
#'  
#' @section Notes:
#' While all functions are based on existing packages, they make the chartbook creation an efficient and convenient process.
#' 
#' In order to use the \code{\link{getBBG}} function, you need to have a running Bloomberg terminal session. If this is not the case, you can still use \code{\link{getFRED}} to download freely available data from the St. Louis Fed database (\url{https://fred.stlouisfed.org}).
#' 
#' @section Author/Maintainer:
#' Kilian Reber \email{kilian.d.reber@gmail.com}
#'  
#' @section Contributors:
#' Tryggvi Thoroddsen \email{tryggvithoroddsen@gmail.com}
#'  
#' @section See Also:
#' \url{https://github.com/kilianreber}
#' 
#' @examples
#' #US Unemployment (FRED data only)
#' zoo <- getFRED(tickers=c('UNRATE', 'NROU'), names=c('Unemployment Rate', 'Natural Unemployment Rate'), na=FALSE)
#' LineChart(title="US Unemployment", data=zoo, y1='in percent', rec=TRUE)
#' 
#' #Global Unemployment Rates (requires Bloomberg)
#' zoo <- getBBG(tickers=c("EHUPUS", "EHUPEU", "EHUPGB", "EHUPJP Index"), names=c("United States", "Eurozone", "United Kingdom", "Japan"), freq="QUARTERLY", time="30Y")
#' LineChart(title="Global Unemployment Rates", data=zoo, d1=1:4, y1="in %", y1_def=c(2,15,2), grid=TRUE, rec=TRUE)
#' 
#' #US Budget Balance vs. UE Rate (requires Bloomberg)
#' zoo <- getBBG(tickers=c("EHUPUS", "EHBBUS"), na=FALSE, names=c("Unemployment", "Budget Balance"), start=as.Date("1969-01-01"))
#' LineChart(title="US Budget Balance vs. Unemployment Rate", data=zoo, d1=1, d2=2, y1="Unemployment (%)", y2="Budget Balance (%GDP)", y2_def=c(-10,2,2), y2_rev=TRUE, leg="top", rec=TRUE)
#' 
#' @docType package
#' @name chartbookR
NULL