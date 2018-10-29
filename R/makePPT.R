################ CREATE PPT SLIDE ################
##################################################

#' Create Powerpoint slide from R plot viewer
#' 
#' Adds plot viewer content to Powerpoint slide; it relies on the package 'officer'
#' 
#' @import devEMF
#' @import officer
#' 
#' @param path required string to specify path for plot viewer temp file
#' @param header optional character string to specify Powerpoint slide header
#' @param source optional character string to specify Powerpoint slide source note
#' @param source_dt optional character string to specify Powerpoint slide source date
#' @param width optional integer to specify chart width
#' @param height optional integer to specify chart height
#' @param left optional integer to specify left-hand margins on Powerpoint slide
#' @param left optional integer to specify top margins on Powerpoint slide
#' 
#' @return Adds plot viewer content to Powerpoint slide
#' 
#' @export
#' 
#' @examples
#' Before charts can be added to a Powerpoint file, it needs to be initiated using a pptx Template file via: 
#' doc <- read_pptx("C:/Documents/Powerpoint/Template.pptx")
#' 
#' Charts can then be added via: 
#' makePPT(path="C:/temp", header="US GDP Figures", source="Bloomberg, Fed. Saint Louis")
#' 
#' Once all charts are added, the Powerpoint file is saved via: 
#' print(doc, target="C:/Documents/Powerpoint/US_Macro_Slideset.pptx")


makePPT <- function(path, width, height, left, top, header, source, source_dt) {

if (missing(path))       {path        <- ""  }
if (missing(header))     {header      <- ""  }
if (missing(source))     {source      <- ""  }
if (missing(source_dt))  {source_dt   <-  paste(format(Sys.Date(), "%d %b %Y")) }
if (source=="")          {source_dt   <- ""}
if (missing(width))      {width       <- 8   }
if (missing(height))     {height      <- 6   }
if (missing(left))       {left        <- 1   }
if (missing(top))        {top         <- 1   }

#Save current plot as enhanced metafile
emf_file <- paste(path, "temp.emf", sep="")
dev.copy(emf, emf_file, width=width, height=height)
dev.off ()

# Create Powerpoint and add plot
doc <- add_slide(doc, "Title and Content", "Default Theme")
doc <- ph_with_text(doc, type="title", str=header)
doc <- ph_with_text(doc, type="ftr", str=paste(source, source_dt, sep=", "))
doc <- ph_with_img_at(doc, src=emf_file, width=8, height=6, left=left, top=left)

}