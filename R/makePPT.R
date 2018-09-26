makePPT <- function(path, width, height, left, top, position, header, source, source_dt) {

library(devEMF)
library(officer)
  
if (missing(path))       {path        <- ""  }
if (missing(header))     {header      <- ""  }
if (missing(source))     {source      <- ""  }
if (missing(source_dt))  {source_dt   <-  paste(format(Sys.Date(), "%m %b %Y")) }
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
doc <- ph_with_text(doc, type="ftr", str=paste(source, source_dt, sep=" ,"))
doc <- ph_with_img_at(doc, src=emf_file, width=8, height=6, left=left, top=left)

}