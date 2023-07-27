wimGraph <- function(background="white", panelBackground=NA,
                     panelBorder = "black",
                     textCol="black", titleCol="black", 
                     axisCol="black",gridCol="gray90",
                     stripCol="black", stripFill="white",
                     title.rel=1.2, axis.title.rel=1.6, axis.text.rel=1.0,
                     strip.text.rel=0.7, textSize=10)
{ 
  theme(panel.border = element_rect(colour = panelBorder, fill=NA, size=0.5),
        plot.background = element_rect(fill = background, linetype=0),
        panel.background = element_rect(fill = panelBackground, linetype=1, color=axisCol),
        legend.key = element_rect(fill = NA),
        legend.background=element_rect(fill=NA),
        text= element_text(color=textCol, size=textSize),
        panel.grid.major = element_line(color=gridCol, size=0.2),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color=stripCol, fill=stripFill),
        strip.text = element_text(color=textCol, size=rel(strip.text.rel)),
        plot.title = element_text(size=rel(title.rel), color=titleCol),
        axis.title  = element_text(size=rel(axis.title.rel), color=titleCol, vjust=0),
        axis.text = element_text(size=rel(axis.text.rel),color=axisCol),
        axis.line.x =  element_line(color=axisCol),			# Changed these two
        axis.line.y =  element_line(color=axisCol),
        axis.ticks=element_line(color=axisCol))
}

wimPalettes <- function (pal="point", ncolors=12, 
                         red=seq(0,1, length=ncolors), 
                         green=0, blue=seq(1,0,length=ncolors))
{
  # Selections are "colorblind","gradient","point","line", "slide" as of 10/17/2016
  # Added "month" for monthly colors from linear interpolation 
  #     of blue,green,red,orange, and back to blue (January April July)
  # Gradient uses rgb for a selected number of colors ncolors.
  # Gradient also uses the red, green, and blue values above
  # Default gradient is blue to red through purple. 
  require(grDevices)
  nc <- ceiling(ncolors/7)
  palC <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  palG <- rgb(red=red, green=green, blue=blue)
  palP <- rep(c("black","red","green4","blue","magenta","cyan","orange","gray"),nc)
  palL <- rep(c("red","black","green4","magenta","blue","gray","cyan","orange"),nc)
  palM <- c("#0000ff", "#0055aa", "#00aa55", "#00ff00", "#55aa00", "#aa5500", 
            "#ff0000", "#ff4400", "#ff8800", "#ffcc00", "#aa8855", "#5544aa")  # Blue..Green..Red..Orange..(blue)
  palO <- c("seagreen4", "red","darkgoldenrod4", "royalblue4","coral","gray50", "lightblue1","turquoise4","darkblue")
  palS <- rep(c("yellow","green","cyan","magenta","gray90","pink","orange"),nc)
  switch(tolower(substring(pal,1,1)), "c"=palC, "g"=palG, "p"=palP, "l"=palL,"m"=palM, "o"=palO, "s"=palS, palP)
}

