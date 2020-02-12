# This function makes a "ranked date" plot where the 95% CI of conodont zone 
# durations are plotted as line segments.

# Requires ggplot2 to be installed and loaded.

# Updated 2019.12.19 CH.

# INPUTS:  dataframe    = data frame containing conodont duration timing and 
#                         identifiers, defaults conodontPositions
#          xpos         = scaled stratigraphic position of midpoint of conodont
#                         zone, defaults to conodontPositions$newPositions
#          xposend      = scaled stratigraphic position of midpoint of conodont 
#                         zone, defaults to conodontPositions$newPositions
#          ypos         = 2.5% confidence interval for age of conodont zone, 
#                         defaults to conodontPositions$`0.025`
#          yposend      = 97.5% confidence interval for age of conodont zone, 
#                         defaults to conodontPositions$`0.975`
#          segmentlabel = identifier/name of conodont zone, defaults to 
#                         conodontPositions$ids
# OUTPUTS: ConodontRankedDatePlot = plot of duration of each conodont zone

conodontPositionsPlot <- function(dataframe = conodontPositions,
                                  xpos = conodontPositions$newPositions,
                                  xposend = conodontPositions$newPositions,
                                  ypos = conodontPositions$`0.025`, 
                                  yposend = conodontPositions$`0.975`,
                                  segmentlabel = conodontPositions$ids) {
  
  # Make "ranked date" plot.
  ConodontRankedDatePlot <- ggplot(dataframe) +
    geom_segment(aes(x = xpos,
                     xend = xposend,
                     y = ypos, 
                     yend = yposend),
                 size = 1.5) +
    # Add zone labels.
    geom_text(mapping = aes(x = xpos,
                            y = ypos),
              label = segmentlabel,
              nudge_y = 5,
              size = 3,
              angle = 90) +
    # Reverse order of y axis.
    scale_y_reverse() +
    # Change ggplot2 theme.
    theme_bw() +
    # Remove gridlines and x axis labels.
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    # Label axes.    
    labs(x = "scaled stratigraphic position",  # Label x axis.
         y = expression({}^206*"Pb/"*{}^238*"U age (Ma)"))  # Label y axis.
  
  # Return plot.
  return(ConodontRankedDatePlot)
  
}