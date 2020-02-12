# This function makes a plot to visualize the stratigraphic correction after 
# each model run. The function plots the continuous deposition line, 
# the midpoint of the HDI, the positions of the dated horizons on each of those 
# lines, and the stratigraphic difference between the points on the two lines.

# Requires ggplot2 to be installed and loaded.

# Updated 2019.12.19 CH.

# INPUTS:  continuousDepositionLine_list = list containing values for the 
#                                          continuous deposition 1:1 line, 
#                                          defaults to continuousDepositionLine
#          HDImidpoint_DF                = data frame containing data for 
#                                          plotting the midpoint of the HDI from 
#                                          the age model, defaults to 
#                                          HDImidpoint
#          datedHorizon_DF               = data frame containing the position of 
#                                          the dated horizions on the HDI and 
#                                          the continuous deposition line, 
#                                          defaults to datedHorizon
# OUTPUTS: ConodontRankedDatePlot        = plot of duration of each conodont 
#                                          zone

stratigraphicCorrectionPlot <- function(
  continuousDepositionLine_list = continuousDepositionLine,
  HDImidpoint_DF = HDImidpoint,
  datedHorizon_DF = datedHorizon) {

# Make plot to visualize the stratigraphic correction.
stratCorrPlot <- ggplot() +
  
  # Plot continuous deposition
  geom_line(aes(x = c(-10, 110), 
                y = c(((continuousDepositionLine$m * - 10) + 
                         continuousDepositionLine$b), 
                      ((continuousDepositionLine$m * 110) + 
                         continuousDepositionLine$b))),
            color = "black",
            linetype = "dashed") +
  
  # Plot HDI from model.
  geom_line(aes(x = HDImidpoint$stratPosition,
                y = HDImidpoint$midpoint),
            color = "black") +
  
  # Plot the difference between the HDI and the continuous deposition line for 
  # the dated horizons.
  geom_segment(aes(x = datedHorizon$continousDepositionPosition,
                   xend = datedHorizon$continousDepositionPosition - datedHorizon$stratCorrection,
                   y = datedHorizon$`0.5`,
                   yend = datedHorizon$`0.5`),
               size = 1,
               color = "lightblue") +
  
  # Plot locations of dated horizons on HDI.
  geom_point(aes(x = datedHorizon$newPositions,
                 y = datedHorizon$`0.5`),
             color = "blue") +
  
  # Add labels for dated horizons.
  geom_text(mapping = aes(x = datedHorizon$newPositions,
                          y = datedHorizon$`0.5`),
            label = datedHorizon$ids,
            nudge_x = 3, nudge_y = -0.5,
            size = 3) +
  
  # Plot locations of dated horizons on continuous deposition line.
  geom_point(aes(x = datedHorizon$continousDepositionPosition,
                 y = datedHorizon$`0.5`),
             color = "purple") +
  
  # Reverse order of y axis.
  scale_y_reverse() +
  
  # Change ggplot2 theme.
  theme_bw() +
  
  # Remove gridlines and x axis labels.
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  
  # Label axes.    
  labs(x = "scaled stratigraphic position",  # Label x axis.
       y = expression({}^206*"Pb/"*{}^238*"U age (Ma)"))  # Label y axis.
  
  # Return plot.
  return(stratCorrPlot)
}