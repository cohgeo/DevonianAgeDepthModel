# This script creates a graph to compare the Stage base ages from different 
# scales from the literature and from this project.

# Updated 2020.08.28 CH

## IMPORT TABLE OF VALUES ------------------------------------------------------

# Clear all from workspace/environment.
  # rm(list = ls())  # Uncomment if needed

# Read in table of ages and uncertainties for each scale.
DF <- read.csv("Lit_ages_and_result_ages.csv",
               header = TRUE)

## MAKE PLOT -------------------------------------------------------------------

# Make a list of x labels.
xlabels <- c("Kaufmann (2006)",	
             "GTS2012 (Becker et al., 2012)",	
             "De Vleeschouwer and Parnell, 2014 (including astrochronology)",	
             "GTS2020 (Becker et al., 2020)",	
             "this study, Kaufmann scale",	
             "this study, Becker 2012 scale",	
             "this study, Becker 2020 scale")


# Plot Kaufmann (2006) literature values.
plot(xlab = "",
  xlim = c(0, 8),
     ylab = "Age (Ma)",
     ylim = c(355, 425),
     main = "Scale comparison",
     x = DF$lit_Kaufmann_x, 
     y = DF$lit_Kaufmann_age,
     pch = 19, col = "red3",
     xaxt = "n")
arrows(x0 = DF$lit_Kaufmann_x,
       y0 = DF$lit_Kaufmann_age - DF$lit_Kaufmann_uncertaintyminus,
       y1 = DF$lit_Kaufmann_age + DF$lit_Kaufmann_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "red3")
text(x = 1:7, y = par("usr")[3] - 0.45, labels = xlabels,
     xpd = NA, srt = 25, adj = 0.965, cex = 0.8)

# Plot Becker et al. (2012) literature values.
points(x = DF$lit_Becker2012_x, 
       y = DF$lit_Becker2012_age,
       pch = 19, col = "orange")
arrows(x0 = DF$lit_Becker2012_x,
       y0 = DF$lit_Becker2012_age - DF$lit_Becker2012_uncertaintyminus,
       y1 = DF$lit_Becker2012_age + DF$lit_Becker2012_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "orange")

# Plot De Vleeschouwer and Parnell (2014) literature values.
points(x = DF$lit_DeVandP2014_x, 
       y = DF$lit_DeVandP2014_age,
       pch = 19, col = "palegoldenrod")
arrows(x0 = DF$lit_DeVandP2014_x,
       y0 = DF$lit_DeVandP2014_age - DF$lit_DeVandP2014_uncertaintyminus,
       y1 = DF$lit_DeVandP2014_age + DF$lit_DeVandP2014_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "palegoldenrod")


# Plot Becker et al. (2020) literature values.
points(x = DF$lit_Becker2020_x, 
       y = DF$lit_Becker2020_age,
       pch = 19, col = "palegreen3")
arrows(x0 = DF$lit_Becker2020_x,
       y0 = DF$lit_Becker2020_age - DF$lit_Becker2020_uncertaintyminus,
       y1 = DF$lit_Becker2020_age + DF$lit_Becker2020_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "palegreen3")

# Plot Kaufmann scale model values.
points(x = DF$Kaufmann_x, 
       y = DF$Kaufmann_age,
       pch = 19, col = "royalblue")
arrows(x0 = DF$Kaufmann_x,
       y0 = DF$Kaufmann_age - DF$Kaufmann_uncertaintyminus,
       y1 = DF$Kaufmann_age + DF$Kaufmann_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "royalblue")

# Plot Becker 2012 scale model values.
points(x = DF$Becker2012_x, 
       y = DF$Becker2012_age,
       pch = 19, col = "purple")
arrows(x0 = DF$Becker2012_x,
       y0 = DF$Becker2012_age - DF$Becker2012_uncertaintyminus,
       y1 = DF$Becker2012_age + DF$Becker2012_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "purple")

# Plot Becker 2020 scale model values.
points(x = DF$Becker2020_x, 
       y = DF$Becker2020_age,
       pch = 19, col = "black")
arrows(x0 = DF$Becker2020_x,
       y0 = DF$Becker2020_age - DF$Becker2020_uncertaintyminus,
       y1 = DF$Becker2020_age + DF$Becker2020_uncertaintyplus,
       length = 0.05, angle = 90, code = 3, col = "black")
