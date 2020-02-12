# This script runs an age-depth model on Devonian dates and conodont zones to
# predict the timing and duration of stage boundaries and to compare different 
# conodont zonation scales.

# This script follows (and borrows some of its code from) a file titled
# GTS12_SplineFigure_age_model_020519.R which was emailed to me by Mark Schmitz 
# on 2019.02.06. Either Mark Schmitz or Robin Trayler is the author of most of 
# the code below in the "SETUP 1: GTS12 only" section, and the comments explain 
# what data I input and why. For the other sections I reused the "SETUP 1: GTS12 
# only" code and added my own. The stratigraphic correction is my own code.

# Updated 2019.12.19 CH


## SETUP -----------------------------------------------------------------------

# Install and load modifiedBChron from Robin Trayler's GitHub repository.
# install.packages("devtools")  # Uncomment if needed
# devtools::install_github("robintrayler/modifiedBChron")  # Uncomment if needed
library(modifiedBChron)

# Set working directory.
setwd("/Users/claireharrigan/Dropbox/IGL + research/Devonian manuscript/DevonianAgeDepthModel")


## SETUP 1: GTS12 only -----------------------------------------------------

# In this setup, I used only GTS12 age data as input. I took the 15 U-Pb 
# dates from the Devonian, the two oldest U-Pb dates from the Carboniferous  
# (see GTS12 appendix for these dates) and the age of the base of the Devonian  
# (from GTS12 figure 22.14, p. 588). This iteration is based on the same 
# framework that GTS12 used to do a spline fit for the Devonian to predict stage 
# boundaries.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2006) standard conodont zonation (fig. 9).


# Read in the age and position data frames.
DevonianData <- read.csv("DevonianData_GTS12.csv",
                         header = TRUE)
DevonianPositions <- read.csv("DevonianPositions_KaufmannStd.csv",
                                header = TRUE)

# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData$ages,
             ageSds = DevonianData$ageSds,
             positions = DevonianData$positions,
             positionThicknesses = DevonianData$positionThicknesses,
             distTypes = DevonianData$distTypes,
             ids = DevonianData$ids)

# Run ageModel.R code.
DevonianModel <- ageModel(ages = DevonianData$ages,
                          ageSds = DevonianData$ageSds,
                          positions = DevonianData$positions,
                          positionThicknesses = DevonianData$positionThicknesses,
                          distTypes = DevonianData$distTypes,
                          ids = DevonianData$ids) 
                          # predictPositions = seq(from = 0, to = 50, by = 0.1)
# Took ~7.5 mins to run ageModel.

# Predict the age of the other points of interest in the section using the 
# agePredict function.
DevonianPredict <- agePredict(model = DevonianModel,
                              newPositions = DevonianPositions$newPositions,
                              newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                              ids = DevonianPositions$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# Visualize the age model plot with the likelihoods illustrated as contours.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          type = c("contour"),
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# Visualize  the parameter plots.
posteriorPlot(model = DevonianModel, 
              prob = 0.95)

# Make a "ranked date" style plot of the predicted positions of the conodont 
# zones to see where they overlap (is that like an error on the conodont zones?)
  # Save the predicted positons of the conodont zones from agePredict as a data
  # frame.
  conodontPositions <- DevonianPredict$HDI
  # Install and load ggplot.
  install.packages("ggplot2")
  library(ggplot2)
  # Make ranked date plot.
  # Plot dates as line segments.
  ggplot(conodontPositions) +
    geom_segment(aes(x = newPositions,
                     xend = newPositions,
                     y = `0.025`, 
                     yend = `0.975`),
                 size = 1.5) +
    # Add zone labels.
    geom_text(mapping = aes(x = newPositions,
                            y = `0.025`),
              label = conodontPositions$ids,
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
  
# Write an object (e.g. an age model) to a file.
saveRDS(DevonianModel,
        file = "DevonianModel.rds",
        ascii = FALSE,
        version = NULL,
        compress = TRUE,
        refhook = NULL)


## SETUP 2, ITERATION 1: Kaufmann and GTS12 ------------------------------------

# In this iteration, I used the GTS12 ages that were used in Kaufmann (2006) as 
# input into ageModel. I used the stratigraphic positions of these ages from
# Kaufmann (2006), fig. 9. For the age error I used the values without the decay
# constant error because these are all 206Pb/238U ages.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).


# Read in the age and position data frames.
DevonianData_s2 <- read.csv("DevonianData_Kaufmann.csv", header = TRUE)
DevonianPositions_s2 <- read.csv("DevonianPositions_KaufmannStd.csv", header = TRUE)

# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData_s2$ages,
             ageSds = DevonianData_s2$ageSds,
             positions = DevonianData_s2$positions,
             positionThicknesses = DevonianData_s2$positionThicknesses,
             distTypes = DevonianData_s2$distTypes,
             ids = DevonianData_s2$ids)

# Run ageModel.R code.
DevonianModel_s2 <- ageModel(ages = DevonianData_s2$ages,
                             ageSds = DevonianData_s2$ageSds,
                             positions = DevonianData_s2$positions,
                             positionThicknesses = DevonianData_s2$positionThicknesses,
                             distTypes = DevonianData_s2$distTypes,
                             ids = DevonianData_s2$ids,
                             predictPositions = seq(from = -10, to = 110, by = 0.25)) 

# Predict the age of the conodont zones using the agePredict function.
DevonianPredict_s2 <- agePredict(model = DevonianModel_s2,
                                 newPositions = DevonianPositions_s2$newPositions,
                                 newPositionThicknesses = DevonianPositions_s2$newPositionThicknesses,
                                 ids = DevonianPositions_s2$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel_s2,
          agePredictOutput = DevonianPredict_s2,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the parameter plots.
posteriorPlot(model = DevonianModel_s2, prob = 0.95)

# Make a "ranked date" style plot of the predicted positions of the conodont 
# zones to see where they overlap (is that like an error on the conodont zones?)
# Save the predicted durations of the conodont zones from agePredict as a data
# frame.
conodontPositions_s2 <- DevonianPredict_s2$HDI
# Plot durations of condont zones as line segments.
ggplot(conodontPositions_s2) +
  geom_segment(aes(x = newPositions,
                   xend = newPositions,
                   y = `0.025`, 
                   yend = `0.975`),
               size = 1.5) +
  # Add zone labels.
  geom_text(mapping = aes(x = newPositions,
                          y = `0.025`),
            label = conodontPositions_s2$ids,
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

# Write an object (e.g. an age model) to a file.
saveRDS(DevonianModel_s2,
        file = "DevonianModel_s2.rds",
        ascii = FALSE,
        version = NULL,
        compress = TRUE,
        refhook = NULL)
  

## SETUP 2, ITERATION 2: CORRECT STRATIGRAPHIC POSITION, RERUN MODEL -----------

# Assume a 1:1 line from scaled stratigraphic position 0 at the base of the 
# Devonian to position 100 at the base of the Carboniferous.
  # Extract the age of the base of the Devonian and the base of the 
  # Carboniferous. Disregard the 95% CI in this case and just use the midpoint.
  baseD_s2_i2 <- conodontPositions_s2[which(conodontPositions_s2$ids == "base of Devonian"), "0.5"] 
  baseC_s2_i2 <- conodontPositions_s2[which(conodontPositions_s2$ids == "base of Carboniferous"), "0.5"] 
  # Calculate a equation for a 1:1 line that passes through the base of the 
  # Devonian and the base of the Carboniferous.
  # Calculate the slope (m) and y intercept (b) 1:1 line. y = (m * x) + b
  m <- (baseC_s2_i2 - baseD_s2_i2) / (100 - 0)
  b <- baseD_s2_i2 - (m * 0)

# Extract the midpoint of the HDI with a particular focus on the points where
# we have dates that need a corrected stratigraphic position.
  # Extract the midpoint of the HDI from the model.
  HDI_s2 <- data.frame(t(DevonianModel_s2$HDI))
  HDI_s2$predictPositions <- DevonianModel_s2$predictPositions
  # Find value of HDI at each of the original stratigraphic positions for each of 
  # the dated horizons.
  # Run agePredict to get the locations of the dated horizons on the HDI.
  DevonianPredict_s2_i2 <- agePredict(model = DevonianModel_s2,
                                      newPositions = DevonianData_s2$positions,
                                      ids = DevonianData_s2$ids)
  HDIdata_s2 <- DevonianPredict_s2_i2$HDI
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon.
  HDIdata_s2$one2oneStratPos <- (HDIdata_s2$`0.5` - b) / m
  # Calculate the difference in the stratigraphic position between the dated
  # dated horizons (blue dots) and the stratigraphic position of a given age on  
  # the 1:1 line (green dots).
  HDIdata_s2$stratCorrection <- HDIdata_s2$one2oneStratPos - HDIdata_s2$newPositions

# Plot 1:1 line, HDI, and positions of points of interest.
ggplot() +
  # Plot 1:1 line.
  geom_line(aes(x = c(-10, 110), 
                  y = c(((m * -10) + b), ((m * 110) + b))),
               color = "black",
            linetype = "dashed") +
  # Plot HDI from model.
  geom_line(aes(x = HDI_s2$predictPositions,
                y = HDI_s2$X50.),
            color = "black") +
  # Plot the difference between the HDI and the 1:1 line for the dated horizons.
  geom_segment(aes(x = HDIdata_s2$one2oneStratPos,
                   xend = HDIdata_s2$one2oneStratPos - HDIdata_s2$stratCorrection,
                   y = HDIdata_s2$`0.5`,
                   yend = HDIdata_s2$`0.5`),
               size = 1,
               color = "lightblue") +
  # Plot locations of dated horizons on HDI.
  geom_point(aes(x = HDIdata_s2$newPositions,
                y = HDIdata_s2$`0.5`),
            color = "blue") +
  # Add labels for dated horizons.
  geom_text(mapping = aes(x = HDIdata_s2$newPositions,
                          y = HDIdata_s2$`0.5`),
            label = HDIdata_s2$ids,
            nudge_x = 3, nudge_y = -0.5,
            size = 3) +
  # Plot locations of dated horizons on 1:1 line.
  geom_point(aes(x = HDIdata_s2$one2oneStratPos,
                 y = HDIdata_s2$`0.5`),
             color = "purple") +
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

# Rerun model with corrected stratigraphic positions.
  # Add corrected positions in the DevonianData_s2 data frame for model input. 
  # In this case I know everything stayed in order. A better way to do this in 
  # the future would be to use inner_join from dplyr with ids as the linking 
  # variable.
  DevonianData_s2$s2_i2_positions <- HDIdata_s2$one2oneStratPos
  
# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData_s2$ages,
             ageSds = DevonianData_s2$ageSds,
             positions = DevonianData_s2$s2_i2_positions,
             positionThicknesses = DevonianData_s2$positionThicknesses,
             distTypes = DevonianData_s2$distTypes,
             ids = DevonianData_s2$ids)

# Run ageModel.R code.
DevonianModel_s2_i2 <- ageModel(ages = DevonianData_s2$ages,
                                ageSds = DevonianData_s2$ageSds,
                                positions = DevonianData_s2$s2_i2_positions,
                                positionThicknesses = DevonianData_s2$positionThicknesses,
                                distTypes = DevonianData_s2$distTypes,
                                ids = DevonianData_s2$ids,
                                predictPositions = seq(from = -10, to = 110, by = 0.25)) 

# Predict the age of the conodont zones using the agePredict function.
DevonianPredict_s2_i2 <- agePredict(model = DevonianModel_s2_i2,
                                 newPositions = DevonianPositions_s2$newPositions,
                                 newPositionThicknesses = DevonianPositions_s2$newPositionThicknesses,
                                 ids = DevonianPositions_s2$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel_s2_i2,
          agePredictOutput = DevonianPredict_s2_i2,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the parameter plots.
posteriorPlot(model = DevonianModel_s2_i2, prob = 0.95)

# Make a "ranked date" style plot of the predicted positions of the conodont 
# zones to see where they overlap (is that like an error on the conodont zones?)
# Save the predicted durations of the conodont zones from agePredict as a data
# frame.
conodontPositions_s2_i2 <- DevonianPredict_s2_i2$HDI
# Plot durations of condont zones as line segments.
ggplot(conodontPositions_s2_i2) +
  geom_segment(aes(x = newPositions,
                   xend = newPositions,
                   y = `0.025`, 
                   yend = `0.975`),
               size = 1.5) +
  # Add zone labels.
  geom_text(mapping = aes(x = newPositions,
                          y = `0.025`),
            label = conodontPositions_s2$ids,
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

# Write an object (e.g. an age model) to a file.
saveRDS(DevonianModel_s2_i2,
        file = "DevonianModel_s2_i2.rds",
        ascii = FALSE,
        version = NULL,
        compress = TRUE,
        refhook = NULL)

# Add the new HDI to the plot showing the stratigraphic correction.
  # Extract the midpoint of the HDI from the model.
  HDI_s2_i2 <- data.frame(t(DevonianModel_s2_i2$HDI))
  HDI_s2_i2$predictPositions <- DevonianModel_s2_i2$predictPositions
  # Plot.
  ggplot() +
    # Plot 1:1 line.
    geom_line(aes(x = c(-10, 110), 
                  y = c(((m * -10) + b), ((m * 110) + b))),
              color = "black",
              linetype = "dashed") +
    # Plot HDI from model.
    geom_line(aes(x = HDI_s2$predictPositions,
                  y = HDI_s2$X50.),
              color = "black") +
    # Plot the difference between the HDI and the 1:1 line for the dated horizons.
    geom_segment(aes(x = HDIdata_s2$one2oneStratPos,
                     xend = HDIdata_s2$one2oneStratPos - HDIdata_s2$stratCorrection,
                     y = HDIdata_s2$`0.5`,
                     yend = HDIdata_s2$`0.5`),
                 size = 1,
                 color = "lightblue") +
    # Plot locations of dated horizons on HDI.
    geom_point(aes(x = HDIdata_s2$newPositions,
                   y = HDIdata_s2$`0.5`),
               color = "blue") +
    # Add labels for dated horizons.
    geom_text(mapping = aes(x = HDIdata_s2$newPositions,
                            y = HDIdata_s2$`0.5`),
              label = HDIdata_s2$ids,
              nudge_x = 3, nudge_y = -0.5,
              size = 3) +
    # Plot locations of dated horizons on 1:1 line.
    geom_point(aes(x = HDIdata_s2$one2oneStratPos,
                   y = HDIdata_s2$`0.5`),
               color = "purple") +
    # Plot midpoint of HDI from second iteration of model.
    geom_line(aes(x = HDI_s2_i2$predictPositions,
                  y = HDI_s2_i2$X50.),
              color = "red",
              linetype = "dashed",
              size = 1) +
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



## SETUP 2, ITERATION 3: Kaufmann and GTS12 and my new dates -------------------

# In this iteration, I used the GTS12 ages that were used in Kaufmann (2006) as 
# well as my three new ages as input into ageModel, replacing the GTS12 ages for
# those horizons. I used the stratigraphic positions of the new ages from  
# Kaufmann (2006), fig. 9 based on the assignment of conodont zone in the text 
# of our maunscript from Jeff Over. For the ages that did not change, I used the
# corrected stratigraphic position found below. For the age error I used the 
# values without the decay constant uncertainty because these are all 206Pb/238U
# ages.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).

  
# Assume a 1:1 line from scaled stratigraphic position 0 at the base of the 
# Devonian to position 100 at the base of the Carboniferous.
  # Extract the age of the base of the Devonian and the base of the 
  # Carboniferous. Disregard the 95% CI in this case and just use the midpoint.
  baseD_s2_i3 <- conodontPositions_s2_i2[which(conodontPositions_s2_i2$ids == "base of Devonian"), "0.5"] 
  baseC_s2_i3 <- conodontPositions_s2_i2[which(conodontPositions_s2_i2$ids == "base of Carboniferous"), "0.5"] 
  # Calculate a equation for a 1:1 line that passes through the base of the 
  # Devonian and the base of the Carboniferous.
  # Calculate the slope (m) and y intercept (b) 1:1 line. y = (m * x) + b
  m_s2_i3 <- (baseC_s2_i3 - baseD_s2_i3) / (100 - 0)
  b_s2_i3 <- baseD_s2_i3 - (m_s2_i3 * 0)
  
  # Extract the midpoint of the HDI with a particular focus on the points where
  # we have dates that need a corrected stratigraphic position.
  # Extract the midpoint of the HDI from the model.
  HDI_s2_i2 <- data.frame(t(DevonianModel_s2_i2$HDI))
  HDI_s2_i2$predictPositions <- DevonianModel_s2_i2$predictPositions
  # Find value of HDI at each of the original stratigraphic positions for each of 
  # the dated horizons.
  # Run agePredict to get the locations of the dated horizons on the HDI.
  DevonianPredict_s2_i3 <- agePredict(model = DevonianModel_s2_i2,
                                      newPositions = DevonianData_s2$positions,
                                      ids = DevonianData_s2$ids)
  HDIdata_s2_i2 <- DevonianPredict_s2_i3$HDI
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon.
  HDIdata_s2_i2$one2oneStratPos <- (HDIdata_s2_i2$`0.5` - b) / m
  # Calculate the difference in the stratigraphic position between the dated
  # dated horizons (blue dots) and the stratigraphic position of a given age on  
  # the 1:1 line (green dots).
  HDIdata_s2_i2$stratCorrection <- HDIdata_s2_i2$one2oneStratPos - HDIdata_s2_i2$newPositions

# Plot.
  ggplot() +
    # Plot 1:1 line.
    geom_line(aes(x = c(-10, 110), 
                  y = c(((m * -10) + b), ((m * 110) + b))),
              color = "black",
              linetype = "dashed") +
    # Plot HDI from first model.
    geom_line(aes(x = HDI_s2$predictPositions,
                  y = HDI_s2$X50.),
              color = "black") +
    # # Plot the difference between the HDI and the 1:1 line for the dated horizons.
    # geom_segment(aes(x = HDIdata_s2$one2oneStratPos,
    #                  xend = HDIdata_s2$one2oneStratPos - HDIdata_s2$stratCorrection,
    #                  y = HDIdata_s2$`0.5`,
    #                  yend = HDIdata_s2$`0.5`),
    #              size = 1,
    #              color = "lightblue") +
    # Plot the difference between the HDI and the 1:1 line for the dated horizons.
    geom_segment(aes(x = HDIdata_s2_i2$one2oneStratPos,
                     xend = HDIdata_s2_i2$one2oneStratPos - HDIdata_s2_i2$stratCorrection,
                     y = HDIdata_s2_i2$`0.5`,
                     yend = HDIdata_s2_i2$`0.5`),
                 size = 1,
                 color = "lightgreen") +
    # # Plot locations of dated horizons on HDI.
    # geom_point(aes(x = HDIdata_s2$newPositions,
    #                y = HDIdata_s2$`0.5`),
    #            color = "blue") +
    # Plot locations of dated horizons on HDI.
    geom_point(aes(x = HDIdata_s2_i2$newPositions,
                   y = HDIdata_s2_i2$`0.5`),
               color = "orange") +
    # Add labels for dated horizons.
    geom_text(mapping = aes(x = HDIdata_s2$newPositions,
                            y = HDIdata_s2$`0.5`),
              label = HDIdata_s2$ids,
              nudge_x = 3, nudge_y = -0.5,
              size = 3) +
    # Plot locations of dated horizons on 1:1 line.
    geom_point(aes(x = HDIdata_s2$one2oneStratPos,
                   y = HDIdata_s2$`0.5`),
               color = "purple") +
    # Plot midpoint of HDI from second iteration of model.
    geom_line(aes(x = HDI_s2_i2$predictPositions,
                  y = HDI_s2_i2$X50.),
              color = "red",
              # linetype = "dashed",
              size = 1) +
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
  
  
  
# Rerun model with corrected stratigraphic positions.
  # Add corrected positions in the DevonianData_s2 data frame for model input. 
  # In this case I know everything stayed in order. A better way to do this in 
  # the future would be to use inner_join from dplyr with ids as the linking 
  # variable.
  DevonianData_s2$s2_i3_positions <- HDIdata_s2_i2$one2oneStratPos
  # Import new ages and positions (my new data).
  DevonianData_s2_i3 <- read.csv("DevonianData_KaufmannPlusNew.csv", header = TRUE)
  # Join DevonianData_s2 to Devonian Data_s2_i3 and delete values from replaced
  # dated horizons.
  # install.packages("dplyr")
  library(dplyr)
  DevonianData_s2_i3 <- right_join(DevonianData_s2, DevonianData_s2_i3)
  # Add positions of D7a*, D7b* and D6 to the s2_i3_positions column.
  DevonianData_s2_i3[12:14, 8] <- DevonianData_s2_i3[12:14, 4]
  
  
  # Visualize the data prior to running the model using ageDepthPlot function.
  ageDepthPlot(ages = DevonianData_s2_i3$ages,
               ageSds = DevonianData_s2_i3$ageSds,
               positions = DevonianData_s2_i3$s2_i3_positions,
               positionThicknesses = DevonianData_s2_i3$positionThicknesses,
               distTypes = DevonianData_s2_i3$distTypes,
               ids = DevonianData_s2_i3$ids)
  
  # Run ageModel.R code.
  DevonianModel_s2_i3 <- ageModel(ages = DevonianData_s2_i3$ages,
                                  ageSds = DevonianData_s2_i3$ageSds,
                                  positions = DevonianData_s2_i3$s2_i3_positions,
                                  positionThicknesses = DevonianData_s2_i3$positionThicknesses,
                                  distTypes = DevonianData_s2_i3$distTypes,
                                  ids = DevonianData_s2_i3$ids,
                                  predictPositions = seq(from = -10, to = 110, by = 0.25)) 
  
  # Predict the age of the conodont zones using the agePredict function.
  DevonianPredict_s2_i2 <- agePredict(model = DevonianModel_s2_i2,
                                      newPositions = DevonianPositions_s2$newPositions,
                                      newPositionThicknesses = DevonianPositions_s2$newPositionThicknesses,
                                      ids = DevonianPositions_s2$ids)
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs.
  modelPlot(model = DevonianModel_s2_i2,
            agePredictOutput = DevonianPredict_s2_i2,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the parameter plots.
  posteriorPlot(model = DevonianModel_s2_i2, prob = 0.95)
  
  # Make a "ranked date" style plot of the predicted positions of the conodont 
  # zones to see where they overlap (is that like an error on the conodont zones?)
  # Save the predicted durations of the conodont zones from agePredict as a data
  # frame.
  conodontPositions_s2_i2 <- DevonianPredict_s2_i2$HDI
  # Plot durations of condont zones as line segments.
  ggplot(conodontPositions_s2_i2) +
    geom_segment(aes(x = newPositions,
                     xend = newPositions,
                     y = `0.025`, 
                     yend = `0.975`),
                 size = 1.5) +
    # Add zone labels.
    geom_text(mapping = aes(x = newPositions,
                            y = `0.025`),
              label = conodontPositions_s2$ids,
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
  
  # Write an object (e.g. an age model) to a file.
  saveRDS(DevonianModel_s2_i2,
          file = "DevonianModel_s2_i2.rds",
          ascii = FALSE,
          version = NULL,
          compress = TRUE,
          refhook = NULL)
  
  # Add the new HDI to the plot showing the stratigraphic correction.
  # Extract the midpoint of the HDI from the model.
  HDI_s2_i2 <- data.frame(t(DevonianModel_s2_i2$HDI))
  HDI_s2_i2$predictPositions <- DevonianModel_s2_i2$predictPositions
  # Plot.
  ggplot() +
    # Plot 1:1 line.
    geom_line(aes(x = c(-10, 110), 
                  y = c(((m * -10) + b), ((m * 110) + b))),
              color = "black",
              linetype = "dashed") +
    # Plot HDI from model.
    geom_line(aes(x = HDI_s2$predictPositions,
                  y = HDI_s2$X50.),
              color = "black") +
    # Plot the difference between the HDI and the 1:1 line for the dated horizons.
    geom_segment(aes(x = HDIdata_s2$one2oneStratPos,
                     xend = HDIdata_s2$one2oneStratPos - HDIdata_s2$stratCorrection,
                     y = HDIdata_s2$`0.5`,
                     yend = HDIdata_s2$`0.5`),
                 size = 1,
                 color = "lightblue") +
    # Plot locations of dated horizons on HDI.
    geom_point(aes(x = HDIdata_s2$newPositions,
                   y = HDIdata_s2$`0.5`),
               color = "blue") +
    # Add labels for dated horizons.
    geom_text(mapping = aes(x = HDIdata_s2$newPositions,
                            y = HDIdata_s2$`0.5`),
              label = HDIdata_s2$ids,
              nudge_x = 3, nudge_y = -0.5,
              size = 3) +
    # Plot locations of dated horizons on 1:1 line.
    geom_point(aes(x = HDIdata_s2$one2oneStratPos,
                   y = HDIdata_s2$`0.5`),
               color = "purple") +
    # Plot midpoint of HDI from second iteration of model.
    geom_line(aes(x = HDI_s2_i2$predictPositions,
                  y = HDI_s2_i2$X50.),
              color = "red",
              linetype = "dashed",
              size = 1) +
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
  

