# This script runs an age-depth model on Devonian dates and conodont zones to
# predict the timing and duration of stage boundaries and to compare different 
# conodont zonation scales.

# This script follows (and borrows some of its code from) a file titled
# GTS12_SplineFigure_age_model_020519.R which was emailed to me by Mark Schmitz 
# on 2019.02.06. Either Mark Schmitz or Robin Trayler is the author of most of 
# the code in the file DevonianAgeDepthModel_StartingCondtions1.R (ecept the 
# conodont "ranked date" plot), and that code is generally followed below. 
# The stratigraphic correction is my own code.

# Updated 2020.01.13 CH


## SETUP -----------------------------------------------------------------------

# Install and load modifiedBChron from Robin Trayler's GitHub repository.
# install.packages("devtools")  # Uncomment if needed
# devtools::install_github("robintrayler/modifiedBChron")  # Uncomment if needed
library(modifiedBChron)

# Install and load ggplot and dplyr.
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr)

# Set working directory.
setwd("/Users/claireharrigan/Dropbox/IGL + research/Devonian manuscript/DevonianAgeDepthModel")

# Load functions.
source("conodontPositionsPlot.R")
source("stratigraphicCorrectionPlot.R")


## STARTING CONDTIONS 2, ITERATION 1: Kaufmann and GTS12 -----------------------

# In this iteration, I used the GTS12 ages that were used in Kaufmann (2006) as 
# input into ageModel. I used the stratigraphic positions of these ages from
# Kaufmann (2006), fig. 9. For the age error I used the values without the decay
# constant error because these are all 206Pb/238U ages.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).


# Read in the age and position data frames.
DevonianData <- read.csv("DevonianData_Kaufmann.csv", header = TRUE)
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
                          ids = DevonianData$ids,
                          predictPositions = seq(from = -10, to = 110, by = 0.25)) 

# Predict the age of the conodont zones using the agePredict function.
DevonianPredict <- agePredict(model = DevonianModel,
                              newPositions = DevonianPositions$newPositions,
                              newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                              ids = DevonianPositions$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the age model plot with the likelihoods illustrated as PDFs and
# the predicted conodont zone positions shown as red error bars.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the parameter plots.
# posteriorPlot(model = DevonianModel, prob = 0.95)

# Plot the predicted positions of the conodont zones to see where they overlap.
  # Save the predicted durations of the conodont zones from agePredict as a data
  # frame.
  conodontPositions <- DevonianPredict$HDI
  # Plot.
  conodontPositionsPlot()

# Find the corrected stratigraphic positions for the dated horizons.
  # Assume a 1:1 line from scaled stratigraphic position 0 at the base of the 
  # Devonian to position 100 at the base of the Carboniferous.
  # Calculate a equation for a 1:1 line that passes through the base of the 
  # Devonian and the base of the Carboniferous and store the values for this
  # line in a list.
  continuousDepositionLine <- list(
    baseD = conodontPositions[which(conodontPositions$ids == "base of Devonian"), "0.5"],
    baseC = conodontPositions[which(conodontPositions$ids == "base of Carboniferous"), "0.5"],
    m = (conodontPositions[which(conodontPositions$ids == "base of Carboniferous"), "0.5"] - 
           conodontPositions[which(conodontPositions$ids == "base of Devonian"), "0.5"]) / (100 - 0),
    b = conodontPositions[which(conodontPositions$ids == "base of Devonian"), "0.5"])
  
  # Extract the ages and stratigraphic positions of the midpoint of the HDI from
  # the model for plotting.
  HDImidpoint <- data.frame(midpoint = data.frame(t(DevonianModel$HDI))[, 2],
                           stratPosition = DevonianModel$predictPositions)
  
  # Use agePredict to find the age of the midpoint of the HDI at the dated 
  # horizons.
  datedHorizonModelAge <- agePredict(model = DevonianModel,
                                     newPositions = DevonianData$positions,
                                     ids = DevonianData$ids)
  # Save the positions and ages from agePredict in a new data frame.
  datedHorizon <- datedHorizonModelAge$HDI
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon to the dated Horizon data frame.
  datedHorizon$continousDepositionPosition <- (datedHorizon$`0.5` - 
                                               continuousDepositionLine$b) / 
                                               continuousDepositionLine$m
  # Calculate the difference between the HDI midpoint from the age model and the
  # continuous deposition line at the dated horizons.
  datedHorizon$stratCorrection <- datedHorizon$continousDepositionPosition - 
    datedHorizon$newPositions

# Make plot to visualize the stratigraphic correction.
  stratigraphicCorrectionPlot()
  
# Save a list of the model inputs and results from the first iteration of the 
# model run so that the data frames and lists can be overwritten in the section 
# below.
DevonianAgeDepthModel_s2i1 <- list(conodontPositions, datedHorizon, 
                                   datedHorizonModelAge, DevonianData,
                                   DevonianModel, DevonianPositions, 
                                   DevonianPredict, HDImidpoint)  
# Write an object (e.g. an age model) to a file.
saveRDS(DevonianModel, file = "DevonianModel_s2i1.rds",
        ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

  
## STARTING CONDITIONS 2, ITERATION 2: -----------------------------------------
## RUN MODEL WITH CORRECTED STRATIGRAPHIC POSITION

# Rerun model with corrected stratigraphic positions.

# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData$ages,
             ageSds = DevonianData$ageSds,
             positions = datedHorizon$continousDepositionPosition,
             positionThicknesses = DevonianData$positionThicknesses,
             distTypes = DevonianData$distTypes,
             ids = DevonianData$ids)

# Run ageModel.R code.
DevonianModel <- ageModel(ages = DevonianData$ages,
                          ageSds = DevonianData$ageSds,
                          positions = datedHorizon$continousDepositionPosition,
                          positionThicknesses = DevonianData$positionThicknesses,
                          distTypes = DevonianData$distTypes,
                          ids = DevonianData$ids,
                          predictPositions = seq(from = -10, to = 110, by = 0.25)) 

# Predict the age of the conodont zones using the agePredict function.
DevonianPredict <- agePredict(model = DevonianModel,
                              newPositions = DevonianPositions$newPositions,
                              newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                              ids = DevonianPositions$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the age model plot with the likelihoods illustrated as PDFs and
# the predicted conodont zone positions shown as red error bars.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))

# Visualize the parameter plots.
# posteriorPlot(model = DevonianModel, prob = 0.95)
 
# Plot the predicted positions of the conodont zones to see where they overlap.
# Save the predicted durations of the conodont zones from agePredict as a data
# frame.
conodontPositions <- DevonianPredict$HDI
# Plot.
conodontPositionsPlot()

# Find the corrected stratigraphic positions for the dated horizons.
# Extract the ages and stratigraphic positions of the midpoint of the HDI from
# the model for plotting.
HDImidpoint <- data.frame(midpoint = data.frame(t(DevonianModel$HDI))[, 2],
                          stratPosition = DevonianModel$predictPositions)

# Use agePredict to find the age of the midpoint of the HDI at the dated 
# horizons.
datedHorizonModelAge <- agePredict(model = DevonianModel,
                                   newPositions = DevonianData$positions,
                                   ids = DevonianData$ids)

# Save the positions and ages from agePredict in a new data frame.
datedHorizon <- datedHorizonModelAge$HDI

# Add the stratigraphic position of the 1:1 line at the model age of each 
# dated horizon to the dated Horizon data frame.
datedHorizon$continousDepositionPosition <- (datedHorizon$`0.5` - 
                                             continuousDepositionLine$b) / 
                                             continuousDepositionLine$m

# Calculate the difference between the HDI midpoint from the age model and the
# continuous deposition line at the dated horizons.
datedHorizon$stratCorrection <- datedHorizon$continousDepositionPosition - 
  datedHorizon$newPositions

# Make plot to visualize the stratigraphic correction.
stratigraphicCorrectionPlot()

# Save a list of the model inputs and results from the first iteration of the 
# model run so that the data frames and lists can be overwritten in the section  
# below.
DevonianAgeDepthModel_s2i2 <- list(conodontPositions, datedHorizon, 
                                   datedHorizonModelAge, DevonianData,
                                   DevonianModel, DevonianPositions, 
                                   DevonianPredict, HDImidpoint)  
# Write an object (e.g. an age model) to a file.
saveRDS(DevonianModel, file = "DevonianModel_s2i2.rds",
        ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)


## STARTING CONDITIONS 2, ITERATION 3: -----------------------------------------
## RUN MODEL WITH MY NEW DATES AND CORRECTED STRATIGRAPHIC POSITION

# In this iteration, I used the GTS12 ages that were used in Kaufmann (2006) as 
# well as my three new ages as input into ageModel, replacing the GTS12 ages for
# those horizons. I used the stratigraphic positions of the new ages from  
# Kaufmann (2006), fig. 9 based on the assignment of conodont zone in the text 
# of our maunscript from Jeff Over. For the ages that did not change, I used the
# corrected stratigraphic position found in iteration 2 of the model. 
# For the age error I used the values without the decay constant uncertainty 
# because these are all 206Pb/238U ages.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).


# Import new dates.
DevonianData <- read.csv("DevonianData_KaufmannPlusNew.csv", header = TRUE)

# Update stratigraphic position of repeated dates.
  # Join the datedHorizon data frame to the newly imported DevonianData data
  # frame, excluding rows from datedHorizon that are not in DevonianData.
  DevonianData <- left_join(DevonianData, datedHorizon, by = "ids")
  # If the value in the DevonianData$continousDepositionPosition column is NA, 
  # put the value from DevonianData$positions into the 
  # DevonianData$continousDepositionPosition column.
  DevonianData$continousDepositionPosition <- 
    ifelse(is.na(DevonianData$continousDepositionPosition), 
           DevonianData$positions, 
           DevonianData$continousDepositionPosition)
  
# Rerun model with corrected stratigraphic positions and my new ages.
  
  # Visualize the data prior to running the model using ageDepthPlot function.
  ageDepthPlot(ages = DevonianData$ages,
               ageSds = DevonianData$ageSds,
               positions = DevonianData$continousDepositionPosition,
               positionThicknesses = DevonianData$positionThicknesses,
               distTypes = DevonianData$distTypes,
               ids = DevonianData$ids)
  
  # Run ageModel.R code.
  DevonianModel <- ageModel(ages = DevonianData$ages,
                            ageSds = DevonianData$ageSds,
                            positions = DevonianData$continousDepositionPosition,
                            positionThicknesses = DevonianData$positionThicknesses,
                            distTypes = DevonianData$distTypes,
                            ids = DevonianData$ids,
                            predictPositions = seq(from = -10, to = 110, by = 0.25)) 
  
  # Predict the age of the conodont zones using the agePredict function.
  DevonianPredict <- agePredict(model = DevonianModel,
                                newPositions = DevonianPositions$newPositions,
                                newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                                ids = DevonianPositions$ids)
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs.
  modelPlot(model = DevonianModel,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs and
  # the predicted conodont zone positions shown as red error bars.
  modelPlot(model = DevonianModel,
            agePredictOutput = DevonianPredict,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the parameter plots.
  # posteriorPlot(model = DevonianModel, prob = 0.95)
  
  # Plot the predicted positions of the conodont zones to see where they overlap.
  # Save the predicted durations of the conodont zones from agePredict as a data
  # frame.
  conodontPositions <- DevonianPredict$HDI
  # Plot.
  conodontPositionsPlot()
  
  # Find the corrected stratigraphic positions for the dated horizons.
  # Extract the ages and stratigraphic positions of the midpoint of the HDI from
  # the model for plotting.
  HDImidpoint <- data.frame(midpoint = data.frame(t(DevonianModel$HDI))[, 2],
                            stratPosition = DevonianModel$predictPositions)
  
  # Use agePredict to find the age of the midpoint of the HDI at the dated 
  # horizons.
  datedHorizonModelAge <- agePredict(model = DevonianModel,
                                     newPositions = DevonianData$positions,
                                     ids = DevonianData$ids)
  
  # Save the positions and ages from agePredict in a new data frame.
  datedHorizon <- datedHorizonModelAge$HDI
  
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon to the dated Horizon data frame.
  datedHorizon$continousDepositionPosition <- (datedHorizon$`0.5` - 
                                                 continuousDepositionLine$b) / 
    continuousDepositionLine$m
  
  # Calculate the difference between the HDI midpoint from the age model and the
  # continuous deposition line at the dated horizons.
  datedHorizon$stratCorrection <- datedHorizon$continousDepositionPosition - 
    datedHorizon$newPositions
  
  # Make plot to visualize the stratigraphic correction.
  stratigraphicCorrectionPlot()
  
  # Save a list of the model inputs and results from the first iteration of the 
  # model run so that the data frames and lists can be overwritten in the section  
  # below.
  DevonianAgeDepthModel_s2i3 <- list(conodontPositions, datedHorizon, 
                                     datedHorizonModelAge, DevonianData,
                                     DevonianModel, DevonianPositions, 
                                     DevonianPredict, HDImidpoint)  
  # Write an object (e.g. an age model) to a file.
  saveRDS(DevonianModel, file = "DevonianModel_s2i3.rds",
          ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

## STARTING CONDITIONS 2, ITERATION 4: -----------------------------------------
## RUN MODEL WITH MY NEW DATES AND CORRECTED STRATIGRAPHIC POSITION
  
# In this iteration, I used the GTS12 ages that were used in Kaufmann (2006) as 
# well as my three new ages as input into ageModel, replacing the GTS12 ages for
# those horizons. I used the stratigraphic positions from the correction done in
# iteration 3.
  
# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).
  
  
# Rerun model with corrected stratigraphic positions.
  
  # Replace continuousDeposition column in DevonianData.
  DevonianData$continousDepositionPosition <- datedHorizon$continousDepositionPosition
  
  # Visualize the data prior to running the model using ageDepthPlot function.
  ageDepthPlot(ages = DevonianData$ages,
               ageSds = DevonianData$ageSds,
               positions = DevonianData$continousDepositionPosition,
               positionThicknesses = DevonianData$positionThicknesses,
               distTypes = DevonianData$distTypes,
               ids = DevonianData$ids)
  
  # Run ageModel.R code.
  DevonianModel <- ageModel(ages = DevonianData$ages,
                            ageSds = DevonianData$ageSds,
                            positions = DevonianData$continousDepositionPosition,
                            positionThicknesses = DevonianData$positionThicknesses,
                            distTypes = DevonianData$distTypes,
                            ids = DevonianData$ids,
                            predictPositions = seq(from = -10, to = 110, by = 0.25)) 
  
  # Predict the age of the conodont zones using the agePredict function.
  DevonianPredict <- agePredict(model = DevonianModel,
                                newPositions = DevonianPositions$newPositions,
                                newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                                ids = DevonianPositions$ids)
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs.
  modelPlot(model = DevonianModel,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs and
  # the predicted conodont zone positions shown as red error bars.
  modelPlot(model = DevonianModel,
            agePredictOutput = DevonianPredict,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the parameter plots.
  # posteriorPlot(model = DevonianModel, prob = 0.95)
  
  # Plot the predicted positions of the conodont zones to see where they overlap.
  # Save the predicted durations of the conodont zones from agePredict as a data
  # frame.
  conodontPositions <- DevonianPredict$HDI
  # Plot.
  conodontPositionsPlot()
  
  # Find the corrected stratigraphic positions for the dated horizons.
  # Extract the ages and stratigraphic positions of the midpoint of the HDI from
  # the model for plotting.
  HDImidpoint <- data.frame(midpoint = data.frame(t(DevonianModel$HDI))[, 2],
                            stratPosition = DevonianModel$predictPositions)
  
  # Use agePredict to find the age of the midpoint of the HDI at the dated 
  # horizons.
  datedHorizonModelAge <- agePredict(model = DevonianModel,
                                     newPositions = DevonianData$positions,
                                     ids = DevonianData$ids)
  
  # Save the positions and ages from agePredict in a new data frame.
  datedHorizon <- datedHorizonModelAge$HDI
  
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon to the dated Horizon data frame.
  datedHorizon$continousDepositionPosition <- (datedHorizon$`0.5` - 
                                                 continuousDepositionLine$b) / 
    continuousDepositionLine$m
  
  # Calculate the difference between the HDI midpoint from the age model and the
  # continuous deposition line at the dated horizons.
  datedHorizon$stratCorrection <- datedHorizon$continousDepositionPosition - 
    datedHorizon$newPositions
  
  # Make plot to visualize the stratigraphic correction.
  stratigraphicCorrectionPlot()
  
  # Save a list of the model inputs and results from the first iteration of the 
  # model run so that the data frames and lists can be overwritten in the section  
  # below.
  DevonianAgeDepthModel_s2i4 <- list(conodontPositions, datedHorizon, 
                                     datedHorizonModelAge, DevonianData,
                                     DevonianModel, DevonianPositions, 
                                     DevonianPredict, HDImidpoint)  
  # Write an object (e.g. an age model) to a file.
  saveRDS(DevonianModel, file = "DevonianModel_s2i4.rds",
          ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
  
  

## STARTING CONDITIONS 2, ITERATION 5: -----------------------------------------
## RUN MODEL WITH GTS20 DATES AND CORRECTED STRATIGRAPHIC POSITION
  
# In this iteration, I used the GTS20 ages that were used in Kaufmann (2006) as 
# well as my three new ages as input into ageModel, replacing the GTS12 ages for
# those horizons. I used the stratigraphic positions of the new ages from  
# Kaufmann (2006), fig. 9 based on the assignment of conodont zone in the text 
# of our maunscript from Jeff Over. For the ages that did not change, I used the
# corrected stratigraphic position found in iteration 2 of the model. 
# For the age error I used the values without the decay constant uncertainty 
# because these are all 206Pb/238U ages.
  
  # I used this model to predict the ages of the conodont zones from Kaufmann
  # (2009) standard conodont zonation (fig. 9).
  
  
  # Import new dates.
  DevonianData <- read.csv("DevonianData_KaufmannPlusNew.csv", header = TRUE)
  
  # Update stratigraphic position of repeated dates.
  # Join the datedHorizon data frame to the newly imported DevonianData data
  # frame, excluding rows from datedHorizon that are not in DevonianData.
  DevonianData <- left_join(DevonianData, datedHorizon, by = "ids")
  # If the value in the DevonianData$continousDepositionPosition column is NA, 
  # put the value from DevonianData$positions into the 
  # DevonianData$continousDepositionPosition column.
  DevonianData$continousDepositionPosition <- 
    ifelse(is.na(DevonianData$continousDepositionPosition), 
           DevonianData$positions, 
           DevonianData$continousDepositionPosition)
  
  # Rerun model with corrected stratigraphic positions and my new ages.
  
  # Visualize the data prior to running the model using ageDepthPlot function.
  ageDepthPlot(ages = DevonianData$ages,
               ageSds = DevonianData$ageSds,
               positions = DevonianData$continousDepositionPosition,
               positionThicknesses = DevonianData$positionThicknesses,
               distTypes = DevonianData$distTypes,
               ids = DevonianData$ids)
  
  # Run ageModel.R code.
  DevonianModel <- ageModel(ages = DevonianData$ages,
                            ageSds = DevonianData$ageSds,
                            positions = DevonianData$continousDepositionPosition,
                            positionThicknesses = DevonianData$positionThicknesses,
                            distTypes = DevonianData$distTypes,
                            ids = DevonianData$ids,
                            predictPositions = seq(from = -10, to = 110, by = 0.25)) 
  
  # Predict the age of the conodont zones using the agePredict function.
  DevonianPredict <- agePredict(model = DevonianModel,
                                newPositions = DevonianPositions$newPositions,
                                newPositionThicknesses = DevonianPositions$newPositionThicknesses,
                                ids = DevonianPositions$ids)
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs.
  modelPlot(model = DevonianModel,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the age model plot with the likelihoods illustrated as PDFs and
  # the predicted conodont zone positions shown as red error bars.
  modelPlot(model = DevonianModel,
            agePredictOutput = DevonianPredict,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"))
  
  # Visualize the parameter plots.
  # posteriorPlot(model = DevonianModel, prob = 0.95)
  
  # Plot the predicted positions of the conodont zones to see where they overlap.
  # Save the predicted durations of the conodont zones from agePredict as a data
  # frame.
  conodontPositions <- DevonianPredict$HDI
  # Plot.
  conodontPositionsPlot()
  
  # Find the corrected stratigraphic positions for the dated horizons.
  # Extract the ages and stratigraphic positions of the midpoint of the HDI from
  # the model for plotting.
  HDImidpoint <- data.frame(midpoint = data.frame(t(DevonianModel$HDI))[, 2],
                            stratPosition = DevonianModel$predictPositions)
  
  # Use agePredict to find the age of the midpoint of the HDI at the dated 
  # horizons.
  datedHorizonModelAge <- agePredict(model = DevonianModel,
                                     newPositions = DevonianData$positions,
                                     ids = DevonianData$ids)
  
  # Save the positions and ages from agePredict in a new data frame.
  datedHorizon <- datedHorizonModelAge$HDI
  
  # Add the stratigraphic position of the 1:1 line at the model age of each 
  # dated horizon to the dated Horizon data frame.
  datedHorizon$continousDepositionPosition <- (datedHorizon$`0.5` - 
                                                 continuousDepositionLine$b) / 
    continuousDepositionLine$m
  
  # Calculate the difference between the HDI midpoint from the age model and the
  # continuous deposition line at the dated horizons.
  datedHorizon$stratCorrection <- datedHorizon$continousDepositionPosition - 
    datedHorizon$newPositions
  
  # Make plot to visualize the stratigraphic correction.
  stratigraphicCorrectionPlot()
  
  # Save a list of the model inputs and results from the first iteration of the 
  # model run so that the data frames and lists can be overwritten in the section  
  # below.
  DevonianAgeDepthModel_s2i5 <- list(conodontPositions, datedHorizon, 
                                     datedHorizonModelAge, DevonianData,
                                     DevonianModel, DevonianPositions, 
                                     DevonianPredict, HDImidpoint)  
  # Write an object (e.g. an age model) to a file.
  saveRDS(DevonianModel, file = "DevonianModel_s2i5.rds",
          ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
  