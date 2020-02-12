# This script runs an age-depth model on Devonian dates and conodont zones to
# predict the timing and duration of stage boundaries and to compare different 
# conodont zonation scales.

# This script follows (and borrows some of its code from) a file titled
# GTS12_SplineFigure_age_model_020519.R which was emailed to me by Mark Schmitz 
# on 2019.02.06. Either Mark Schmitz or Robin Trayler is the author of most of 
# the code below in the "STARTING CONDTIONS 1, ITERATION 1: GTS12 only" section, 
# and the comments explain what data I input and why. The conodont zone "ranked
# date" plot is my own code.

# Updated 2019.12.19 CH


## SETUP -----------------------------------------------------------------------

# Install and load modifiedBChron from Robin Trayler's GitHub repository.
# install.packages("devtools")  # Uncomment if needed
# devtools::install_github("robintrayler/modifiedBChron")  # Uncomment if needed
library(modifiedBChron)

# Install and load ggplot.
# install.packages("ggplot2")
library(ggplot2)

# Set working directory.
setwd("/Users/claireharrigan/Dropbox/IGL + research/Devonian manuscript/DevonianAgeDepthModel")

# Load functions.
source("conodontPositionsPlot.R")


## STARTING CONDTIONS 1, ITERATION 1: GTS12 only -------------------------------

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




