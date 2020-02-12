# This script runs an age-depth model on Devonian dates and conodont zones to
# predict the timing and duration of stage boundaries and to compare different 
# conodont zonation scales.

# This script follows (and borrows some of its code from) a file titled
# GTS12_SplineFigure_age_model_020519.R which was emailed to me by Mark Schmitz 
# on 2019.02.06. Either Mark Schmitz or Robin Trayler is the author of most of 
# the code in the file DevonianAgeDepthModel_StartingCondtions1.R (ecept the 
# conodont "ranked date" plot), and that code is generally followed below. 
# The stratigraphic correction is my own code.

# Updated 2020.01.22 CH


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


## GTS20 + Kaufmann scale ------------------------------------------------------

# In this iteration, I used the 206Pb/238U GTS20 ages. 
# I used the stratigraphic positions of these ages from the written description 
# of the position's conodont zone(s) and the scaling of Kaufmann (2006), fig. 9. 
# For the age error I used the values without the decay constant error because 
# these are all 206Pb/238U ages.

# I used this model to predict the ages of the conodont zones from Kaufmann
# (2009) standard conodont zonation (fig. 9).


# Read in the age and position data frames.
DevonianData <- read.csv("DevonianData_GTS20.csv", header = TRUE)
DevonianPositions <- read.csv("DevonianPositions_KaufmannStd.csv", 
                              header = TRUE)

# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData$age,
             ageSds = DevonianData$error_2sig_nolambda,
             positions = DevonianData$midpoint_Kaufmann,
             positionThicknesses = DevonianData$halfwidth_Kaufmann,
             distTypes = DevonianData$distTypes,
             ids = DevonianData$GTS20_ids)

# Run ageModel.R code.
DevonianModel_Kaufmann <- ageModel(ages = DevonianData$ages,
                                   ageSds = DevonianData$error_2sig_nolambda,
                                   positions = DevonianData$midpoint_Kaufmann,
                                   positionThicknesses = DevonianData$halfwidth_Kaufmann,
                                   distTypes = DevonianData$distTypes,
                                   ids = DevonianData$GTS20_ids,
                                   predictPositions = seq(from = -20, to = 120, by = 0.25)) 