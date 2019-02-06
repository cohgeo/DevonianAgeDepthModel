# This script runs a Devonian age-depth model to predict the Emsian-Eifelian
# and Eifelian-Givetian boundaries. 
# This script follows (and borrows much of its code from) a file titled
# GTS12_SplineFigure_age_model_020519.R which was emailed to me by Mark Schmitz 
# on 2019.02.06. Either Mark Schmitz or Robin Trayler is the author of most of 
# the code below.
# Updated 2019.02.06 CH


## SETUP -----------------------------------------------------------------------

# Install Bchron from Robin Trayler's GitHub repository.
# install.packages("devtools")  # Uncomment if necessary
devtools::install_github('robintrayler/modifiedBChron')

# Load Bchron package into workspace.
library(modifiedBChron)

# Set working directory.
setwd("/Users/claireharrigan/Dropbox/Devonian manuscript/DevonianAgeDepthModel")


## IMPORT DATA, PLOT DATA ------------------------------------------------------

# Read in or create the age and position data frames.
DevonianData <- read.csv("DevonianData.csv",
                         header = TRUE)
# DevonianPositions <- read.csv("DevonianPositions.csv")
DevonianPositions <- data.frame("predictPositions" = c(10, 40),
                                "ids" = c("Emsian-Eifelian boundary", 
                                          "Eifelian-Givetian boundary"))

# Visualize the data prior to running the model using ageDepthPlot function.
ageDepthPlot(ages = DevonianData$ages,
             ageSds = DevonianData$ageSds,
             positions = DevonianData$positions,
             positionThicknesses = DevonianData$positionThicknesses,
             distTypes = DevonianData$distTypes,
             ids = DevonianData$ids)


## RUN AGE MODEL, PLOT MODEL ---------------------------------------------------

# Run ageModel.R code.
DevonianModel <- ageModel(ages = DevonianData$ages,
                          ageSds = DevonianData$ageSds,
                          positions = DevonianData$positions,
                          positionThicknesses = DevonianData$positionThicknesses,
                          distTypes = DevonianData$distTypes,
                          ids = DevonianData$ids,
                          predictPositions = seq(from = 0, to = 50, by = 0.1)) 

# Predict the age of the other points of interest in the section using the 
# agePredict function.
DevonianPredict <- agePredict(model = DevonianModel,
                              newPositions = DevonianPositions$predictPositions,
                              ids = DevonianPositions$ids)

# Visualize the age model plot with the likelihoods illustrated as PDFs.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# Visualize the age model plot with the likelihoods illustrated as a ribbon.
modelPlot(model = DevonianModel,
          agePredictOutput = DevonianPredict,
          type = c("contour"),
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# Visualize (and save) the parameter plots.
# To print a pdf to the working directory add ", PDF = TRUE)" to the line below.
posteriorPlot(model = DevonianModel, 
              prob = 0.95)


## SAVE RESULTS ----------------------------------------------------------------

# # Write the predicted positions to a file.
# write.csv(DevonianPredict$HDI, file = "DevonianPredict$HDI.csv")
# 
# # Write an object (e.g. an age model) to a file.
# saveRDS(DevonianModel, 
#         file = "DevonianModel.rds", 
#         ascii = FALSE, 
#         version = NULL,
#         compress = TRUE, 
#         refhook = NULL)
# 
# # Read an object (e.g. an age model) from a saved file.
# DevonianModel <- readRDS("DevonianModel.rds", 
#                          refhook = NULL)
