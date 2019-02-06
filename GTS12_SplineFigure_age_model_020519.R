###### Tuesday, November 5, 2018
# experiments with Late Jurassic - Early Cretaceous data using Robin's modified bchron

# install Bchron from Robin's GitHub repository
devtools::install_github('robintrayler/modifiedBChron')

# note that devtools isn't installed automatically in RStudio, so you might need to run this line
# install.packages("devtools")

# load Bchron into workspace
library(modifiedBChron)

# getwd()  ... to see what the working directory is...
# setwd() ... to set the working directory, note that you can use tab completion...
# list.files() ...lists the files in the folder
setwd("~/Dropbox/Mark Schmitz - Robin Trayler/JCBoundary/")

# read in the age and position data frames
GTS12splinefigdata <- read.csv("GTS12splinefigdata.csv",header=TRUE)
GTS12splinefigPositions <- read.csv("GTS12splinefigpos.csv")

# visualize the data prior to running the model using ageDepthPlot function
ageDepthPlot(ages=GTS12splinefigdata$ages,
             ageSds = GTS12splinefigdata$ageSds,
             positions = GTS12splinefigdata$positions,
             positionThicknesses = GTS12splinefigdata$positionThicknesses,
             distTypes = GTS12splinefigdata$distTypes,
             ids = GTS12splinefigdata$ids)

# run ageModel.R code
GTS12splinefigModel1 <- ageModel(ages=GTS12splinefigdata$ages,
                                 ageSds = GTS12splinefigdata$ageSds,
                                 positions = GTS12splinefigdata$positions,
                                 positionThicknesses = GTS12splinefigdata$positionThicknesses,
                                 distTypes = GTS12splinefigdata$distTypes,
                                 ids = GTS12splinefigdata$ids,
                                 predictPositions = seq(0, 40, by = 0.1)) 

# predict the age of the other points of interest in the yezo section using the agePredict function
GTS12splinefigPredict1 <- agePredict(model = GTS12splinefigModel1,
                                     newPositions = GTS12splinefigPositions$predictPositions,
                                     ids = GTS12splinefigPositions$ids)

# visualize the age model plot with the likelihoods illustrated as PDFs
modelPlot(model = GTS12splinefigModel1,
          agePredictOutput = GTS12splinefigPredict1,
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# visualize the age model plot with the likelihoods illustrated as contoured
modelPlot(model = GTS12splinefigModel1,
          agePredictOutput = GTS12splinefigPredict1,
          type = c("contour"),
          scale = 0.5,
          predictLabels = c("both"), # options are 'ages', 'ids', 'NA'
          legend = c("adjacent"))  # options are 'color', 'adjacent', 'NA'

# visualize (and save) the parameter plots; if you want to print a pdf to the working directory set PDF = TRUE)
parameterPlot(model = GTS12splinefigModel1)

# write the predicted positions to a file
write.csv(GTS12splinefigPredict1$HDI, file = "GTS12splinefigPredict1$HDI.csv")

# write an object (e.g. an age model) to a file...
saveRDS(GTS12splinefigModel1, file = "GTS12splinefigModel1.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# read an object (e.g. an age model) from a saved file...
# GTS12splinefigModel1 <- readRDS("GTS12splinefigModel1.rds", refhook = NULL)
