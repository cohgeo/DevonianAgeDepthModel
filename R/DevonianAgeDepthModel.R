# This script runs an age-depth model on Devonian ages and their associated
# relative stratigraphic position to predict the age of Devonian Stage 
# boundaries and the age of conodont biozones boundaries.

# Updated 2020.09.10 CH

## SETUP -----------------------------------------------------------------------

# Clear all from your environment.
  # rm(list = ls())  # Uncomment if needed

# Install and load modifiedBChron from Robin Trayler's GitHub repository.
  # install.packages("devtools")  # Uncomment if needed
  # devtools::install_github("robintrayler/modifiedBChron")  # Uncomment if needed
  library(modifiedBChron)

# Set working directory 
  # Change the text in quotes to match your directory on your computer.
  # setwd("/Users/claireharrigan/Dropbox/IGL + research/Devonian/DevonianAgeDepthModel")


# IMPORT MODEL INPUTS ----------------------------------------------------------

# The code required to run each model is the same, so to change which scale you
# use for the starting point, uncomment and run the code below that corresponds
# to your starting scale. To run the next model, clear everything from your
# environment and uncomment and run the code below that corresponds to the next 
# scale for which you want to run a model.

# Import model inputs.
  # KAUFMANN SCALE
    # Import radioisotpic ages, anchored astrochronology durations, and relative
    # stratigraphic positions.
    DevonianData <- read.csv("./data/DevonianData_Kaufmann.csv",
                             header = TRUE)
    # Import relative stratigraphic position of Stages.
    DevonianPositions <- read.csv("./data/DevonianPositions_Kaufmann.csv",
                                  header = TRUE)
    # Import relative stratigraphic position of conodont biozone positions.
    # Because the Kaufmann scale is comprised of an alternative and a standard
    # scale, you will need to run agePredict for each of the lines below.
    DevonianPositions.c <- read.csv("./data/DevonianPositions_Kaufmann_alt conodont.csv",
                                    header = TRUE)
    DevonianPositions.c <- read.csv("./data/DevonianPositions_Kaufmann_std conodont.csv",
                                    header = TRUE)
    # Save a title for the plots.
    title <- "Kaufmann scale"

  # BECKER 2012 SCALE
    # # Import radioisotpic ages, anchored astrochronology durations, and relative
    # # stratigraphic positions.
    # DevonianData <- read.csv("./data/DevonianData_Becker2012.csv",
    #                          header = TRUE)
    # # Import relative stratigraphic position of Stages.
    # DevonianPositions <- read.csv("./data/DevonianPositions_Becker2012.csv",
    #                               header = TRUE)
    # # Import relative stratigraphic position of conodont biozone positions.
    # DevonianPositions.c <- read.csv("./data/DevonianPositions_Becker 2012_conodont.csv",
    #                                 header = TRUE)
    # # Save a title for the plots.
    # title <- "Becker 2012 scale"
    
  # BECKER 2020 SCALE
    # # Import radioisotpic ages, anchored astrochronology durations, and relative
    # # stratigraphic positions.
    # DevonianData <- read.csv("./data/DevonianData_Becker2020.csv",
    #                          header = TRUE)
    # # Import relative stratigraphic position of Stages.
    # DevonianPositions <- read.csv("./data/DevonianPositions_Becker2020.csv",
    #                               header = TRUE)
    # # Import relative stratigraphic position of conodont biozone positions.
    # DevonianPositions.c <- read.csv("./data/DevonianPositions_Becker 2020_conodont.csv",
    #                                 header = TRUE)
    # # Save a title for the plots.
    # title <- "Becker 2020 scale"


## VISUALIZE MODEL INPUTS ------------------------------------------------------

# Visualize the age distributions and relative stratigraphic positions prior to 
# running the model.
  ageDepthPlot(ages = DevonianData$age,
               ageSds = DevonianData$uncertainty_2sig,
               positions = DevonianData$midpoint,
               positionThicknesses = DevonianData$halfwidth,
               distTypes = DevonianData$distTypes,
               ids = DevonianData$ids,
               xlim = c(435, 345),
               ylim = c(-10, 110),
               main = title)
  
  
## RUN MODEL -------------------------------------------------------------------

# Run ageModel function to create an age-depth model.
  DevonianModel <- ageModel(ages = DevonianData$age,
                            ageSds = DevonianData$uncertainty_2sig,
                            positions = DevonianData$midpoint,
                            positionThicknesses = DevonianData$halfwidth,
                            distTypes = DevonianData$distTypes,
                            ids = DevonianData$ids,
                            predictPositions = seq(from = -20, 
                                                   to = 120, 
                                                   by = 0.25)) 
  
# Or,
# Load in previous model results.
  # Uncomment and run the following code to load a previously saved model.
  # # KAUFMANN SCALE
  #   DevonianModel <- readRDS("./results/DevonianModel_Kaufmann.rds")
  # # BECKER 2012 SCALE
  #   DevonianModel <- readRDS("./results/DevonianModel_Becker2012.rds")
  # # BECKER 2020 SCALE
  #   DevonianModel <- readRDS("./results/DevonianModel_Becker2020.rds")
  
## SAVE MODEL ------------------------------------------------------------------

# Save model.
  # Uncomment and run the following code to save the ouput of the ageModel 
  # function (write DevonianModel to a file) so that it can be loaded into R and
  # used again later.
  # # KAUFMANN SCALE
  #   saveRDS(DevonianModel, file = "./results/DevonianModel_Kaufmann.rds",
  #           ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
  # # BECKER 2012 SCALE
  #   saveRDS(DevonianModel, file = "./results/DevonianModel_Becker2012.rds",
  #           ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
  # # BECKER 2020 SCALE
  #   saveRDS(DevonianModel, file = "./results/DevonianModel_Becker2020.rds",
  #           ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)

      
## PREDICT STAGE OR CONODONT BIOZONE BOUNDARY AGES -----------------------------
    
# Predict the age of the Stage boundaries using the agePredict function.
  DevonianPredict <- agePredict(model = DevonianModel,
                              newPositions = DevonianPositions$predictPositions,
                              ids = DevonianPositions$ids)
  # Save the median and 95% HDI bounds from the model results.
  HDI <- DevonianPredict$HDI
    
# Predict the age of the conodont biozone boundaries using the agePredict 
# function.
  DevonianPredict.c <- agePredict(model = DevonianModel,
                            newPositions = DevonianPositions.c$predictPositions,
                            ids = DevonianPositions.c$ids)
  # Save the median and 95% HDI bounds from the model results.
  HDI.c <- DevonianPredict.c$HDI
    
  
## SAVE STAGE OR CONODONT BIOZONE BOUNDARY AGES --------------------------------
  
# Save model results (predicted Stage or conodont biozone boundary ages). 
  # Uncomment and run the following code to write the median and highest 
  # denisty interval results for predicted positions to to a csv file.
  # # KAUFMANN SCALE
  #   write.csv(HDI,
  #             file = "./results/Results_Kaufmann_stage ages.csv")
  #   write.csv(HDI.c,
  #             file = "./results/Results_Kaufmann_alternative conodont biozone ages.csv")
  #   write.csv(HDI.c,
  #             file = "./results/Results_Kaufmann_standard conodont biozone ages.csv")
  # # BECKER 2012 SCALE
  #   write.csv(HDI,
  #             file = "./results/Results_Becker2012_stages ages.csv")
  #   write.csv(HDI.c,
  #             file = "./results/Results_Becker2012_conodont biozone ages.csv")
  # # BECKER 2020 SCALE
  #   write.csv(HDI,
  #             file = "./results/Results_Becker2020_stage ages.csv")
  #   write.csv(HDI.c,
  #             file = "./results/Results_Becker2020_conodont biozone ages.csv")

  
## VISUALIZE MODEL RESULTS -----------------------------------------------------

# Visualize the parameter plots.
  # posteriorPlot(model = DevonianModel, prob = 0.95)
  
# Visualize the age-depth model with a plot of the likelihoods illustrated as 
# PDFs.
  modelPlot(model = DevonianModel,
            scale = 0.5, predictLabels = c("both"), legend = c("adjacent"),
            main = title,
            xlim = c(435, 345),
            ylim = c(-10, 110))
  # Add a dashed line that goes from the model median position at the base of  
  # the Devonian to the model median position at the top of the Devonian. This   
  # line shows the position of the model median if the model is linearized.
  lines(x = c(HDI[which(HDI$ids == "baseLochkovian"), "0.5"], 
              HDI[which(HDI$ids == "baseCarboniferous"), "0.5"]),
        y = c(0, 100),
        col = "black",
        lty = "dashed",
        lwd = 2)
    
# Visualize the age model plot with the likelihoods illustrated as PDFs and
# the predicted Stage boundary positions shown as red error bars.
  # modelPlot(model = DevonianModel,
  #           agePredictOutput = DevonianPredict,
  #           scale = 0.5, predictLabels = c("both"), legend = c("adjacent"),
  #           main = title)

# Visualize the age model plot with the likelihoods illustrated as PDFs and
# the predicted conodont biozone boundary positions shown as red error bars.
  # modelPlot(model = DevonianModel,
  #           agePredictOutput = DevonianPredict.c,
  #           scale = 0.5, predictLabels = c("both"), legend = c("adjacent"),
  #           main = title)
  

## LINERIZE MODEL, EXTRACT STAGE POSITIONS ON REVISED SCALE --------------------
  
# Calculate an equation for a line that passes through the base of the Devonian
# and the base of the Carboniferous. 
  # Store the values for this line in a list.
  linearModel <- list(
    m = (100 - 0) / (HDI[which(HDI$ids == "baseCarboniferous"), "0.5"] -
                       HDI[which(HDI$ids == "baseLochkovian"), "0.5"]),
    b = (((100 - 0) / (HDI[which(HDI$ids == "baseCarboniferous"), "0.5"] -
                         HDI[which(HDI$ids == "baseLochkovian"), "0.5"])) * 
           HDI[which(HDI$ids == "baseLochkovian"), "0.5"]) * -1)

# Make a data frame of x and y values before and after rescaling.
  # Make data frame, save names of positions.
  rescaledScale <- data.frame(ids = DevonianPositions$ids,
                    # Save original y values (input into model).
                    y0 = DevonianPositions$predictPositions,  
                    # Save the x value of the linearized model at horizons of 
                    # interest (same as x values produced by the model).
                    x0 = HDI$`0.5`,  
                    # Save y value of the linearized model at x0.
                    y.rescale = (linearModel$m * HDI$`0.5`) + linearModel$b) 

# Plot difference between positions on model before and after rescaling.
  # Plot crosses representing Stage positions of ages from the age-depth model.
  plot(x = rescaledScale$x0,
       y = rescaledScale$y0, 
       type = "p",
       main = title,
       xlim = c(435, 345),
       ylim = c(-10, 110),
       xlab = "age (Ma)", 
       ylab = "scaled stratigraphic position",
       col = "blue",
       pch = 3)
  # Plot crosses representing shifting Stage positions along the y axis to 
  # linearize the model results.
  points(x = rescaledScale$x0,
         y = rescaledScale$y.rescale, 
         col = "red",
         pch = 3)
  # Add a legend.
  legend(425, 100, 
         title = "Stage positions",
         legend = c("age-depth model", "linearized model"),
         col = c("blue", "red"),
         pch = 3)
  
  
##  LINERIZE MODEL, EXTRACT CONODONT BIOZONE POSITIONS ON REVISED SCALE --------
  
# Calculate an equation for a line that passes through the base of the Devonian
# and the base of the Carboniferous. 
  # Store the values for this line in a list.  
  linearModel.c <- list(
    m = (100 - 0) / (HDI.c[which(HDI.c$ids == "baseCb"), "0.5"] -
                       HDI.c[which(HDI.c$ids == "baseD"), "0.5"]),
    b = (((100 - 0) / (HDI.c[which(HDI.c$ids == "baseCb"), "0.5"] -
                         HDI.c[which(HDI.c$ids == "baseD"), "0.5"])) * 
           HDI.c[which(HDI.c$ids == "baseD"), "0.5"]) * -1)
  
# Make a data frame of x and y values before and after rescaling.
  # Make data frame, save names of positions.
  rescaledScale.c <- data.frame(ids = DevonianPositions.c$ids,
                  # Save original y values (input into model).
                  y0 = DevonianPositions.c$predictPositions,  
                  # Save the x value of the linearized model at horizons of 
                  # interest (same as x values produced by the model).
                  x0 = HDI.c$`0.5`,  
                  # Save y value of the linearized model at x0.
                  y.rescale = (linearModel.c$m * HDI.c$`0.5`) + linearModel.c$b)   
  
# Plot difference between positions on model before and after rescaling.
  # Plot crosses representing Stage positions of ages from the age-depth model.
  plot(x = rescaledScale.c$x0,
       y = rescaledScale.c$y0, 
       type = "p",
       main = title,
       xlim = c(435, 345),
       ylim = c(-10, 110),
       xlab = "age (Ma)", 
       ylab = "scaled stratigraphic position",
       col = "blue",
       pch = 3)
  # Plot crosses representing shifting Stage positions along the y axis to 
  # linearize the model results.
  points(x = rescaledScale.c$x0,
         y = rescaledScale.c$y.rescale, 
         col = "red",
         pch = 3)
  # Add a legend.
  legend(425, 100, 
         title = "conodont biozone positions",
         legend = c("age-depth model", "linearized model"),
         col = c("blue", "red"),
         pch = 3)
  
  
## SAVE STAGE OR CONODONT BIOZONE BOUNDARY RESCALED STRATIGRAPHIC POSITIONS ----
  
# Save model results (predicted Stage or conodont biozone boundary ages). 
  # Uncomment and run the following code to write the median and highest 
  # denisty interval results for predicted positions to to a csv file.
  # # KAUFMANN SCALE
  #   write.csv(rescaledScale,
  #     file = "Results_Kaufmann_rescaled stage positions.csv")
  #   write.csv(rescaledScale.c,
  #     file = "Results_Kaufmann_rescaled alternative conodont biozone positions.csv")
  #   write.csv(rescaledScale.c,
  #     file = "Results_Kaufmann_rescaled standard conodont biozone positions.csv")
  # # BECKER 2012 SCALE
  #   write.csv(rescaledScale,
  #     file = "Results_Becker2012_rescaled stage positions.csv")
  #   write.csv(rescaledScale.c,
  #     file = "Results_Becker2012_rescaled conodont biozone positions.csv")
  # # BECKER 2020 SCALE
  #   write.csv(rescaledScale,
  #     file = "Results_Becker2020_rescaled stage positions.csv")
  #   write.csv(rescaledScale.c,
  #     file = "Results_Becker2020_rescaled conodont biozone positions.csv")
  

  