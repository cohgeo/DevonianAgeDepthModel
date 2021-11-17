# This script calculates ages and propagates uncertainty for floating 
# astrochronology Stage durations.

# Updated 2020.08.14 CH

## SETUP -----------------------------------------------------------------------

# Clear all from workspace/environment.
# rm(list = ls())  # Uncomment if needed

# Import data frames.
  # Import data frame of ages and uncertainties of anchor points.
  DF.ast.anchors <- read.csv("./data/DF.ast.anchors.csv", 
                             header = TRUE)
  # Import data frame of durations and uncertainties of Stages and parts of 
  # Stages.
  DF.ast.dur <- read.csv("./data/DF.ast.dur.csv", 
                         header = TRUE)
  # Import data frame to hold results of the astrochronology propagation done in
  # this script.
  DF.ast <- read.csv("./data/DF.ast.csv", 
                     header = TRUE)

# Indicate which of the conodont scales (Kaufmann, Becker 2012, Becker 2020) to
# use for this iteration of running the script below.
  # KAUFMANN SCALE
    scale <- "Kaufmann"
    # Set where to store extrapolation results in DF.ast.
    age.col <- 2
    age.uncert.col <- 3
  # BECKER 2012 SCALE
    # scale <- "Becker 2012"
    # # Set where to store extrapolation results in DF.ast.
    # age.col <- 4
    # age.uncert.col <- 5
  # BECKER 2020 SCALE
    # scale <- "Becker 2020"
    # # Set where to store extrapolation results in DF.ast.
    # age.col <- 6
    # age.uncert.col <- 7
  
  
## PLOTTING TEMPLATES ----------------------------------------------------------

# Plot anchoring age distribution (Gaussian).
  # Set the anchoring age to plot.
  anchor <- "D6"
  # Create a sequence of x values.
  x.G <- seq(DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                            colnames(DF.ast.anchors) == "age"] -
               (5 * DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                                colnames(DF.ast.anchors) == "age.uncertainty"]),
             DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                            colnames(DF.ast.anchors) == "age"] +
               (5 * DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                                colnames(DF.ast.anchors) == "age.uncertainty"]),
             length = 1000)
  # Calculate y values based on age and uncertainty.
  y.G <- dnorm(x.G, 
               DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                              colnames(DF.ast.anchors) == "age"],
               DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                              colnames(DF.ast.anchors) == "age.uncertainty"])
  # Make plot of age distribution.
  plot(x.G, y.G / max(y.G), 
       type = "l", main = anchor, xlab = "Age (Ma)", ylab = "Probability")
  
# Plot an astrochronology duration distribution (uniform).
  # Set the Stage duration to plot.
  stage <- "Eifelian.above.A-D14"
  # Create a squence of x values.
  x.U <- seq((DF.ast.dur[which(DF.ast.dur$stage == stage), 
                         colnames(DF.ast.dur) == "duration"] - 
                DF.ast.dur[which(DF.ast.dur$stage == stage), 
                           colnames(DF.ast.dur) == "dur.uncertainty"] - 1), 
             (DF.ast.dur[which(DF.ast.dur$stage == stage), 
                         colnames(DF.ast.dur) == "duration"] + 
                DF.ast.dur[which(DF.ast.dur$stage == stage), 
                           colnames(DF.ast.dur) == "dur.uncertainty"] + 1), 
             length = 1000)
  y.U <- dunif(x.U, 
               DF.ast.dur[which(DF.ast.dur$stage == stage), 
                          colnames(DF.ast.dur) == "duration"] - 
                 DF.ast.dur[which(DF.ast.dur$stage == stage), 
                            colnames(DF.ast.dur) == "dur.uncertainty"], 
               DF.ast.dur[which(DF.ast.dur$stage == stage), 
                          colnames(DF.ast.dur) == "duration"] + 
                 DF.ast.dur[which(DF.ast.dur$stage == stage), 
                            colnames(DF.ast.dur) == "dur.uncertainty"])
  # Make plot of age distribution.
  plot(x.U, y.U / max(y.U), 
       type = "l", main = stage, xlab = "Age (Ma)", ylab = "Probability")
  

## ANCHOR: D27 -----------------------------------------------------------------
# NOTE: The ages for the Stage boundaries based on anchors D27, D15, and D14 are 
# the same for the three scales (Kaufmann, Becker 2012, Becker 2020).
  
# Using D27 as an anchor, determine the age and uncertainty of the next Stage
# boundary down in depth. Sample a Gaussian distribution for the anchoring age
# and a uniform distribution for the astrochronology duration n times to get a
# new distribution representing the age of the Stage boundary of interest. 
# Use the mean and two standard deviation value to represent the age of that
# distribution (assumes a Gaussian distribution).
  
# Set the number of times to randomly sample each distribution.
  n <- 100000  
  
# Set the anchor point and Stage.
  anchor <- "D27"
  stage <- "Famennian"
  
# Create a new distribution for the base of the Famennian.
  # Sample the distribution for D27. Divide age.uncertainty by 2 to get 1 sigma.
  dist.A_baseFamennian_D27 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Famennian duration.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
# Make a density plot of the new distribution.
  plot(density(dist.A_baseFamennian_D27))
# Make a histogram of the new distribution.
  hist(dist.A_baseFamennian_D27)
# Save results.
  # Calculate the mean of the new distribution to use as the new age for 
  # the base of the Famennian.
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D27"), c(2, 4, 6)] <- 
    mean(dist.A_baseFamennian_D27)
  # Calculate the standard deviation of the new distribution and multiply it by
  # 2 to use as the 2-sigma uncertainty for this distribution.
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D27"), c(3, 5, 7)] <- 
    sd(dist.A_baseFamennian_D27) * 2
  
# Create a new distribution for the base of the Frasnian.
  stage <- "Frasnian"
  # Sample dist.A_baseFamennian_D27 and the duration of the Frasnian.
  dist.A_baseFrasnian_D27 <- sample(dist.A_baseFamennian_D27,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseFamennian_D27) +
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseFrasnian_D27))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D27"), c(2, 4, 6)] <- 
    mean(dist.A_baseFrasnian_D27)  # mean
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D27"), c(3, 5, 7)] <- 
    sd(dist.A_baseFrasnian_D27) * 2  # 2 sigma
  
# Create a new distribution for the base of the Givetian.
  stage <- "Givetian"
  # Sample dist.A_baseFrasnian_D27 and the duration of the Givetian.
  dist.A_baseGivetian_D27 <- sample(dist.A_baseFrasnian_D27,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseFrasnian_D27) +
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseGivetian_D27))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D27"), c(2, 4, 6)] <- 
    mean(dist.A_baseGivetian_D27)  # mean
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D27"), c(3, 5, 7)] <- 
    sd(dist.A_baseGivetian_D27) * 2  # 2 sigma
  
# Create a new distribution for the base of the Eifelian.
  stage <- "Eifelian"
  # Sample dist.A_baseGivetian_D27 and the duration of the Eifelian.
  dist.A_baseEifelian_D27 <- sample(dist.A_baseGivetian_D27,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseGivetian_D27) +
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEifelian_D27))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D27"), c(2, 4, 6)] <- 
    mean(dist.A_baseEifelian_D27)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D27"), c(3, 5, 7)] <- 
    sd(dist.A_baseEifelian_D27) * 2  # 2 sigma
  
  
## ANCHOR: D15 -----------------------------------------------------------------
# NOTE: The ages for the Stage boundaries based on anchors D27, D15, and D14 are 
# the same for the three scales (Kaufmann, Becker 2012, Becker 2020).
  
# Using D15 as an anchor, determine the age and uncertainty of the Stage 
# boundaries.

# Set the anchor point.
  anchor <- "D15"
  
# Set the age and uncertainty of A-D15 to be the same as D15.
  DF.ast[which(DF.ast$ID == "A-D15"), c(2, 4, 6)] <- 
    DF.ast.anchors[which(DF.ast.anchors$anchor == "D15"), 2]
  DF.ast[which(DF.ast$ID == "A-D15"), c(3, 5, 7)] <- 
    DF.ast.anchors[which(DF.ast.anchors$anchor == "D15"), 3] 
  
# Create a new distribution for the base of the Eifelian.
  # Set the Stage.
  stage <- "Eifelian.below.A-D15"
  # Sample the distribution for A-D15. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseEifelian_D15 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Eifelian duration below A-D15.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseEifelian_D15))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D15"), c(2, 4, 6)] <- 
    mean(dist.A_baseEifelian_D15)
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D15"), c(3, 5, 7)] <- 
    sd(dist.A_baseEifelian_D15) * 2
 
# Create a new distribution for the base of the Givetian.
  stage <- "Eifelian.above.A-D15"
  # Sample the distribution for A-D15. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseGivetian_D15 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Eifelian duration below A-D15.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseGivetian_D15))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D15"), c(2, 4, 6)] <- 
    mean(dist.A_baseGivetian_D15)
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D15"), c(3, 5, 7)] <- 
    sd(dist.A_baseGivetian_D15) * 2
  
# Sample dist.A_baseGivetian_D15 and the duration of the Givetian to get an age
# for the base of the Frasnian.
  stage <- "Givetian"
  dist.A_baseFrasnian_D15 <- sample(dist.A_baseGivetian_D15,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseGivetian_D15) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseFrasnian_D15))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D15"), c(2, 4, 6)] <- 
    mean(dist.A_baseFrasnian_D15)  # mean
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D15"), c(3, 5, 7)] <- 
    sd(dist.A_baseFrasnian_D15) * 2  # 2 sigma

# Sample dist.A_baseFrasnian_D15 and the duration of the Frasnian to get an age
  # for the base of the Frasnian.
  stage <- "Frasnian"
  dist.A_baseFamennian_D15 <- sample(dist.A_baseFrasnian_D15,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseFrasnian_D15) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseFamennian_D15))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D15"), c(2, 4, 6)] <- 
    mean(dist.A_baseFamennian_D15)  # mean
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D15"), c(3, 5, 7)] <- 
    sd(dist.A_baseFamennian_D15) * 2  # 2 sigma
  
# Sample dist.A_baseFamennian_D15 and the duration of the Famennian to get an 
# age for the base of the Carboniferous.
  stage <- "Famennian"
  dist.A_baseCarboniferous_D15 <- sample(dist.A_baseFamennian_D15,
                                     size = n,
                                     replace = TRUE,
                                     prob = dist.A_baseFamennian_D15) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseCarboniferous_D15))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseCarboniferous-D15"), c(2, 4, 6)] <- 
    mean(dist.A_baseCarboniferous_D15)  # mean
  DF.ast[which(DF.ast$ID == "A-baseCarboniferous-D15"), c(3, 5, 7)] <- 
    sd(dist.A_baseCarboniferous_D15) * 2  # 2 sigma
 
  
## ANCHOR: D14 -----------------------------------------------------------------
# NOTE: The ages for the Stage boundaries based on anchors D27, D15, and D14 are 
# the same for the three scales (Kaufmann, Becker 2012, Becker 2020).
  
# Using D14 as an anchor, determine the age and uncertainty of the Stage 
# boundaries.
  
# Set the anchor point.
  anchor <- "D14"
  
# Set the age and uncertainty of A-D14 to be the same as D14.
  DF.ast[which(DF.ast$ID == "A-D14"), c(2, 4, 6)] <- 
    DF.ast.anchors[which(DF.ast.anchors$anchor == "D14"), 2]
  DF.ast[which(DF.ast$ID == "A-D14"), c(3, 5, 7)] <- 
    DF.ast.anchors[which(DF.ast.anchors$anchor == "D14"), 3] 
  
# Create a new distribution for the base of the Eifelian.
  # Set the Stage.
  stage <- "Eifelian.below.A-D14"
  # Sample the distribution for A-D14. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseEifelian_D14 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Eifelian duration below A-D14.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseEifelian_D14))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D14"), c(2, 4, 6)] <- 
    mean(dist.A_baseEifelian_D14)
  DF.ast[which(DF.ast$ID == "A-baseEifelian-D14"), c(3, 5, 7)] <- 
    sd(dist.A_baseEifelian_D14) * 2
  
# Create a new distribution for the base of the Givetian.
  stage <- "Eifelian.above.A-D14"
  # Sample the distribution for A-D14. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseGivetian_D14 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Eifelian duration below A-D14.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseGivetian_D14))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D14"), c(2, 4, 6)] <- 
    mean(dist.A_baseGivetian_D14)
  DF.ast[which(DF.ast$ID == "A-baseGivetian-D14"), c(3, 5, 7)] <- 
    sd(dist.A_baseGivetian_D14) * 2
  
# Sample dist.A_baseGivetian_D14 and the duration of the Givetian to get an age
# for the base of the Frasnian.
  stage <- "Givetian"
  dist.A_baseFrasnian_D14 <- sample(dist.A_baseGivetian_D14,
                                    size = n,
                                    replace = TRUE,
                                    prob = dist.A_baseGivetian_D14) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseFrasnian_D14))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D14"), c(2, 4, 6)] <- 
    mean(dist.A_baseFrasnian_D14)  # mean
  DF.ast[which(DF.ast$ID == "A-baseFrasnian-D14"), c(3, 5, 7)] <- 
    sd(dist.A_baseFrasnian_D14) * 2  # 2 sigma
  
# Sample dist.A_baseFrasnian_D14 and the duration of the Frasnian to get an age
# for the base of the Frasnian.
  stage <- "Frasnian"
  dist.A_baseFamennian_D14 <- sample(dist.A_baseFrasnian_D14,
                                     size = n,
                                     replace = TRUE,
                                     prob = dist.A_baseFrasnian_D14) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseFamennian_D14))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D14"), c(2, 4, 6)] <- 
    mean(dist.A_baseFamennian_D14)  # mean
  DF.ast[which(DF.ast$ID == "A-baseFamennian-D14"), c(3, 5, 7)] <- 
    sd(dist.A_baseFamennian_D14) * 2  # 2 sigma
  
# Sample dist.A_baseFamennian_D14 and the duration of the Famennian to get an 
# age for the base of the Carboniferous.
  stage <- "Famennian"
  dist.A_baseCarboniferous_D14 <- sample(dist.A_baseFamennian_D14,
                                         size = n,
                                         replace = TRUE,
                                         prob = dist.A_baseFamennian_D14) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseCarboniferous_D14))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseCarboniferous-D14"), c(2, 4, 6)] <- 
    mean(dist.A_baseCarboniferous_D14)  # mean
  DF.ast[which(DF.ast$ID == "A-baseCarboniferous-D14"), c(3, 5, 7)] <- 
    sd(dist.A_baseCarboniferous_D14) * 2  # 2 sigma
  
  
## ANCHOR: D6, KAUFMANN ---------------------------------------------------------
  
# Using D6 as an anchor, determine the age and uncertainty of the Kaufmann  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D6"

# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.K"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D6_K <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D6_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 2] <- 
    mean(dist.A_baseLochkovian_D6_K)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 3] <- 
    sd(dist.A_baseLochkovian_D6_K) * 2
  
# Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.K"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D6_K <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D6_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 2] <- 
    mean(dist.A_basePragian_D6_K)
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 3] <- 
    sd(dist.A_basePragian_D6_K) * 2
  
# Sample dist.A_basePragian_D6_K and the duration of the Pragian to get an age
  # for the base of the Emsian
  stage <- "Pragian"
  dist.A_baseEmsian_D6_K <- sample(dist.A_basePragian_D6_K,
                                 size = n,
                                 replace = TRUE,
                                 prob = dist.A_basePragian_D6_K) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D6_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 2] <- 
    mean(dist.A_baseEmsian_D6_K)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 3] <- 
    sd(dist.A_baseEmsian_D6_K) * 2  # 2 sigma
  
  
## ANCHOR: D5, KAUFMANN ---------------------------------------------------------

# Using D5 as an anchor, determine the age and uncertainty of the Kaufmann  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D5"
  
# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.K"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D5_K <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D5_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 2] <- 
    mean(dist.A_baseLochkovian_D5_K)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 3] <- 
    sd(dist.A_baseLochkovian_D5_K) * 2
  
  # Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.K"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D5_K <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D5_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 2] <- 
    mean(dist.A_basePragian_D5_K)
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 3] <- 
    sd(dist.A_basePragian_D5_K) * 2
  
  # Sample dist.A_basePragian_D5_K and the duration of the Pragian to get an age
  # for the base of the Emsian
  stage <- "Pragian"
  dist.A_baseEmsian_D5_K <- sample(dist.A_basePragian_D5_K,
                                 size = n,
                                 replace = TRUE,
                                 prob = dist.A_basePragian_D5_K) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D5_K))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 2] <- 
    mean(dist.A_baseEmsian_D5_K)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 3] <- 
    sd(dist.A_baseEmsian_D5_K) * 2  # 2 sigma

## ANCHOR: D6, BECKER 2012 ------------------------------------------------------
  
# Using D6 as an anchor, determine the age and uncertainty of the Becker 2012  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D6"
  
# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.B12"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D6_B12 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D6_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 4] <- 
    mean(dist.A_baseLochkovian_D6_B12)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 5] <- 
    sd(dist.A_baseLochkovian_D6_B12) * 2
  
# Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.B12"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D6_B12 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D6_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 4] <- 
    mean(dist.A_basePragian_D6_B12)
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 5] <- 
    sd(dist.A_basePragian_D6_B12) * 2
  
# Sample dist.A_basePragian_D6_B12 and the duration of the Pragian to get an age
# for the base of the Emsian.
  stage <- "Pragian"
  dist.A_baseEmsian_D6_B12 <- sample(dist.A_basePragian_D6_B12,
                                 size = n,
                                 replace = TRUE,
                                 prob = dist.A_basePragian_D6_B12) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D6_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 4] <- 
    mean(dist.A_baseEmsian_D6_B12)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 5] <- 
    sd(dist.A_baseEmsian_D6_B12) * 2  # 2 sigma
  
  
## ANCHOR: D5, BECKER 2012 -----------------------------------------------------
  
# Using D5 as an anchor, determine the age and uncertainty of the Becker 2012  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D5"
  
# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.B12"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D5_B12 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D5_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 4] <- 
    mean(dist.A_baseLochkovian_D5_B12)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 5] <- 
    sd(dist.A_baseLochkovian_D5_B12) * 2
  
# Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.B12"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D5_B12 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D5_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 4] <- 
    mean(dist.A_basePragian_D5_B12)
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 5] <- 
    sd(dist.A_basePragian_D5_B12) * 2
  
# Sample dist.A_basePragian_D5_B12 and the duration of the Pragian to get an age
# for the base of the Emsian
  stage <- "Pragian"
  dist.A_baseEmsian_D5_B12 <- sample(dist.A_basePragian_D5_B12,
                                 size = n,
                                 replace = TRUE,
                                 prob = dist.A_basePragian_D5_B12) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D5_B12))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 4] <- 
    mean(dist.A_baseEmsian_D5_B12)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 5] <- 
    sd(dist.A_baseEmsian_D5_B12) * 2  # 2 sigma  
  

## ANCHOR: D6, BECKER 2020 -----------------------------------------------------
  
# Using D6 as an anchor, determine the age and uncertainty of the Becker 2020  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D6"
  
# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.B20"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D6_B20 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D6_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 6] <- 
    mean(dist.A_baseLochkovian_D6_B20)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D6"), 7] <- 
    sd(dist.A_baseLochkovian_D6_B20) * 2
  
# Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.B20"
  # Sample the distribution for D6. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D6_B20 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D6.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D6_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 6] <- 
    mean(dist.A_basePragian_D6_B20)
  DF.ast[which(DF.ast$ID == "A-basePragian-D6"), 7] <- 
    sd(dist.A_basePragian_D6_B20) * 2
  
# Sample dist.A_basePragian_D6_B20 and the duration of the Pragian to get an age
# for the base of the Emsian.
  stage <- "Pragian"
  dist.A_baseEmsian_D6_B20 <- sample(dist.A_basePragian_D6_B20,
                                     size = n,
                                     replace = TRUE,
                                     prob = dist.A_basePragian_D6_B20) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D6_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 6] <- 
    mean(dist.A_baseEmsian_D6_B20)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D6"), 7] <- 
    sd(dist.A_baseEmsian_D6_B20) * 2  # 2 sigma
  
  
## ANCHOR D5, BECKER 2020 ------------------------------------------------------
  
# Using D5 as an anchor, determine the age and uncertainty of the Becker 2020  
# Stage boundaries.
  
# Set the anchor point.
  anchor <- "D5"
  
# Create a new distribution for the base of the Lochkovian.
  # Set the Stage.
  stage <- "Lochkovian.below.D5D6.B20"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_baseLochkovian_D5_B20 <- rnorm(n,
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) + 
    # Sample the distribution of the Lochkovian duration below D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_baseLochkovian_D5_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 6] <- 
    mean(dist.A_baseLochkovian_D5_B20)
  DF.ast[which(DF.ast$ID == "A-baseLochkovian-D5"), 7] <- 
    sd(dist.A_baseLochkovian_D5_B20) * 2
  
# Create a new distribution for the base of the Pragian.
  stage <- "Lochkovian.above.D5D6.B20"
  # Sample the distribution for D5. Divide age.uncertainty by 2 to get 1 
  # sigma.
  dist.A_basePragian_D5_B20 <- rnorm(n, 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age"], 
    DF.ast.anchors[which(DF.ast.anchors$anchor == anchor), 
                   colnames(DF.ast.anchors) == "age.uncertainty"] / 2) - 
    # Sample the distribution of the Pragian duration above D5.
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Make a density plot of the new distribution.
  plot(density(dist.A_basePragian_D5_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 6] <- 
    mean(dist.A_basePragian_D5_B20)
  DF.ast[which(DF.ast$ID == "A-basePragian-D5"), 7] <- 
    sd(dist.A_basePragian_D5_B20) * 2
  
# Sample dist.A_basePragian_D5_B20 and the duration of the Pragian to get an age
# for the base of the Emsian.
  stage <- "Pragian"
  dist.A_baseEmsian_D5_B20 <- sample(dist.A_basePragian_D5_B20,
                                     size = n,
                                     replace = TRUE,
                                     prob = dist.A_basePragian_D5_B20) -
    runif(n, 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] - 
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"], 
          DF.ast.dur[which(DF.ast.dur$stage == stage), 
                     colnames(DF.ast.dur) == "duration"] +
            DF.ast.dur[which(DF.ast.dur$stage == stage), 
                       colnames(DF.ast.dur) == "dur.uncertainty"]) 
  # Plot results.
  plot(density(dist.A_baseEmsian_D5_B20))
  # Save results.
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 6] <- 
    mean(dist.A_baseEmsian_D5_B20)  # mean
  DF.ast[which(DF.ast$ID == "A-baseEmsian-D5"), 7] <- 
    sd(dist.A_baseEmsian_D5_B20) * 2  # 2 sigma  
  
  
## SAVE DF.ast -----------------------------------------------------------------
  
# Save DF.ast data frame as a .csv file.
  # write.csv(DF.ast, file = "./results/DF.ast.complete.csv")
  
  
  
  
  
  
  
  
  
  
  
  