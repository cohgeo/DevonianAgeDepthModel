# DevonianAgeDepthModel

## Introduction

This repository contains the data, code, and result files for [Harrigan et al.,](https://pubs.geoscienceworld.org/gsa/gsabulletin/article-abstract/doi/10.1130/B36128.1/609662/Recalibrating-the-Devonian-time-scale-A-new-method?redirectedFrom=fulltext) [^1]. All astrochronology extrapolations and age-depth modeling was done in [R](https://www.r-project.org)[^2], and the scripts are available in this repository in the [R](./R/) folder. 

## Usage
Supplemental Material S4 is an R script (`Astrochronology.R`) used to anchor astrochronology durations to determine the astrochronologic age of stage boundaries used in the age-depth modeling. The .csv files used as input in that script are available in the [data](./data/) folder and are summarized in Table S4 (Supplemental Material S2). The results of anchoring the floating astrochronology durations are provided in Table S5 (Supplemental Material S2).

Supplemental Material S3 is an R script (`DevonianAgeDepthModel.R`) that runs an age-depth model on Devonian radioisotopic and anchored astrochronologic ages and their associated relative stratigraphic positions to predict the age of Devonian stage boundaries and the ages of conodont biozones boundaries. The .csv files used as input in that script are available in the [data](./data/) folder and summarized in Tables S6-S8 (Supplemental Material S2).

Running `DevonianAgeDepthModel.R` results in recalibrated stage and conodont biozone boundary ages and scaled stratigraphic positions for each scale. Because this procedure relies on a probabilistic model, the model results will vary slightly each time the model is run, even with the same starting parameters and data. Since the model highest density interval and 95% confidence interval will vary slightly between model runs, the linearization process will result in slightly different final stage and conodont biozone boundary scaled stratigraphic positions. We have provided the conodont biozone model results in Tables S9-S11 (Supplemental Material S2) with the caveat that these are one possible model outcome and running the code again could produce slight differences in the predicted ages and scaled stratigraphic positions. In sensitivity tests, we found that stage and conodont biozone boundary positions typically varied by an average of ~0.07 scaled stratigraphic position units (where 0 = the position of the base of the Devonian and 100 = the position of the base of the Carboniferous), and ages varied by an average of ~0.03 Ma.

[^1]: Harrigan, C.O., Schmitz, M.D., Over, D.J., Trayler, R.B., and Davydov, V.I., 2021, Recalibrating the Devonian time scale: A new method for integrating radioisotopic and astrochronologic ages in a Bayesian framework: GSA Bulletin, https://doi.org/10.1130/B36128.1.

[^2]: R Core Team, 2021, R: A Language and Environment for Statistical Computing: Vienna, Austria, R Foundation for Statistical Computing.
