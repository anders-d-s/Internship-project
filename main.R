#clear variables
rm(list = ls())

#Load necessary packages
source("requirements.R")

load("Data/mom_groups.RData")
load("Data/ivol_groups.RData")
load("Data/monthly_factors.RData")

source("Rcode/PortfolioFormation.R")

source("Rcode/FF3LongShortRegressions.R")

source("Rcode/CumulativeReturnsPlot.R")

source("Rcode/AR1.R")