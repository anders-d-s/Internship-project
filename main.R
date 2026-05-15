#clear variables
rm(list = ls())

#Load necessary packages
source("requirements.R")

#source("Rcode/data.R")
#source("Rcode/ivol_mom_calc.R")
#source("Rcode/ivol_mom_groups.R")

load("Data/mom_groups.RData")
load("Data/ivol_groups.RData")
load("Data/monthly_factors.RData")

keep <- c("keep","ivol_groups","mom_groups","monthly_factors")

source("Rcode/PortfolioFormation.R")

source("Rcode/ControlForFirstPrincipalComponent.R")

source("Rcode/FF3LongShortRegressions.R")

source("Rcode/CumulativeReturnsPlot.R")

source("Rcode/AR1.R")

source("Rcode/SummaryStatistics.R")
