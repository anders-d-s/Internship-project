#clear variables
rm(list = ls())

#Load necessary packages
source("requirements.R")

#Rerun post publication data code
source("RCode/PostPublication/data.R")
source("RCode/PostPublication/ivol_mom_calc.R")
source("RCode/PostPublication/ivol_mom_groups.R")

#c("standard", "postpublication")

type <- "standard"

if (type == "standard") {
  load("Data/mom_groups.RData")
  load("Data/ivol_groups.RData")
  load("Data/monthly_factors.RData")
} else if (type == "postpublication") {
  load("RCode/PostPublication/mom_groups.RData")
  load("RCode/PostPublication/ivol_groups.RData")
  load("RCode/PostPublication/monthly_factors.RData")
}

keep <- c("keep","ivol_groups","mom_groups","monthly_factors")

source("Rcode/PortfolioFormation_3x3.R")

source("Rcode/MonotonicityTest.R")

source("Rcode/ControlForFirstPrincipalComponent.R")

source("Rcode/FF3LongShortRegressions.R")

source("Rcode/CumulativeReturnsPlot.R")

source("Rcode/AR1.R")

source("Rcode/SummaryStatistics.R")

# 5x5 results
rm(list = ls())
source("Rcode/PortfolioFormation_5x5.R")
