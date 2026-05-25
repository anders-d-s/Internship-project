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
#c("22d", "252d")
vol_calc <- "22d"

if (type == "standard") {
  if (vol_calc == "22d") {
    print("22 Day Volatility")
    load("Data/ivol_groups_22dVol.RData")
    load("Data/mom_groups_22dVol.RData")
    load("Data/monthly_factors.RData")
    
  } else if (vol_calc == "252d") {
  print("252 Day Volatility")
  load("Data/ivol_groups.RData")
  load("Data/mom_groups.RData")
  load("Data/monthly_factors.RData")
  }
} else if (type == "postpublication") { 
  print("Post Publication")
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
