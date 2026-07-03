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
vol_type <- "252d"

#c("12_1", "9_1", 6_1", "3_1")
mom_type <- "6_1"

#-------------------------------------------------------------------------
mom_file_name <- paste0(
  "Data/mom_groups_",
  mom_type,
  "_",
  vol_type,
  "_ivol.RData"
)

ivol_file_name <- paste0(
  "Data/ivol_groups_",
  vol_type,
  ".RData"
)

load("Data/monthly_factors.RData")
e <- new.env()
obj_name <- load(mom_file_name, envir = e)
mom_groups <- e[[obj_name]]

obj_name <- load(ivol_file_name, envir = e)
ivol_groups <- e[[obj_name]]



if (type == "postpublication") { 
  print("Post Publication")
  load("RCode/PostPublication/mom_groups.RData")
  load("RCode/PostPublication/ivol_groups.RData")
  load("RCode/PostPublication/monthly_factors.RData")
}

keep <- c("keep","ivol_groups","mom_groups","monthly_factors")

#-------------------------------------------------------------------------

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
source("Rcode/MonotonicityTest_5x5")

# univariate portfolio
rm(list = ls())
source("Rcode/UnivariatePortfolioSort.R")
