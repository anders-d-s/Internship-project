#clear variables
rm(list = ls())

#Load necessary packages
source("requirements.R")

#-------------------------------------------------------------------------
#run post publication code
source("RCode/PostPublication/data.R")
source("RCode/PostPublication/ivol_mom_calc.R")
source("RCode/PostPublication/ivol_mom_groups.R")
#-------------------------------------------------------------------------
#c("22d", "252d")
vol_type <- "252d"

#c("12_1", "9_1", 6_1", "3_1")
mom_type <- "12_1"

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
source("Rcode/5x5/ivol_mom_groups_5x5.R")
source("Rcode/5x5/PortfolioFormation_5x5.R")
source("Rcode/5x5/MonotonicityTest_5x5.R")

# univariate portfolio
rm(list = ls())
source("Rcode/UnivariatePortfolioSort.R")
