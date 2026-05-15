#Note: You need to truncate the data from beginning and rerun data.R, ivol_mom_calc.R and ivol_mom_groups.R



#prepublication data is set to NA
factor_info <- read_excel("Data/FactorInformation.xlsx")
factor_info$releasedate[is.na(factor_info$releasedate)] <- round(mean(factor_info$releasedate, na.rm = TRUE))

monthly_factors_adjusted <- monthly_factors

for (col in factor_info$name) {
  if (col %in% names(monthly_factors_adjusted)) {
    publicationdate <- factor_info$releasedate[factor_info$name == col]
    monthly_factors_adjusted[[col]][year(monthly_factors_adjusted$date) < publicationdate] <- NA
  }
}


