#Note: You need to truncate the data from beginning and rerun data.R, ivol_mom_calc.R and ivol_mom_groups.R


#prepublication data is set to NA
factor_info <- read_excel("Data/FactorInformation.xlsx")
factor_info$releasedate[is.na(factor_info$releasedate)] <- round(mean(factor_info$releasedate, na.rm = TRUE))

daily_factors_adjusted <- daily_factors

for (col in factor_info$name) {
  if (col %in% names(daily_factors_adjusted)) {
    publicationdate <- factor_info$releasedate[factor_info$name == col]
    daily_factors_adjusted[[col]][year(daily_factors_adjusted$date) < publicationdate] <- NA
  }
}

remove(daily_factors)

daily_factors <- daily_factors_adjusted

remove(daily_factors_adjusted)
