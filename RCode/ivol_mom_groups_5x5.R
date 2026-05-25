load("Data/monthly_ivol.RData")
load("Data/monthly_factors.RData")

# Remove mkt factor
monthly_factors <- monthly_factors[, setdiff(names(monthly_factors), "mkt")]
row.names(monthly_factors) <- NULL

################################################################################
# IVOL — quintile breakpoints (20/40/60/80)
ivol_groups <- monthly_ivol
ivol_groups[, -which(names(ivol_groups) == "date")] <- NA

factor_cols <- setdiff(names(monthly_ivol), "date")

for (i in 1:nrow(monthly_ivol)) {
  
  row_vals <- as.numeric(monthly_ivol[i, factor_cols])
  
  p20 <- quantile(row_vals, probs = 0.2, na.rm = TRUE)
  p40 <- quantile(row_vals, probs = 0.4, na.rm = TRUE)
  p60 <- quantile(row_vals, probs = 0.6, na.rm = TRUE)
  p80 <- quantile(row_vals, probs = 0.8, na.rm = TRUE)
  
  groups <- ifelse(row_vals <= p20, "IV1",
                   ifelse(row_vals <= p40, "IV2",
                          ifelse(row_vals <= p60, "IV3",
                                 ifelse(row_vals <= p80, "IV4", "IV5"))))
  
  ivol_groups[i, factor_cols] <- as.list(groups)
}

save(ivol_groups, file = "Data/ivol_groups_5x5.RData")

################################################################################
# MOM — quintile breakpoints within each IVOL group
factor_cols <- setdiff(names(monthly_factors), "date")

mom_groups <- monthly_factors[, c("date", factor_cols)]
mom_groups[, factor_cols] <- NA
mom_groups <- mom_groups[-(1:12), ]
row.names(mom_groups) <- NULL

for (i in 1:(nrow(monthly_factors) - 12)) {
  
  mom_vals <- as.numeric(monthly_factors[i + 11, factor_cols])
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  
  mom_row <- rep(NA, length(mom_vals))
  
  for (iv in c("IV1", "IV2", "IV3", "IV4", "IV5")) {
    
    idx_iv <- which(ivol_grp == iv)
    
    if (length(idx_iv) > 0) {
      
      mom_subset <- mom_vals[idx_iv]
      
      p20 <- quantile(mom_subset, 0.2, na.rm = TRUE)
      p40 <- quantile(mom_subset, 0.4, na.rm = TRUE)
      p60 <- quantile(mom_subset, 0.6, na.rm = TRUE)
      p80 <- quantile(mom_subset, 0.8, na.rm = TRUE)
      
      mom_row[idx_iv] <- ifelse(mom_subset <= p20, "M1",
                                ifelse(mom_subset <= p40, "M2",
                                       ifelse(mom_subset <= p60, "M3",
                                              ifelse(mom_subset <= p80, "M4", "M5"))))
    }
  }
  
  mom_groups[i, factor_cols] <- mom_row
}

save(mom_groups, file = "Data/mom_groups_5x5.RData")

################################################################################
# Test — counts and percentages
tab <- table(
  IVOL = as.vector(as.matrix(ivol_groups[, factor_cols])),
  MOM  = as.vector(as.matrix(mom_groups[, factor_cols]))
)

tab_percent <- prop.table(tab, margin = 1) * 100
tab_percent
