load("Data/monthly_ivol_252d.RData")

#################################################################################
# IVOL
# 30p/70p
ivol_groups <- monthly_ivol
ivol_groups[, -which(names(ivol_groups) == "date")] <- NA

factor_cols <- setdiff(names(monthly_ivol), "date")

for (i in 1:nrow(monthly_ivol)) {
  
  row_vals <- as.numeric(monthly_ivol[i, factor_cols])
  
  p30 <- quantile(row_vals, probs = 0.3, na.rm = TRUE)
  p70 <- quantile(row_vals, probs = 0.7, na.rm = TRUE)
  
  groups <- ifelse(row_vals <= p30, "IV1",
                   ifelse(row_vals <= p70, "IV2", "IV3"))
  
  ivol_groups[i, factor_cols] <- as.list(groups)
}

save(ivol_groups, file = "RCode/Independent/ivol_groups_252d.RData")


###########################################################################
# MOM
# Independent double sort:
# Rank momentum across the FULL cross-section each month
# (not within IVOL groups).

build_mom_groups_independent <- function(mom_signal, factor_cols) {
  
  # Remove leading rows where the momentum signal is unavailable
  has_mom <- rowSums(!is.na(mom_signal[, factor_cols, drop = FALSE])) > 0
  mom_signal <- mom_signal[has_mom, ]
  
  mom_groups <- mom_signal[, c("date", factor_cols)]
  mom_groups[, factor_cols] <- NA
  row.names(mom_groups) <- NULL
  
  for (i in seq_len(nrow(mom_signal))) {
    
    mom_vals <- as.numeric(mom_signal[i, factor_cols])
    
    p30 <- quantile(mom_vals, probs = 0.30, na.rm = TRUE)
    p70 <- quantile(mom_vals, probs = 0.70, na.rm = TRUE)
    
    groups <- ifelse(mom_vals <= p30, "M1",
                     ifelse(mom_vals <= p70, "M2", "M3"))
    
    mom_groups[i, factor_cols] <- as.list(groups)
  }
  
  mom_groups
}

# Load the four momentum signals
load("Data/mom_signal_12_1.RData")
load("Data/mom_signal_9_1.RData")
load("Data/mom_signal_6_1.RData")
load("Data/mom_signal_3_1.RData")

factor_cols_mom <- setdiff(names(mom_signal_12_1), "date")

# Check that all momentum files contain the same factor columns
stopifnot(identical(sort(factor_cols_mom), sort(factor_cols)))

# Build independent momentum groups
mom_groups_12_1_252d_independent <- tail(
  build_mom_groups_independent(mom_signal_12_1, factor_cols_mom),
  404
)

mom_groups_9_1_252d_independent <- tail(
  build_mom_groups_independent(mom_signal_9_1, factor_cols_mom),
  404
)

mom_groups_6_1_252d_independent <- tail(
  build_mom_groups_independent(mom_signal_6_1, factor_cols_mom),
  404
)

mom_groups_3_1_252d_independent <- tail(
  build_mom_groups_independent(mom_signal_3_1, factor_cols_mom),
  404
)

# Save
save(
  mom_groups_12_1_252d_independent,
  file = "RCode/Independent/mom_groups_12_1_252d_independent.RData"
)

save(
  mom_groups_9_1_252d_independent,
  file = "RCode/Independent/mom_groups_9_1_252d_independent.RData"
)

save(
  mom_groups_6_1_252d_independent,
  file = "RCode/Independent/mom_groups_6_1_252d_independent.RData"
)

save(
  mom_groups_3_1_252d_independent,
  file = "RCode/Independent/mom_groups_3_1_252d_independent.RData"
)

###########################################################################
# Test

check_tab <- function(mom_groups, ivol_groups, factor_cols) {
  
  # Keep only common dates
  common_dates <- intersect(mom_groups$date, ivol_groups$date)
  
  ivol_sub <- ivol_groups[ivol_groups$date %in% common_dates, factor_cols]
  mom_sub  <- mom_groups[mom_groups$date %in% common_dates, factor_cols]
  
  tab <- table(
    IVOL = as.vector(as.matrix(ivol_sub)),
    MOM  = as.vector(as.matrix(mom_sub))
  )
  
  prop.table(tab) * 100
}

check_tab(
  mom_groups_12_1_252d_independent,
  ivol_groups,
  factor_cols_mom
)

check_tab(
  mom_groups_9_1_252d_independent,
  ivol_groups,
  factor_cols_mom
)

check_tab(
  mom_groups_6_1_252d_independent,
  ivol_groups,
  factor_cols_mom
)

check_tab(
  mom_groups_3_1_252d_independent,
  ivol_groups,
  factor_cols_mom
)
