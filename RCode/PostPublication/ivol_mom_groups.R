load("RCode/PostPublication/monthly_factors.RData")

# Exclude date and mkt, keep only the factor return columns
factor_cols <- setdiff(names(monthly_factors), c("date", "mkt"))

log_rets <- log(1 + monthly_factors[, factor_cols])

build_mom_signal <- function(log_rets, N, dates) {
  
  width <- N - 1
  
  roll_sum <- zoo::rollapply(log_rets, width = width, FUN = sum, align = "right",
                             fill = NA, na.rm = FALSE)
  
  n <- nrow(roll_sum)
  mom_log <- rbind(
    matrix(NA_real_, nrow = 1, ncol = ncol(roll_sum),
           dimnames = list(NULL, colnames(roll_sum))),
    roll_sum[1:(n - 1), , drop = FALSE]
  )
  
  mom <- exp(mom_log) - 1
  
  out <- data.frame(date = dates, mom)
  row.names(out) <- NULL
  out
}

mom_signal_12_1 <- build_mom_signal(log_rets, 12, monthly_factors$date)
mom_signal_9_1  <- build_mom_signal(log_rets, 9,  monthly_factors$date)
mom_signal_6_1  <- build_mom_signal(log_rets, 6,  monthly_factors$date)
mom_signal_3_1  <- build_mom_signal(log_rets, 3,  monthly_factors$date)
mom_signal_2_1  <- build_mom_signal(log_rets, 2,  monthly_factors$date)

#------------------------------------------------------------------------------

load("RCode/PostPublication/monthly_ivol.RData")

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

###########################################################################

build_mom_groups <- function(mom_signal, ivol_groups, factor_cols) {
  
  # Keep only dates present in both inputs
  common_dates <- intersect(mom_signal$date, ivol_groups$date)
  
  mom_signal   <- mom_signal[mom_signal$date %in% common_dates, ]
  ivol_aligned <- ivol_groups[ivol_groups$date %in% common_dates, ]
  
  mom_signal   <- mom_signal[order(mom_signal$date), ]
  ivol_aligned <- ivol_aligned[order(ivol_aligned$date), ]
  
  # Drop leading rows where the momentum signal isn't defined yet
  has_mom <- rowSums(!is.na(mom_signal[, factor_cols, drop = FALSE])) > 0
  mom_signal   <- mom_signal[has_mom, ]
  ivol_aligned <- ivol_aligned[has_mom, ]
  
  mom_groups <- mom_signal[, c("date", factor_cols)]
  mom_groups[, factor_cols] <- NA
  row.names(mom_groups) <- NULL
  
  for (i in seq_len(nrow(mom_signal))) {
    
    mom_vals <- as.numeric(mom_signal[i, factor_cols])
    ivol_grp <- as.character(ivol_aligned[i, factor_cols])  # matched by column name
    
    mom_row <- rep(NA, length(mom_vals))
    
    for (iv in c("IV1", "IV2", "IV3")) {
      
      idx_iv <- which(ivol_grp == iv)
      
      if (length(idx_iv) > 0) {
        
        mom_subset <- mom_vals[idx_iv]
        
        p30 <- quantile(mom_subset, 0.3, na.rm = TRUE)
        p70 <- quantile(mom_subset, 0.7, na.rm = TRUE)
        
        mom_row[idx_iv] <- ifelse(mom_subset <= p30, "M1",
                                  ifelse(mom_subset <= p70, "M2", "M3"))
      }
    }
    
    mom_groups[i, factor_cols] <- mom_row
  }
  
  mom_groups
}

factor_cols_mom <- setdiff(names(mom_signal_12_1), "date")

# Sanity check: the momentum signals and the IVOL groups should cover the
# same set of factor columns
stopifnot(identical(sort(factor_cols_mom), sort(factor_cols)))
stopifnot(identical(sort(names(mom_signal_2_1)), sort(names(mom_signal_12_1))))

mom_groups_12_1_252d_ivol <- build_mom_groups(mom_signal_12_1, ivol_groups, factor_cols_mom)
mom_groups_9_1_252d_ivol  <- build_mom_groups(mom_signal_9_1,  ivol_groups, factor_cols_mom)
mom_groups_6_1_252d_ivol  <- build_mom_groups(mom_signal_6_1,  ivol_groups, factor_cols_mom)
mom_groups_3_1_252d_ivol  <- build_mom_groups(mom_signal_3_1,  ivol_groups, factor_cols_mom)
mom_groups_2_1_252d_ivol  <- build_mom_groups(mom_signal_2_1,  ivol_groups, factor_cols_mom)

###########################################################################
# Test

check_tab <- function(mom_groups, ivol_groups, factor_cols) {
  
  common_dates <- intersect(mom_groups$date, ivol_groups$date)
  
  ivol_sub <- ivol_groups[ivol_groups$date %in% common_dates, factor_cols]
  mom_sub  <- mom_groups[mom_groups$date %in% common_dates, factor_cols]
  
  tab <- table(
    IVOL = as.vector(as.matrix(ivol_sub)),
    MOM  = as.vector(as.matrix(mom_sub))
  )
  
  prop.table(tab, margin = 1) * 100
}

check_tab(mom_groups_12_1_252d_ivol, ivol_groups, factor_cols_mom)
check_tab(mom_groups_9_1_252d_ivol,  ivol_groups, factor_cols_mom)
check_tab(mom_groups_6_1_252d_ivol,  ivol_groups, factor_cols_mom)
check_tab(mom_groups_3_1_252d_ivol,  ivol_groups, factor_cols_mom)
check_tab(mom_groups_2_1_252d_ivol,  ivol_groups, factor_cols_mom)


mom_groups <- mom_groups_2_1_252d_ivol