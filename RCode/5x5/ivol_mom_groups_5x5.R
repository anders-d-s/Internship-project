load("Data/monthly_ivol_252d.RData")

# Helper: assign quantile-based group labels for an arbitrary number of buckets
assign_quantile_group <- function(x, n_groups, prefix) {
  
  probs <- seq(0, 1, length.out = n_groups + 1)
  breaks <- quantile(x, probs = probs, na.rm = TRUE, type = 7)
  
  # findInterval handles the "<=" cutoffs and NAs cleanly for any n_groups
  grp <- findInterval(x, breaks[-length(breaks)], rightmost.closed = TRUE)
  grp[is.na(x)] <- NA
  grp[grp < 1] <- 1  # guard against values below the first breakpoint due to ties
  
  paste0(prefix, grp)
}

#################################################################################
# IVOL
# Quintiles: 20/40/60/80
N_IVOL <- 5

ivol_groups <- monthly_ivol
ivol_groups[, -which(names(ivol_groups) == "date")] <- NA

factor_cols <- setdiff(names(monthly_ivol), "date")

for (i in 1:nrow(monthly_ivol)) {
  
  row_vals <- as.numeric(monthly_ivol[i, factor_cols])
  
  groups <- assign_quantile_group(row_vals, N_IVOL, "IV")
  
  ivol_groups[i, factor_cols] <- as.list(groups)
}

#save(ivol_groups, file = "Data/ivol_groups_252d.RData")

###########################################################################
# MOM
# Dependent double sort: within each IVOL group, rank on the momentum
# signal and assign M1 (bottom 20%) ... M5 (top 20%).

N_MOM <- 5

build_mom_groups <- function(mom_signal, ivol_groups, factor_cols, n_ivol_groups, n_mom_groups) {
  
  common_dates <- intersect(mom_signal$date, ivol_groups$date)
  
  mom_signal   <- mom_signal[mom_signal$date %in% common_dates, ]
  ivol_aligned <- ivol_groups[ivol_groups$date %in% common_dates, ]
  
  mom_signal   <- mom_signal[order(mom_signal$date), ]
  ivol_aligned <- ivol_aligned[order(ivol_aligned$date), ]
  
  has_mom <- rowSums(!is.na(mom_signal[, factor_cols, drop = FALSE])) > 0
  mom_signal   <- mom_signal[has_mom, ]
  ivol_aligned <- ivol_aligned[has_mom, ]
  
  mom_groups <- mom_signal[, c("date", factor_cols)]
  mom_groups[, factor_cols] <- NA
  row.names(mom_groups) <- NULL
  
  ivol_labels <- paste0("IV", seq_len(n_ivol_groups))
  
  for (i in seq_len(nrow(mom_signal))) {
    
    mom_vals <- as.numeric(mom_signal[i, factor_cols])
    ivol_grp <- as.character(ivol_aligned[i, factor_cols])
    
    mom_row <- rep(NA, length(mom_vals))
    
    for (iv in ivol_labels) {
      
      idx_iv <- which(ivol_grp == iv)
      
      if (length(idx_iv) > 0) {
        
        mom_subset <- mom_vals[idx_iv]
        mom_row[idx_iv] <- assign_quantile_group(mom_subset, n_mom_groups, "M")
      }
    }
    
    mom_groups[i, factor_cols] <- mom_row
  }
  
  mom_groups
}

# Load the four pre-built momentum signals
load("Data/mom_signal_12_1.RData")
load("Data/mom_signal_9_1.RData")
load("Data/mom_signal_6_1.RData")
load("Data/mom_signal_3_1.RData")

factor_cols_mom <- setdiff(names(mom_signal_12_1), "date")

stopifnot(identical(sort(factor_cols_mom), sort(factor_cols)))

mom_groups_12_1_252d_ivol <- build_mom_groups(mom_signal_12_1, ivol_groups, factor_cols_mom, N_IVOL, N_MOM)
mom_groups_9_1_252d_ivol  <- build_mom_groups(mom_signal_9_1,  ivol_groups, factor_cols_mom, N_IVOL, N_MOM)
mom_groups_6_1_252d_ivol  <- build_mom_groups(mom_signal_6_1,  ivol_groups, factor_cols_mom, N_IVOL, N_MOM)
mom_groups_3_1_252d_ivol  <- build_mom_groups(mom_signal_3_1,  ivol_groups, factor_cols_mom, N_IVOL, N_MOM)

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