load("Data/monthly_factors.RData")

factor_cols <- setdiff(names(monthly_factors), c("date", "mkt"))

log_rets <- log(1 + monthly_factors[, factor_cols])

build_mom_signal <- function(log_rets, N, dates) {
  
  width <- N - 1
  
  roll_sum <- zoo::rollapply(log_rets, width = width, FUN = sum, align = "right",
                             fill = NA, na.rm = FALSE)
  
  n <- nrow(roll_sum)
  mom_log <- rbind(
    matrix(NA_real_, nrow = 2, ncol = ncol(roll_sum),
           dimnames = list(NULL, colnames(roll_sum))),
    roll_sum[1:(n - 2), , drop = FALSE]
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

save(mom_signal_12_1, file = "Data/mom_signal_12_1.RData")
save(mom_signal_9_1,  file = "Data/mom_signal_9_1.RData")
save(mom_signal_6_1,  file = "Data/mom_signal_6_1.RData")
save(mom_signal_3_1,  file = "Data/mom_signal_3_1.RData")