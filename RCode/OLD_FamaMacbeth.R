load("Data/monthly_ivol.RData")

#Lag by 1 to get momentum
monthly_mom <- monthly_factors %>%
  mutate(across(-date, ~lag(.x, 1)))

#remove mkt and first 12 rows to match length of data for IVOL
monthly_mom <- monthly_mom[-(1:12), !(names(monthly_mom) %in% "mkt")]

returns <- monthly_factors[-(1:12), !(names(monthly_factors) %in% "mkt")]

# ── 1st stage: time-series regression per asset to get betas ──────────────────
assets <- names(returns)[-1]
ts_results <- data.frame()

for (asset in assets) {
  ret  <- returns[[asset]]
  mom  <- monthly_mom[[asset]]
  ivol <- monthly_ivol[[asset]]
  
  df <- data.frame(ret = ret, mom = mom, ivol = ivol) %>% na.omit()
  df$mom_ivol <- df$mom * df$ivol
  
  fit <- lm(ret ~ mom + ivol + mom_ivol, data = df)
  
  ts_results <- rbind(ts_results, data.frame(
    asset     = asset,
    beta_mom  = coef(fit)[2],
    beta_ivol = coef(fit)[3],
    beta_int  = coef(fit)[4]
  ))
}

# ── 2nd stage: cross-sectional regression per date using betas ────────────────
fm_results <- data.frame()

for (t in returns$date) {
  ret <- as.numeric(returns[returns$date == t, -1])
  
  df <- data.frame(
    ret       = ret,
    beta_mom  = ts_results$beta_mom,
    beta_ivol = ts_results$beta_ivol,
    beta_int  = ts_results$beta_int
  ) %>% na.omit()
  
  fit <- lm(ret ~ beta_mom + beta_ivol + beta_int, data = df)
  
  fm_results <- rbind(fm_results, data.frame(
    date        = t,
    lambda0     = coef(fit)[1],
    lambda_mom  = coef(fit)[2],
    lambda_ivol = coef(fit)[3],
    lambda_int  = coef(fit)[4]
  ))
}

# ── FM variance and t-stats (equation 4 from slides) ─────────────────────────
T <- nrow(fm_results)

lambda_mean <- colMeans(fm_results[, -1])

fm_variance <- sapply(names(lambda_mean), function(j) {
  (1/T^2) * sum((fm_results[[j]] - lambda_mean[j])^2)
})

fm_se    <- sqrt(fm_variance)
fm_tstat <- lambda_mean / fm_se

print(rbind(mean = lambda_mean, se = fm_se, tstat = fm_tstat))

rm(list = setdiff(ls(), c("portfolio_returns_3x3","ivol_groups","mom_groups","monthly_factors")))
