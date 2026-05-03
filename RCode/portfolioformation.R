setwd("C:/Users/Ander/Desktop/Praktik/Data")
library(sandwich)
library(lmtest)

load("mom_groups.RData")
load("ivol_groups.RData")
load("monthly_factors.RData")

##################################################################################
#Portfolio formation
# Factor columns (exclude date)
factor_cols <- setdiff(names(ivol_groups), "date")

# Create dataframe to store 9 bivariate portfolio returns
portfolio_returns_bi <- data.frame(
  date = ivol_groups$date,
  IV1_M1 = NA, IV1_M2 = NA, IV1_M3 = NA,
  IV2_M1 = NA, IV2_M2 = NA, IV2_M3 = NA,
  IV3_M1 = NA, IV3_M2 = NA, IV3_M3 = NA
)

# Loop over months
for (i in 1:nrow(ivol_groups)) {
  
  # IVOL and MOM groups for this month
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  mom_grp  <- as.character(mom_groups[i, factor_cols])
  
  # Returns for this month
  returns <- as.numeric(monthly_factors[i, factor_cols])
  
  # Loop over all combinations of IVOL × MOM
  for (iv in c("IV1","IV2","IV3")) {
    for (mo in c("M1","M2","M3")) {
      # Logical index for factors in both groups
      idx <- (ivol_grp == iv) & (mom_grp == mo)
      
      # Portfolio name
      port_name <- paste0(iv, "_", mo)
      
      # Average return
      portfolio_returns_bi[i, port_name] <- mean(returns[idx], na.rm = TRUE)
    }
  }
}


# Exclude the date column
portfolio_cols <- setdiff(names(portfolio_returns_bi), "date")

# Compute column-wise mean
portfolio_means <- colMeans(portfolio_returns_bi[, portfolio_cols], na.rm = TRUE)

# View results
portfolio_means*100


################################################################################
######################## Long short factor regressions, FF3 ####################
################################################################################

#Import factors
load("monthly_factors.RData")
data <- as.data.frame(matrix(NA,392,0))
data$date <- monthly_factors[["date"]][13:404]
data$mkt <- monthly_factors[["mkt"]][13:404]
data$market_equity <- monthly_factors[["market_equity"]][13:404]
data$be_me <- monthly_factors[["be_me"]][13:404]

data$IV1_LS_MOM <- portfolio_returns_bi$IV1_M3 - portfolio_returns_bi$IV1_M1
data$IV2_LS_MOM <- portfolio_returns_bi$IV2_M3 - portfolio_returns_bi$IV2_M1
data$IV3_LS_MOM <- portfolio_returns_bi$IV3_M3 - portfolio_returns_bi$IV3_M1
data$LS_IV_LS_MOM <- (portfolio_returns_bi$IV3_M3 - portfolio_returns_bi$IV3_M1) - (portfolio_returns_bi$IV1_M3 - portfolio_returns_bi$IV1_M1)

#regressions
reg_IV1_LS_MOM <- lm(IV1_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_IV2_LS_MOM <- lm(IV2_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_IV3_LS_MOM <- lm(IV3_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_LS_IV_LS_MOM <- lm(LS_IV_LS_MOM ~ mkt + market_equity + be_me, data = data)

coeftest(reg_IV1_LS_MOM, vcov = NeweyWest(reg_IV1_LS_MOM, lag = 6, prewhite = FALSE))
coeftest(reg_IV2_LS_MOM, vcov = NeweyWest(reg_IV2_LS_MOM, lag = 6, prewhite = FALSE))
coeftest(reg_IV3_LS_MOM, vcov = NeweyWest(reg_IV3_LS_MOM, lag = 6, prewhite = FALSE))
coeftest(reg_LS_IV_LS_MOM, vcov = NeweyWest(reg_LS_IV_LS_MOM, lag = 6, prewhite = FALSE))


  ################################################################################
############## Long-horizon momentum returns and IVol ##########################
################################################################################
#à la Figure 1 i Arena, Haggard & Yan 2008

plot.ts(cumprod(1 + data$IV1_LS_MOM) - 1)
plot.ts(cumprod(1 + data$IV2_LS_MOM) - 1)
plot.ts(cumprod(1 + data$IV3_LS_MOM) - 1)
#måske have et rollende konfidensinterval på 12 observationer?

################################################################################
######################## AR1 factor regressions ################################
################################################################################
# Load libraries
library(dplyr)
library(purrr)
library(ggplot2)

# ---- 1. Prepare data ----
# Remove date column (assumes it's the first column)
data_only <- monthly_factors %>% select(-date)

# Remove "mkt" column if it exists
data_only <- data_only %>% select(-mkt)

# ---- 2. Function to get AR(1) coef + SE ----
get_ar1_info <- function(x) {
  x <- na.omit(x)
  
  if (length(x) < 5) {
    return(c(ar1 = NA, se = NA))
  }
  
  model <- arima(x, order = c(1, 0, 0))
  
  ar1 <- coef(model)["ar1"]
  se  <- sqrt(diag(model$var.coef))["ar1"]
  
  c(ar1 = ar1, se = se)
}

# ---- 3. Apply to all columns ----
ar1_results <- map(data_only, get_ar1_info)

results_df <- do.call(rbind, ar1_results) %>% as.data.frame()

results_df$Factor <- rownames(results_df)
rownames(results_df) <- NULL

# ---- 4. Compute 95% confidence intervals ----
results_df <- results_df %>%
  mutate(
    lower = ar1.ar1 - 1.96 * se.ar1,
    upper = ar1.ar1 + 1.96 * se.ar1
  )

# ---- 5. Plot (sorted low → high) ----
ggplot(results_df, aes(x = reorder(Factor, ar1.ar1), y = ar1.ar1)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  labs(
    title = "AR(1) Coefficients with 95% Confidence Intervals",
    x = "Factor",
    y = "AR(1) Slope"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################
######################## Factor Sample Summary Statistics ######################
################################################################################

library(dplyr)

# drop date and mkt
factors <- monthly_factors %>%
  select(-date, -mkt)

expected_returns <- data.frame(
  Factor = names(factors),
  Mean = sapply(factors, function(x) mean(x, na.rm = TRUE))
)

# optional: sort
expected_returns <- expected_returns %>%
  arrange(desc(Mean))

sharpe_ratios <- data.frame(
  Factor = names(factors),
  Sharpe = sapply(factors, function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    m / s
  })
)

# optional: sort
sharpe_ratios <- sharpe_ratios %>%
  arrange(desc(Sharpe))

#sharpe_ratios$Sharpe_annualized <- sharpe_ratios$Sharpe * sqrt(12)

library(ggplot2)
library(dplyr)

ggplot(expected_returns, aes(x = reorder(Factor, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Expected Monthly Returns of Factors",
    x = "Factor",
    y = "Mean Return"
  ) +
  coord_flip()

ggplot(sharpe_ratios, aes(x = reorder(Factor, Sharpe), y = Sharpe)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(
    title = "Sharpe Ratios of Factors (Monthly)",
    x = "Factor",
    y = "Sharpe Ratio"
  ) +
  coord_flip()


################################################################################
#################################### Fama macbeth ##############################
################################################################################

load("monthly_ivol.RData")
library(dplyr)

#Lag by 1 to get momentum
monthly_mom <- monthly_factors %>%
  mutate(across(-date, ~lag(.x, 1)))

#remove mkt and first 12 rows to match length of data for IVOL
monthly_mom <- monthly_mom[-(1:12), !(names(monthly_mom) %in% "mkt")]

returns <- monthly_factors[-(1:12), !(names(monthly_factors) %in% "mkt")]

library(dplyr)

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


################################################################################
#################################### Fama macbeth ##############################
################################################################################

