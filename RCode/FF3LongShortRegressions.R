data <- as.data.frame(matrix(NA,nrow = nrow(ivol_groups),0))

#remove first 12 observations due (diff in nrow mom_groups to monthly_factors)
data$date <- monthly_factors[["date"]][13:nrow(monthly_factors)]
data$mkt <- monthly_factors[["mkt"]][13:nrow(monthly_factors)]
data$market_equity <- monthly_factors[["market_equity"]][13:nrow(monthly_factors)]
data$be_me <- monthly_factors[["be_me"]][13:nrow(monthly_factors)]

data$IV1_LS_MOM <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
data$IV2_LS_MOM <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
data$IV3_LS_MOM <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1
data$LS_IV_LS_MOM <- (portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1) - (portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1)

#regressions
reg_IV1_LS_MOM <- lm(IV1_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_IV2_LS_MOM <- lm(IV2_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_IV3_LS_MOM <- lm(IV3_LS_MOM ~ mkt + market_equity + be_me, data = data)
reg_LS_IV_LS_MOM <- lm(LS_IV_LS_MOM ~ mkt + market_equity + be_me, data = data)

print(coeftest(reg_IV1_LS_MOM, vcov = NeweyWest(reg_IV1_LS_MOM, lag = 6, prewhite = FALSE)))
print(coeftest(reg_IV2_LS_MOM, vcov = NeweyWest(reg_IV2_LS_MOM, lag = 6, prewhite = FALSE)))
print(coeftest(reg_IV3_LS_MOM, vcov = NeweyWest(reg_IV3_LS_MOM, lag = 6, prewhite = FALSE)))
print(coeftest(reg_LS_IV_LS_MOM, vcov = NeweyWest(reg_LS_IV_LS_MOM, lag = 6, prewhite = FALSE)))

#clean up
rm(list = setdiff(ls(), keep))