data <- as.data.frame(matrix(NA,nrow = nrow(ivol_groups),0))

#remove first 12 observations due (diff in nrow mom_groups to monthly_factors)
data$date <- monthly_factors[["date"]][13:nrow(monthly_factors)]
data$mkt <- monthly_factors[["mkt"]][13:nrow(monthly_factors)]
data$market_equity <- monthly_factors[["market_equity"]][13:nrow(monthly_factors)]
data$be_me <- monthly_factors[["be_me"]][13:nrow(monthly_factors)]
data$ret_12_1 <- monthly_factors[["ret_12_1"]][13:nrow(monthly_factors)]
data$ret_1_0 <- monthly_factors[["ret_1_0"]][13:nrow(monthly_factors)]

data$IV1_LS_MOM <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
data$IV2_LS_MOM <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
data$IV3_LS_MOM <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1
data$LS_IV_LS_MOM <- (portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1) - (portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1)

#regressions
reg_IV1_LS_MOM <- lm(IV1_LS_MOM ~ ret_1_0 , data = data)
reg_IV2_LS_MOM <- lm(IV2_LS_MOM ~ ret_1_0, data = data)
reg_IV3_LS_MOM <- lm(IV3_LS_MOM ~ ret_1_0, data = data)
reg_LS_IV_LS_MOM <- lm(LS_IV_LS_MOM ~ ret_1_0, data = data)

print("IV1_LS_MOM")
print(round(coeftest(reg_IV1_LS_MOM,
                     vcov = NeweyWest(reg_IV1_LS_MOM, lag = 6, prewhite = FALSE)), 3))

print("IV2_LS_MOM")
print(round(coeftest(reg_IV2_LS_MOM,
                     vcov = NeweyWest(reg_IV2_LS_MOM, lag = 6, prewhite = FALSE)), 3))

print("IV3_LS_MOM")
print(round(coeftest(reg_IV3_LS_MOM,
                     vcov = NeweyWest(reg_IV3_LS_MOM, lag = 6, prewhite = FALSE)), 3))

print("LS_IV_LS_MOM")
print(round(coeftest(reg_LS_IV_LS_MOM,
                     vcov = NeweyWest(reg_LS_IV_LS_MOM, lag = 6, prewhite = FALSE)), 3))
#clean up
rm(list = setdiff(ls(), keep))