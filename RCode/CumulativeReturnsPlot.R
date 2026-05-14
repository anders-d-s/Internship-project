data <- as.data.frame(matrix(NA,nrow = nrow(ivol_groups),0))

#remove first 12 observations due (diff in nrow mom_groups to monthly_factors)
data$date <- monthly_factors[["date"]][13:nrow(monthly_factors)]
data$IV1_LS_MOM <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
data$IV2_LS_MOM <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
data$IV3_LS_MOM <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1
data$LS_IV_LS_MOM <- (portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1) - (portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1)


plot.ts(cumprod(1 + data$IV1_LS_MOM) - 1)
plot.ts(cumprod(1 + data$IV2_LS_MOM) - 1)
plot.ts(cumprod(1 + data$IV3_LS_MOM) - 1)

#clean up
rm(list = setdiff(ls(), keep))