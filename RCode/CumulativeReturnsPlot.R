
data <- as.data.frame(matrix(NA, nrow = nrow(ivol_groups), 0))
#remove first 12 observations due (diff in nrow mom_groups to monthly_factors)
data$date <- monthly_factors[["date"]][13:nrow(monthly_factors)]
data$IV1_LS_MOM <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
data$IV2_LS_MOM <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
data$IV3_LS_MOM <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1
data$LS_IV_LS_MOM <- (portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1) - (portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1)

# Ensure date is a proper Date object for nice x-axis formatting
data$date <- as.Date(data$date)

# Compute cumulative returns
data$IV1_cum <- cumprod(1 + data$IV1_LS_MOM) - 1
data$IV2_cum <- cumprod(1 + data$IV2_LS_MOM) - 1
data$IV3_cum <- cumprod(1 + data$IV3_LS_MOM) - 1

# Helper function for a consistent, clean theme
plot_cum_return <- function(df, y_col, title, line_color = "#2C3E50") {
  ggplot(df, aes(x = date, y = .data[[y_col]])) +
    geom_line(color = line_color, linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = title,
      x = NULL,
      y = "Cumulative return"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.y = element_text(size = 11)
    )
}

p1 <- plot_cum_return(data, "IV1_cum", "Cumulative Return: Momentum LS within IV1", "#1B9E77")
p2 <- plot_cum_return(data, "IV2_cum", "Cumulative Return: Momentum LS within IV2", "#D95F02")
p3 <- plot_cum_return(data, "IV3_cum", "Cumulative Return: Momentum LS within IV3", "#7570B3")

print(p1)
print(p2)
print(p3)

ggsave("png_files/IV1_LS_MOM_cumret.png", plot = p1, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("png_files/IV2_LS_MOM_cumret.png", plot = p2, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("png_files/IV3_LS_MOM_cumret.png", plot = p3, width = 8, height = 5, dpi = 300, bg = "white")

#clean up
rm(list = setdiff(ls(), keep))