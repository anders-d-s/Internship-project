results <- list()

# Test monotonicity across M1->M2->M3->M4->M5 within each IV group
for (iv in 1:5) {
  cols <- paste0("IV", iv, "_M", 1:5)
  results[[paste0("IV", iv, "_across_M")]] <- monoSummary(
    portfolio_returns_5x5 %>% select(all_of(cols)),
    bootstrapRep = 1000,
    block_length = 6,
    increasing = TRUE
  )
}

# Test monotonicity across IV1->IV2->IV3->IV4->IV5 within each M group
for (m in 1:5) {
  cols <- paste0("IV", 1:5, "_M", m)
  results[[paste0("M", m, "_across_IV")]] <- monoSummary(
    portfolio_returns_5x5 %>% select(all_of(cols)),
    bootstrapRep = 1000,
    block_length = 6,
    increasing = TRUE
  )
}

# Joint test across M (all IV groups simultaneously)
results[["joint_across_M"]] <- monoSummary(
  portfolio_returns_5x5 %>% select(
    IV1_M1, IV1_M2, IV1_M3, IV1_M4, IV1_M5,
    IV2_M1, IV2_M2, IV2_M3, IV2_M4, IV2_M5,
    IV3_M1, IV3_M2, IV3_M3, IV3_M4, IV3_M5,
    IV4_M1, IV4_M2, IV4_M3, IV4_M4, IV4_M5,
    IV5_M1, IV5_M2, IV5_M3, IV5_M4, IV5_M5
  ),
  bootstrapRep = 1000,
  block_length = 6,
  increasing = TRUE
)

# Joint test across IV (all M groups simultaneously)
results[["joint_across_IV"]] <- monoSummary(
  portfolio_returns_5x5 %>% select(
    IV1_M1, IV2_M1, IV3_M1, IV4_M1, IV5_M1,
    IV1_M2, IV2_M2, IV3_M2, IV4_M2, IV5_M2,
    IV1_M3, IV2_M3, IV3_M3, IV4_M3, IV5_M3,
    IV1_M4, IV2_M4, IV3_M4, IV4_M4, IV5_M4,
    IV1_M5, IV2_M5, IV3_M5, IV4_M5, IV5_M5
  ),
  bootstrapRep = 1000,
  block_length = 6,
  increasing = TRUE
)