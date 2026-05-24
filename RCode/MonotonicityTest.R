results <- list()

# Test monotonicity across M1->M2->M3 within each IV group
for (iv in 1:3) {
  cols <- paste0("IV", iv, "_M", 1:3)
  results[[paste0("IV", iv, "_across_M")]] <- monoSummary(
    portfolio_returns_3x3 %>% select(all_of(cols)),
    bootstrapRep = 1000,
    block_length = 6,
    increasing = TRUE
  )
}

# Test monotonicity across IV1->IV2->IV3 within each M group
for (m in 1:3) {
  cols <- paste0("IV", 1:3, "_M", m)
  results[[paste0("M", m, "_across_IV")]] <- monoSummary(
    portfolio_returns_3x3 %>% select(all_of(cols)),
    bootstrapRep = 1000,
    block_length = 6,
    increasing = TRUE
  )
}

# Joint test across M (all IV groups simultaneously)
results[["joint_across_M"]] <- monoSummary(
  portfolio_returns_3x3 %>% select(IV1_M1, IV1_M2, IV1_M3,
                                   IV2_M1, IV2_M2, IV2_M3,
                                   IV3_M1, IV3_M2, IV3_M3),
  bootstrapRep = 1000,
  block_length = 6,
  increasing = TRUE
)

# Joint test across IV (all M groups simultaneously)
results[["joint_across_IV"]] <- monoSummary(
  portfolio_returns_3x3 %>% select(IV1_M1, IV2_M1, IV3_M1,
                                   IV1_M2, IV2_M2, IV3_M2,
                                   IV1_M3, IV2_M3, IV3_M3),
  bootstrapRep = 1000,
  block_length = 6,
  increasing = TRUE
)
