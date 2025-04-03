extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
                                     indices = indices_xmpl,
                                     threshold = 400000,
                                     first_orig_year = 1989,
                                     last_orig_year = 2023,
                                     expected_year_of_growing_large = 3,
                                     reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                     pool_of_annuities = pool_of_annuities_xmpl)

pools <- generate_pools(extended_claims_data = extended_claims_data,
                        reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                        years_for_pools = 2014:2023,
                        start_of_tail = 17,
                        end_of_tail = 50,
                        lower_outlier_limit = -Inf,
                        upper_outlier_limit = Inf,
                        pool_of_annuities = pool_of_annuities_xmpl)

large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
                                          first_orig_year = 1989,
                                          last_orig_year = 2023)

history <- generate_history_per_claim(data = extended_claims_data,
                                      column = "Cl_payment_cal",
                                      first_orig_year = 1989,
                                      last_orig_year = 2023)

test_that("sicr works for full example", {
   result <- sicr(n = 1,
                  large_claims_list = large_claims_list,
                  first_orig_year = 1989,
                  last_orig_year = 2023,
                  pools = pools,
                  indices = indices_xmpl,
                  history = history,
                  exposure = exposure_xmpl,
                  years_for_ibnr_pools = 2014:2023,
                  active_annuities = active_annuities_xmpl,
                  age_shift = age_shift_xmpl,
                  mortality = mortality_xmpl,
                  reinsurance = reinsurance_xmpl,
                  progress = FALSE,
                  summary = FALSE)
   expect_equal(dim(result), c(831, 250, 7))
   expect_equal(sum(result[,,1]), 123645823)
   expect_equal(sum(result[,,2]), 48425537)
})

test_that("sicr works with only one claim", {
   large_claims_list_1 <- large_claims_list[700,]
   history_1 <- history[700,, drop = FALSE]
   result <- sicr(n = 1,
                  large_claims_list = large_claims_list_1,
                  first_orig_year = 1989,
                  last_orig_year = 2023,
                  pools = pools,
                  indices = indices_xmpl,
                  history = history_1,
                  exposure = exposure_xmpl,
                  years_for_ibnr_pools = 2014:2023,
                  active_annuities = active_annuities_xmpl,
                  age_shift = age_shift_xmpl,
                  mortality = mortality_xmpl,
                  reinsurance = reinsurance_xmpl,
                  progress = FALSE,
                  summary = FALSE)
   expect_equal(sum(result[,,1]), 1558905.662)
})

test_that("sicr works with only one ibnr claim in history", {
   nn <- which(large_claims_list$Dev_year_of_growing_large == 7)
   large_claims_list_1 <- large_claims_list[nn[1],]
   history_1 <- history[nn[1],, drop = FALSE]
   result <- sicr(n = 1,
                  large_claims_list = large_claims_list_1,
                  first_orig_year = 1989,
                  last_orig_year = 2023,
                  pools = pools,
                  indices = indices_xmpl,
                  history = history_1,
                  exposure = exposure_xmpl,
                  years_for_ibnr_pools = NULL,
                  active_annuities = active_annuities_xmpl,
                  age_shift = age_shift_xmpl,
                  mortality = mortality_xmpl,
                  reinsurance = reinsurance_xmpl,
                  progress = FALSE,
                  summary = FALSE)
   expect_equal(nrow(result), 1)
})

test_that("sicr works with no ibnr claim in history", {
   nn <- which(large_claims_list$Dev_year_of_growing_large == 1)
   large_claims_list_1 <- large_claims_list[nn,]
   history_1 <- history[nn,, drop = FALSE]
   result <- sicr(n = 1,
                  large_claims_list = large_claims_list_1,
                  first_orig_year = 1989,
                  last_orig_year = 2023,
                  pools = pools,
                  indices = indices_xmpl,
                  history = history_1,
                  exposure = exposure_xmpl,
                  years_for_ibnr_pools = NULL,
                  active_annuities = active_annuities_xmpl,
                  age_shift = age_shift_xmpl,
                  mortality = mortality_xmpl,
                  reinsurance = reinsurance_xmpl,
                  progress = FALSE,
                  summary = FALSE)
   expect_equal(dim(result), c(NROW(large_claims_list_1), 250, 7))
})


test_that("sicr works with only one future ibnr claim", {
   nn <- which(large_claims_list$Dev_year_of_growing_large == 7)
   large_claims_list_1 <- large_claims_list[nn[1:6],]
   history_1 <- history[nn[1:6],, drop = FALSE]
   result <- sicr(n = 1,
                  large_claims_list = large_claims_list_1,
                  first_orig_year = 1989,
                  last_orig_year = 2023,
                  pools = pools,
                  indices = indices_xmpl,
                  history = history_1,
                  exposure = exposure_xmpl,
                  years_for_ibnr_pools = NULL,
                  active_annuities = active_annuities_xmpl,
                  age_shift = age_shift_xmpl,
                  mortality = mortality_xmpl,
                  reinsurance = reinsurance_xmpl,
                  progress = FALSE,
                  summary = FALSE)
   expect_equal(nrow(result), 6)
})

