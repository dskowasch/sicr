
test_that("generate_pools work", {
   new_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
                            indices = indices_xmpl,
                            threshold = 400000,
                            first_orig_year = 1989,
                            last_orig_year = 2023,
                            expected_year_of_growing_large = 3,
                            reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                            pool_of_annuities = minimal_pool_of_annuities_xmpl)
   pools <- generate_pools(extended_claims_data = new_data,
                           reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                           years_for_pools = 2014:2023,
                           start_of_tail = 17,
                           end_of_tail = 50,
                           lower_outlier_limit = -Inf,
                           upper_outlier_limit = Inf,
                           pool_of_annuities = minimal_pool_of_annuities_xmpl)
   expect_length(pools, 3)
   expect_length(pools[[1]], 6)
   expect_length(pools[[1]][[1]], 15)
   expect_lt(min(pools[[2]][[2]]$Ind_cl_payment_cal), -1995)

   # does limiting work
   pools_with_lower_limit <- generate_pools(extended_claims_data = new_data,
                                            reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                            years_for_pools = 2014:2023,
                                            start_of_tail = 17,
                                            end_of_tail = 50,
                                            lower_outlier_limit = -1000,
                                            upper_outlier_limit = Inf,
                                            pool_of_annuities = minimal_pool_of_annuities_xmpl)
   expect_gt(min(pools_with_lower_limit[[2]][[2]]$Ind_cl_payment_cal), -2000)

   # does it work with empty data
   empty_data <- new_data[-(1:NROW(new_data)),]
   empty_pools <- generate_pools(extended_claims_data = empty_data,
                                 reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                 years_for_pools = 2014:2023,
                                 start_of_tail = 17,
                                 end_of_tail = 50,
                                 lower_outlier_limit = -1000,
                                 upper_outlier_limit = Inf,
                                 pool_of_annuities = minimal_pool_of_annuities_xmpl)
   expect_length(empty_pools, 3)
   expect_length(empty_pools[[2]], 6)
   expect_length(empty_pools[[1]][[1]], 15)
})
