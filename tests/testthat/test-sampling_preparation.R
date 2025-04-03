test_that("pool2matrix works", {
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
   pools_as_matrix <- pools2matrix(pools)
   expect_equal(pools_as_matrix[[3]], pools[[3]]) # pool of annuities should be the same
   expect_equal(length(pools_as_matrix[[1]]), 6 * 16) # checking number of pools with 6 reserve classes and 16 dev years
   expect_equal(NROW(pools_as_matrix[[2]]), 6 * 16 * max(pools_as_matrix[[1]])) # checking number of entries in matrix
   xmpl_payment <- pools[[1]][[4]][[2]]$Ind_cl_payment_cal # checking random payment ...
   ind <- get_matrix_info(3, 3, 17, 6, pools_as_matrix[[1]], "start")
   expect_equal(pools_as_matrix[[2]][ind, 1], xmpl_payment)
})
