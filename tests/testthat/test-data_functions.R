library(dplyr)
test_that("reduce_data works", {
   minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      pull(Claim_id) %>%
      unique() %>%
      length() %>%
      expect_equal(30)
})


test_that("add_missing_years works", {
   minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      NROW() %>%
      expect_equal(662)
})


test_that("add_columns works", {
   data <- minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      add_columns()
   data %>% NCOL() %>% expect_equal(19)
   c("Cl_incurred", "Payment_cum", "Entry_an_reserve") %in%
      colnames(data) %>% all() %>% expect_true()
})


test_that("add_indexed_columns works", {
   minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      add_columns() %>%
      add_indexed_columns(indices_xmpl) %>%
      NCOL() %>%
      expect_equal(34)
})


test_that("filter_large_claims works", {
   minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      add_columns() %>%
      add_indexed_columns(indices_xmpl) %>%
      filter_large_claims(400000, 1989, 3) %>%
      pull(Claim_id) %>%
      unique() %>%
      length() %>%
      expect_equal(19)
})


test_that("add_classes works", {
   minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      add_columns() %>%
      add_indexed_columns(indices_xmpl) %>%
      filter_large_claims(400000, 1989, 3) %>%
      add_classes(c(1, 200001, 400001, 700001, 1400001)) %>%
      colnames() %>%
      expect_in(c("Entry_reserve_class"), .)
})


test_that("attach_future_annuities works", {
   data <- minimal_claims_data_xmpl %>%
      reduce_data(indices_xmpl, 400000) %>%
      add_missing_years(2023) %>%
      add_columns() %>%
      add_indexed_columns(indices_xmpl) %>%
      filter_large_claims(400000, 1989, 3) %>%
      add_classes(c(1, 200001, 400001, 700001, 1400001)) %>%
      attach_future_annuities(minimal_pool_of_annuities_xmpl)
      data %>% colnames() %>% expect_in(c("New_annuity_5"), .)
      data %>% select(starts_with("New_Annuity_")) %>% sum() %>% expect_equal(15)
})


test_that("prepare_data works", {
   new_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
                            indices = indices_xmpl,
                            threshold = 400000,
                            first_orig_year = 1989,
                            last_orig_year = 2023,
                            expected_year_of_growing_large = 3,
                            reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                            pool_of_annuities = minimal_pool_of_annuities_xmpl)
   expect_equal(NCOL(new_data), 44)
   expect_equal(NROW(new_data), 469)
   new_data_without_annuities <- prepare_data(claims_data = minimal_claims_data_xmpl,
                            indices = indices_xmpl,
                            threshold = 400000,
                            first_orig_year = 1989,
                            last_orig_year = 2023,
                            expected_year_of_growing_large = 3,
                            reserve_classes = c(1, 200001, 400001, 700001, 1400001))
   expect_equal(NCOL(new_data_without_annuities), 44)
   expect_equal(NROW(new_data_without_annuities), 469)
   expect_equal(sum(new_data_without_annuities$New_Annuity_1), 0)
   expect_equal(new_data_without_annuities[,1:39], new_data[,1:39])

})
