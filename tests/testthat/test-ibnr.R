library(dplyr)

# prepare data
extended_claims_data_xmpl <- prepare_data(claims_data = claims_data_xmpl,
                                          indices = indices_xmpl,
                                          threshold = 4e5,
                                          first_orig_year = 1989,
                                          last_orig_year = 2023,
                                          expected_year_of_growing_large = 3,
                                          reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                          pool_of_annuities = pool_of_annuities_xmpl)

# generate claims list
large_claims_list_xmpl <- generate_claims_list(extended_claims_data = extended_claims_data_xmpl,
                                               first_orig_year = 1989,
                                               last_orig_year = 2023)

# generate example exposure with 5% annual exposure increase since 1989
exposure_xmpl <- data.frame(Origin_year = 1989:2023,
                            Exposure = 1000 * 1.05^(0:34))

ibnr <- prepare_ibnr(large_claims_list_xmpl,
                     1989,
                     2023,
                     exposure_xmpl,
                     2014:2023)

test_that("prepare_ibnr creates two objects", {
   expect_length(ibnr, 2)})

test_that("prepare_ibnr first object is a dataframe with correct dimensions", {
   expect_true(is.data.frame(ibnr[[1]]))
   expect_equal(dim(ibnr[[1]]), c(63, 3))})

test_that("prepare_ibnr first object has correct entries", {
   expect_equal(sum(ibnr[[1]]$Dev_year_since_large), -393)})

test_that("prepare_ibnr second object is list with correct length", {
   expect_true(is.list(ibnr[[2]]))
   expect_length(ibnr[[2]], 34)})

test_that("prepare_ibnr second object random sample is correct", {
   expect_equal(ibnr[[2]][[6]], c(527, 552, 576, 621))
})

### do special cases work as well?
# case 1: no ibnr claim yet seen
large_claims_list_xmpl_0 <- filter(large_claims_list_xmpl, Dev_year_of_growing_large == 1)
ibnr_0 <- prepare_ibnr(large_claims_list_xmpl_0,
                       1989,
                       2023,
                       exposure_xmpl,
                       2014:2023)

test_that("prepare_ibnr works without ibnr claims", {
   expect_length(ibnr_0, 2)
   expect_true(is.data.frame(ibnr_0[[1]]))
   expect_equal(dim(ibnr_0[[1]]), c(0, 3))
   expect_true(is.list(ibnr_0[[2]]))
   expect_length(ibnr_0[[2]], 34)})

# case 2: just one ibnr claim in data
large_claims_list_xmpl_1 <- large_claims_list_xmpl[700,]
ibnr_1 <- prepare_ibnr(large_claims_list_xmpl_1,
                       1989,
                       2023,
                       exposure_xmpl,
                       2014:2023)

test_that("prepare_ibnr works with just one ibnr claim", {
   expect_length(ibnr_1, 2)
   expect_true(is.data.frame(ibnr_1[[1]]))
   expect_equal(dim(ibnr_1[[1]]), c(0, 3))
   expect_true(is.list(ibnr_1[[2]]))
   expect_length(ibnr_1[[2]], 34)
   expect_equal(ibnr_1[[2]][[3]], c(1))})

