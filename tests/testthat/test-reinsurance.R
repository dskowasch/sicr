library(dplyr)

### prepare data
# Create large claims list
extended_claims_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
                                     indices = indices_xmpl,
                                     threshold = 400000,
                                     first_orig_year = 1989,
                                     last_orig_year = 2023,
                                     expected_year_of_growing_large = 3,
                                     reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                     pool_of_annuities = minimal_pool_of_annuities_xmpl)

large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
                                          first_orig_year = 1989,
                                          last_orig_year = 2023)

# generate history
history <- generate_history_per_claim(extended_claims_data, "Cl_payment_cal", 1989, 2023)

# generate future payments with fictive constant payments
# of 1.000.000 for 10 years for claim in row 10
future_payments <- matrix(0, nrow = NROW(history), ncol = 250)
future_payments[10, 1:10] <- 1e6

# output "history"
result_history <- xl_cashflow(future_payments = future_payments,
                              claims_list = large_claims_list,
                              history = history,
                              reinsurance = reinsurance_xmpl,
                              indices = indices_xmpl,
                              output = "history")
# output "future"
result_future <- xl_cashflow(future_payments = future_payments,
                             claims_list = large_claims_list,
                             history = history,
                             reinsurance = reinsurance_xmpl,
                             indices = indices_xmpl,
                             output = "future")

result_total <- xl_cashflow(future_payments = future_payments,
                            claims_list = large_claims_list,
                            history = history,
                            reinsurance = reinsurance_xmpl,
                            indices = indices_xmpl,
                            output = "total")

test_that("dimensions are correct", {
   expect_length(result_history[1,], 2023 - 1989 + 1)
   expect_length(result_future[1,], 250)
   expect_length(result_total[1,], 250 + 2023 - 1989 + 1)
   expect_length(result_total[,1], NROW(large_claims_list))
   })





# simple example to test for correct calculation
future_payments <- rbind(c(rep(5e5, 5), rep(0, 245)))
history <- rbind(rep(1e6, 4))
colnames(history) <- 2020:2023
claims_list <- data.frame(Claim_id = "Test1",
                          Origin_year = 2020,
                          Exit_reserve_class = 1,
                          Dev_year_since_large = 4,
                          Dev_year_of_growing_large = 1,
                          Large_since = 2020,
                          Cl_reserve = 5000000)
reinsurance <- reinsurance_xmpl[reinsurance_xmpl$Origin_year > 2019,]
reinsurance$Margin_type[1] <- "none"
reinsurance$Quota_share[1] <- 0

test_that("calculation is correct for this simple example", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                            claims_list = claims_list,
                            history = history,
                            reinsurance = reinsurance,
                            indices = indices_xmpl,
                            output = "total")[1,1:10]),
                35e5)
})


reinsurance$Margin_type[1] <- "APK"
test_that("calculation is correct for APK", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                                claims_list = claims_list,
                                history = history,
                                reinsurance = reinsurance,
                                indices = indices_xmpl,
                                output = "total")),
                3799380.39)
})

reinsurance$Margin_type[1] <- "SIC"
test_that("calculation is correct for SIC", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                                claims_list = claims_list,
                                history = history,
                                reinsurance = reinsurance,
                                indices = indices_xmpl,
                                output = "total")),
                3562825.1)
})

reinsurance$Margin_type[1] <- "FIC"
test_that("calculation is correct for FIC", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                                claims_list = claims_list,
                                history = history,
                                reinsurance = reinsurance,
                                indices = indices_xmpl,
                                output = "total")),
                4016409.6)
})

# with quota share of 20% and APK
reinsurance$Quota_share[1] <- 0.2
reinsurance$Margin_type[1] <- "APK"
test_that("calculation is correct for quota share of 20% and APK (so then for SIC and FIC)", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                                claims_list = claims_list,
                                history = history,
                                reinsurance = reinsurance,
                                indices = indices_xmpl,
                                output = "total")),
                3571694.1)
})

# history is shortened to see if it works if limit is not entirely used
history[1,1:3] <- 0
test_that("calculation is correct for not entirely used limit", {
   expect_equal(sum(xl_cashflow(future_payments = future_payments,
                                claims_list = claims_list,
                                history = history,
                                reinsurance = reinsurance,
                                indices = indices_xmpl,
                                output = "total")),
                1042865.122)
})

