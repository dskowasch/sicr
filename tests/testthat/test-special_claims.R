extended_claims_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
                                     indices = indices_xmpl,
                                     threshold = 400000,
                                     first_orig_year = 1989,
                                     last_orig_year = 2023,
                                     expected_year_of_growing_large = 3,
                                     reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                     pool_of_annuities = minimal_pool_of_annuities_xmpl)

external_patterns <- list(c(0.5, 0.3, 0.2),
                          c(1),
                          c(0.7, 0.3))

test_that("compute_special_claims works in this normal case", {
   special_claims <- data.frame(Claim_id = c("Claim#43", "Claim#44", "Claim#68"),
                                Reserve2BE_percentage = c(0.8, 0.5, 1.2),
                                Rollout_type = c("linear", "constant", "external"),
                                Pattern_id = c(1,1,3))
   reinsurance_xmpl[reinsurance_xmpl$Origin_year == 2019, "Priority"] <- 100000

   result <- compute_special_claims(special_claims = special_claims,
                                    extended_claims_data = extended_claims_data,
                                    first_orig_year = 1989,
                                    last_orig_year = 2023,
                                    end_of_tail = 50,
                                    external_patterns = external_patterns,
                                    active_annuities = minimal_active_annuities_xmpl,
                                    age_shift = age_shift_xmpl,
                                    mortality = mortality_xmpl,
                                    reinsurance = reinsurance_xmpl,
                                    indices = indices_xmpl)
   expect_equal(length(result), 6)
   expect_equal(sum(result$special_claims_payments[,as.integer(colnames(result$special_claims_payments)) > 2023]),
                0.8 * extended_claims_data$Cl_reserve[extended_claims_data$Claim_id == "Claim#43" &
                                                         extended_claims_data$Calendar_year == 2023] +
                  0.5 *  extended_claims_data$Cl_reserve[extended_claims_data$Claim_id == "Claim#44" &
                                                            extended_claims_data$Calendar_year == 2023] +
                   1.2 * extended_claims_data$Cl_reserve[extended_claims_data$Claim_id == "Claim#68" &
                                                            extended_claims_data$Calendar_year == 2023])
   expect_true(sum(result$special_annuities_payments) > 0)
   expect_true(sum(result$special_ceded_xl_payments["2019",]) > 0)
   expect_equal(sum(result$special_ceded_quota_payments["2006",]),
                0.3 * (sum(result$special_claims_payments["2006",]) + sum(result$special_annuities_payments["2006",])))

})

test_that("compute_special_claims works when no special claim is defined", {
   special_claims <- data.frame(Claim_id = c(),
                                Reserve2BE_percentage = c(),
                                Rollout_type = c(),
                                Pattern_id = c())
   reinsurance_xmpl[reinsurance_xmpl$Origin_year == 2019, "Priority"] <- 100000

   result <- compute_special_claims(special_claims = special_claims,
                                    extended_claims_data = extended_claims_data,
                                    first_orig_year = 1989,
                                    last_orig_year = 2023,
                                    end_of_tail = 50,
                                    external_patterns = external_patterns,
                                    active_annuities = minimal_active_annuities_xmpl,
                                    age_shift = age_shift_xmpl,
                                    mortality = mortality_xmpl,
                                    reinsurance = reinsurance_xmpl,
                                    indices = indices_xmpl)
   expect_equal(length(result), 6)
   expect_equal(sum(result$special_claims_payments), 0)
   expect_equal(sum(result$special_annuities_payments), 0)
   expect_equal(dim(result$special_ceded_xl_payments), c(35, 285))

})

test_that("compute_special_claims works when just one special claim is defined", {
   special_claims <- data.frame(Claim_id = c("Claim#43"),
                                Reserve2BE_percentage = c(0.8),
                                Rollout_type = c("linear"),
                                Pattern_id = c(1,1,3))
   reinsurance_xmpl[reinsurance_xmpl$Origin_year == 2006, "Priority"] <- 100000

   result <- compute_special_claims(special_claims = special_claims,
                                    extended_claims_data = extended_claims_data,
                                    first_orig_year = 1989,
                                    last_orig_year = 2023,
                                    end_of_tail = 50,
                                    external_patterns = external_patterns,
                                    active_annuities = minimal_active_annuities_xmpl,
                                    age_shift = age_shift_xmpl,
                                    mortality = mortality_xmpl,
                                    reinsurance = reinsurance_xmpl,
                                    indices = indices_xmpl)
   expect_equal(length(result), 6)
   expect_true(sum(result$special_claims_payments) > 0)
   expect_true(sum(result$special_annuities_payments) > 0)
   expect_true(sum(result$special_ceded_xl_payments) > 0)

})



