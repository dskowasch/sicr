# tests for generate_annuity_payments
test_that("generate_annuity_payments works in standard case", {
   result <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
                                        last_orig_year = 2023,
                                        indices = indices_xmpl)
   expect_equal(result[1,1:10],
                rep(12168.21,10),
                tolerance = 1e-3)
})

test_that("generate_annuity_payments works with indexation", {
   result <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
                                       last_orig_year = 2025,
                                       indices = indices_xmpl)
   expect_equal(result[1,1:10],
                rep(12168.21,10) * 1.03^2,
                tolerance = 1e-3)
})

test_that("generate_annuity_payments works with only one annuity", {
   result <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl[1,],
                                       last_orig_year = 2023,
                                       indices = indices_xmpl)
   expect_equal(result[,1:10],
                rep(12168.21, 10),
                tolerance = 1e-3)
   expect_equal(dim(result), c(1, 130))
})

test_that("generate_annuity_payments works with start and end years", {
   annuity <- minimal_active_annuities_xmpl[1,]
   annuity$Annuity_start <- 2027
   annuity$Annuity_end <- 2029
   result <- generate_annuity_payments(annuities = annuity,
                                       last_orig_year = 2023,
                                       indices = indices_xmpl)
   expect_equal(result[,1:10],
                c(rep(0, 3), rep(12168.21, 3), rep(0, 4)),
                tolerance = 1e-3)
})


test_that("generate_annuity_payments applies dynamic properly", {
   # Annuity starts in next year
   annuities <- minimal_active_annuities_xmpl[1,]
   annuities$Dynamic <- 0.05
   annuities$Annuity_start <- 2024
   result <- generate_annuity_payments(annuities = annuities,
                                       last_orig_year = 2023,
                                       indices = indices_xmpl)
   expect_equal(result[,1:3],
                12168.21 * 1.05^(0:2),
                tolerance = 1e-3)

   # Annuity starts in four years
   annuities <- minimal_active_annuities_xmpl[1,]
   annuities$Dynamic <- 0.05
   annuities$Annuity_start <- 2027
   result <- generate_annuity_payments(annuities = annuities,
                                       last_orig_year = 2023,
                                       indices = indices_xmpl)
   expect_equal(result[,1:6],
                c(rep(0, 3), 12168.21 * 1.05^(0:2)),
                tolerance = 1e-3)

   # Annuity has started four years ago
   annuities <- minimal_active_annuities_xmpl[1,]
   annuities$Dynamic <- 0.05
   annuities$Annuity_start <- 2019
   result <- generate_annuity_payments(annuities = annuities,
                                       last_orig_year = 2023,
                                       indices = indices_xmpl)
   expect_equal(result[,1:3],
                12168.21 * 1.05^(5:7),
                tolerance = 1e-3)
})


# tests for generate_annuity_probabilities
test_that("generate_annuity_probabilities works in standard case", {
   annuity <- minimal_active_annuities_xmpl[1,]
   annuity$Birth_year <- 1946 # age shift is 0 here
   result <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   expect_equal(result[1, 2], 0.2568458, tolerance = 1e-5)
})

test_that("generate_annuity_probabilities works with age shift", {
   annuity <- minimal_active_annuities_xmpl[1,]
   annuity$Birth_year <- 1956 # age shift is -2 here
   annuity$Calendar_year <- 2033 # Birth_year and Calendar_year are shifted by 10 years so that age stays the same
   result <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   expect_equal(result[1, 2], 0.2501875, tolerance = 1e-5)
})

test_that("dimensions of generate_annuity_probabilities works are correct", {
   annuity <- minimal_active_annuities_xmpl
   result <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   expect_equal(dim(result), c(3, 130), tolerance = 1e-5)

   # only one annuity
   annuity <- minimal_active_annuities_xmpl[1,]
   result <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   expect_equal(dim(result), c(1, 130), tolerance = 1e-5)
})


# tests for roll_out_annuity
test_that("roll_out_annuity works", {
   # deterministic
   annuity <- minimal_active_annuities_xmpl
   payments <- generate_annuity_payments(annuity, 2023, indices_xmpl)
   probs <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   result <- roll_out_annuity(payments, probs, stop.at = "end_of_probs")
   expect_equal(dim(result), c(3, 130), tolerance = 1e-5)

   # only one annuity
   annuity <- minimal_active_annuities_xmpl[1,]
   payments <- generate_annuity_payments(annuity, 2023, indices_xmpl)
   probs <- generate_annuity_probabilities(annuity, mortality_xmpl, age_shift_xmpl)
   result <- roll_out_annuity(payments, probs, stop.at = "end_of_probs")
   expect_equal(result, payments*(1 - probs))
   expect_equal(dim(result), c(1, 130), tolerance = 1e-5)

   set.seed(1234)
   result1 <- roll_out_annuity(payments, probs)
   set.seed(123)
   result2 <- roll_out_annuity(payments, probs)
   expect_false(sum(result1) == sum(result2))
})

