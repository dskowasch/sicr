test_that("apply_index works", {
   # for a constant inflation of 2% and index year 2022
   indices <- data.frame(Calendar_year = 2015:2023,
                         Index_gross = rep(0.02, 9))
   indices$Transition_factor <- cumprod(1 + indices$Index_gross) /
      cumprod(1 + indices$Index_gross)[indices$Calendar_year == 2022]
   expect_equal(apply_index(1000, 2022, 2023, indices), 1020)
   expect_equal(apply_index(1000, 2021, 2023, indices), 1040.4)
   expect_error(apply_index(1000, 2012, 2023, indices), "must appear")
})


test_that("generate_triangle works", {
   claims_data <- data.frame(Claim_id = c(rep("Claim1", 3), "Claim2", rep("Claim3", 2)),
                             Origin_year = c(rep(2010, 4), rep(2012, 2)),
                             Calendar_year = c(2010:2012, 2012, 2012, 2014),
                             Dev_year_since_large = c(0, 1, 2, 1, 1, 2),
                             Cl_payment_cal = rep(1000, 6))
   expect_equal(sum(generate_triangle(claims_data, "Cl_payment_cal", 2009, 2014, TRUE)), 6000)
   expect_equal(sum(generate_triangle(claims_data, "Cl_payment_cal", 2009, 2014)), 5000)
   expect_error(generate_triangle(claims_data, "Cl_reserve", 2009, 2014), "not contain column")
   claims_data$Dev_year_since_large <- NULL
   expect_error(generate_triangle(claims_data, "Cl_payment_cal", 2009, 2014), "must contain column 'Dev_year_since_large'")
})


test_that("convert_single2dev_year_triangle works", {
   cashflow <- matrix(0, nrow = 5, ncol = 3)
   rownames(cashflow) <- paste0("Claim", as.character(1:5))
   colnames(cashflow) <- 2010:2012
   cashflow[c(1, 2, 3, 6, 7, 12, 14, 15)] <- 1000
   claims_orig_years <- c(2010, 2010, 2010, 2012, 2012)
   expect_equal(sum(convert_single2dev_year_triangle(cashflow, claims_orig_years, 2010, 2012)), 8000)
   expect_error(convert_single2dev_year_triangle(cashflow[,1:2], claims_orig_years, 2010, 2012), "one column for each origin year")
   expect_error(convert_single2dev_year_triangle(cashflow[1:4,], claims_orig_years, 2010, 2012), "same number of rows")
})


test_that("convert_single2dev_year_prediction works", {
   cashflow <- matrix(0, nrow = 5, ncol = 250)
   rownames(cashflow) <- paste0("Claim", as.character(1:5))
   cashflow[c(1:10, 13:15, 18:20, 25)] <- 1000
   claims_orig_years <- c(2010, 2010, 2010, 2012, 2012)
   expect_equal(sum(convert_single2dev_year_prediction(cashflow, claims_orig_years, 2010, 2012)), 17000)
   expect_error(convert_single2dev_year_prediction(cashflow[,1:249], claims_orig_years, 2010, 2012), "cashflow must have 250 future calendar years as columns")
   expect_error(convert_single2dev_year_prediction(cashflow[1:4,], claims_orig_years, 2010, 2012), "same number of rows")
})


test_that("generate_history_per_claim works", {
   claims_data <- data.frame(Claim_id = c(rep("Claim1", 3), "Claim2", rep("Claim3", 2)),
                             Origin_year = c(rep(2010, 4), rep(2012, 2)),
                             Calendar_year = c(2010:2012, 2012, 2012, 2014),
                             Dev_year_since_large = c(0, 1, 2, 1, 1, 2),
                             Cl_payment_cal = rep(1000, 6))
   expect_equal(sum(generate_history_per_claim(claims_data, "Cl_payment_cal", 2009, 2014)), 6000)
   expect_equal(generate_history_per_claim(claims_data, "Cl_payment_cal", 2009, 2014)[2,4], 1000)
})


test_that("tail_factor works", {
   expect_equal(tail_factor(c(17:18, 50:51), 17, 50), c(1, 0.9706, 0.0294, 0), tolerance = 1e-3)
})


test_that("cal_year2dev_year works", {
   cal_year_triangle <- matrix(c(1000, 0, 0, 200, 800, 0, 100, 100, 900), 3)
   rownames(cal_year_triangle) <- colnames(cal_year_triangle) <- 2012:2014
   expect_equal(cal_year2dev_year(cal_year_triangle)[2,1], 800)
})


test_that("dev_year2cal_year works", {
   dev_year_triangle <- matrix(c(1000, 800, 900, 200, 100, 0, 100, 0, 0), 3)
   rownames(dev_year_triangle) <- 2012:2014
   colnames(dev_year_triangle) <- 1:3
   expect_equal(dev_year2cal_year(dev_year_triangle)[2,2], 800)
})
