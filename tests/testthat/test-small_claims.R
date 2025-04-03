test_that("cl_factor works", {
   mat <- matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3)
   expect_equal(cl_factor(mat, 1), 1.1667, tolerance = 1e-3)
   expect_equal(cl_factor(mat, 1, 5), 1.1667, tolerance = 1e-3)
   expect_equal(cl_factor(mat, 1, 1), 1.125, tolerance = 1e-3)
   expect_equal(cl_factor(mat, 2), 1.0833, tolerance = 1e-3)
   expect_warning(cl_factor(mat, 3), "no data available for this dev_year, development factor is set to 1")
   })


test_that("generate_pattern works", {
   mat <- matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3)
   expect_equal(generate_pattern(mat), c(1.1667, 1.0833), tolerance = 1e-3)
   expect_equal(generate_pattern(triangle = matrix(c(1000, 800, 1200, 1200, 900, 1350, 1300, 950, 1400), 3),
                                 volume = 1,
                                 external_pattern = c(1.2, 1.1, 1.05, 1.01),
                                 int2ext_transition = 3),
                c(1.125, 1.0833, 1.05, 1.01), tolerance = 1e-3)

})


test_that("fill_up_triangle works", {
   mat <- matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3)
   pattern <- c(1.2, 1.1, 1.05, 1.01)
   expect_equal(sum(fill_up_triangle(mat)), 10291.67, tolerance = 1e-1)
   expect_equal(sum(fill_up_triangle(mat, pattern)), 18590.08, tolerance = 1e-1)
})


test_that("result2best_estimate works", {
   paid <- matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3)
   ultimate <- c(1300, 975, 1516.667)
   filled_triangle <- matrix(c(1000, 800, 1200, 1200, 900, 1400, 1300, 975, 1516.667), 3)
   expect_equal(result2best_estimate(ultimate, paid), c(0, 75, 316.667), tolerance = 1e-1)
   expect_equal(result2best_estimate(filled_triangle, paid), c(0, 75, 316.667), tolerance = 1e-1)
})


test_that("merge_results works", {
   pai <- c(100, 200, 300)
   filled_paid <- matrix(c(70, 150, 220, 90, 180, 270, 100, 200, 300), 3)
   inc <- c(110, 190, 290)
   expect_equal(merge_results(pai, inc, 1, 1), c(105, 195, 295))
   expect_equal(merge_results(pai, inc, c(2,1,0), 1), c(103.333, 195, 290), tolerance = 1e-1)
   expect_equal(merge_results(filled_paid, inc, c(2,1,0), 1), c(103.333, 195, 290), tolerance = 1e-1)
   expect_error(merge_results(pai, inc[1:2], 1, 1), "same number of rows")
   expect_error(merge_results(pai, inc, c(2, 1), 1), "length 1 or number")
   expect_error(merge_results(pai, inc, 1, c(2,1)), "length 1 or number")
})
