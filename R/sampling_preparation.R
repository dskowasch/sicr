#' Convert pools to matrix
#'
#' @description
#' This function reduces the complexity of the pools structure by converting it to a matrix to sample from. See details
#' for more information.
#'
#' @param pools List as output of [generate_pools()].
#'
#' @return List of three objects representing the pools, see details.
#' @keywords internal
#'
#' @details
#' The list `pools` contains the objects `pretail_pools`, `tail_pools` and `pool_of_annuities`. The first two are lists
#' that contain lists themselves that contain pools per development year and reserve class. This format makes it
#' easy to get a good overview of a pool by accessing it via the assigned indices of the lists. But on the other side,
#' this format doesn't allow for vectorized sampling. \cr
#' Hence this function turns the first two objects of `pools` into a `sample_matrix`. This matrix contains 7 columns
#' that are needed for sampling, that is `Ind_cl_payment_cal`, `Exit_reserve_class`, `New_annuity_1`, ...,
#' `New_annuity_5`. The rows of the pools are appended to the matrix, starting
#' with reserve class 0 and development year 2, continuing with reserve class 0 and development year 3 to `tail_start - 1`
#' and the same procedure with the other reserve classes. The tail pools are attached at the end. \cr \cr
#' As different pool sizes make it hard to find the start and the end of a pool in this matrix, every pool gets assigned
#' the same number of rows, and that is the maximum size of all pools. Every row that does not belong to the
#' original pool is set to 0. \cr \cr
#' __Example:__ A setup consisting of the reserve classes 0 to 3 and the development years 2 to 16 plus tail will
#' result in \eqn{4 * (15 + 1) = 64} pools. If the maximum size of the pools is 1.000 rows, each pool will be assigned 1.000
#' rows in `sample_matrix`, so the matrix has dimension `(64.000, 7)`. \cr \cr
#' Of course not every pool is of this maximum size, so the vector `lengths_vector` is created which contains the sizes
#' of every pool. \cr \cr
#' The first row of the pool in `sample_matrix` is calculated by
#'
#' \deqn{
#' row_1 =
#' \left\{
#'    \begin{array}{l}
#'       \bigl(rc \cdot \left( tail_1 - 2 \right) + dy - 2 \bigr) \cdot n_{pool,\ max} + 1 \quad \text{for pretail pools } \\
#'       \bigl(n_{rc} \cdot \left( tail_1 - 2 \right) + rc \bigr) \cdot n_{pool,\ max} + 1 \quad \text{for tail pools}
#'    \end{array}
#' \right.
#' }
#' where
#' \deqn{
#' \begin{array}{l}
#'    rc \quad \text{is the reserve class} \\
#'    dy \quad \text{is the desired development year} \\
#'    tail_1 \quad \text{is the first tail year} \\
#'    n_{rc} \quad \text{is the number of reserve classes and} \\
#'    n_{pool,\ max} \quad \text{is the maximum pool size}
#' \end{array}
#' }
#'
#' The rows to sample from can be derived from \eqn{row_1} and the corresponding entry of `lengths_vector`. The function
#' [get_matrix_info()] returns these values.
#'
#'
#' @examples \dontrun{
#' # this example uses data provided with this package
#' new_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
#'                          indices = indices_xmpl,
#'                          threshold = 400000,
#'                          first_orig_year = 1989,
#'                          last_orig_year = 2023,
#'                          expected_year_of_growing_large = 3,
#'                          reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                          pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' pools <- generate_pools(extended_claims_data = new_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' pools_as_matrix <- pools2matrix(pools)}
pools2matrix <- function(pools) {
   ### lengths vector
   # start_of_tail and number_of_reserve_classes is derived from pools
   start_of_tail <-  length(pools[[1]][[1]]) + 2
   number_of_reserve_classes <- length(pools[[1]])

   # Initialise and fill vector with pool sizes, at first only for pretail pools
   lengths_vector <- vector("numeric", number_of_reserve_classes * (start_of_tail - 2))

   for (reserve_class in 0:(number_of_reserve_classes - 1)) {
      for (dev_year in 2:(start_of_tail - 1)) {
         lengths_vector[reserve_class * (start_of_tail - 2) + dev_year - 1] <-
            NROW(pools[[1]][[reserve_class + 1]][[dev_year - 1]])
      }
   }

   # add pool sizes of tail pools
   for (reserve_class in 0:(number_of_reserve_classes - 1)) {
      lengths_vector <- c(lengths_vector, NROW(pools[[2]][[reserve_class + 1]]))
   }

   ### sample matrix
   # Matrix representing the pools is initiated and filled. It contains vectors of claim payments, new_reserve_classes
   #   and new_annuities 1 to 5.
   sample_matrix <- matrix(0,
                           nrow = max(lengths_vector) * (number_of_reserve_classes) * ((start_of_tail - 1)),
                           ncol = 7)
   for (reserve_class in 0:(number_of_reserve_classes - 1)) {
      # start with pretail pools of reserve_class
      for (dev_year in 2:(start_of_tail - 1)) {
         range <- get_matrix_info(dev_year = dev_year,
                                  reserve_class = reserve_class,
                                  start_of_tail = start_of_tail,
                                  number_of_reserve_classes = number_of_reserve_classes,
                                  lengths_vector = lengths_vector,
                                  output = "range")
         sample_matrix[range,] <-  as.matrix(pools[[1]][[reserve_class + 1]][[dev_year - 1]][, 1:7])
      }

      # tail pools of reserve_class
      range <- get_matrix_info(dev_year = start_of_tail,
                               reserve_class = reserve_class,
                               start_of_tail = start_of_tail,
                               number_of_reserve_classes = number_of_reserve_classes,
                               lengths_vector = lengths_vector,
                               output = "range")

      sample_matrix[range,] <- as.matrix(pools[[2]][[reserve_class + 1]][, 1:7])
   }

   ### Output: created objects plus unchanged pool of annuities
   return(list(lengths_vector, sample_matrix, pools[[3]]))
}

#' Helper function to get start, sample_size and range of a pool in the sample_matrix
#'
#' @description
#' The function [pools2matrix()] converts the pools into a matrix for faster simulation. This function helps to
#' find the start and end of a pool in the matrix. For description check details of [pools2matrix()].
#'
#' @param dev_year Development year of desired pool.
#' @param reserve_class Reserve class of desired pool.
#' @param start_of_tail Start of tail.
#' @param number_of_reserve_classes Number of reserve classes.
#' @param lengths_vector Vector containing the lengths of each pool (first entry of list output of [pools2matrix()]).
#' @param output One of `sample_size`, `start` or `range`.
#'
#' @return Integer stating the row number if `output` equals `sample_size` or `start`, integer vector of row numbers in case of `range`.
#' @keywords internal
#'
#' @examples \dontrun{
#' # this example uses data provided with this package
#' new_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
#'                          indices = indices_xmpl,
#'                          threshold = 400000,
#'                          first_orig_year = 1989,
#'                          last_orig_year = 2023,
#'                          expected_year_of_growing_large = 3,
#'                          reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                          pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' pools <- generate_pools(extended_claims_data = new_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' pools_as_matrix <- pools2matrix(pools)
#' get_matrix_info(10, 2, 17, 6, pools_as_matrix[[1]], "start")
#' get_matrix_info(10, 2, 17, 6, pools_as_matrix[[1]], "sample_size")
#' get_matrix_info(10, 2, 17, 6, pools_as_matrix[[1]], "range")}
get_matrix_info <- function(dev_year,
                            reserve_class,
                            start_of_tail,
                            number_of_reserve_classes,
                            lengths_vector,
                            output) {
   sample_size <- function() {
      ifelse(dev_year > (start_of_tail - 1), # case pretail first, then case tail
             lengths_vector[number_of_reserve_classes * (start_of_tail - 2) + reserve_class + 1],
             lengths_vector[dev_year - 1 + reserve_class * (start_of_tail - 2)])}

   start <- function() {
      ifelse(dev_year <= (start_of_tail - 1), # case pretail first, then case tail
             (reserve_class * (start_of_tail - 2) + dev_year - 2) * max(lengths_vector) + 1,
             (number_of_reserve_classes * (start_of_tail - 2) + reserve_class) * max(lengths_vector) + 1)
   }

   if (output == "sample_size") return(sample_size())
   if (output == "start") return(start())
   if (output == "range") return(start():(start() + sample_size() - 1))
   stop("Output must be sample_size, start or range!")
}
