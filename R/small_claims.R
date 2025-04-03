#' chain ladder factor
#' @description
#' calculating the chain-ladder factor for a given development year and claims triangle
#'
#' @param triangle cumulative claims triangle. An (mxn)-matrix where only the upper left triangle (including diagonal) will be used
#' @param dev_year development year to calculate chain ladder factor for. An Integer > 0
#' @param volume Number of newest calendar years to be used. Default: NULL,
#' An Integer > 0 or NULL for no limitation
#'
#'
#' @return A number
#' @export
#'
#'
#' @examples
#' cl_factor(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3), 1)
cl_factor <- function(triangle, dev_year, volume = NULL){
   n <- min(ncol(triangle), nrow(triangle))
   rows_count <- nrow(triangle)
   columns_count <- ncol(triangle)
   if (dev_year >= n) {
      warning("no data available for this dev_year, development factor is set to 1")
      return(1)}
   if (dev_year < 1 | dev_year %% 1 != 0) {
      stop("dev_year must be integer greater 0")}
   if (rows_count < columns_count) {
      stop("ncol of triangle must be smaller or equal to nrow")}

   # first and last line to be considered
   if (is.null(volume)) {volume <- rows_count}
   if (volume < 1 | volume %% 1 != 0) {
      stop("volume must be integer greater 0")}
   last_row <- rows_count - dev_year
   first_row <- max(1, last_row - volume + 1)

   # sum of columns for chain ladder factor
   colsum_before <- sum(triangle[first_row:last_row, dev_year])
   colsum_after <- sum(triangle[first_row:last_row, dev_year + 1])

   # cl-factor is set to 1 if colsum_before is 0
   if (colsum_before == 0) {
      warning("cl-factor is undefined and is set to 1")
      return(1)}

   return(colsum_after/colsum_before)
}




#' Claims Development Pattern
#'
#' @description
#' generating the claims development pattern from a claims triangle and mixing it with an external pattern, e.g. for considering tail patterns

#' @param triangle cumulative claims triangle. An (mxn)-matrix where only the upper left triangle (including diagonal) will be used
#' @param volume Number of newest calendar years to be used. Default: NULL,
#' An Integer > 0 or NULL for no limitation
#' @param external_pattern numeric vector or NULL if no external pattern shall be used, Default: NULL
#' @param int2ext_transition year of transition from internal to external pattern. Integer or NULL for no transition.
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' generate_pattern(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3))
#' generate_pattern(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3), 1,
#'                         c(1.2, 1.1, 1.05, 1.01), 3)
generate_pattern <- function(triangle,
                             volume = NULL,
                             external_pattern = NULL,
                             int2ext_transition = NULL){
   # length of original triangle
   ndim0 <- ncol(triangle)

   # length of return vector
   # external_pattern may be longer than original triangle
   ndim <- if (is.null(external_pattern)) {ndim0} else {NROW(external_pattern) + 1}

   # create internal pattern with chain ladder
   internal_pattern <- rep(1, ndim0 - 1)
   for (dev_year in (1:(ndim0 - 1))) {
      internal_pattern[dev_year] <- cl_factor(triangle, dev_year, volume)
   }

   # append external pattern
   pattern <- internal_pattern
   if (is.null(int2ext_transition)) {int2ext_transition <- ndim0} else {
      if (int2ext_transition > ndim | int2ext_transition %% 1 != 0) {int2ext_transition <- ndim0}}
   if (!is.null(external_pattern) & ndim > int2ext_transition) {
      pattern[min(ndim0, int2ext_transition):(ndim - 1)] <-
         external_pattern[min(ndim0, int2ext_transition):(ndim - 1)]
   }
   return(pattern)
}



#' Filling up claims triangle to rectangle
#' @description
#' uses a claims development pattern to calculate the lower right triangle of a matrix
#'
#' @param triangle cumulative claims triangle. An (mxn)-matrix where only the upper left triangle (including diagonal) will be used
#' @param pattern Default: NULL, numeric vector or NULL if no external pattern shall be used
#' The pattern vector may be longer than the triangle for applying a tail pattern.
#'
#' @return filled triangle. numeric matrix
#' @export
#'
#' @examples
#' fill_up_triangle(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3))
#' fill_up_triangle(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3),
#'                         c(1.2, 1.1, 1.05, 1.01))
fill_up_triangle <- function(triangle,
                             pattern = NULL){
   # If pattern = NULL, pattern is derived from triangle without tail
   if (is.null(pattern)) {
      pattern <- generate_pattern(triangle = triangle)
   }

   ndim0 <- ncol(triangle)
   ndim <- NROW(pattern) + 1
   n_row <- nrow(triangle)

   # initialise return matrix
   filled_triangle <- triangle
   if (ndim > ndim0) {
      filled_triangle <- cbind(filled_triangle, matrix(0, ndim0, ndim - ndim0))
   }

   # pretail
   for (aj in (1:(ndim0 - 1))) {
      filled_triangle[ (n_row - aj + 1):n_row, aj + 1] <-
         filled_triangle[(n_row - aj + 1):n_row, aj] *
         pattern[aj]
   }

   # tail
   if (ndim > ndim0) {
      for (aj in ndim0:(ndim - 1)) {
         filled_triangle[,aj + 1] <- filled_triangle[,aj] * pattern[aj]
      }
   }
   colnames(filled_triangle) <- 1:ncol(filled_triangle)
   return(filled_triangle)
}


#' deriving best estimate
#'
#' @description deriving best estimate from ultimate vector and paid triangle
#' @param result ultimate vector or filled up cumulative claims triangle. numeric vector or numeric matrix
#' @param paid cumulative claims triangle. An (mxn)-matrix where only the upper left triangle (including diagonal) will be used
#'
#' @return best estimate. numeric vector
#' @export
#'
#' @examples
#' result2best_estimate(c(1300, 975, 1516.667),
#'                      matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3))
#' result2best_estimate(matrix(c(1000, 800, 1200, 1200, 900, 1400, 1300, 975, 1516.667), 3),
#'                      matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3))

result2best_estimate <- function(result, paid){
   if ((NROW(paid) < 2) | (NROW(paid) != NROW(result))) {
      stop("paid and result must have same number of rows (minimum 2)")
   }
   result <- as.matrix(result)
   result_vector <- result[,ncol(result)]
   return(result_vector - diag(paid[,nrow(paid):1]))
}


#' merge paid and incurred results
#'
#' @description merging the results of paid and incurred triangle methods with given weights
#' @param paid_result ultimate vector or filled up cumulative paid claims triangle. numeric vector or numeric matrix
#' @param incurred_result ultimate vector or filled up cumulative incurred claims triangle. numeric vector or numeric matrix
#' @param weight_paid Default: 1. numeric vector of length 1 or same as number of rows of paid_result
#' @param weight_incurred Default: 1. numeric vector of length 1 or same as number of rows of incurred_result
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' pai <- c(100, 200, 300)
#' filled_paid <- matrix(c(70, 150, 220, 90, 180, 270, 100, 200, 300), 3)
#' inc <- c(110, 190, 290)
#' merge_results(pai, inc, 1, 1)
#' merge_results(pai, inc, c(2,1,0), 1)
#' merge_results(filled_paid, inc, c(2,1,0), 1)
#'
merge_results <- function(paid_result,
                          incurred_result,
                          weight_paid = 1,
                          weight_incurred = 1) {
   if (NROW(paid_result) != NROW(incurred_result)) {
      stop("paid_result and incurred_result must have the same number of rows")
   }
   if (!length(weight_paid) %in% c(1, NROW(paid_result))) {
      stop("weight_paid must have length 1 or number of rows of paid_result")
   }
   if (!length(weight_incurred) %in% c(1, NROW(incurred_result))) {
      stop("weight_incurred must have length 1 or number of rows of incurred_result")
   }

   weighted_paid <- weight_paid * as.matrix(paid_result)[,NCOL(paid_result)]
   weighted_incurred <- weight_incurred * as.matrix(incurred_result)[,NCOL(incurred_result)]
   return((weighted_paid + weighted_incurred) / (weight_paid + weight_incurred))
}


#' change development factors to settlement rates
#'
#' @param pattern numeric vector of development factors per development year
#' @return numeric vector
#' @keywords internal
#' @examples \dontrun{
#' settlement_rates(c(1.7, 1.3, 1.1, 1, 1, 1))
#' }
settlement_rates <- function(pattern) {
   c(rev(cumprod(rev(1 / pattern))), 1)
}


#' roll out best estimate to a cashflow
#'
#' @param best_estimate numeric vector.
#' @param pattern numeric vector of development factors per development year
#' @param dev_year integer vector > 0
#' @note best_estimate and dev_year must have same length
#' @return numeric vector of length 250
#' @keywords internal
#' @examples \dontrun{
#' rollout_best_estimate(1000, c(1.7, 1.3, 1.1, 1.05, 1, 1), 3)
#' }
rollout_best_estimate <- function(best_estimate, pattern, dev_year) {
   not_vec_rollout_best_estimate <- function(best_estimate, pattern, dev_year) {
      cashflow <- rep(0, 250)
      pattern_length <- NROW(pattern) + 1

      if (!is.numeric(dev_year) | dev_year < 1 | dev_year %% 1 != 0) {
         cashflow[1] <- best_estimate
         warning("dev_year must be positive integer. Best estimate was allocated to the first subsequent year.")
         return(cashflow)}

      if (dev_year > (pattern_length - 1)) {
         cashflow[1] <- best_estimate
         return(cashflow)}

      # change development factors to settlement rates
      settle_rates <- settlement_rates(pattern)

      origin_value <- best_estimate / (1 - settle_rates[dev_year])
      if (is.na(origin_value) | is.infinite(origin_value)) {
         cashflow[1] <- best_estimate} else {
            cashflow[1:(pattern_length - dev_year)] <- origin_value * cum2inc(settle_rates)[(dev_year + 1):pattern_length]
         }

      return(cashflow) }
   vec_rollout_best_estimate <- Vectorize(not_vec_rollout_best_estimate,
                                          vectorize.args = c("best_estimate", "dev_year"))

   result <- vec_rollout_best_estimate(best_estimate, pattern, dev_year)
   return(result)
}

#' rolling out best estimate vector
#'
#' @description
#' rolling out the best estimate for the last n origin years with given pattern
#'
#' @param best_estimate numeric vector. last entry is for the youngest origin year
#' @param pattern numeric vector
#' @param last_orig_year integer.
#'
#' @return numeric matrix of future payments per origin year with 250 future calendar years
#' @export
#'
#' @examples best_estimate2cashflow(c(100, 400, 1000), c(1.7, 1.3, 1.05, 1, 1), 2020)
best_estimate2cashflow <- function(best_estimate, pattern, last_orig_year){
   best_estimate2cashflow <- t(rollout_best_estimate(best_estimate = best_estimate,
                                                     pattern = pattern,
                                                     dev_year = NROW(best_estimate):1))
   rownames(best_estimate2cashflow) <- (last_orig_year - NROW(best_estimate2cashflow) + 1):last_orig_year
   colnames(best_estimate2cashflow) <- (last_orig_year + 1):(last_orig_year + 250)
   return(best_estimate2cashflow)
}


#' Calculate small claims results in one step
#'
#' @description
#' This function calculates all small claims results in one step.
#'
#' @param first_orig_year Desired first origin year.
#' @param last_orig_year Desired last origin year.
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param all_claims_paid Numeric matrix with origin years as rows and same years as calendar years as columns.
#' @param all_claims_reserved Numeric matrix with origin years as rows and same years as calendar years as columns.
#' @param all_annuities_paid Default: NULL \cr
#' Numeric matrix with origin years as rows and same years as calendar years as columns. \cr
#' No information about small claims' annuities history will be returned if NULL.
#' @param all_annuities_reserved Default: NULL \cr
#' Numeric matrix with origin years as rows and same years as calendar years as columns. \cr
#' No information about small claims' annuities history will be returned if NULL.
#' @param large_claims_list Dataframe of large claims generated with [generate_claims_list()].
#' @param large_claims_results List generated with [aggregate_large_claims_results()].
#' @param special_claims Dataframe with special claims, see details of [compute_special_claims()].
#' @param special_claims_results List generated with [compute_special_claims()]
#' @param volume_paid_method Default: NULL. Volume to use for paid method, see [cl_factor()].
#' @param volume_incurred_method Default: NULL. Volume to use for incurred method, see [cl_factor()].
#' @param external_paid_pattern Default: NULL. Development pattern to use for paid method, see [generate_pattern()].
#' @param external_incurred_pattern Default: NULL. Development pattern to use for incurred method, see [generate_pattern()].
#' @param int2ext_transition_paid Default: NULL. Transition development year to use for paid method, see [generate_pattern()].
#' @param int2ext_transition_incurred Default: NULL. Transition development year to use for incurred method, see [generate_pattern()].
#' @param weight_paid Default: 1. Weight of paid method in result selection, see [merge_results()].
#' @param weight_incurred Default: 1. Weight of incurred method in result selection, see [merge_results()].
#' @param active_annuities Default: NULL. Dataframe of active annuities, see [active_annuities_xmpl].
#' @param mortality Default: NULL. Dataframe, see description of [mortality_xmpl].
#' @param age_shift Default: NULL. Dataframe, see description of [age_shift_xmpl].
#' @param pool_of_annuities Default: NULL. Dataframe, see description of `pool_of_annuities` in details of [prepare_data()].
#' @param reinsurance Default: NULL Dataframe with reinsurance information, see details of [xl_cashflow()] or [reinsurance_xmpl]. \cr
#' Only necessary if reinsurance shall be considered.
#'
#' @returns
#' List with six results matrices for small claims. Each matrix consist of one row per
#' development year between `first_orig_year` and `last_orig_year`. Matrices with suffix `...payments` consist of
#' one column for each historic calendar year between `first_orig_year` and `last_orig_year` and 250 predicted
#' calendar years. Matrices with suffix `...reserved` only contain the historic part as no reserves are predicted. \cr
#' 1. `small_claims_payments` with historic and predicted claim payments (e.g. without annuities).
#' 2. `small_claims_reserved` with historic claim reserves (without annuities).
#' 3. `small_known_annuities_payments` with historic and predicted payments for known annuities. Historic payments
#' are set to 0 if `all_annuities_paid` is `NULL`.
#' 4. `small_annuities_reserved` with historic annuities' reserves. All entries are 0 if `small_annuities_reserved` is `NULL`.
#' 5. `small_future_annuities_payments` with predicted payments for future annuities, the historic part is all 0.
#' 6. `small_ceded_quota_payments` with historic and predicted reinsurance payments for quota share treaties. \cr \cr
#'
#' Note that there are no reserves for future annuities. reserved triangles for reinsurance are often not available,
#' so they are not included here.
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#'
#'
#' # The claims with claim_ids Claim#1474, Claim#3164 and Claim#3200 are identified as special claims.
#' special_claims <- data.frame(Claim_id = c("Claim#1474", "Claim#3164", "Claim#3200"),
#'                              Reserve2BE_percentage = c(0.8, 0.5, 1.2),
#'                              Rollout_type = c("linear", "constant", "external"),
#'                              Pattern_id = c(1,1,3))
#'
#' external_patterns <- list(c(0.5, 0.3, 0.2),
#'                           c(1),
#'                           c(0.7, 0.3))
#'
#' special_claims_results <- compute_special_claims(special_claims = special_claims,
#'                                                  extended_claims_data = extended_claims_data,
#'                                                  first_orig_year = 1989,
#'                                                  last_orig_year = 2023,
#'                                                  end_of_tail = 50,
#'                                                  external_patterns = external_patterns,
#'                                                  active_annuities = active_annuities_xmpl,
#'                                                  age_shift = age_shift_xmpl,
#'                                                  mortality = mortality_xmpl,
#'                                                  reinsurance = reinsurance_xmpl,
#'                                                  indices = indices_xmpl)
#'
#' # filter large claims
#' large_extended_claims_data <-
#'    extended_claims_data[!extended_claims_data$Claim_id %in% special_claims$Claim_id,]
#'
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = large_extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' history <- generate_history_per_claim(data = large_extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2023)
#' # simulation result
#' sim_result <- sicr(n = 1,
#'                    large_claims_list = large_claims_list,
#'                    first_orig_year = 1989,
#'                    last_orig_year = 2023,
#'                    pools = pools,
#'                    indices = indices_xmpl,
#'                    history = history,
#'                    exposure = exposure_xmpl,
#'                    years_for_ibnr_pools = 2014:2023,
#'                    active_annuities = active_annuities_xmpl,
#'                    age_shift = age_shift_xmpl,
#'                    mortality = mortality_xmpl,
#'                    reinsurance = reinsurance_xmpl)
#'
#' large_claims_results <- aggregate_large_claims_results(
#'                               sim_result = sim_result,
#'                               large_claims_list = large_claims_list,
#'                               large_extended_claims_data = large_extended_claims_data,
#'                               history = history,
#'                               reinsurance = reinsurance_xmpl,
#'                               indices = indices_xmpl,
#'                               first_orig_year = 1989,
#'                               last_orig_year = 2023)
#'
#'
#' small_claims_results <- compute_small_claims(
#'                               first_orig_year = 1989,
#'                               last_orig_year = 2023,
#'                               indices = indices_xmpl,
#'                               all_claims_paid = all_claims_paid_xmpl,
#'                               all_claims_reserved = all_claims_reserved_xmpl,
#'                               all_annuities_paid = all_annuities_paid_xmpl,
#'                               all_annuities_reserved = all_annuities_reserved_xmpl,
#'                               large_claims_list = large_claims_list,
#'                               large_claims_results = large_claims_results,
#'                               special_claims = special_claims,
#'                               special_claims_results = special_claims_results,
#'                               active_annuities = active_annuities_xmpl,
#'                               mortality = mortality_xmpl,
#'                               age_shift = age_shift_xmpl,
#'                               pool_of_annuities = pool_of_annuities_xmpl,
#'                               reinsurance = reinsurance_xmpl)
#' print(small_claims_results)
compute_small_claims <- function(first_orig_year,
                                 last_orig_year,
                                 indices,
                                 all_claims_paid,
                                 all_claims_reserved,
                                 all_annuities_paid = NULL,
                                 all_annuities_reserved = NULL,
                                 large_claims_list,
                                 large_claims_results,
                                 special_claims,
                                 special_claims_results,
                                 volume_paid_method = NULL,
                                 volume_incurred_method = NULL,
                                 external_paid_pattern = NULL,
                                 external_incurred_pattern = NULL,
                                 int2ext_transition_paid = NULL,
                                 int2ext_transition_incurred = NULL,
                                 weight_paid = 1,
                                 weight_incurred = 1,
                                 active_annuities = NULL,
                                 mortality = NULL,
                                 age_shift = NULL,
                                 pool_of_annuities = NULL,
                                 reinsurance = NULL) {
   # initiate dummys
   empty_triangle <- skeleton_triangle(first_orig_year, last_orig_year)
   empty_prediction <- skeleton_prediction(first_orig_year, last_orig_year)
   empty_total <- cbind(empty_triangle, empty_prediction)

   # check data
   if (any(!is.null(active_annuities),
           !is.null(pool_of_annuities))) {
      if (any(is.null(mortality), is.null(age_shift))) {
         stop("If active_annuities or pool_of_annuities is not NULL,
              mortality and age_shift is needed.")
      }
   }

   # initiate empty results results for the case that it shall not be calculated
   small_known_annuities_payments <- empty_total
   small_annuities_reserved <- empty_triangle
   small_future_annuities_payments <-  empty_total
   small_ceded_quota_payments <- empty_total

   ##### step 1: derive small claims triangles
   small_claims_paid <-
      all_claims_paid - cut2history(large_claims_results$large_claims_payments + special_claims_results$special_claims_payments)
   small_claims_reserved <-
      all_claims_reserved - large_claims_results$large_claims_reserved - special_claims_results$special_claims_reserved
   small_annuities_paid <-
      if (is.null(all_annuities_paid)) {
         empty_triangle} else {
            all_annuities_paid - cut2history(large_claims_results$large_known_annuities_payments +
                                                         special_claims_results$special_annuities_payments)
         }

   if (!is.null(all_annuities_reserved)) {
      small_annuities_reserved <- all_annuities_reserved -
         large_claims_results$large_annuities_reserved -
         special_claims_results$special_annuities_reserved
   }


   ##### step 2: apply chain ladder for claims payments (without annuities)
   ### input triangles must be cumulative
   small_claims_paid_cum <- inc2cum(small_claims_paid)
   small_claims_incurred <- small_claims_paid_cum + small_claims_reserved

   ### generate small claims patterns for paid and incurred triangles via the generate_pattern-function.
   # Only the parameter triangle is mandatory. If other parameters are missing, the pattern will be derived from a standard
   # chain ladder without tail.
   # Parameter volume specifies how many last observations shall be included when calculating the development factors.
   # A given external_pattern will automatically used for the tail. It can also be used for earlier development factors
   # if the parameter int2ext_transition is specified. In particular, int2ext_transition must be set to 1 if only the
   # external pattern shall be used.

   paid_pattern <- generate_pattern(triangle = cal_year2dev_year(small_claims_paid_cum),
                                    volume = volume_paid_method,
                                    external_pattern = external_paid_pattern,
                                    int2ext_transition = int2ext_transition_paid)
   incurred_pattern <- generate_pattern(triangle = cal_year2dev_year(small_claims_incurred),
                                        volume = volume_incurred_method,
                                        external_pattern = external_incurred_pattern,
                                        int2ext_transition = int2ext_transition_incurred)


   # The calculated patterns are used to fill up the triangles.
   small_claims_paid_cum_filled <- fill_up_triangle(triangle = cal_year2dev_year(small_claims_paid_cum),
                                                             pattern = paid_pattern)
   small_claims_incurred_filled <- fill_up_triangle(triangle = cal_year2dev_year(small_claims_incurred),
                                                    pattern = incurred_pattern)


   # The results can be aggregated to a weighted average to get the ultimates.
   small_claims_result <- merge_results(paid_result = small_claims_paid_cum_filled,
                                        incurred_result = small_claims_incurred_filled,
                                        weight_paid = weight_paid,
                                        weight_incurred = weight_incurred)


   # Subtract cumulated payments from ultimates to get the best estimates.
   small_best_estimate_per_orig_year <- result2best_estimate(result = small_claims_result,
                                                             paid = cal_year2dev_year(small_claims_paid_cum))


   # Mit dem Zahlungsmuster (frei waehlbar) und dem Best Estimate wird der zukuenftige Cashflow erzeugt.
   # use paid pattern to roll out best estimate to a cashflow
   small_claims_prediction <- best_estimate2cashflow(best_estimate = small_best_estimate_per_orig_year,
                                                     pattern = paid_pattern,
                                                     last_orig_year = last_orig_year)


   # bind history and prediction
   small_claims_payments <- cbind(small_claims_paid, small_claims_prediction)



   ##### step 2: Roll out annuities stemming from small claims
   # Reduce dataframe active_annuities to small claims.
   small_active_annuities <- active_annuities[!(active_annuities$Claim_id %in% c(large_claims_list$Claim_id, special_claims$Claim_id)),]

   if (NROW(small_active_annuities) > 0) {
      # roll them out ...
      small_annuities_rolledout <- roll_out_annuity(payments = generate_annuity_payments(annuities = small_active_annuities,
                                                                                         last_orig_year = last_orig_year),
                                                    probs = generate_annuity_probabilities(annuities = small_active_annuities,
                                                                                           age_shift = age_shift,
                                                                                           mortality = mortality),
                                                    stop.at = "end_of_probs")


      # ... aggregate prediction to origin years
      small_known_annuities_prediction <- generate_annuities_prediction(annuities = small_active_annuities,
                                                                        annuities_rolled_out = small_annuities_rolledout,
                                                                        first_orig_year = first_orig_year,
                                                                        last_orig_year = last_orig_year)


      # bind history and prediction
      small_known_annuities_payments <- cbind(small_annuities_paid, small_known_annuities_prediction)
   }



   ##### step 3: Derive a best estimate for future annuities stemming from small claims
   # generate parameters for the method
   small_claims_future_annuities_params <-
      generate_small_claims_annuities_parameters(pool_of_annuities = pool_of_annuities,
                                                 large_claims_list = large_claims_list,
                                                 special_claims = special_claims,
                                                 indices = indices,
                                                 small_claims_reserved = small_claims_reserved,
                                                 age_shift,
                                                 mortality)


   # apply parameters
   small_future_annuities_payments <-
      generate_small_claims_future_annuities(small_claims_annuities_parameters_list = small_claims_future_annuities_params,
                                             small_claims_reserved = small_claims_reserved,
                                             indices = indices)


   # apply quota share reinsurance if existing
   # XL reinsurance is assumed to be 0 because the threshold should be chosen above xl priority.
   if (!is.null("reinsurance")) {
      small_ceded_quota_payments <- (small_claims_payments +
                                        small_known_annuities_payments +
                                        small_future_annuities_payments) *
         reinsurance$Quota_share[reinsurance$Origin_year %in% first_orig_year:last_orig_year]

   }

   return(list(small_claims_payments = small_claims_payments,
               small_claims_reserved = small_claims_reserved,
               small_known_annuities_payments = small_known_annuities_payments,
               small_annuities_reserved = small_annuities_reserved,
               small_future_annuities_payments = small_future_annuities_payments,
               small_ceded_quota_payments = small_ceded_quota_payments))

}


