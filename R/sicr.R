#' Simulation of large claims per stochastic individual claims reserving
#'
#' @description
#' This function is the main function of the sicr package. It uses the prepared objects to simulate
#' future payments for large claims.
#'
#'
#' @param n Integer. Desired number of realisations.
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#' @param first_orig_year First origin year with complete history.
#' @param last_orig_year Last origin year.
#' @param pools List as output of [generate_pools()].
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param history numeric matrix with one row per claim with historic payments,
#' generated by [generate_history_per_claim()]. \cr
#' If generated in other ways, note that colnames must be the origin years. \cr
#' This matrix is needed to calculate ibnr payments and reinsurance if desired.
#' @param exposure Dataframe that must contain one row for each origin year between `first_orig_year`
#' and `last_orig_year` and the columns `Origin_year` and `Exposure`. \cr
#' Default: NULL. In this case, a constant exposure is assumed.
#' @param years_for_ibnr_pools Vector containing the calendar years that shall be used to build the pools. \cr
#' Default: NULL. In this case, all years are used for building ibnr pools.
#' @param active_annuities Dataframe of active annuities, see [active_annuities_xmpl]. Default: NULL.\cr
#' Only necessary if annuities shall be considered.
#' @param age_shift Dataframe, see description of [age_shift_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param mortality Dataframe, see description of [mortality_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param reinsurance Dataframe with reinsurance information, see details of [xl_cashflow()] or [reinsurance_xmpl]. Default: NULL\cr
#' Only necessary if reinsurance shall be considered.
#' @param seed Default: 12345. Setting a seed makes the simulation reproducible.
#' @param progress Default: TRUE. Set to FALSE if no progress bar shall be shown during simulation.
#' @param summary Default: TRUE. Set to FALSE if no summary shall be shown after the simulation.
#' @param detailed_export Default: FALSE. Set to TRUE if the output shall contain more detailed technical information
#' about the simulation.
#'
#' @return
#' Numeric three-dimensional array with \cr
#' - one row per known claim followed by one row per expected ibnr claim
#' - 250 columns for future calendar years
#' - 7 (or 15 if `detailed_export` is true) matrices for the following information:
#'   1. Sum of gross payments (Gross_sum)
#'   2. Sum of reinsurance payments (Reinsurance_sum)
#'   3. Claim payments (without annuity payments) (Claim_payments)
#'   4. Payments for active annuities (Active_annuities_payments)
#'   5. Payments for expected future annuities (Future_annuities_payments)
#'   6. Reinsurance payments from xl treaties (Reinsurance_xl)
#'   7. Reinsurance payments from quota share treaties (Reinsurance_quota_share)
#'   8. (Reserve class (Reserve_class))
#'   9. (Development year (Dev_year))
#'   10. (Sampled future annuities index 1 (Future_annuities_index1))
#'   11. (Sampled future annuities index 2 (Future_annuities_index2))
#'   12. (Sampled future annuities index 3 (Future_annuities_index3))
#'   13. (Sampled future annuities index 4 (Future_annuities_index4))
#'   14. (Sampled future annuities index 5 (Future_annuities_index5))
#'   15. (Random sampling index (Random_sampling_index))
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
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2023)
#' # smallest version
#' result <- sicr(n = 5,
#'                large_claims_list = large_claims_list,
#'                first_orig_year = 1989,
#'                last_orig_year = 2023,
#'                pools = pools,
#'                indices = indices_xmpl,
#'                history = history)
#'
#' # more complex version
#' result <- sicr(n = 5,
#'                large_claims_list = large_claims_list,
#'                first_orig_year = 1989,
#'                last_orig_year = 2023,
#'                pools = pools,
#'                indices = indices_xmpl,
#'                history = history,
#'                exposure = exposure_xmpl,
#'                years_for_ibnr_pools = 2014:2023,
#'                active_annuities = active_annuities_xmpl,
#'                age_shift = age_shift_xmpl,
#'                mortality = mortality_xmpl,
#'                reinsurance = reinsurance_xmpl)
#' @importFrom stats qnorm sd
#' @importFrom utils flush.console
sicr <- function(n,
                 large_claims_list,
                 first_orig_year,
                 last_orig_year,
                 pools,
                 indices,
                 history,
                 exposure = NULL,
                 years_for_ibnr_pools = NULL,
                 active_annuities = NULL,
                 age_shift = NULL,
                 mortality = NULL,
                 reinsurance = NULL,
                 seed = 12345,
                 progress = TRUE,
                 summary = TRUE,
                 detailed_export = FALSE) {


   ##### ---------- INITIATE NECESSARY OBJECTS - Part 1 ---------- #####
   # ----- Number of known claims and dev years -----
   claims_count <- NROW(large_claims_list)
   ibnr_count <- 0
   hist_dev_years_count <- NCOL(history)
   dev_years_count <- hist_dev_years_count + 250


   # ----- Transform history into a matrix to prevent errors
   history <- rbind(history) # NULL remains NULL, matrix remains matrix, vector becomes matrix


   # ----- pools as matrix for faster simulation -----
   start_of_tail <- attr(pools, "start_of_tail")
   end_of_tail <- attr(pools, "end_of_tail")
   number_of_reserve_classes <- length(attr(pools, "reserve_classes")) + 1
   pools_as_matrix <- pools2matrix(pools)


   # ----- extract pool_of_annuities from pools list
   pool_of_annuities <- pools[[3]]

   # ----- create ibnr list and ibnr pools
   ##### Spätgroßschäden #####
   ibnr <- prepare_ibnr(large_claims_list = large_claims_list,
                        first_orig_year = first_orig_year,
                        last_orig_year = last_orig_year,
                        exposure = exposure,
                        years_for_ibnr_pools = years_for_ibnr_pools)

   ibnr_list <- ibnr[[1]]
   ibnr_pools <- ibnr[[2]]
   rm(ibnr)

   ##### ---------- INITIATE ADDITIONAL FUNCTIONS ---------- #####
   sample_next_year <- function(classes_vector_before, dev_year_vector_before) {
      # The pools have been transitioned to a matrix, so the segment of the matrix that belongs to each
      # pool has to be identified.
      # First the starting point of the pool...
      start_of_pool_in_matrix <- get_matrix_info(dev_year = dev_year_vector_before + 1,
                                                 reserve_class = classes_vector_before,
                                                 start_of_tail = start_of_tail,
                                                 number_of_reserve_classes = number_of_reserve_classes,
                                                 lengths_vector = pools_as_matrix[[1]],
                                                 output = "start")
      # then the number of historic observations in the pool.
      sample_sizes <- get_matrix_info(dev_year = dev_year_vector_before + 1,
                                      reserve_class = classes_vector_before,
                                      start_of_tail = start_of_tail,
                                      number_of_reserve_classes = number_of_reserve_classes,
                                      lengths_vector = pools_as_matrix[[1]],
                                      output = "sample_size")
      # For each claim a random row of the pool is sampled:
      random_row <- floor(runif(claims_count,
                                1,
                                sample_sizes + 1))

      # initiate result matrix
      result <- matrix(0, nrow = claims_count, ncol = 8)

      # determine claims that have not yet reached the end of tail
      active_claims <- which(dev_year_vector_before + 1 <= end_of_tail)
      result[active_claims, 1:7] <- pools_as_matrix[[2]][start_of_pool_in_matrix[active_claims] - 1 + random_row[active_claims],]
      result[,1] <- result[,1] * tail_factor(dev_year = dev_year_vector_before + 1,
                                             start_of_tail = start_of_tail,
                                             end_of_tail = end_of_tail)
      result[active_claims, 8] <- random_row[active_claims]

      return(result)
   }


   sample_ibnr <- function(gross_result_known_claims) {
      ### initialize functions
      # this function returns the payments vector of a chosen historic ibnr claim (history and sampled future)
      non_vec_history <- function(shift, index) {
         return(c(rep(0, shift),
                  history[index,],
                  gross_result_known_claims[index,])[seq(dev_years_count)])
      }

      # Vectorized version
      vec_history <- Vectorize(non_vec_history,
                               vectorize.args = c("shift", "index"))


      # sample indices of historic claims which became large at the same development year as in ibnr_list
      ibnr_indices <- sapply(ibnr_list$Dev_year_of_growing_large,
                             function(year) {
                                sample_1(ibnr_pools[[year - 1]], 1)})


      # generating ibnr payments
      shift <- ibnr_list$Origin_year - large_claims_list$Origin_year[ibnr_indices]
      ibnr_payments <- t(vec_history(shift = shift,
                                     index = ibnr_indices))
      rownames(ibnr_payments) <- paste0("IBNR", 1:NROW(ibnr_list))
      colnames(ibnr_payments) <- first_orig_year:(last_orig_year + 250)


      # indexating nonzero matrix cells
      nonzero <- which(ibnr_payments != 0, arr.ind = TRUE)
      new_cal_years <- first_orig_year - 1 + nonzero[,2]
      original_cal_years <- new_cal_years - shift[nonzero[,1]]
      ibnr_payments[nonzero] <- apply_index(payments = ibnr_payments[nonzero],
                                            original_cal_years = original_cal_years,
                                            new_cal_years = new_cal_years,
                                            indices = indices)

      # setting all payments as small claim to 0
      leading_zeros <- ibnr_list$Origin_year - first_orig_year + ibnr_list$Dev_year_of_growing_large - 1
      ibnr_large_claim_payments <- ibnr_payments
      for (current_row in seq(nrow(ibnr_large_claim_payments))) {
         ibnr_large_claim_payments[current_row, seq(leading_zeros[current_row])] <- 0
      }
      ibnr_large_claim_payments <- t(tail(t(ibnr_large_claim_payments), 250))


      # generating xl reinsurance share
      if (include_reinsurance) {
         ibnr_reinsurance <- xl_cashflow(future_payments = t(tail(t(ibnr_payments), 250)),
                                         # instead of a claims_list, only Origin_year and Claim_id is needed
                                         claims_list = data.frame(Origin_year = ibnr_list$Origin_year,
                                                                  Claim_id = rownames(ibnr_payments)),
                                         history = ibnr_payments[, seq(hist_dev_years_count), drop = FALSE],
                                         reinsurance = reinsurance,
                                         indices = indices)
      } else ibnr_reinsurance <- matrix(0, nrow(ibnr_list), 250)

      return(list("ibnr_large_claim_payments" = ibnr_large_claim_payments,
                  "ibnr_reinsurance" = ibnr_reinsurance))
   }


   # Confidence interval
   se <- function(results, alpha = 0.05, reinsurance = FALSE) {
      len <- length(results)
      mean_result <-  mean(results)
      standard_error <- sd(results) / sqrt(len)
      interval <- qnorm(1 - alpha/2) * standard_error
      return(paste0("   (standard error",
                    if (reinsurance) {" (only XL)"},
                    ": ",
                    if (all(mean_result != 0, length(results) > 1)) {
                       paste0(nice_format(standard_error / mean_result * 100), "%")
                    } else {
                       "-"
                    },
                    ")"))
   }


   ##### check for extra calculations #####
   include_active_annuities <- !any(is.null(active_annuities),
                                    is.null(age_shift),
                                    is.null(mortality))

   include_future_annuities <- !any(is.null(pool_of_annuities),
                                    is.null(age_shift),
                                    is.null(mortality))

   include_ibnr <- NROW(ibnr_list) != 0

   include_reinsurance <- !any(is.null(reinsurance),
                               is.null(history))


   ##### check data #####
   # active annuities
   if (!active_annuities_ok(active_annuities, last_orig_year)) {
      stop("Dataframe active_annuities is not valid.")
   }


   # pool of annuities
   if (!pool_of_annuities_ok(pool_of_annuities)) {
      stop("Dataframe pool_of_annuities is not valid.")
   }


   # mortality
   if (!mortality_ok(mortality)) {
      stop("Dataframe mortality is not valid.")
   }


   # age_shift
   if (!age_shift_ok(age_shift)) {
      stop("Dataframe age_shift is not valid.")
   }


   # exposure
   if (!exposure_ok(exposure, first_orig_year, last_orig_year)) {
      stop("Dataframe exposure is not valid.")
   }


   # indices
   if (!indices_ok(indices, first_orig_year, last_orig_year)) {
      stop("Dataframe indices is not valid.")
   }


   # reinsurance
   if (!reinsurance_ok(reinsurance, indices, first_orig_year, last_orig_year)) {
      stop("Dataframe reinsurance is not valid.")
   }


   # check if pools' attributes correspond to large_claims_list's attributes
   if (!all(attr(pools, "index_year") == attr(large_claims_list, "index_year"),
            attr(pools, "reserve_classes") == attr(large_claims_list, "reserve_classes"))) {
      warning("One of the attributes index_year or reserve_classes doesn't match between pools and large_claims_list.
              Maybe the pools were generated with different parameters.
              This may lead to wrong results.")
   }

   set.seed(seed)


   ##### ---------- INITIATE NECESSARY OBJECTS ---------- #####

   # ----- Arrays to save sum over realisations -----
   known_claims_array <- array(data = 0,
                               dim = c(claims_count, 250, 12))
   reinsurance_xl_array <- array(data = 0,
                                 dim = c(claims_count, 250))

   if (include_ibnr) {
      ibnr_count <- NROW(ibnr_list)
      ibnr_array <- ibnr_reinsurance_xl_array <- array(data = 0,
                                                       dim = c(ibnr_count, 250))}

   # ----- start array -----
   # start_array is the starting point for the simulation with Exit_reserve_class and Dev_year_since_large
   # as first two columns. These can be used to sample the next year for each claim. start_array is filled up
   # with these information and the new starting point with next development year and new exit reserve class
   # will be put into column 2.
   start_array <- array(0, dim = c(claims_count, 250, 12))
   start_array[, 1, 1] <- large_claims_list$Exit_reserve_class
   start_array[, 1, 2] <- large_claims_list$Dev_year_since_large


   # ----- results vector to calculate standard error and confidence interval -----
   gross_results <- c()
   reinsurance_results <- c()

   # ----- objects for active annuities -----
   if (include_active_annuities) {
      # filter active annuities to those belonging to large claims
      active_annuities <- active_annuities[active_annuities$Claim_id %in% large_claims_list$Claim_id,]

      # if no annuity is left, active annuities are excluded from this procedure
      if (NROW(active_annuities) == 0) {include_active_annuities <- FALSE} else {
         # generate the necessary objects
         active_annuities_payments <- generate_annuity_payments(annuities = active_annuities,
                                                                last_orig_year = last_orig_year)
         active_annuities_probabilities <- generate_annuity_probabilities(annuities = active_annuities,
                                                                          age_shift = age_shift,
                                                                          mortality = mortality)
      }
   }

   # ----- objects for future annuities -----
   if (include_future_annuities) {
      # generate objects for the calculation of future annuities
      future_annuities_payments <- generate_annuity_payments(annuities = pool_of_annuities,
                                                             last_orig_year = last_orig_year,
                                                             indices = indices,
                                                             future_annuities = TRUE)
      future_annuities_probabilities <- generate_annuity_probabilities(annuities = pool_of_annuities,
                                                                       age_shift = age_shift,
                                                                       mortality = mortality)
   }


   # ----- objects for reinsurance -----
   if (include_reinsurance) {
      xl_indices_per_claim <- generate_xl_indices_per_claim(claims_list = large_claims_list,
                                                            reinsurance = reinsurance,
                                                            indices = indices,
                                                            first_orig_year = first_orig_year,
                                                            last_orig_year = last_orig_year)
   }


   # ----- progress bar and time stop -----
   if (progress) {start_time <- Sys.time()}


   ##### ---------- SAMPLING ---------- #####
   for (realisation in seq(n)) {
      # each realisation starts with the start array
      known_claims_realisation <- start_array

      # show progress bar
      if (progress) {
         progress_percentage <- realisation / n * 100
         duration <- Sys.time() - start_time
         cat("\r", " ", realisation, " of ", n, " (", progress_percentage, "%", ")",
             " --- remaining time approx. " ,
             nice_format(duration / (realisation - 1) * (n - realisation + 1)), "          "
             , sep = "")
         flush.console()
         Sys.sleep(0.1)
      }


      # fill up result year by year
      for (year in seq(end_of_tail)) {
         known_claims_one_year <- sample_next_year(classes_vector_before = known_claims_realisation[, year, 1],
                                                   dev_year_vector_before = known_claims_realisation[, year, 2])

         # transfer result of the next year to result array
         known_claims_realisation[, year, 3:9] <- known_claims_one_year[, c(1, 3:8)] # cl_payments and new annuities 1 to 5
         known_claims_realisation[, year + 1, 2] <- known_claims_realisation[, year, 2] + 1 # new development year in next column
         known_claims_realisation[, year + 1, 1] <- known_claims_one_year[, 2] # new reserve class in next column
         # cl_payments must be indexed:
         known_claims_realisation[, year, 3] <- known_claims_realisation[, year, 3] * tail(indices$Transition_factor, 250)[year]
      }


      # roll out active annuities
      if (include_active_annuities) {
         known_claims_realisation[ , 1:130, 10] <- roll_out_active_annuities(payments_active_annuities = active_annuities_payments,
                                                                             probs_active_annuities = active_annuities_probabilities,
                                                                             active_annuities = active_annuities,
                                                                             large_claims_list = large_claims_list)
      }


      # roll out future annuities
      if (include_future_annuities) {
         future_annuities_indices <- known_claims_realisation[,,4:8, drop = FALSE]
         array <- rbind(known_claims_realisation[,,9])
         positive_indices <- which(future_annuities_indices > 0, arr.ind = TRUE)
         if (NROW(positive_indices) > 0) {
            for (i in 1:NROW(positive_indices)) {
               current_row <- positive_indices[i,1]
               current_column <- positive_indices[i,2]
               current_matrix <- positive_indices[i,3]
               index <- future_annuities_indices[current_row, current_column, current_matrix]
               known_claims_realisation[current_row, current_column:(current_column + 129), 11] <-
                  known_claims_realisation[current_row, current_column:(current_column + 129), 11] +
                  # the future_annuity_payments have been indexed to last_orig_year and must now be
                  # indexed to the future year when the annuity is built
                  apply_index(roll_out_annuity(future_annuities_payments[index,],
                                               future_annuities_probabilities[index,]),
                              last_orig_year,
                              last_orig_year + current_column,
                              indices) *
                  # in case of a new annuity during tail, the payments have to be reduced...
                  tail_factor(dev_year = large_claims_list$Dev_year_since_large[current_row] + current_column,
                              start_of_tail,
                              end_of_tail)
            }
         }
      }


      # sum up claims' and annuities' payments
      known_claims_realisation[ , , 12] <-
         known_claims_realisation[ , , 3] +
         known_claims_realisation[ , , 10] +
         known_claims_realisation[ , , 11]


      # add result to known_claims_array
      known_claims_array <- known_claims_array + known_claims_realisation


      # predict reinsurance xl share
      if (include_reinsurance) {
         reinsurance_xl_realisation <- xl_cashflow(future_payments = rbind(known_claims_realisation[,,12]),
                                                   claims_list = large_claims_list,
                                                   history = history,
                                                   reinsurance = reinsurance,
                                                   xl_indices_per_claim = xl_indices_per_claim,
                                                   output = "future")
         reinsurance_xl_array <- reinsurance_xl_array + reinsurance_xl_realisation
      }


      ### predict ibnr
      if (include_ibnr) {
         ibnr_realisation <- sample_ibnr(rbind(known_claims_realisation[,,12]))
         ibnr_array <- ibnr_array + ibnr_realisation[[1]]
         if (include_reinsurance) {ibnr_reinsurance_xl_array <- ibnr_reinsurance_xl_array + ibnr_realisation[[2]]}
      }


      ### add results to vectors for confidence intervals
      gross_results <- c(gross_results,
                         sum(known_claims_realisation[ , , 12]) +
                            if (include_ibnr) {sum(ibnr_realisation[[1]])} else {0})
      if (include_reinsurance) {
         reinsurance_results <- c(reinsurance_results,
                                  sum(reinsurance_xl_realisation) +
                                     if (include_ibnr) {sum(ibnr_realisation[[2]])} else {0})}
   } ##### ----- END OF SAMPLING ----- #####


   # calculate mean over all realisations
   known_claims_array <- known_claims_array / n
   if (include_reinsurance) {reinsurance_xl_array <- reinsurance_xl_array / n}
   if (include_ibnr) {
      ibnr_array <- ibnr_array / n
      ibnr_reinsurance_xl_array <- ibnr_reinsurance_xl_array / n}


   ##### ----- GENERATE OUTPUT ----- #####
   output <- array(data = 0,
                   dim = c(claims_count + ibnr_count,
                           250,
                           15),
                   dimnames = list(c(large_claims_list$Claim_id,
                                     if(include_ibnr) {paste0("IBNR", 1:ibnr_count)} else {c()}),
                                   (last_orig_year + 1):(last_orig_year + 250),
                                   c("Gross_sum",
                                     "Reinsurance_sum",
                                     "Claim_payments",
                                     "Active_annuities_payments",
                                     "Future_annuities_payments",
                                     "Reinsurance_xl",
                                     "Reinsurance_quota_share",
                                     "Reserve_class",
                                     "Dev_year",
                                     "Future_annuities_index1",
                                     "Future_annuities_index2",
                                     "Future_annuities_index3",
                                     "Future_annuities_index4",
                                     "Future_annuities_index5",
                                     "Random_sampling_index")))

   # Output is an array containing the following matrices:
   #  1. gross sum
   #  2. reinsurance sum
   #  3. claim payments
   #  4. active annuities' payments
   #  5. future annuities' payments
   #  6. reinsurance xl
   #  7. reinsurance quota share
   # from here on the arrays are only returned if detailled output has been chosen
   #  8. reserve classes
   #  9. development years
   #  10. future annuities indices 1
   #  11. future annuities indices 2
   #  12. future annuities indices 3
   #  13. future annuities indices 4
   #  14. future annuities indices 5
   #  15. random index of the sample matrix

   #  Each matrix has one row per active claim and below that one row per expected ibnr claim named IBNR1...IBNRn
   #  Each matrix has 250 columns for future years
   output[1:claims_count, , c(1, 3:5, 8:15)] <- known_claims_array[ , , c(12, 3, 10:11, 1:2, 4:9)]
   if (include_ibnr) {
      ibnr_rows <- seq(claims_count + 1, claims_count + ibnr_count)
      output[ibnr_rows, , 1] <- ibnr_array
      if (include_reinsurance) {
         output[ibnr_rows, , 6] <- ibnr_reinsurance_xl_array
         output[ibnr_rows, , 7] <- output[ibnr_rows, , 1] *
            reinsurance$Quota_share[match(ibnr_list$Origin_year, reinsurance$Origin_year)]
      }
   }
   output[1:claims_count, , 6] <- reinsurance_xl_array
   if (include_reinsurance) {
      output[1:claims_count, , 7] <- output[1:claims_count, , 1] *
         reinsurance$Quota_share[match(large_claims_list$Origin_year, reinsurance$Origin_year)]
   }
   output[, , 2] <- output[, , 6] + output[, , 7]


   ##### ---- WRITE SUMMARY TO CONSOLE ----- #####
   # Simulation time
   if (summary) {
      if (progress) {cat(paste0(" \n \n Simulation time: ", nice_format(Sys.time() - start_time)))}
      cat(paste0("\n \n Gross Best Estimate: ",
                 nice_format(sum(output[,, "Gross_sum"])), se(gross_results),
                 "\n           ...Known claims ",
                 nice_format(sum(output[1:claims_count, , "Gross_sum"]))))

      if (any(include_active_annuities, include_future_annuities)) {
         cat(paste0(
            "\n                     ...Claims ",
            nice_format(sum(output[1:claims_count, , "Claim_payments"])),
            "\n                     ...Active annuities ",
            nice_format(sum(output[1:claims_count, , "Active_annuities_payments"])),
            "\n                     ...Future annuities ",
            nice_format(sum(output[1:claims_count, , "Future_annuities_payments"]))
         ))
      }

      cat(paste0(
         "\n           ...IBNR ",
         nice_format(if (include_ibnr) {sum(output[ibnr_rows, , "Gross_sum"])} else {0})
      ))

      if (include_reinsurance) {
         cat(paste0(
            "\n \n Reinsurance Best Estimate: ",
            nice_format(sum(output[,, "Reinsurance_sum"])), se(reinsurance_results, reinsurance = TRUE),
            "\n           ...Known Claims ",
            nice_format(sum(output[1:claims_count, , "Reinsurance_sum"])),
            "\n                     ...XL ",
            nice_format(sum(output[1:claims_count, , "Reinsurance_xl"])),
            "\n                     ...Quota Share ",
            nice_format(sum(output[1:claims_count, , "Reinsurance_quota_share"])),
            "\n           ...IBNR ",
            nice_format(if (include_ibnr) {sum(output[ibnr_rows, , "Reinsurance_sum"])} else {0}),
            "\n                     ...XL ",
            nice_format(if (include_ibnr) {sum(output[ibnr_rows, , "Reinsurance_xl"])} else {0}),
            "\n                     ...Quota Share ",
            nice_format(if (include_ibnr) {sum(output[ibnr_rows, , "Reinsurance_quota_share"])} else {0})
         ))
      }
   }

   # the matrices 8 to 15 of the output array help understanding the results in detail but may be
   # confusing for standard users, so they are not returned by default
   if (!detailed_export) output <- output[,,1:7, drop = FALSE]
   attr(output, "ibnr_list") <- ibnr_list
   return(output)

}


