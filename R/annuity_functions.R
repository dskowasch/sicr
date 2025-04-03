#' Create matrix of agreed future payments (without mortality probabilities)
#'
#' @description
#' Creates a matrix with one row per annuitiy and agreed future payments in columns. Mortality probabilities
#' are not taken into account here.
#'
#' @param annuities dataframe, see description of `pool_of_annuities` in details of [prepare_data()].
#' @param last_orig_year Integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year just ended.
#' @param indices dataframe for indexation, see details of [prepare_data()].
#' Only needed if at least one annuity's calendar_year is not equal to last_orig_year, in particular not needed for
#' rolling out active annuities. Default: Null
#' @param future_annuities TRUE if this function is used to calculate future annuities, FALSE for known annuities.
#' Default: FALSE
#'
#' @details
#' In order to reduce computing time when rolling out annuities, they are split into two matrices, one with agreed
#' future payments and one with the corresponding survival probabilities. This is the function for the first. \cr \cr
#' The function uses the entries of the columns `Annual_payment`, `Dynamic`, `Annuity_start` and `Annuity_end` to
#' create the deterministic agreed future payments. If `Calendar_year` is not equal to `last_orig_year`, the
#' payments are indexed to `last_orig_year` which is important when using annuity pools to predict future annuities.
#' In this case the dataframe `indices` is needed.
#'
#' @return dataframe with one row per row in 'annuities' and 130 columns for future possible payments
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' # Example without indexation
#' print(minimal_active_annuities_xmpl)
#'
#' # show only the first 10 columns
#' generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
#'                           last_orig_year = 2023)[,1:10]
#'
#' # Example with indexation
#' print(minimal_pool_of_annuities_xmpl)
#'
#' # show only the first 10 columns
#' generate_annuity_payments(annuities = minimal_pool_of_annuities_xmpl,
#'                           last_orig_year = 2023,
#'                           indices = indices_xmpl,
#'                           future_annuities = TRUE)[,1:10]
#' @importFrom utils tail
generate_annuity_payments <- function(annuities,
                                      last_orig_year,
                                      indices = NULL,
                                      future_annuities = FALSE){
   if (NROW(annuities) < 1) {return(NULL)}

   # Skeleton of annuity payments
   annuity_payments <- matrix(0, nrow = NROW(annuities), ncol = 130)

   # find parameters
   leading_zeros <- pmax(0, annuities$Annuity_start - annuities$Entering_year)
   lasting <- ifelse(annuities$Annuity_end == 0, 130, annuities$Annuity_end - annuities$Annuity_start)
   cut_first <- annuities$Calendar_year - annuities$Entering_year + 1
   # if future annuities are sampled from a pool of historic annuities, the start year has to be shifted by 1.
   # This way the payments in the annuities' entering year will also be considered, in contrast to known annuities
   # where payments from annuities' entering year have alrady been made.
   if (future_annuities) {cut_first <- cut_first - 1}

   # index annual payment if necessary
   if (any(annuities$Calendar_year != last_orig_year)) {
      indexed_payment_per_year <- apply_index(payments = annuities$Annual_payment,
                                              original_cal_years = annuities$Calendar_year,
                                              new_cal_years = last_orig_year,
                                              indices = indices)
   } else {
      indexed_payment_per_year <- annuities$Annual_payment
   }
   # loop through rows
   for (current_row in 1:NROW(annuity_payments)) {
      # build payments
      payments <- c(rep(0, leading_zeros[current_row]),
                    indexed_payment_per_year[current_row] *
                       (1 + annuities$Dynamic[current_row])^(0:(lasting[current_row])))
      if (cut_first[current_row] > 0) {payments <- tail(payments, -cut_first[current_row])}
      length(payments) <- 130
      payments[is.na(payments)] <- 0
      annuity_payments[current_row,] <- payments
   }

   return(annuity_payments)
}


#' Create matrix of future mortality probabilities
#'
#' @description
#' Creates a matrix with one row per annuitiy and mortality probabilities in columns.
#'
#' @param annuities dataframe, see description of `pool_of_annuities` in details of [prepare_data()].
#' @param mortality dataframe, see description of [mortality_xmpl].
#' @param age_shift dataframe, see description of [age_shift_xmpl].
#'
#' @details
#' In order to reduce computing time when rolling out annuities, they are split into two matrices, one with agreed
#' future payments and one with the corresponding survival probabilities. This is the function for the latter. \cr \cr
#' The function uses the biometric entries of the dataframe `annuities` to
#' create the matrix of the corresponding probabilities. \cr \cr
#' The first column is always 0 due to the assumption that annuities are paid in advance at the start of the year. \cr \cr
#'
#' @return dataframe with one row per row in 'annuities' and 130 columns for future mortality probabilities.
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' print(minimal_active_annuities_xmpl)
#'
#' # show only the first 10 columns
#' generate_annuity_probabilities(annuities = minimal_active_annuities_xmpl,
#'                                mortality = mortality_xmpl,
#'                                age_shift = age_shift_xmpl)[,1:10]
generate_annuity_probabilities <- function(annuities, mortality, age_shift) {

   # the following helper function selects the age shift for specific gender and birth year and handles possible errors
   f_age_shift <- function(gender, birth_year) {
      ### non-vectorized version
      non_vec_f <- function(gender, birth_year) {
         # An error may occur if birth_year is not found in age_shift$Birth_year.
         # Minimising to greatest and maximising to lowest birth year delivers a suitable age shift though.
         min_birth_year <- min(age_shift$Birth_year[age_shift$Gender == gender])
         max_birth_year <- max(age_shift$Birth_year[age_shift$Gender == gender])
         if (birth_year < min_birth_year) {
            birth_year <- min_birth_year
            warning("birth_year is smaller than every Birth_year in age_shift and is set to the minimum Birth_year")
         }
         if (birth_year > max_birth_year) {
            birth_year <- max_birth_year
            warning("birth_year is greater than every Birth_year in age_shift and is set to the maximum Birth_year")
         }

         age_shift$Age_shift[age_shift$Gender == gender & age_shift$Birth_year == birth_year]}

      ### vectorized version
      vec_f <- Vectorize(non_vec_f, vectorize.args = c("gender", "birth_year"))

      return(as.integer(vec_f(gender, birth_year)))
   }

   # An error may occur if the minimal age shift from age_shift is lower than the minimal age from mortality.
   # In this case the dataframe mortality is extended by the needed additional rows.
   # A warning shall indicate that the dataframe mortality should be completed.
   for (gender in c("m", "f")) {
      min_age <- min(mortality$Age[mortality$Gender == gender])
      min_age_shift <- min(age_shift$Age_shift[age_shift$Gender == gender])
      if (min_age > min_age_shift) {
         mortality <- rbind(mortality,
                            data.frame(Gender = gender,
                                       Age = min_age_shift:(min_age - 1),
                                       Probability = mortality$Probability[mortality$Gender == gender & mortality$Age == min_age]))
         warning(paste0("The minimal age shift in age_shift for gender ", gender, " is ", min_age_shift,
                        " but the minimal age with a probability in the dataframe 'mortality' is ", min_age,
                        ". This probability will be used for ages lower than ", min_age, "."))
      }
   }
   mortality <- mortality[order(mortality$Gender, mortality$Age), ]

   # non-vectorized function
   non_vec_f <- function(gender, age) {
      sp <- c(1, cumprod(1 - with(mortality, {Probability[Gender == gender & Age %in% (age:121)]})))
      length(sp) <- 130
      sp[is.na(sp)] <- 0
      return(sp)
   }

   # vectorized function
   vec_f <- Vectorize(non_vec_f, vectorize.args = c("gender", "age"), USE.NAMES = FALSE)

   gender <- ifelse(annuities$Gender == "w", "f", annuities$Gender) # Prevent error from german language
   age <- annuities$Calendar_year - annuities$Birth_year + f_age_shift(gender, annuities$Birth_year)
   return(1 - t(vec_f(gender, age)))
}


#' Rolling out one or more annuities
#'
#' @param payments numeric vector or matrix of agreed future payments with one row per annuity and 130 columns for future payments.
#' @param probs numeric vector or matrix with to `payments` corresponding mortality probabilities. Must be of same size as `payments`.
#' @param stop.at Chose between `random_death_year` (stochastic approach) and `end_of_probs` (deterministic approach).
#' The first samples a random death year of the annuity recipient and returns the agreed payments up to that year and 0 after.
#' The latter multiplies the payments with the corresponding mortality probabilities and returns a best estimate payment per year.
#'
#' @return numeric matrix of the same size as `payments` with future annuity payments.
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' print(minimal_active_annuities_xmpl)
#'
#' # Create payments matrix
#' payments <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
#'                                       last_orig_year = 2023)
#' print(payments[,1:10])
#'
#' # Create probability matrix
#' probs <- generate_annuity_probabilities(annuities = minimal_active_annuities_xmpl,
#'                                         mortality = mortality_xmpl,
#'                                         age_shift = age_shift_xmpl)
#' print(probs[,1:10])
#'
#' # stochastic with random death year
#' roll_out_annuity(payments, probs, stop.at = "random_death_year")
#'
#' # deterministic multiplying with mortality probabilities
#' roll_out_annuity(payments, probs, stop.at = "end_of_probs")
#' @importFrom stats runif
roll_out_annuity <- function(payments, probs, stop.at = "random_death_year"){
   if (stop.at == "random_death_year") {
      if (is.matrix(probs)) {
         # Generate random number of future survival years.
         n <- NROW(probs)
         random_numbers <- runif(n)
         survival_years <- max.col(random_numbers <= probs,
                                   ties.method = "first")
         return((col(matrix(nrow = n, ncol = 130)) < survival_years) * payments)
      } else {
         return((seq(130) < which.max(runif(1) <= probs)) * payments)
      }
   }

   if (stop.at == "end_of_probs") {
      return(payments * (1 - probs))
   }

   if (!stop.at %in% c("random_death_year", "end_of_probs")) {
      stop("stop.at must be 'random_death_year' or 'end_of_probs'")
   }
}


#' Rolling out active annuities corresponding to large claims
#'
#' @param payments_active_annuities matrix of agreed future payments corresponding to active annuities, see [generate_annuity_payments()].
#' @param probs_active_annuities matrix of mortality probabilities corresponding to active annuities, see [generate_annuity_probabilities()].
#' @param active_annuities Dataframe of active annuities, see [active_annuities_xmpl].
#' @param large_claims_list Dataframe of large claims generated with [generate_claims_list()].
#'
#' @return Numeric matrix with one row per large claim and 130 future annuity payments.
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' print(minimal_active_annuities_xmpl)
#'
#' # Create payments matrix
#' payments <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
#'                                       last_orig_year = 2023)
#' print(payments[,1:10])
#'
#' # Create probability matrix
#' probs <- generate_annuity_probabilities(annuities = minimal_active_annuities_xmpl,
#'                                         mortality = mortality_xmpl,
#'                                         age_shift = age_shift_xmpl)
#' print(probs[,1:10])
#'
#' # Create large claims list
#' extended_claims_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = minimal_pool_of_annuities_xmpl)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' roll_out_active_annuities(payments_active_annuities = payments,
#'                           probs_active_annuities = probs,
#'                           active_annuities = minimal_active_annuities_xmpl,
#'                           large_claims_list = large_claims_list)
roll_out_active_annuities <- function(payments_active_annuities,
                                      probs_active_annuities,
                                      active_annuities,
                                      large_claims_list){
   if (is.null(payments_active_annuities) | is.null(probs_active_annuities)) {
      return(matrix(0, nrow = NROW(large_claims_list), ncol = 130))}

   # roll out annuities
   cut_payments <- roll_out_annuity(payments = payments_active_annuities,
                                    probs = probs_active_annuities,
                                    stop.at = "random_death_year")

   # Sometimes one claim is assigned to more than one annuity. They have to be aggregated in that case.
   annuity_sum_per_claim <- rowsum(cut_payments, active_annuities$Claim_id)

   # Assigning the annuity sums to the claims
   result_matrix <- annuity_sum_per_claim[match(large_claims_list$Claim_id, rownames(annuity_sum_per_claim)),]

   # claims without assigned annuity will get NAs that must be set to 0.
   result_matrix[is.na(result_matrix)] <- 0

   # add rownames
   rownames(result_matrix) <- large_claims_list$Claim_id

   return(result_matrix)
}


#' Generate annuities prediction per development year from rolled out single annuities
#'
#' @param annuities dataframe, see description of `pool_of_annuities` in details of [prepare_data()].
#' @param annuities_rolled_out matrix as result of [roll_out_annuity()]
#' @param first_orig_year Integer value for the first origin year, will be treated as the first observed calendar year.
#' @param last_orig_year Integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year that just ended.
#'
#' @return numeric matrix with one row per year between `first_orig_year` and `last_orig_year` and 250 columns for
#' predicted future annuity payments.
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' print(minimal_active_annuities_xmpl)
#'
#' # Create payments matrix
#' payments <- generate_annuity_payments(annuities = minimal_active_annuities_xmpl,
#'                                       last_orig_year = 2023)
#' print(payments[,1:10])
#'
#' # Create probability matrix
#' probs <- generate_annuity_probabilities(annuities = minimal_active_annuities_xmpl,
#'                                         mortality = mortality_xmpl,
#'                                         age_shift = age_shift_xmpl)
#' print(probs[,1:10])
#'
#' annuities_rolled_out <- roll_out_annuity(payments = payments,
#'                                          probs = probs,
#'                                          stop.at = "random_death_year")
#'
#' # only print the first 10 columns
#' generate_annuities_prediction(annuities = minimal_active_annuities_xmpl,
#'                               annuities_rolled_out = annuities_rolled_out,
#'                               first_orig_year = 1989,
#'                               last_orig_year = 2023)[,1:10]
generate_annuities_prediction <- function(annuities,
                                          annuities_rolled_out,
                                          first_orig_year,
                                          last_orig_year) {
   orig_years <- first_orig_year:last_orig_year

   result_matrix <- skeleton_prediction(first_orig_year = first_orig_year,
                                        last_orig_year = last_orig_year)

   if (NROW(annuities) < 1) {return(result_matrix)}

   # iterate over annuities
   for (i in 1:NROW(annuities)) {
      claim_number <- annuities$Claim_id[i]
      year_indices <- match(annuities$Origin_year[annuities$Claim_id == claim_number], orig_years)[1]

      # if there is at least one row for this annuity, sum up the corresponding rows
      if (!is.na(year_indices)) {
         result_matrix[year_indices, ] <-
            result_matrix[year_indices, ] +
            c(annuities_rolled_out[i, ], rep(0, 120))
      }
   }

   return(result_matrix)
}

