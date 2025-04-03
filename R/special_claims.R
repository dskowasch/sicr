#' Converting a reserve into a cashflow by given patterns
#'
#' @description
#' This is a helper function for the function [compute_special_claims()]. It is designed for the purpose of
#' rolling out a reserve of a special claim either by a given external pattern or linear or constant.
#'
#' @param reserve Numeric value of the reserve to be rolled out.
#' @param years Integer value stating how many future years shall be considered. Default: 1 \cr
#' This parameter is not used if `type` is set to `external`.
#' @param type Desired way of rolling out reserve. Must be one of `linear`, `constant` or `external`. Default: `linear`.\cr
#' In case of `linear` or `constant` the parameter `years` will be used. `external` requires the parameter `external_pattern`.
#' @param external_pattern Numeric vector with future payments as a percentage of `reserve`. Sum must be 1.
#'
#' @return Numeric vector of length 250 with future payments.
#' @export
#'
#' @examples
#' # Default type linear and years 1
#' roll_out_single_claim(1000)
#' roll_out_single_claim(1000, 5)
#'
#' # constant
#' roll_out_single_claim(1000, 5, "constant")
#'
#' # external_pattern
#' roll_out_single_claim(1000, type = "external", external_pattern = c(0.3, 0.5, 0.1, 0.1, 0, 0))
#'
roll_out_single_claim <- function(reserve, years = 1, type = "linear", external_pattern = NULL) {
   if (!type %in% c("linear", "constant", "external")) {
      art <- "linear"
      warning("The parameter 'type' must be 'linear', 'constant' or 'external', will be set to 'lineaer'.")
   }

   rolled_out_reserve <- rep(0, 250)

   if (type == "external") {
      if (is.null(external_pattern) |
          !is.numeric(external_pattern) |
          !is.vector(external_pattern) |
          length(external_pattern) > 250) {
         warning("external_pattern not usable, 'type' will be set to 'linear'.")
         type <- "linear"
      } else {
         if (abs(1 - sum(external_pattern)) > 0.05) {
            warning("Sum of external_pattern is not near 1, so the cashflow will not add up to reserve.")
         }
         rolled_out_reserve[1:length(external_pattern)] <- reserve * external_pattern
      }
   }

   if (type == "linear") {
      if (!years %in% 1:250) {
         warning("Parameter 'years' must be an integer between 1 and 250, will be set to 1 here.")
         years <- 1
      }

      rolled_out_reserve[1:years] <-
         reserve *
         ((years:1) / years) /
         sum((years:1) / years)
   }

   if (type == "constant") {
      if (!years %in% 1:250) {
         warning("Parameter 'years' must be an integer between 1 and 250, will be set to 1 here.")
         years <- 1
      }
      rolled_out_reserve[1:years] <- reserve/years
   }

   return(rolled_out_reserve)
}


#' Compute everything belonging to the chosen special claims
#'
#' @description
#' This function computes every needed information about the chosen special claims in one step.
#'
#'
#'
#' @param special_claims Dataframe with special claims, see details.
#' @param extended_claims_data Prepared claims data dataframe, see details of [prepare_data()]. \cr It is not necessary to
#' reduce this dataframe to special claims, this will be done by the function.
#' @param first_orig_year Integer value for the first origin year, will be treated as the first observed calendar year.
#' @param last_orig_year Integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year that just ended.
#' @param end_of_tail Chosen development year of tail end.
#' @param external_patterns List of external patterns that can be referred to in `special_claims`, see details. Default: NULL\cr
#' Only necessary if special_claims$type is set to external.
#' @param active_annuities Dataframe of active annuities, see [active_annuities_xmpl]. Default: NULL.\cr
#' Only necessary if annuities shall be considered.
#' @param age_shift Dataframe, see description of [age_shift_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param mortality Dataframe, see description of [mortality_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param reinsurance Dataframe with reinsurance information, see details of [xl_cashflow()] or [reinsurance_xmpl]. Default: NULL\cr
#' Only necessary if reinsurance shall be considered.
#' @param indices Dataframe for indexation, see details of [prepare_data()]. Default: NULL\cr
#' Only necessary if reinsurance shall be considered.
#'
#' @details
#' Some claims differ to historic claims so much that they cannot be reserved with the available pools. This mostly
#' applies to very large claims that have not been observed yet hence there is no adequate historic data of similar
#' claims in the pools. These claims must be separated and projected deterministically. \cr \cr
#' This package comes with some graphic options to identify special claims. After this process, the dataframe
#' `special_claims` must be created with one row per special claim and the following columns:
#'    + `Claim_id` \cr type: character. Unique claim id.
#'    + `Reserve2BE_percentage` \cr type: numeric. This number will be multiplied with the latest claim reserve to calculate
#'    the best estimate reserve, e.g. 0.8 if 80% of the reserve will be needed for future payments.
#'    + `Rollout_type` \cr type: character. One of `linear`, `constant` or `external`.
#'       + `linear` means that the payments will decrease evenly and reach 0 at `end_of_tail`. The sum is the desired best estimate.
#'       + `constant` means equal payments until `end_of_tail` that add to the desired best estimate.
#'       + `external` means that the best estimate is rolled out by an external pattern. The patterns must be specified
#'       in the list `external_patterns`. If `external` is chosen, the column `Pattern_id` is used.
#'    + `Pattern_id` type: integer. The number of the element of list `external_patterns` where the desired
#'    external pattern is specified.
#'
#' The list `external_patterns` specifies the external patterns that special_claims$Pattern_id refers to. Hence this list
#' must at least contain `max(special_claims$Pattern_id)` elements. \cr
#' Each element is a numeric vector with future payments as a percentage of the best estimate reserve. Sum of vector must be 1.
#'
#'
#' @return List of matrices. Each matrix consists of one row per year between `first_orig_year` and `last_orig_year` but the count of
#' columns varies (with n be the number of origin years): \cr
#' 1. `special_claims_payments` with historic and future claim payments (250 + n columns)
#' 2. `special_claims_reserved` with historic claims reserves (n columns)
#' 3. `special_annuities_payments` with historic and future annuity payments (250 + n columns)
#' 4. `special_annuities_reserved` with historic annuities reserves (n columns)
#' 5. `special_ceded_quota_payments` with historic and future payments from the quota share reinsurance contract (250 + n columns)
#' 6. `special_ceded_xl_payments` with historic and future payments from the xl reinsurance contract (250 + n columns)
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' extended_claims_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' # The claims with claim_ids Claim#43, Claim#44 and Claim#68 are identified as special claims.
#' special_claims <- data.frame(Claim_id = c("Claim#43", "Claim#44", "Claim#68"),
#'                              Reserve2BE_percentage = c(0.8, 0.5, 1.2),
#'                              Rollout_type = c("linear", "constant", "external"),
#'                              Pattern_id = c(1,1,3))
#' print(special_claims)
#'
#' external_patterns <- list(c(0.5, 0.3, 0.2),
#'                           c(1),
#'                           c(0.7, 0.3))
#'print(external_patterns)
#'
#' # First example ignores annuities and reinsurance.
#' result <- compute_special_claims(special_claims = special_claims,
#'                                  extended_claims_data = extended_claims_data,
#'                                  first_orig_year = 1989,
#'                                  last_orig_year = 2023,
#'                                  end_of_tail = 50,
#'                                  external_patterns = external_patterns)
#' print(str(result))
#' print(result$special_claims_payments[, c("2024", "2025", "2026", "2027", "2028", "2029")])
#'
#' # Second example also computes annuities for claim#43
#' result <- compute_special_claims(special_claims = special_claims,
#'                                  extended_claims_data = extended_claims_data,
#'                                  first_orig_year = 1989,
#'                                  last_orig_year = 2023,
#'                                  end_of_tail = 50,
#'                                  external_patterns = external_patterns,
#'                                  active_annuities = minimal_active_annuities_xmpl,
#'                                  age_shift = age_shift_xmpl,
#'                                  mortality = mortality_xmpl)
#' print(result$special_annuities_payments[, c("2024", "2025", "2026", "2027", "2028", "2029")])
#'
#' # Third example also computes reinsurance
#' # Priority in 2019 is set to 100.000 to produce a positive xl portion
#' reinsurance_xmpl[reinsurance_xmpl$Origin_year == 2019, "Priority"] <- 100000
#'
#' result <- compute_special_claims(special_claims = special_claims,
#'                                  extended_claims_data = extended_claims_data,
#'                                  first_orig_year = 1989,
#'                                  last_orig_year = 2023,
#'                                  end_of_tail = 50,
#'                                  external_patterns = external_patterns,
#'                                  active_annuities = minimal_active_annuities_xmpl,
#'                                  age_shift = age_shift_xmpl,
#'                                  mortality = mortality_xmpl,
#'                                  reinsurance = reinsurance_xmpl,
#'                                  indices = indices_xmpl)
#' print(result$special_ceded_xl_payments[, c("2024", "2025", "2026", "2027", "2028", "2029")])
#' print(result$special_ceded_quota_payments[, c("2024", "2025", "2026", "2027", "2028", "2029")])

compute_special_claims <- function(special_claims,
                                   extended_claims_data,
                                   first_orig_year,
                                   last_orig_year,
                                   end_of_tail,
                                   external_patterns = NULL,
                                   active_annuities = NULL,
                                   age_shift = NULL,
                                   mortality = NULL,
                                   reinsurance = NULL,
                                   indices = NULL) {

   # initiate dummys
   empty_triangle <- skeleton_triangle(first_orig_year, last_orig_year)
   empty_prediction <- skeleton_prediction(first_orig_year, last_orig_year)
   empty_total <- cbind(empty_triangle, empty_prediction)

   # reduce extended_claims_data to special claim rows and delete claims outside of origin year range.
   extended_special_claims_data <- extended_claims_data[extended_claims_data$Claim_id %in% special_claims$Claim_id &
                                                           extended_claims_data$Origin_year %in% first_orig_year:last_orig_year,]

   # reduce special_claims to actual existing claims, so that non existent claim numbers are filtered out
   # This could for example be the case if the Claim_id has changed which was not considered in special_claims dataframe.
   special_claims <- special_claims[special_claims$Claim_id %in% extended_special_claims_data$Claim_id,]

   # stop if no special claims are left
   if (NROW(special_claims) < 1) {return(list(special_claims_payments = empty_total,
                                              special_claims_reserved = empty_triangle,
                                              special_annuities_payments = empty_total,
                                              special_annuities_reserved = empty_triangle,
                                              special_ceded_quota_payments = empty_total,
                                              special_ceded_xl_payments = empty_total))}

   # Shall the calculation include annuities and reinsurance?
   if (any(is.null(active_annuities),
           is.null(age_shift),
           is.null(mortality))) {with_annuities <- FALSE } else {with_annuities <- TRUE}
   if (any(is.null(reinsurance),
           is.null(indices))) {with_reinsurance <- FALSE } else {with_reinsurance <- TRUE}


   # generate reserves
   special_claims_reserved <- generate_triangle(extended_special_claims_data,
                                                "Cl_reserve",
                                                first_orig_year,
                                                last_orig_year)
   special_annuities_reserved <- generate_triangle(extended_special_claims_data,
                                                   "An_reserve",
                                                   first_orig_year,
                                                   last_orig_year)

   # generate paid triangles
   special_claims_paid <- generate_triangle(extended_special_claims_data,
                                                     "Cl_payment_cal",
                                                     first_orig_year,
                                                     last_orig_year)

   special_annuities_paid <- generate_triangle(extended_special_claims_data,
                                                        "An_payment_cal",
                                                        first_orig_year,
                                                        last_orig_year)

   # Initiate matrices skeletons
   claims_prediction <- annuities_prediction <- empty_prediction
   special_ceded_xl_payments <- special_ceded_quota_payments <- empty_total

   # Generate claims list dataframe
   claims_list_special_claims <- generate_claims_list(extended_claims_data = extended_special_claims_data,
                                                      last_orig_year = last_orig_year,
                                                      first_orig_year = first_orig_year)

   # generate single claims history to apply reinsurance
   history_claims = generate_history_per_claim(extended_special_claims_data,
                                               "Cl_payment_cal",
                                               first_orig_year,
                                               last_orig_year)
   history_annuities = generate_history_per_claim(extended_special_claims_data,
                                                  "An_payment_cal",
                                                  first_orig_year,
                                                  last_orig_year)

   # Reduce active annuities to those belonging to special claims
   active_annuities_special_claims <- active_annuities[active_annuities$Claim_id %in% special_claims$Claim_id,]

   # set with_annuities to FALSE if there aren't any
   if (NROW(active_annuities_special_claims) < 1) {with_annuities <- FALSE}

   # Iterate over each special claim and do
   #     1. roll out claims reserve
   #     2. add this cashflow to the according row in claims_prediction
   #     3. roll out annuities of this claim
   #     4. add this cashflow to the according row in annuities_prediction
   #     5. add both cashflows and bind with sum of history of the claim to apply reinsurance
   #     6. add reinsurance cashflow to according row in special_ceded_xl_payments

   for (current_row in 1:NROW(claims_list_special_claims)) {
      claim_id <- claims_list_special_claims$Claim_id[current_row]
      origin_year <- claims_list_special_claims$Origin_year[current_row]
      cl_reserve <- claims_list_special_claims$Cl_reserve[current_row]
      dev_year_since_large <- claims_list_special_claims$Dev_year_since_large[current_row]

      # 1. roll out claims reserve
      match_claims <- match(claim_id, special_claims$Claim_id)
      rolled_out_claim <- roll_out_single_claim(reserve = cl_reserve * special_claims$Reserve2BE_percentage[match_claims],
                                                years = end_of_tail - dev_year_since_large,
                                                type = special_claims$Rollout_type[match_claims],
                                                external_pattern = external_patterns[[special_claims$Pattern_id[match_claims]]])

      # 2. add to claims_prediction
      claims_prediction[as.character(origin_year),] <-
         claims_prediction[as.character(origin_year),] + rolled_out_claim

      # 3. roll out annuities of this claim
      if (with_annuities) {
         # identify annuities belonging to this claim
         relevant_annuities <- active_annuities_special_claims[active_annuities_special_claims$Claim_id == claim_id,]
         if (NROW(relevant_annuities) > 0) {
            rolled_out_annuities <- colSums(roll_out_annuity(payments = generate_annuity_payments(annuities = relevant_annuities,
                                                                                                  last_orig_year = last_orig_year),
                                                             probs = generate_annuity_probabilities(annuities = relevant_annuities,
                                                                                                    age_shift = age_shift,
                                                                                                    mortality = mortality),
                                                             stop.at = "end_of_probs"))
            # 4. Add to annuities_prediction
            annuities_prediction[as.character(origin_year), 1:130] <-
               annuities_prediction[as.character(origin_year), 1:130] + rolled_out_annuities

            # 5a. Add to rolled_out_claim to prepare reinsurance calculation
            rolled_out_claim[1:130] <- rolled_out_claim[1:130] + rolled_out_annuities
         }
      }

      # 5b. apply reinsurance
      if (with_reinsurance) {
         xl_portion <- xl_cashflow(future_payments = rbind(rolled_out_claim),
                                   claims_list = claims_list_special_claims[current_row,],
                                   history = history_claims[claim_id, , drop = FALSE] +
                                      history_annuities[claim_id, , drop = FALSE],
                                   reinsurance = reinsurance,
                                   indices = indices,
                                   output = "total")
         # 6. Add reinsurance portion to special_ceded_xl_payments
         special_ceded_xl_payments[as.character(origin_year),] <-
            special_ceded_xl_payments[as.character(origin_year),] + xl_portion
      }
   }

   # bind claims and annuities with history
   special_claims_payments <- cbind(special_claims_paid, claims_prediction)
   special_annuities_payments <- cbind(special_annuities_paid, annuities_prediction)

   # calculate quota share
   if (with_reinsurance) {
      special_ceded_quota_payments <- (special_claims_payments + special_annuities_payments) *
         reinsurance$Quota_share[reinsurance$Origin_year %in% first_orig_year:last_orig_year]}

   return(list(special_claims_payments = special_claims_payments,
               special_claims_reserved = special_claims_reserved,
               special_annuities_payments = special_annuities_payments,
               special_annuities_reserved = special_annuities_reserved,
               special_ceded_quota_payments = special_ceded_quota_payments,
               special_ceded_xl_payments = special_ceded_xl_payments))

}

