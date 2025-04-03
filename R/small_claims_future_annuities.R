#' Estimate for future annuities stemming from small claims
#'
#' @description
#' Payments for future annuities are an important part of the large claims simulation. But depending on the threshold
#' annuities may also be a part of the estimation for small claims. This could be the case if the annuity belongs to a
#' claim that has never exceeded the threshold or if the annuity has been created in the years before a claim
#' exceeds the threshold. \cr \cr
#' There may be several ways to estimate the future payments for this part. This implementation uses the historic
#' annuities that have been created for small claims to predict a number of future annuities per origin and development
#' year by an additive method. This number will then be multiplied with a mean annuity. \cr \cr
#' This function provides the parameters for this method using the input data that should be given anyway if large claims
#' are simulated. \cr
#' In the second step, these parameters can be used by the function [generate_small_claims_future_annuities()].
#'
#'
#' @param pool_of_annuities Dataframe of all historic annuities, see description of `pool_of_annuities` in
#' details of [prepare_data()].
#' @param large_claims_list Dataframe of large claims generated with [generate_claims_list()].
#' @param special_claims Dataframe with special claims, see details of [compute_special_claims()].
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param small_claims_reserved Numeric matrix of small claims reserves with origin years as rows and calendar
#' years as columns. This matrix can be generated as difference from the all claims' triangle and the large and special
#' claims triangle. Rownames must be the origin years. The reserves serve as volume measure for the additive method.
#' @param age_shift Dataframe, see description of [age_shift_xmpl].
#' @param mortality Dataframe, see description of [mortality_xmpl].
#'
#' @return
#' A list of two numeric parameter vectors:\cr
#' 1. `factors_additive_method` which can be applied to the small claims reserves to predict the future number of small
#' claims' annuities.
#' 2. `mean_small_claims_annuity` which is multiplied to the future number of small claims' annuities.
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package.
#' # A few preparing steps are necessary to generate the needed arguments from the example
#' # objects provided with this package.
#'
#' # The claims with claim_ids Claim#43, Claim#44 and Claim#68 are identified as special claims.
#' special_claims <- data.frame(Claim_id = c("Claim#43", "Claim#44", "Claim#68"),
#'                              Reserve2BE_percentage = c(0.8, 0.5, 1.2),
#'                              Rollout_type = c("linear", "constant", "external"),
#'                              Pattern_id = c(1,1,3))
#'
#' external_patterns <- list(c(0.5, 0.3, 0.2),
#'                           c(1),
#'                           c(0.7, 0.3))
#'
#' # Create large claims list
#' extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # remove special claims from extended_claims_data
#' extended_large_claims_data <- extended_claims_data[
#'                extended_claims_data$Claim_id %in% special_claims$Claim_id,]
#' large_claims_reserved <- generate_triangle(data = extended_large_claims_data,
#'                                                     col_name = "Cl_reserve",
#'                                                     first_orig_year = 1989,
#'                                                     last_orig_year = 2023)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#'
#' result_special_claims <- compute_special_claims(special_claims = special_claims,
#'                                                 extended_claims_data = extended_claims_data,
#'                                                 first_orig_year = 1989,
#'                                                 last_orig_year = 2023,
#'                                                 end_of_tail = 50,
#'                                                 external_patterns = external_patterns)
#'
#' special_claims_reserved <- result_special_claims$special_claims_reserved
#'
#' small_claims_reserved <- all_claims_reserved_xmpl -
#'                      large_claims_reserved - special_claims_reserved
#'
#' # apply these objects as arguments to the function
#' generate_small_claims_annuities_parameters(
#'                pool_of_annuities = pool_of_annuities_xmpl,
#'                large_claims_list = large_claims_list,
#'                special_claims = special_claims,
#'                indices = indices_xmpl,
#'                small_claims_reserved = small_claims_reserved,
#'                age_shift = age_shift_xmpl,
#'                mortality = mortality_xmpl)#'
generate_small_claims_annuities_parameters <- function(pool_of_annuities,
                                                       large_claims_list,
                                                       special_claims,
                                                       indices,
                                                       small_claims_reserved,
                                                       age_shift,
                                                       mortality) {
   # The idea for the estimation of future annuities of small claims is to calculate the future number of annuities
   # per development year and per origin year in a first step and multiply this number by a mean annuity in a second step.

   # Procedure:
   # 1. Reduce pool_of_annuities to relevant annuities and aggregate them to an annuities count triangle.
   # 2. Divide them by the sum of the reserves for basis claims after the first development year of the corresponding
   #     origin years. The result are factors for each development year that may be applied to the current small claims
   #     reserves. They may also be applied to other lobs or other undertakings, but care must be taken for exact
   #     indexation.
   # 3. Create one fictive small claims annuity by rolling out each historic relevant annuity (with indexed payments)
   #     and building the mean over the future payments.
   # 4. Multiply the result of 1. and 2. and aggregate the results.

   # This function only resembles points 1 to 3 which means it only provides the parameter for this method.
   # Point 4 will be provided with the function generate_small_claims_future_annuities() to allow for using external
   # factors and mean annuities.

   # parameters are derived from small_claims_reserved
   last_orig_year <- max(as.integer(rownames(small_claims_reserved)))
   first_orig_year <- min(as.integer(rownames(small_claims_reserved)))
   years_count <- last_orig_year - first_orig_year + 1

   index_year <- attr(indices, "index_year")

   ### 1a. Reduce pool_of_annuity to relevant annuities (see below)
   #  reduce pool to annuities belonging to claims between first_orig_year and last_orig_year
   pool_of_annuities <- pool_of_annuities[pool_of_annuities$Origin_year %in% c(first_orig_year:last_orig_year),]

   # condition1 are the row indices of pool_of_annuities where the annuity belongs to a large claim but was built
   # before the claim became.
   condition1 <- which(large_claims_list$Large_since[match(pool_of_annuities$Claim_id,
                                                           large_claims_list$Claim_id)]
                       > pool_of_annuities$Entering_year)

   # condition2 are the row indices of pool_of_annuities where the annuity belongs neither to a large
   # nor to a special claim.
   condition2 <- which(!pool_of_annuities$Claim_id %in% c(large_claims_list$Claim_id, special_claims$Claim_id))

   # apply conditions
   small_annuities <- pool_of_annuities[c(condition1, condition2),]

   # If there is no annuity of a small claim, factors are 0.
   if (NROW(small_annuities) == 0) {
      return(list(factors_additive_method = rep(0, years_count),
                  mean_small_claims_annuity = rep(0, 130)))
   }

   # else the parameters are calculated
   # the development year will be needed
   small_annuities$Development_year <- small_annuities$Calendar_year - small_annuities$Origin_year + 1

   ### 1b. Create matrix with the number of historic small claims' annuities
   # rows = Origin years, columns = Development years
   small_annuities_count <- matrix(0, years_count, years_count)
   for (orig_year in (first_orig_year:last_orig_year)) {
      for (dev_year in (1:years_count)) {
         small_annuities_count[orig_year - (first_orig_year - 1), dev_year] <-
            NROW(small_annuities[ # Count all annuities...
               small_annuities$Origin_year == orig_year & # ...of the corresponding origin year...
                  small_annuities$Development_year == dev_year,]) # ...and the corresponding development year.
      }
   }

   ### 2. Divide the number of claims per development year by the corresponding reserves after first development year
   # Beforehand indexate reserves to index_year because large and small claims have also been
   #     identified on the base of index_year.
   reserved <- apply_index(payments = cal_year2dev_year(small_claims_reserved)[,1],
                           original_cal_years = first_orig_year:last_orig_year,
                           new_cal_years = index_year,
                           indices = indices)
   # Calculate sum of reserves for corresponding origin years, e.g. for n-th development year the sum of reserves
   #     of origin years that have already passed the n-th development year.
   denominator <- unname(rev(cumsum(reserved)))

   # Calculate factors for the additive method
   factors_additive_method <- ifelse(denominator > 0,
                                     colSums(small_annuities_count) / denominator,
                                     rep(0, years_count))

   # Calculate the mean annuity of small claims indexed to index_year
   mean_small_claims_annuity <- colMeans(generate_annuity_payments(annuities = small_annuities,
                                                                   last_orig_year = index_year,
                                                                   indices = indices,
                                                                   future_annuities = TRUE) *
                                            (1 - generate_annuity_probabilities(annuities = small_annuities,
                                                                                age_shift = age_shift,
                                                                                mortality = mortality)))

   # return parameter list
   return(list(factors_additive_method = factors_additive_method,
               mean_small_claims_annuity = mean_small_claims_annuity))
}



#' Estimate for future annuities stemming from small claims
#'
#' @description
#' Payments for future annuities are an important part of the large claims simulation. But depending on the threshold
#' annuities may also be a part of the estimation for small claims. This could be the case if the annuity belongs to a
#' claim that has never exceeded the threshold or if the annuity has been created in the years before a claim
#' exceeds the threshold. \cr \cr
#' There may be several ways to estimate the future payments for this part. This implementation uses the historic
#' annuities that have been created for small claims to predict a number of future annuities per origin and development
#' year by an additive method. This number will then be multiplied with a mean annuity. \cr \cr
#' This function calculates the estimate for future annuities stemming from small claims using the parameters that
#' can either be generated by [generate_small_claims_annuities_parameters()] or have been delivered from external.
#'
#' @param small_claims_annuities_parameters_list List of parameter vectors as a result of
#' [generate_small_claims_annuities_parameters()].
#' @param small_claims_reserved Numeric matrix of small claims reserves with origin years as rows and calendar
#' years as columns. This matrix can be generated as difference from the all claims' triangle and the large and special
#' claims triangle. Rownames must be the origin years. The reserves serve as volume measure for the additive method.
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#'
#' @return A numeric matrix with one row per origin year and one column for each past and 250 future calendar years.
#' The columns for past calendar years are all 0, but the columns for future calendar years contain the estimated
#' payments for future annuities stemming from small claims.
#' @export
#'
#' @examples
#' #' # this example uses data provided with this package.
#' # A few preparing steps are necessary to generate the needed arguments from the example
#' # objects provided with this package.
#'
#' # The claims with claim_ids Claim#43, Claim#44 and Claim#68 are identified as special claims.
#' special_claims <- data.frame(Claim_id = c("Claim#43", "Claim#44", "Claim#68"),
#'                              Reserve2BE_percentage = c(0.8, 0.5, 1.2),
#'                              Rollout_type = c("linear", "constant", "external"),
#'                              Pattern_id = c(1,1,3))
#'
#' external_patterns <- list(c(0.5, 0.3, 0.2),
#'                           c(1),
#'                           c(0.7, 0.3))
#'
#' # Create large claims list
#' extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # remove special claims from extended_claims_data
#' extended_large_claims_data <- extended_claims_data[
#'                extended_claims_data$Claim_id %in% special_claims$Claim_id,]
#' large_claims_reserved <- generate_triangle(data = extended_large_claims_data,
#'                                                     col_name = "Cl_reserve",
#'                                                     first_orig_year = 1989,
#'                                                     last_orig_year = 2023)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#'
#' result_special_claims <- compute_special_claims(special_claims = special_claims,
#'                                                 extended_claims_data = extended_claims_data,
#'                                                 first_orig_year = 1989,
#'                                                 last_orig_year = 2023,
#'                                                 end_of_tail = 50,
#'                                                 external_patterns = external_patterns)
#'
#' special_claims_reserved <- result_special_claims$special_claims_reserved
#'
#' small_claims_reserved <- all_claims_reserved_xmpl -
#'                      large_claims_reserved - special_claims_reserved
#'
#' # apply these objects as arguments to the function to create parameters
#' params <- generate_small_claims_annuities_parameters(
#'                pool_of_annuities = pool_of_annuities_xmpl,
#'                large_claims_list = large_claims_list,
#'                special_claims = special_claims,
#'                indices = indices_xmpl,
#'                small_claims_reserved = small_claims_reserved,
#'                age_shift = age_shift_xmpl,
#'                mortality = mortality_xmpl)
#'
#' # use this parameter list to get the estimate
#' generate_small_claims_future_annuities(
#'    small_claims_annuities_parameters_list = params,
#'    small_claims_reserved = small_claims_reserved,
#'    indices = indices_xmpl)[c("2022", "2023"), 1:50]
#'
generate_small_claims_future_annuities <- function(small_claims_annuities_parameters_list,
                                                   small_claims_reserved,
                                                   indices) {
   # parameters are derived from small_claims_reserve_triangle
   last_orig_year <- max(as.integer(rownames(small_claims_reserved)))
   first_orig_year <- min(as.integer(rownames(small_claims_reserved)))
   years_count <- last_orig_year - first_orig_year + 1

   # derive index_year
   index_year <- attr(indices, "index_year")

   # extract parameters from parameters list
   factors_additive_method <- small_claims_annuities_parameters_list$factors_additive_method
   mean_small_claims_annuity <- small_claims_annuities_parameters_list$mean_small_claims_annuity
   dev_years_count <- length(factors_additive_method)

   # Number of expected future small claims' annuities
   small_future_annuities_count <- matrix2triangle(outer(cal_year2dev_year(small_claims_reserved)[,1],
                                                         factors_additive_method,
                                                         "*"))
   small_future_annuities_count <- cbind(small_future_annuities_count[, 2:dev_years_count, drop = FALSE],
                                         rep(0, years_count))

   # indexate to future years. The indexation is applied to the future numbers and the mean annuity is not changed.
   for (current_column in 1:NCOL(small_future_annuities_count)) {
      small_future_annuities_count[,current_column] <- apply_index(small_future_annuities_count[, current_column, drop = FALSE],
                                                                   index_year,
                                                                   last_orig_year + current_column,
                                                                   indices)
   }

   # small_future_annuities_count is in development year presentation and must be switched to calendar year presentation
   switched <- matrix(0, years_count, years_count)
   for (current_row in 2:years_count) {
      shift <- years_count - current_row
      switched[current_row, 1:(years_count - shift)] <- switched[current_row, 1:(years_count - shift)] +
         small_future_annuities_count[current_row, (shift + 1):years_count]
   }

   # produce result
   result_matrix <- skeleton_prediction(first_orig_year, last_orig_year)
   for (current_row in 1:years_count) {
      for (current_column in 1:dev_years_count) {
         result_matrix[current_row, (current_column):(current_column + 129)] <-
            result_matrix[current_row, (current_column):(current_column + 129)] +
            mean_small_claims_annuity * switched[current_row, current_column]
      }
   }

   return(cbind(skeleton_triangle(first_orig_year, last_orig_year),
                result_matrix))
}




