#' Generating additive model to estimate future ibnr claim numbers
#'
#' @description
#' This function calculates the number of ibnr claims per development year and divides it by the
#' exposure these ibnr claims have raised from.
#'
#' @param large_claims_list Dataframe with one row per known large claim as a result of [generate_claims_list()]
#' @param first_orig_year Desired first origin year.
#' @param last_orig_year Desired last origin year.
#' @param exposure Dataframe that must contain one row for each origin year between `first_orig_year` and `last_orig_year`
#' and the columns `Origin_year` and `Exposure`.
#' @param years_for_ibnr_pools Vector containing the calendar years that shall be used to build the pools.
#' Thus ibnr claims that have became large in other calendar years don't affect the model.
#'
#' @return Dataframe with columns `Dev_year`, `Ibnr_numbers`, `Sum_of_exposures` and `Factor`.
#' @export
#'
#' @examples
#' # prepare data
#' extended_claims_data_xmpl <- prepare_data(claims_data = claims_data_xmpl,
#'                                           indices = indices_xmpl,
#'                                           threshold = 4e5,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023,
#'                                           expected_year_of_growing_large = 3,
#'                                           reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                           pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # generate claims list
#' large_claims_list_xmpl <- generate_claims_list(extended_claims_data = extended_claims_data_xmpl,
#'                                                first_orig_year = 1989,
#'                                                last_orig_year = 2023)
#'
#' # generate additive ibnr model using the last ten calendar years
#' get_additive_ibnr_model(large_claims_list = large_claims_list_xmpl,
#'                         first_orig_year = 1989,
#'                         last_orig_year = 2023,
#'                         exposure = exposure_xmpl,
#'                         years_for_ibnr_pools = 2014:2023)
#'
get_additive_ibnr_model <- function(large_claims_list,
                                    first_orig_year,
                                    last_orig_year ,
                                    exposure,
                                    years_for_ibnr_pools){

   Large_since <- NULL
   orig_years <- first_orig_year:last_orig_year
   max_dev_year <- NROW(orig_years) - 1
   dev_years <- 1:max_dev_year

   vexposure <- exposure$Exposure[match(orig_years, exposure$Origin_year)]

   ibnr_numbers <- sum_of_exposures <- integer(max_dev_year)

   # large_claims_list is filtered to chosen years that shall be used for the pools
   filtered_large_claims_list <- large_claims_list[large_claims_list$Large_since %in% years_for_ibnr_pools,]

   # calculate number of ibnr-claims that occured per development year
   # calculate the exposure sum of origin years that could have produced ibnr claims per development year.
   for (dev_year in c(1:max_dev_year)) {
      ibnr_numbers[dev_year] <- NROW(filtered_large_claims_list[
         filtered_large_claims_list$Dev_year_of_growing_large == dev_year,])
      sum_of_exposures[dev_year] <- sum(vexposure[(orig_years + dev_year - 1) %in% years_for_ibnr_pools])
   }

   additive_factors <- ibnr_numbers / sum_of_exposures

   return(data.frame(Dev_year = dev_years,
                     Ibnr_numbers = ibnr_numbers,
                     Sum_of_exposures = sum_of_exposures,
                     Factor = additive_factors))
}


#' Derive expected future ibnr numbers
#'
#' @description
#' Expected future ibnr numbers are calculated from the additive ibnr model and the exposure.
#'
#' @param additive_ibnr_model Dataframe as a result of [get_additive_ibnr_model()].
#' @param exposure Dataframe that must contain one row for each desired origin year
#' and the columns `Origin_year` and `Exposure`.
#'
#' @return Expected ibnr numbers matrix with one row per origin year and one column per development year.
#' @export
#'
#' @examples
#' add_mod <- data.frame(Dev_year = 1:5,
#'                       Ibnr_numbers = c(100, 44, 15, 0, 2),
#'                       Sum_of_exposures = c(42000, 40000, 38000, 37000, 35000),
#'                       Factor = c(0.00238, 0.0011, 0.00039, 0, 0.00006))
#' exp <- data.frame(Origin_year = 2020:2023,
#'                   Exposure = c(1000, 1200, 1500, 1900))
#' get_expected_ibnr_numbers(add_mod, exp)
get_expected_ibnr_numbers <- function(additive_ibnr_model, exposure){
   # to save computing time the matrix is only created until the maximum development year with factor > 0
   max_dev_year_with_ibnr <- max(c(2, subset(additive_ibnr_model,
                                             additive_ibnr_model$Ibnr_numbers > 0)$Dev_year))

   factors <- additive_ibnr_model$Factor[2:max_dev_year_with_ibnr]

   # write exposure in every column
   expected_ibnr_numbers <- matrix(rev(exposure$Exposure),
                                   nrow = NROW(exposure),
                                   ncol = max_dev_year_with_ibnr - 1)
   # set upper left triangle to 0 and revert rows
   expected_ibnr_numbers[lower.tri(expected_ibnr_numbers)] <- 0
   expected_ibnr_numbers <- expected_ibnr_numbers[NROW(expected_ibnr_numbers):1, , drop = FALSE]

   # apply factors
   expected_ibnr_numbers <- t(t(expected_ibnr_numbers) * factors)

   # names
   rownames(expected_ibnr_numbers) <- exposure$Origin_year
   colnames(expected_ibnr_numbers) <- 2:max_dev_year_with_ibnr

   return(expected_ibnr_numbers)
}


#' Rounding expected ibnr matrix
#'
#' @description
#' As only whole ibnr numbers can be used in the simulation, the expected ibnr matrix must be rounded. \cr \cr
#' Algorithms for rounding should allocate the rounded remainders to origin and development years
#' in a way that rowsums and colsums are barely changed. They must moreover secure that 0-entries remain 0 to
#' prevent subsequent errors. \cr \cr
#'
#' This algorithm is one possible ways to do so and works very well in most examples. \cr \cr
#' __Thanks to Michele Pellino for idea and implementation!__
#'
#'
#' @param expected_ibnr_numbers Matrix as a result of [get_expected_ibnr_numbers()]
#'
#' @return Matrix with same dimensions.
#' @export
#'
#' @examples
#' # small example
#' exp_ibnr <- matrix(c(0,0,0,2.09,0,0,0.585,0.741,0,0,0,0,0.06,0.072,0.09,0.114), 4)
#' rownames(exp_ibnr) <- 2020:2023
#' colnames(exp_ibnr) <- 2:5
#' # Unrounded:
#' print(exp_ibnr)
#' # Rounded:
#' round_expected_ibnr_numbers(exp_ibnr)
#'
#' # more complex example
#' # prepare data
#' extended_claims_data_xmpl <- prepare_data(claims_data = claims_data_xmpl,
#'                                           indices = indices_xmpl,
#'                                           threshold = 4e5,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023,
#'                                           expected_year_of_growing_large = 3,
#'                                           reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                           pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # generate claims list
#' large_claims_list_xmpl <- generate_claims_list(extended_claims_data = extended_claims_data_xmpl,
#'                                                first_orig_year = 1989,
#'                                                last_orig_year = 2023)
#'
#' # generate additive ibnr model using the last ten calendar years
#' am <- get_additive_ibnr_model(large_claims_list = large_claims_list_xmpl,
#'                               first_orig_year = 1989,
#'                               last_orig_year = 2023,
#'                               exposure = exposure_xmpl,
#'                               years_for_ibnr_pools = 2014:2023)
#'
#' expected_ibnr_numbers <- get_expected_ibnr_numbers(am, exposure_xmpl)
#' rounded_expected_ibnr_numbers <- round_expected_ibnr_numbers(expected_ibnr_numbers)
#' # Unrounded:
#' print(expected_ibnr_numbers)
#' # Rounded:
#' print(rounded_expected_ibnr_numbers)
#' # colsums before and after
#' print(data.frame(ColSums_unrounded = colSums(expected_ibnr_numbers),
#'                  ColSums_round = colSums(rounded_expected_ibnr_numbers)))
#'
#' # rowsums before and after
#' print(data.frame(RowSums_unrounded = rowSums(expected_ibnr_numbers),
#'                  RowSums_round = rowSums(rounded_expected_ibnr_numbers)))
#'
round_expected_ibnr_numbers <- function(expected_ibnr_numbers){
   rounded_expected_ibnr_numbers <- expected_ibnr_numbers
   for (i in 1:nrow(rounded_expected_ibnr_numbers)) { # loop origin years
      remainders <- 0 # variable saves the remainders after rounding
      for (j in 1:ncol(rounded_expected_ibnr_numbers)) { # loop development years
         decimal <- expected_ibnr_numbers[i,j]
         if (decimal == 0) {next} # 0-entries are skipped; rest is added later
         rounded_expected_ibnr_numbers[i,j] <- round(decimal + remainders,0)
         remainders <- decimal + remainders - rounded_expected_ibnr_numbers[i,j]
      }
   }
   return(rounded_expected_ibnr_numbers)
}


#' Transform exptected ibnr matrix to dataframe
#'
#' @description
#' The matrix with rounded expected numbers of ibnr large claims is transformed to a dataframe with
#' one row per expected claim.
#'
#' @param expected_rounded_ibnr_numbers Matrix as a result of [get_expected_ibnr_numbers()] and [round_expected_ibnr_numbers()]. \cr
#' Rownames must be equal to origin years!
#'
#' @return Dataframe with one row per expected future ibnr large claim and columns `Origin_year`,
#' `Dev_year_of_growing_large` and `Dev_year_since_large`.
#'    * `Origin_year` is the origin year of the expected future ibnr claim.
#'    * `Dev_year_of_growing_large` is the expected development year in which the expected ibnr claim
#'    exceeds the threshold.
#'    * `Dev_year_since_large` is the "new" development year where counting starts in the calendar year
#'    the claim exceeded the threshold. As these claims are expected to exceed the threshold in the future,
#'    this column must be < 1.
#'
#' @export
#'
#' @examples
#' exp_ibnr <- matrix(c(0,0,0,2,0,0,1,1,0,0,0,0,0,0,0,1), 4)
#' rownames(exp_ibnr) <- 2020:2023
#' colnames(exp_ibnr) <- 2:5
#' ibnr_transform2dataframe(exp_ibnr)
#'
#' @importFrom dplyr "%>%" filter rename mutate select arrange desc
#' @importFrom tidyr uncount
#'
ibnr_transform2dataframe <- function(expected_rounded_ibnr_numbers){
   # Setting arguments to NULL to prevent note in CMD check
   Freq <- Var1 <- Var2 <- Origin_year <- Dev_year_of_growing_large <- Numbers <- NULL

   # find last origin year
   last_orig_year <- max(as.integer(rownames(expected_rounded_ibnr_numbers)))

   ibnr_df <- as.data.frame(as.table(expected_rounded_ibnr_numbers)) %>% # transform to dataframe
      rename(Origin_year = Var1, Dev_year_of_growing_large = Var2, Numbers = Freq) %>%
      # mutate factors to integer values to apply uncount
      mutate(Origin_year = as.integer(as.character(Origin_year)),
             Dev_year_of_growing_large = as.integer(as.character(Dev_year_of_growing_large))) %>%
      # uncount duplicates rows where Numbers > 1 and deletes rows with Numbers = 0
      uncount(weights = Numbers) %>%
      # add Dev_year_since_large
      mutate(Dev_year_since_large = last_orig_year - Origin_year - Dev_year_of_growing_large + 2) %>%
      arrange(desc(Origin_year), Dev_year_of_growing_large)
   return(ibnr_df)
}


#' Generate ibnr pools
#'
#' @description
#' For sampling purposes this pool shall contain a list with one vector per development year.
#' The vector contains historic ibnr claims.
#'
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#' @param first_orig_year First origin year with complete history.
#' @param last_orig_year Last origin year.
#' @param years_for_ibnr_pools Vector containing the calendar years that shall be used to build the pools.
#' Thus ibnr claims that have become large in other calendar years don't affect the model.
#'
#' @return List of one vector per development year with each vector containing the row numbers of the
#' claims in `large_claims_list` that have become large in that development year and in a calendar year
#' in `years_for_ibnr_pools`.
#' @export
#'
#' @examples
#' # prepare data
#' extended_claims_data_xmpl <- prepare_data(claims_data = claims_data_xmpl,
#'                                           indices = indices_xmpl,
#'                                           threshold = 4e5,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023,
#'                                           expected_year_of_growing_large = 3,
#'                                           reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                           pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # generate claims list
#' large_claims_list_xmpl <- generate_claims_list(extended_claims_data = extended_claims_data_xmpl,
#'                                                first_orig_year = 1989,
#'                                                last_orig_year = 2023)
#' generate_ibnr_pools(large_claims_list_xmpl, 1989, 2023, 2014:2023)
generate_ibnr_pools <- function(large_claims_list,
                                first_orig_year,
                                last_orig_year,
                                years_for_ibnr_pools){
   ibnr_pools <- list()
   for (dev_year in c(1:(last_orig_year - first_orig_year))) {
      ibnr_pools[[dev_year]] <- which(large_claims_list$Dev_year_of_growing_large == (dev_year + 1) &
                                         large_claims_list$Large_since %in% years_for_ibnr_pools)
   }
   return(ibnr_pools)
}


#' Prepares necessary ibnr objects
#'
#' @description
#' The function generates the expected future ibnr claims and the pools of historic ibnr claims
#' from which the claim payments of the future ibnr claims will be sampled.
#'
#'
#' @param large_claims_list Dataframe with one row per known large claim as a result of [generate_claims_list()]
#' @param first_orig_year Desired first origin year.
#' @param last_orig_year Desired last origin year.
#' @param exposure Dataframe that must contain one row for each origin year between `first_orig_year` and `last_orig_year`
#' and the columns `Origin_year` and `Exposure`. \cr
#' Default: NULL. In this case, a constant exposure is assumed.
#' @param years_for_ibnr_pools Vector containing the calendar years that shall be used to build the pools. \cr
#' Default: NULL. In this case, all years are used for building ibnr pools.
#'
#' @details
#' The first object is generated in four steps with the functions [get_additive_ibnr_model()], [get_expected_ibnr_numbers()],
#' [round_expected_ibnr_numbers()] and [ibnr_transform2dataframe()]. If one are more functions shall be
#' replaced by own implementations, the functions may be used separately.
#'
#'
#' @return List of two objects: \cr
#' 1. Dataframe with one row per expected future ibnr large claim and columns `Origin_year`,
#' `Dev_year_of_growing_large` and `Dev_year_since_large`.
#'    + `Origin_year` is the origin year of the expected future ibnr claim.
#'    + `Dev_year_of_growing_large` is the expected development year in which the expected ibnr claim
#'    exceeds the threshold.
#'    + `Dev_year_since_large` is the "new" development year where counting starts in the calendar year
#'    the claim exceeded the threshold. As these claims are expected to exceed the threshold in the future,
#'    this column must be < 1.
#' 2. List of one vector per development year with each vector containing the row numbers of the
#' claims in `large_claims_list` that have become large in that development year and in a calendar year
#' in `years_for_ibnr_pools`.
#'
#' @export
#'
#' @examples
#' # prepare data
#' extended_claims_data_xmpl <- prepare_data(claims_data = claims_data_xmpl,
#'                                           indices = indices_xmpl,
#'                                           threshold = 4e5,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023,
#'                                           expected_year_of_growing_large = 3,
#'                                           reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                           pool_of_annuities = pool_of_annuities_xmpl)
#'
#' # generate claims list
#' large_claims_list_xmpl <- generate_claims_list(extended_claims_data = extended_claims_data_xmpl,
#'                                                first_orig_year = 1989,
#'                                                last_orig_year = 2023)
#'
#' # generate example exposure with 5% annual exposure increase since 1989
#'
#' prepare_ibnr(large_claims_list_xmpl,
#'              1989,
#'              2023,
#'              exposure_xmpl,
#'              2014:2023)
#'
#' @importFrom dplyr "%>%"
prepare_ibnr <- function(large_claims_list,
                         first_orig_year,
                         last_orig_year,
                         exposure = NULL,
                         years_for_ibnr_pools = NULL) {

   # if no exposure is given, a constant exposure is assumed:
   if (is.null(exposure)) {exposure <- data.frame(Origin_year = first_orig_year:last_orig_year,
                                                  Exposure = 1)}
   # if years_for_ibnr_pools is not given, all years will be used
   if (is.null(years_for_ibnr_pools)) {years_for_ibnr_pools <- first_orig_year:last_orig_year}

   df <- get_additive_ibnr_model(large_claims_list,
                                 first_orig_year,
                                 last_orig_year,
                                 exposure,
                                 years_for_ibnr_pools) %>%
      get_expected_ibnr_numbers(exposure) %>%
      round_expected_ibnr_numbers() %>%
      ibnr_transform2dataframe()

   pools <- generate_ibnr_pools(large_claims_list,
                                first_orig_year,
                                last_orig_year,
                                years_for_ibnr_pools)

   return(list(df, pools))
}



