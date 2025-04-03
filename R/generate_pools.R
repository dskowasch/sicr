#' Generate pools to sample from
#'
#' @param extended_claims_data Dataframe as output of [prepare_data()].
#' @param reserve_classes Numeric vector to specify reserve classes at the niveau of `index_year`, see details of [prepare_data()].
#' @param years_for_pools Vector containing the calendar years that shall be used to build the pools.
#' @param start_of_tail First year to be treated as a tail year.
#' @param end_of_tail Last development year.
#' @param lower_outlier_limit Numeric value to specify lower outlier limit at the niveau of `index_year` see details of [prepare_data()].
#' Lower payments will be set to lower_outlier_limit.
#' @param upper_outlier_limit Numeric value to specify upper outlier limit at the niveau of `index_year`, see details of [prepare_data()].
#' Higher payments will be set to upper_outlier_limit.
#' @param pool_of_annuities Pool of annuities that was used to generate `extended_claims_data`.
#'
#' @return List of three objects, see details.
#' @export
#'
#' @details
#' The function returns a list of three objects:
#' 1. __pretail pools__ \cr
#' This is a list containing one list per reserve class which contain one dataframe per development year. These dataframes
#' are the pools of historic claims from which to sample if a newer claim has the corresponding development year and reserve class.
#' It consists of the following columns:
#'    + `Ind_cl_payment_cal` is the indexed calendar year payment.
#'    + `Exit_reserve_class` is the "new" reserve class after the year.
#'    + `New_annuity_1` to `New_annuity_5` are the indices of new annuities in the dataframe `pool_of_annuities`.
#'    + `Ind_entry_cl_reserve` is the entry claim reserve which is only used for analysing purposes, for example to identify special claims.
#' 2. __tail pools__ \cr
#' This is a list containing one dataframe per class. No distinction is made between development years except in the indexed
#' calendar year payments. These are raised to the niveau of the first tail year. The columns are the same as in 1.
#' 3. __pool of annuities__ \cr
#' As the first two lists refer to the index in this pool, it has to be attached to enable sampling.
#'
#' Note that in the pools list the first reserve class is always reserve class 0 and the first development year is always development year 2!
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
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' # pretail pools reserve class 0 and development year 2
#' print(pools[[1]][[1]][[1]])
#'
#' # tail pools reserve class 2
#' print(pools[[2]][[3]])
#'
#' # pool of annuities
#' print(pools[[3]])
generate_pools <- function(extended_claims_data,
                           reserve_classes,
                           years_for_pools,
                           start_of_tail,
                           end_of_tail,
                           lower_outlier_limit = -Inf,
                           upper_outlier_limit = Inf,
                           pool_of_annuities){
   ### pretail pools
   pretail_pools <-  c() # Initiate list for pretail pools to be filled by class pools
   for (reserve_class in c(0:length(reserve_classes))) {
      class_pool <-  c() # Initiate list for class pool
      for (dev_year in (2:(start_of_tail - 1))) {
         class_and_dev_year_pool <-  extended_claims_data[extended_claims_data$Dev_year_since_large == dev_year &
                                                             extended_claims_data$Entry_reserve_class == reserve_class &
                                                             extended_claims_data$Calendar_year %in% years_for_pools,
                                                          c("Ind_cl_payment_cal",
                                                            "Exit_reserve_class",
                                                            "New_annuity_1",
                                                            "New_annuity_2",
                                                            "New_annuity_3",
                                                            "New_annuity_4",
                                                            "New_annuity_5",
                                                            "Ind_entry_cl_reserve")]

         # If that pool is empty, create dataframe with one dummy row where each payment
         #   and new annuity is 0 and reserve class stays the same.
         if (NROW(class_and_dev_year_pool) == 0) {
            class_and_dev_year_pool[1,] <- c(0, reserve_class, 0, 0, 0, 0, 0, 0)}

         # apply outlier limits
         class_and_dev_year_pool$Ind_cl_payment_cal <- pmax(lower_outlier_limit,
                                                            pmin(upper_outlier_limit,
                                                                 class_and_dev_year_pool$Ind_cl_payment_cal))

         # append class_and_dev_year_pool to class_pool
         class_pool[[length(class_pool) + 1]] = class_and_dev_year_pool
      }

      # append class_pool to pretail_pools
      pretail_pools[[length(pretail_pools) + 1]] = class_pool
   }

   ### tail pools
   tail_pools <- c() # Initiate list for tail pools to be filled by class pools
   for (reserve_class in c(0:length(reserve_classes))) {
      # Each tail payment is filtered here.
      class_pool <- extended_claims_data[extended_claims_data$Dev_year_since_large >= start_of_tail &
                                            extended_claims_data$Dev_year_since_large <= end_of_tail &
                                            extended_claims_data$Entry_reserve_class == reserve_class &
                                            extended_claims_data$Calendar_year %in% years_for_pools,
                                         c("Ind_cl_payment_cal",
                                           "Exit_reserve_class",
                                           "New_annuity_1",
                                           "New_annuity_2",
                                           "New_annuity_3",
                                           "New_annuity_4",
                                           "New_annuity_5",
                                           "Dev_year_since_large",
                                           "Ind_entry_cl_reserve")]

      # If that pool is empty, create dataframe with one dummy row where each payment
      #   and new annuity is 0 and reserve class stays the same.
      if (nrow(class_pool) == 0) {
         class_pool[1,] <- c(0, reserve_class, 0, 0, 0, 0, 0, 0, 0)}


      # The tail method provides that payments decrease linear from start_of_tail to end_of_tail.
      # Here all payments are indexed to the niveau of start_of_tail.
      class_pool$Ind_cl_payment_cal <- class_pool$Ind_cl_payment_cal /
         tail_factor(class_pool$Dev_year_since_large, start_of_tail, end_of_tail)

      # Column Dev_year_since_large is no longer needed
      class_pool <- class_pool[, !names(class_pool) == "Dev_year_since_large"]

      # apply outlier limits
      class_pool$Ind_cl_payment_cal <- pmax(lower_outlier_limit,
                                            pmin(upper_outlier_limit,
                                                 class_pool$Ind_cl_payment_cal))
      # append class_pool to tail_pools
      tail_pools[[length(tail_pools) + 1]] <- class_pool
   }

   pools <- list(pretail_pools, tail_pools, pool_of_annuities)

   # Setting the parameters as attributes
   attr(pools, "reserve_classes") <- reserve_classes
   attr(pools, "years_for_pools") <- years_for_pools
   attr(pools, "start_of_tail") <- start_of_tail
   attr(pools, "end_of_tail") <- end_of_tail
   attr(pools, "lower_outlier_limit") <- lower_outlier_limit
   attr(pools, "upper_outlier_limit") <- upper_outlier_limit
   attr(pools, "threshold") <- attr(extended_claims_data, "threshold")
   attr(pools, "index_year") <- attr(extended_claims_data, "index_year")
   return(pools)
}


#' Converts pools into a dataframe
#'
#' @description
#' This function converts the first two of the three list entries of the object `pools` into an
#' easier to read dataframe.
#'
#' @param pools List as output of [generate_pools()].
#'
#' @returns Dataframe with the entries of pools plus the columns `Dev_year_since_large`
#' and `Entry_reserve_class`.
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
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = minimal_pool_of_annuities_xmpl)
#'
#' pools_as_df <- pools2dataframe(pools)
pools2dataframe <- function(pools) {
   # Initialise dataframe
   df <- data.frame(Dev_year_since_large = integer(),
                    Entry_reserve_class = integer(),
                    Ind_cl_payment_cal = numeric(),
                    Exit_reserve_class = integer(),
                    New_annuity_1 = integer(),
                    New_annuity_2 = integer(),
                    New_annuity_3 = integer(),
                    New_annuity_4 = integer(),
                    New_annuity_5 = integer(),
                    Ind_entry_cl_reserve = numeric())

   # get number of reserve classes and pretail development years
   reserve_classes <- attr(pools, "reserve_classes")
   max_reserve_class <- length(reserve_classes)
   max_dev_year <- length(pools[[1]][[1]]) + 1

   # fill dataframe with pretail pools entries
   for (reserve_class in 0:max_reserve_class) {
      for (dev_year in 2:max_dev_year) {
         df <- rbind(df,
                     data.frame(Dev_year_since_large = dev_year,
                                Entry_reserve_class = reserve_class,
                                pools[[1]][[reserve_class + 1]][[dev_year - 1]]))
      }
   }

   # fill dataframe with tail pools entries
   for (reserve_class in 0:max_reserve_class) {
      df <- rbind(df,
                  data.frame(Dev_year_since_large = max_dev_year + 1,
                             Entry_reserve_class = reserve_class,
                             pools[[2]][[reserve_class + 1]]))
   }

   return(df)
}
