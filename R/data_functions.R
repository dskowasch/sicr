#' expand dataframe historic_indices by expected future indices
#'
#' @param historic_indices dataframe, see details
#' @param first_orig_year First origin year with full history.
#' @param last_orig_year Last origin year.
#' @param index_gross_future expected future index for gross claims, default: 0
#' @param index_re_future expected future index for index clause, see details
#'
#' @details
#' **historic_indices** must be a dataframe with columns `Calendar_year` and
#' `Index_gross` and (optional) `Index_re`. \cr
#' **historic_indices** must contain one row for each historic `Calendar_year` that shall be used
#' * `Index_gross` is the claim payment development from one year to another, e.g. 0.02 for 2% increase
#' * `Index_re` is the contractually fixed claim payment development that is to be used in special index
#' clauses that are a common part of longtail xl resinsurance programs.
#' If this column is missing, it will be set to 0. \cr \cr
#'
#' The function extends the dataframe historic_indices by the column
#' index_re_future if missing and by 250 future calendar_years.
#'
#'
#' @return extended dataframe
#' @export
#'
#' @examples
#' # for a constant inflation of 2%
#' historic_indices <- data.frame(Calendar_year = 2015:2023,
#'                                Index_gross = 0.03)
#' print(historic_indices)
#' expand_historic_indices(historic_indices = historic_indices,
#'                         first_orig_year = 2015,
#'                         last_orig_year = 2023,
#'                         index_gross_future = 0.03)
expand_historic_indices <- function(historic_indices,
                                    first_orig_year,
                                    last_orig_year,
                                    index_gross_future = 0,
                                    index_re_future = 0) {
   # check historic_indices
   if (!historic_indices_ok(historic_indices, first_orig_year, last_orig_year)) {
      stop("Dataframe historic_indices is not valid.")}

   # expand dataframe by index_re if necessary, index_re is set to 0 in this case
   if (!"Index_re" %in% colnames(historic_indices)) {historic_indices$Index_re <- 0}

   # indices for 250 future calendar years are set to index_gross_future and index_re_future
   data.frame(Calendar_year = (min(historic_indices$Calendar_year):(max(historic_indices$Calendar_year) + 250)),
              Index_gross = c(historic_indices$Index_gross, rep(index_gross_future, 250)),
              Index_re = c(historic_indices$Index_re, rep(index_re_future, 250)))
}


#' adding column Transition_factor to dataframe expanded_indices
#' @description
#' The transition_factor for year j indexes a payment of year j to the niveau of payments in the fixed index_year.
#'
#' @param expanded_indices dataframe, see details
#' @param index_year desired year that the payments are indexed to
#'
#' @details
#' **expanded_indices** must be a dataframe with columns `Calendar_year` and `Index_gross` and `Index_re`. \cr
#' **expanded_indices** must contain one row for each historic `Calendar_year` that
#' shall be used plus 250 future calendar years.
#' * `Index_gross` is the claim payment development from one year to another, e.g. 0.02 for 2% increase
#' * `Index_re` is the contractually fixed claim payment development that is to be used in special index clauses that are a common part of longtail xl resinsurance programs. If this column is missing, it will be set to 0.
#' @return dataframe indices with new column Transition_factor
#' @export
#'
#' @examples
#' expanded_indices <- expand_historic_indices(data.frame(Calendar_year = 2015:2023,
#'                                                        Index_gross = 0.03),
#'                                             first_orig_year = 2015,
#'                                             last_orig_year = 2023,
#'                                             0.03,
#'                                             0.02)
#' add_transition_factor(expanded_indices, index_year = 2022)
add_transition_factor <- function(expanded_indices, index_year) {
   expanded_indices$Transition_factor <- cumprod(1 + expanded_indices$Index_gross) /
         cumprod(1 + expanded_indices$Index_gross)[expanded_indices$Calendar_year == index_year]
   attr(expanded_indices, "index_year") <- index_year
   return(expanded_indices)
}


#' Generate claims list
#'
#' @description
#' The prepared data will be reduced to claims with origin year between `first_orig_year` and `last_orig_year`
#' and to the calendar year `last_orig_year`. Only necessary columns will be returned.
#'
#'
#' @param extended_claims_data prepared dataframe, see details of [prepare_data()].
#' @param last_orig_year Integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year just ended.
#' @param first_orig_year Integer value for the first origin year with complete history.
#'
#' @return dataframe with current large claim status
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
#' claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                     first_orig_year = 1989,
#'                                     last_orig_year = 2023)
#' print(claims_list)
#'
#' @importFrom dplyr "%>%" filter select
generate_claims_list <- function(extended_claims_data, first_orig_year, last_orig_year){
   # Setting arguments to NULL to prevent note in CMD check
   Calendar_year <- Origin_year <- NULL

   claims_list <- extended_claims_data %>%
      filter(Calendar_year == last_orig_year,
             Origin_year >= first_orig_year) %>%
      select("Claim_id",
             "Origin_year",
             "Exit_reserve_class",
             "Dev_year_since_large",
             "Dev_year_of_growing_large",
             "Large_since",
             "Cl_reserve",
             "Ind_cl_reserve") %>%
      as.data.frame()
      return(claims_list)
}


#' Reduce claims data to possible large claims
#'
#' @description
#' The original dataframe is reduced to possible large claims to save computing time in the next steps. \cr \cr
#' This is not yet a precise separation as the indexed columns are missing at this point. \cr \cr
#' **This function resembles only step 1 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param claims_data dataframe, see details of [prepare_data()].
#' @param indices dataframe, see details of [prepare_data()].
#' @param threshold numeric value to separate small from large claims, see details of [prepare_data()].
#'
#' @return subset of dataframe `claims_data` where preselected small claims have been eliminated
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' reduce_data(minimal_claims_data_xmpl, indices_xmpl, threshold = 400000)
#'
#' @importFrom data.table as.data.table ":="
#' @importFrom dplyr "%>%" pull arrange filter
reduce_data <- function(claims_data, indices, threshold) {
   # Setting arguments to NULL to prevent note in CMD check
   Origin_year <- Claim_id <- Calendar_year <- Cl_payment_cal <- An_payment_cal <-
      Cl_reserve <- An_reserve <- . <- cum_payments <- cum_incurred <- NULL

   if (NROW(claims_data) < 1) {return(claims_data)}

   # find minimal indexed threshold
   minimal_threshold <- claims_data %>%
      pull(Calendar_year) %>%
      unique() %>%
      match(indices$Calendar_year) %>%
      min(indices$Transition_factor[.]) * threshold

   # identify claims that have at least once exceeded minimal_threshold
   large_claim_ids <- claims_data %>%
      arrange(Origin_year, Claim_id, Calendar_year) %>%
      as.data.table() %>%
      .[, cum_payments := cumsum(Cl_payment_cal + An_payment_cal), by = Claim_id] %>%
      .[, cum_incurred := cum_payments + pmax(0, Cl_reserve + An_reserve)] %>%
      .[cum_incurred > minimal_threshold] %>%
      as.data.frame() %>%
      pull(Claim_id) %>%
      unique()

   # filter claims_data to these claims
   claims_data <- claims_data %>%
      filter(Claim_id %in% large_claim_ids) %>%
      as.data.frame()

   return(claims_data)
}


#' Add missing rows
#'
#' @description
#' `claims_data` is requested to only contain rows in which one of `Cl_reserve`, `An_reserve`, `Cl_payment_cal` or `An_payment_cal` is not equal to 0. \cr
#' This step adds rows so that claims_data contains one row per calendar year between the origin year and the last origin year for each claim. \cr \cr
#' **This function resembles only step 2 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param claims_data dataframe after step 1, see details of [prepare_data()].
#' @param last_orig_year Integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year just ended, see details of [prepare_data()].
#'
#' @return dataframe with added rows
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023)
#'
#' @importFrom dplyr "%>%" select distinct left_join mutate across arrange
add_missing_years <- function(claims_data,  last_orig_year){

   if (NROW(claims_data) < 1) {return(claims_data)}

   # 1. create dataframe with all Claim_ids and corresponding Origin_year
   claims_list <- claims_data %>%
      select("Claim_id", "Origin_year") %>%
      distinct()

   # 2. create one list for each claim with every year between claim's origin year and last_orig_year
   cal_years <- Map(seq.default, claims_list$Origin_year, last_orig_year)

   # 3. create dataframe with Claim_id, Origin_year and Calendar_years...
   extended_claims_data <- data.frame(Claim_id = rep(claims_list$Claim_id, lengths(cal_years)),
                                      Origin_year = rep(claims_list$Origin_year, lengths(cal_years)),
                                      Calendar_year = unlist(cal_years)) %>%
      # ... and join with claims_data to fill up missing years
      left_join(claims_data, by = c("Claim_id", "Origin_year", "Calendar_year")) %>%
      # NAs must be set to 0
      mutate(across(c("Cl_payment_cal", "Cl_reserve", "An_payment_cal", "An_reserve"),
                    ~ replace(.x, is.na(.x), 0))) %>%
      arrange("Origin_year", "Claim_id", "Calendar_year") %>%
      as.data.frame()
      return(extended_claims_data)
}

#' Add columns
#'
#' @description
#' This step adds derived columns to `claims_data` that are needed for further operations. \cr \cr
#' **This function resembles only step 3 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param extended_claims_data dataframe after step 2, see details of [prepare_data()].
#'
#' @return dataframe with added columns
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023) %>%
#'    add_columns()
#'
#' @importFrom dplyr "%>%" arrange group_by mutate ungroup lag
add_columns <- function(extended_claims_data) {
   # Setting variables to NULL to prevent note in CMD check
   Origin_year <-  Claim_id <-  Calendar_year <-  Development_year <-
      Payment_cal <- Cl_payment_cal <- An_payment_cal <- Reserve <-
      Cl_reserve <- An_reserve <- Cl_payment_cum <- An_payment_cum <-
      Payment_cum <- Cl_incurred <- An_incurred <- Incurred <-
      Entry_cl_reserve <- Entry_an_reserve <- Entry_reserve <- NULL
   extended_claims_data <- extended_claims_data %>%
      # extended_claims_data must be sorted for cumulation
      arrange(Origin_year, Claim_id, Calendar_year) %>%
      # add columns
      mutate(Development_year = Calendar_year - Origin_year + 1,
             Payment_cal = Cl_payment_cal + An_payment_cal,
             Reserve = Cl_reserve + An_reserve) %>%
      # grouping for cumsum
      group_by(Claim_id, Origin_year) %>%
      mutate(Cl_payment_cum = cumsum(Cl_payment_cal),
             An_payment_cum = cumsum(An_payment_cal),
             Payment_cum = Cl_payment_cum + An_payment_cum,
             Cl_incurred = Cl_payment_cum + Cl_reserve,
             An_incurred = An_payment_cum + An_reserve,
             Incurred = Payment_cum + Reserve) %>%
      ungroup() %>%
      # Entry reserve is 0 in Origin_year and previous years' value of Cl_reserve else.
      mutate(Entry_cl_reserve = ifelse(Calendar_year == Origin_year, 0, lag(Cl_reserve)),
             Entry_an_reserve = ifelse(Calendar_year == Origin_year, 0, lag(An_reserve)),
             Entry_reserve = Entry_cl_reserve + Entry_an_reserve) %>%
      as.data.frame()
      return(extended_claims_data)
}


#' Add indexed columns
#'
#' @description
#' This step uses the dataframe indices to index every payment and every reserve to the index year. \cr \cr
#' **This function resembles only step 4 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param extended_claims_data dataframe after step 3, see details of [prepare_data()].
#' @param indices dataframe, see details of [prepare_data()].
#'
#' @return dataframe with added columns
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023) %>%
#'    add_columns() %>%
#'    add_indexed_columns(indices_xmpl)
#'
#' @importFrom dplyr "%>%" arrange mutate lag group_by ungroup select
add_indexed_columns <- function(extended_claims_data, indices) {
   # Setting variables to NULL to prevent note in CMD check
   Origin_year <- Claim_id <- Calendar_year <- index_factors <- Cl_payment_cal <-
      An_payment_cal <- Ind_cl_payment_cal <- Ind_an_payment_cal <- Cl_reserve <-
      An_reserve <- Ind_cl_reserve <- Ind_an_reserve <- Ind_entry_cl_reserve <-
      Ind_entry_an_reserve <- Ind_cl_payment_cum <- Ind_an_payment_cum <-
      Ind_cl_incurred <- Ind_an_incurred <- NULL

   extended_claims_data <- extended_claims_data %>%
      # extended_claims_data must be sorted for cumulation
      arrange(Origin_year, Claim_id, Calendar_year) %>%
      # zu jedem Calendar_year aus extended_claims_data wird der passende Transition_factor aus indices rausgesucht
      mutate(index_factors = indices$Transition_factor[match(Calendar_year, indices$Calendar_year)],
             Ind_cl_payment_cal = ifelse(Cl_payment_cal == 0, 0, Cl_payment_cal / index_factors),
             Ind_an_payment_cal = ifelse(An_payment_cal == 0, 0, An_payment_cal / index_factors),
             Ind_payment_cal = Ind_cl_payment_cal + Ind_an_payment_cal,
             Ind_cl_reserve = ifelse(Cl_reserve == 0, 0, Cl_reserve / index_factors),
             Ind_an_reserve = ifelse(An_reserve == 0, 0, An_reserve / index_factors),
             Ind_entry_cl_reserve = ifelse(Calendar_year == Origin_year, 0, lag(Ind_cl_reserve)),
             Ind_entry_an_reserve = ifelse(Calendar_year == Origin_year, 0, lag(Ind_an_reserve)),
             Ind_entry_reserve = Ind_entry_cl_reserve + Ind_entry_an_reserve,
             Ind_reserve = Ind_cl_reserve + Ind_an_reserve) %>%
      # grouping for cumsum
      group_by(Claim_id) %>%
      mutate(Ind_cl_payment_cum = cumsum(Ind_cl_payment_cal),
             Ind_an_payment_cum = cumsum(Ind_an_payment_cal),
             Ind_payment_cum = Ind_cl_payment_cum + Ind_an_payment_cum,
             Ind_cl_incurred = Ind_cl_payment_cum + Ind_cl_reserve,
             Ind_an_incurred = Ind_an_payment_cum + Ind_an_reserve,
             Ind_incurred = Ind_cl_incurred + Ind_an_incurred) %>%
      ungroup() %>%
      # delete helper column
      select(-index_factors) %>%
      as.data.frame()

   attr(extended_claims_data, "index_year") <- attr(indices, "index_year")
   return(extended_claims_data)
}


#' Filtering extended_claims_data to large claims and add derived columns
#'
#' @description
#' The indexed columns allow for the exact calculation of __which__ claim has become large and __when__.
#' Claims that have not become large yet are eliminated. \cr \cr
#' **This function resembles only step 5 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param extended_claims_data dataframe after step 4, see details of [prepare_data()].
#' @param threshold numeric value to separate small from large claims, see details of [prepare_data()].
#' @param first_orig_year Integer value for the first origin year for which a full claim history is available, see details of [prepare_data()].
#' @param expected_year_of_growing_large Integer Value for claims without complete history, see details, see details of [prepare_data()].
#'
#' @return dataframe filtered to large claims
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' new_data <- minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023) %>%
#'    add_columns() %>%
#'    add_indexed_columns(indices_xmpl) %>%
#'    filter_large_claims(400000, 1989, 3)
#' head(new_data)
#' @importFrom dplyr "%>%" filter pull group_by summarise left_join mutate arrange
filter_large_claims <- function(extended_claims_data,
                                threshold,
                                first_orig_year,
                                expected_year_of_growing_large = 3) {

   if (NROW(extended_claims_data) < 1) {
      extended_claims_data <- mutate(extended_claims_data,
                                     Large_since = integer(0),
                                     Dev_year_of_growing_large = integer(0),
                                     Dev_year_since_large = integer(0))
      return(extended_claims_data)
   }

   # Setting variables to NULL to prevent note in CMD check
   Ind_incurred <- Claim_id <- Calendar_year <- Large_since <- Origin_year <- NULL
   # Generate claim_ids list of claims which have at least once exceeded the threshold
   large_claim_id_list <- extended_claims_data %>%
      filter(Ind_incurred > threshold) %>%
      pull(Claim_id) %>%
      unique()

   # dataframe large_since assigns first calendar year as large claim to each claimd_id of large_claim_id_list
   large_since <- extended_claims_data %>%
      filter(Claim_id %in% large_claim_id_list, Ind_incurred > threshold) %>%
      group_by(Claim_id) %>%
      summarise(Large_since = min(Calendar_year))

   # this information is joined to extended_claims_data
   extended_claims_data <- extended_claims_data %>%
      left_join(large_since, by = "Claim_id") %>%
      filter(!is.na(Large_since)) %>% #large claims are filtered here
      # create derived columns
      mutate(
         Large_since = ifelse(Origin_year < first_orig_year, Origin_year + expected_year_of_growing_large - 1, Large_since),
         Dev_year_of_growing_large = Large_since - Origin_year + 1,
         Dev_year_since_large = Calendar_year - Large_since + 1
      ) %>%
      arrange(Origin_year, Claim_id, Calendar_year) %>% # Order could have been changed here
      as.data.frame()
   attr(extended_claims_data, "threshold") <- threshold
   return(extended_claims_data)
}


#' Adding reserve classes to extended_claims_data
#'
#' @description
#' The columns `Entry_reserve_class` and `Exit_reserve_class` can now be derived from `Ind_entry_cl_reserve` and `Ind_cl_reserve`. \cr \cr
#'
#' **This function resembles only step 6 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param extended_claims_data dataframe after step 5, see details of [prepare_data()].
#' @param reserve_classes numeric vector to specify reserve classes at the niveau of index_year, see details of [prepare_data()].
#'
#' @return dataframe with entry and exit reserve classes
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' new_data <- minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023) %>%
#'    add_columns() %>%
#'    add_indexed_columns(indices_xmpl) %>%
#'    filter_large_claims(400000, 1989, 3) %>%
#'    add_classes(c(1, 200001, 400001, 700001, 1400001))
#' head(new_data)
#' @importFrom dplyr "%>%" mutate
add_classes <- function(extended_claims_data, reserve_classes) {
   # Setting variables to NULL to prevent note in CMD check
   Ind_entry_cl_reserve <- Ind_cl_reserve <- NULL
   extended_claims_data <- extended_claims_data %>%
      mutate(Entry_reserve_class = findInterval(Ind_entry_cl_reserve, reserve_classes),
             Exit_reserve_class = findInterval(Ind_cl_reserve, reserve_classes)) %>%
      as.data.frame()
   attr(extended_claims_data, "reserve_classes") <- reserve_classes
   return(extended_claims_data)
}


#' Attach future annuities to extended_claims_data
#'
#' @description
#' The columns `New_annuity_1` to `New_annuity_5` are added to the dataframe. If new annuities have been agreed on
#' in the calendar year and the claim of the row, these columns contain the row number of these annuities in the corresponding
#' dataframe `pool_of_annuities`. These columns are needed for the pool as the sampling shall consider claim payments and
#' new reserve classes as well as new annuities. \cr \cr
#'
#' The function always adds __five__ columns for new annuities which is sufficient for the rare case that five new annuities are agreed on
#' for one single claim in one calendar year. Each further new annuity in one year will be ignored, which could lead to a slight underestimation
#' of the best estimate. To avoid this, data may be manually adjusted, for example by moving further annuities to another calendar year or
#' by aggregating annuities. \cr \cr
#'
#' **This function resembles only step 7 of the data preparation process, see description in details of [prepare_data()]**
#'
#' @param extended_claims_data dataframe after step 6, see details of [prepare_data()].
#' @param pool_of_annuities dataframe containing one row for each annuity that has ever been agreed on, see details of [prepare_data()].
#'
#' @return prepared dataframe
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' library(dplyr)
#' new_data <- minimal_claims_data_xmpl %>%
#'    reduce_data(indices_xmpl, 400000) %>%
#'    add_missing_years(2023) %>%
#'    add_columns() %>%
#'    add_indexed_columns(indices_xmpl) %>%
#'    filter_large_claims(400000, 1989, 3) %>%
#'    add_classes(c(1, 200001, 400001, 700001, 1400001)) %>%
#'    attach_future_annuities(minimal_pool_of_annuities_xmpl)
#' head(new_data)
#' @importFrom dplyr "%>%" group_by mutate row_number ungroup summarise across starts_with left_join
attach_future_annuities <- function(extended_claims_data, pool_of_annuities = NULL){
   # Setting variables to NULL to prevent note in CMD check
   Claim_id <- Entering_year <- new_annuity_number <- NULL

   # if extended_claims_data is empty, only the empty columns will be added
   if (NROW(extended_claims_data) < 1) {
      for (i in 1:5) {
         extended_claims_data[paste0("New_annuity_", i)] <- numeric(0)
      }
      return(extended_claims_data)
   }

   # if pool_of_annuities is empty or NULL, extended_claims_data is returned with five new dummy-columns.
   if (is.null(pool_of_annuities) | NROW(pool_of_annuities) < 1) {
      extended_claims_data[paste0("New_annuity_", 1:5)] <- 0
      return(extended_claims_data)}

   # The number of new annuities for one claim in one year can be greater than 1.
   # Therefore new annuities per claim and year are numbered consecutively.
   pool_of_annuities <- pool_of_annuities %>%
      group_by(Claim_id, Entering_year) %>%
      mutate(new_annuity_number = row_number()) %>%
      ungroup()

   # only five new annuities per claim and per calendar year are used
   if (max(pool_of_annuities$new_annuity_number) > 5) {
      warning("pool_of_annuities contains claims with more than five new annuities in one calendar year.
              Only the first five of them are used, see details of attach_future_annuities function.")
   }

   # The columns 'New_annuity_1' to 'New_annuity_5' are attached to pool_of_annuities.
   # They contain the new annuity's row number in pool_of_annuities.
   for (i in 1:5) {
      pool_of_annuities <- pool_of_annuities %>%
         mutate(!!paste0("New_annuity_", i) := (new_annuity_number == i) * row_number())
   }

   # Delete helper column
   pool_of_annuities$new_annuity_number <- NULL

   # For the join with extended_claims_data, pool_of_annuities is reduced to 'Claim_id', #
   # 'Entering_year' and 'New_annuity_1...5' and summarized to one row per claim and year".
   pool_of_annuities <- pool_of_annuities %>%
      group_by(Claim_id, Entering_year) %>%
      summarise(across(starts_with("New_annuity_"),
                       sum,
                       .names = "{.col}"),
                .groups = "drop") %>%
      ungroup() %>%
      as.data.frame()

   # join extended_claims_data and pool_of_annuities to attach 'New_annuitiy_1...5' to extended_claims_data
   extended_claims_data <- extended_claims_data %>%
      left_join(pool_of_annuities,
                by = c("Claim_id" = "Claim_id", "Calendar_year" = "Entering_year")) %>%
      mutate(across(starts_with("New_annuity_"), ~ replace(.x, is.na(.x), 0)))
   attr(extended_claims_data, "pool_of_annuities") <- pool_of_annuities
   return(extended_claims_data)
}


#' Prepare data for further usage
#'
#' @description
#' The function filters large claims and adds columns that are needed for generating pools and for sampling. \cr
#' It summarises seven subfunctions, see details for detailed description of parameters and subfunctions.
#'
#' @param claims_data dataframe, see details.
#' @param indices dataframe, see details.
#' @param threshold numeric value, see details.
#' @param first_orig_year integer or numeric value, see details.
#' @param last_orig_year integer or numeric value, see details.
#' @param expected_year_of_growing_large integer or numeric value, see details.
#' @param reserve_classes numeric vector, see details.
#' @param pool_of_annuities dataframe, see details.
#'
#' @details
#' Data preparation before applying `sicr` is done in seven steps. The subfunctions for each step can be used separately.
#' This allows users to skip steps in order to use own implementations instead. \cr \cr
#'
#' ## Description of needed input data
#' * `first_orig_year` \cr
#' Integer value for the first origin year for which a full claim history is available. \cr
#' Information from older claims for which history is only available from `first_orig_year` to today may though be considered for the pools, see `claims_data`.
#'
#' * `last_orig_year` \cr
#' Integer value for the last origin year which will also be treated as the last calendar year, usually the calendar year just ended.
#'
#' * `claims_data` \cr
#' For each claim that could exceed the threshold after indexation this dataframe must contain one row
#' for every calendar year in which at least one of `Cl_reserve`, `An_reserve`, `Cl_payment_cal` or
#' `An_payment_cal` is not equal to 0. \cr \cr
#' To consider older claims for which history is only available from `first_orig_year` until now, add one row for this claim for calendar year `first_orig_year - 1`
#' and fill `Cl_payment_cal` and `An_payment_cal` with the cumulated payments up to that year and `Cl_reserve` and `An_reserve` with the
#' reserve at the end of `first_orig_year - 1`. This allows for checking if a claim belongs to large or small claims,
#' but due to the missing first part of the history the Dev_year_of_growing_large (see step 5) can't be determined for these claims. Dev_year_of_growing_large
#' has to be estimated instead (see parameter expected_year_of_growing_large). \cr \cr
#' claims_data must consist of the following columns:
#'    + `Claim_id` \cr type: character. Each claim must be assigned a unique claim id. These must correspond to the
#'    claim id's in the dataframe `pool_of_annuities`.
#'    + `Origin_year` \cr type: integer or numeric. Origin year of the claim.
#'    + `Calendar_year` \cr type: integer or numeric. Calendar year of payments and reserves of the claim.
#'    + `Cl_payment_cal` \cr type: numeric. Claim payments made in this calendar year without annuity payments.
#'    + `Cl_reserve` \cr type: numeric. Claim reserve at the end of this calendar year without annuity reserves.
#'    + `An_payment_cal` \cr type: numeric. Annuity payments made in this calendar year.
#'    + `An_reserve` \cr type: numeric. Annuity reserve at the end of this calendar year.
#'
#' * `pool_of_annuities` \cr
#' This dataframe contains one row for each annuity that has ever been agreed on regardless of whether the annuity is still active or not. \cr \cr
#' `pool_of_annuities` must consist of the following columns:
#'    + `Claim_id` \cr type: character. Each claim must be assigned a unique claim id. These must correspond to the
#'    claim id's in the dataframe `claims_data`.
#'    + `Annuity_id` \cr type: character. Annuity ID for the case of multiple annuities in one claim.
#'    + `Origin_year` \cr type: integer or numeric. Origin year of the claim.
#'    + `Calendar_year` \cr type: integer or numeric. Calendar year of information.
#'    + `Entering_year` \cr type: integer or numeric. Year in which insurer and recipient have agreed on the annuity.
#'    + `Annuity_start` \cr type: integer or numeric. Year in which payment starts, may be a past or a future year.
#'    + `Annuity_end` \cr type: integer or numeric. Year in which payment ends, may be a past or a future year.
#'    + `Birth_year` \cr type: integer or numeric. Birth year of recipient for assigning survival probability from mortality tables.
#'    + `Gender` \cr type: character "m" or "f". Gender of recipient for assigning mortality table.
#'    + `Annual_payment` \cr type: numeric. Agreed annual payment.
#'    + `Dynamic` \cr type: numeric. If a dynamic increase of the annual payment is part of the agreement, it can be specified here, e.g. 0.02 for 2% annual payment increasement.
#'
#' * `indices` \cr
#' This dataframe contains one row for each historic calendar year and 250 future calendar years. \cr
#' Although the indices for the future calendar years (as well as the column `Index_re`) are not needed here but in the subsequent simulation,
#' it is required to create all rows and all columns of this dataframe just once with the helper functions.
#' [expand_historic_indices()] and [add_transition_factor()]. \cr \cr
#' `indices` must consist of the following columns:
#'    + `Calendar_year` \cr type: integer or numeric.
#'    + `Index_gross` \cr type: numeric. Claim payment development from one year to another, e.g. 0.02 for 2% increase
#'    + `Index_re` \cr type: numeric. Contractually fixed claim payment development that is to be used in special index
#'    clauses that are a common part of longtail xl resinsurance programs.
#'    + `Transition_factor` \cr type: numeric. The transition_factor for year j indexes a payment of year j to the niveau
#'    of payments in the fixed index_year.
#'
#' * `threshold` \cr
#' Numeric value to separate small from large claims. `Threshold` has to be stated at the niveau of index_year.
#'
#' * `reserve_classes` \cr numeric vector to specify reserve classes at the niveau of index_year, for example
#' `c(1, 1001)` for the three reserve classes `(-Inf, 1)`, `[1, 1001)` and `[1001, Inf)`.
#'
#' * `expected_year_of_growing_large` \cr
#' Integer value for the estimated development year of becoming large for claims without full history (see description above). Default: 3.
#'
#'
#' ## Step 1 - Reducing data
#' `claims_data` is reduced to possible large claims.
#'
#' ## Step 2 - Add missing rows
#' `claims_data` is requested to only contain rows in which one of `Cl_reserve`, `An_reserve`, `Cl_payment_cal` or `An_payment_cal` is not equal to 0. \cr
#' This step adds rows so that claims_data contains one row per calendar year between the origin year and the last origin year for each claim.
#'
#' ## Step 3 - Add columns
#' This step adds derived columns to `claims_data` that are needed for further operations:
#' * `Development_year`
#' * Sum of claim and annuity payments and reserve
#'    + `Payment_cal` as the sum of `Cl_payment_cal` and `An_payment_cal`
#'    + `Reserve` as the sum of `Cl_reserve` and `An_reserve`
#' * cumulated columns
#'    + `Cl_payment_cum` as the cumulated claim payments
#'    + `An_payment_cum` as the cumulated annuity payments
#'    + `Payment_cum` as the sum of the two latter
#' * incurred columns
#'    + `Cl_incurred` as the sum of `Cl_payment_cum` and `Cl_reserve`
#'    + `An_incurred` as the sum of `An_payment_cum` and `An_reserve`
#'    + `Incurred` as the sum of the two latter
#' * Entry reserve columns
#'    + `Entry_cl_reserve` as the `Cl_reserve` of the beginning of the year
#'    + `Entry_an_reserve` as the `An_reserve` of the beginning of the year
#'    + `Entry_reserve` as the sum of the two latter
#' ## Step 4 - Add indexed columns
#' This step uses the dataframe indices to index every payment and every reserve to the indexation year. \cr \cr
#' The following columns are calculated:
#' * indexed payments
#'    + `Ind_cl_payment_cal`
#'    + `Ind_an_payment_cal`
#'    + `Ind_payment_cal` as the sum of the two latter
#' * indexed reserves
#'    + `Ind_cl_reserve`
#'    + `Ind_an_reserve`
#'    + `Ind_reserve` as the sum of the two latter
#' * indexed entry reserves
#'    + `Ind_entry_cl_reserve`
#'    + `Ind_entry_an_reserve`
#'    + `Ind_entry_reserve` as the sum of the two latter
#' * cumulated columns
#'    + `Ind_cl_payment_cum`
#'    + `Ind_an_payment_cum`
#'    + `Ind_payment_cum` as the sum of the two latter
#'    + `Ind_cl_incurred`
#'    + `Ind_an_incurred`
#'    + `Ind_incurred` as the sum of the two latter
#'
#' ## Step 5 - Filtering large claims
#' The indexed columns allow for the exact calculation of __which__ claim has become large and __when__.
#' Claims that have not become large yet are eliminated. \cr \cr
#' The following new columns are derived from this information:
#' * `Large_since` as the calendar year in which the claim exceeded the threshold for the first time
#' * `Dev_year_of_growing_large` as the development year in which the claim exceeded the threshold for the first time
#' * `Dev_year_since_large` as the "new" development year where counting starts in year `Large_since` instead of the origin year
#'
#' ## Step 6 - Add reserve classes
#' The columns `Entry_reserve_class` and `Exit_reserve_class` can now be derived from `Ind_entry_cl_reserve` and `Ind_cl_reserve`.
#'
#' ## Step 7 - Attach future annuities
#' The columns `New_annuity_1` to `New_annuity_5` are added to the dataframe. If new annuities have been agreed on
#' in the calendar year and the claim of the row, these columns contain the row number of these annuities in the corresponding
#' dataframe `pool_of_annuities`. These columns are needed for the pool as the sampling shall consider claim payments and
#' new reserve classes as well as new annuities. \cr \cr
#'
#' The function always adds __five__ columns for new annuities which is sufficient for the rare case that five new annuities are agreed on
#' for one single claim in one calendar year. Each further new annuity in one year will be ignored, which could lead to a slight underestimation
#' of the best estimate. To avoid this, data may be manually adjusted, for example by moving further annuities to another calendar year or
#' by aggregating annuities.
#'
#'
#' @return dataframe of extended claims data filtered to large claims
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' new_data <- prepare_data(claims_data = minimal_claims_data_xmpl,
#'                          indices = indices_xmpl,
#'                          threshold = 400000,
#'                          first_orig_year = 1989,
#'                          last_orig_year = 2023,
#'                          expected_year_of_growing_large = 3,
#'                          reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                          pool_of_annuities = minimal_pool_of_annuities_xmpl)
#' head(new_data)
#'
#' @importFrom dplyr "%>%"
prepare_data <- function(claims_data,
                         indices,
                         threshold,
                         first_orig_year,
                         last_orig_year,
                         expected_year_of_growing_large = 3,
                         reserve_classes,
                         pool_of_annuities = NULL) {
   ### Data checks
   # claims_data
   if (!claims_data_ok(claims_data, last_orig_year)) {
      stop("Dataframe claims_data is not valid.")
   }

   # indices
   if (!indices_ok(indices, first_orig_year, last_orig_year)) {
      stop("Dataframe indices is not valid.")
   }

   # pool of annuities
   if (!pool_of_annuities_ok(pool_of_annuities)) {
      stop("Dataframe pool_of_annuities is not valid.")
   }

   claims_data %>%
      reduce_data(indices, threshold) %>%
      add_missing_years(last_orig_year) %>%
      add_columns() %>%
      add_indexed_columns(indices) %>%
      filter_large_claims(threshold, first_orig_year, expected_year_of_growing_large) %>%
      add_classes(reserve_classes) %>%
      attach_future_annuities(pool_of_annuities)
}
