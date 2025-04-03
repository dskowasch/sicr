#' Helper function to create a matrix with indices per claim stemming from reinsurance treaty clause
#'
#' @description
#' This helper function converts the dataframes `reinsurance` and `indices` into a matrix with
#' one row per claim and one column for each past year from `first_orig_year` to `last_orig_year` and
#' for 250 future calendar years. \cr
#' This matrix will be used to properly calculate the reinsurance payments.
#'
#'
#' @param claims_list dataframe of claims generated with [generate_claims_list()].
#' @param reinsurance dataframe of reinsurance structures, see details of [xl_cashflow()].
#' @param indices dataframe for indexation, see details of [prepare_data()].
#' @param first_orig_year integer value for the first origin year, will be treated as the first observed calendar year.
#' @param last_orig_year integer value for the last origin year which will also be treated as the last calendar year,
#' usually the calendar year that just ended.
#'
#' @return numeric matrix with one row per claim and one column for each past and 250 future calendar years. Entries
#' for years before the claim origin year are set to 1. The other entries are indexation factors.
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package
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
#' print(reinsurance_xmpl)
#'
#' # show only first 20 columns
#' generate_xl_indices_per_claim(claims_list = large_claims_list,
#'                               reinsurance = reinsurance_xmpl,
#'                               indices = indices_xmpl,
#'                               first_orig_year = 1989,
#'                               last_orig_year = 2023)[,1:20]
#'
generate_xl_indices_per_claim <- function(claims_list,
                                          reinsurance,
                                          indices,
                                          first_orig_year,
                                          last_orig_year){
   # if reinsurance contains rows for younger origin years that last_orig_year, these rows must be deleted
   reinsurance <- reinsurance[reinsurance$Origin_year <= last_orig_year,]

   number_of_rows <- last_orig_year - first_orig_year + 1
   number_of_calendar_years <- NROW(indices)
   xl_indices_matrix <- matrix(nrow = number_of_rows,
                               ncol = number_of_calendar_years)
   for (current_row in 1:number_of_rows) {
      row_in_reinsurance_df <- which(reinsurance$Origin_year == (first_orig_year + current_row - 1))
      # throw error if there is no row for that origin year in reinsurance
      if (length(row_in_reinsurance_df) == 0) {
         stop(paste("No row in dataframe reinsurance for origin year", first_orig_year + current_row - 1))
      }
      indexation_year <- reinsurance$Base_year[row_in_reinsurance_df]
      margin_type <- reinsurance$Margin_type[row_in_reinsurance_df]
      if (margin_type == "FIC") {
         margin_type <- "APK"
         margin <- 0
      } else {
         margin <- reinsurance$Margin[row_in_reinsurance_df]}

      if (indexation_year == 0 | margin_type == "none") {
         xl_indices_matrix[current_row,] <- rep(1, number_of_calendar_years)
      } else {
         cum_indices <- cumprod((1 + indices$Index_re)[which(indices$Calendar_year == indexation_year):NROW(indices)])
         if (margin_type == "APK") {
            xl_indices_matrix[current_row,] <- c(rep(1, indexation_year - indices$Calendar_year[1]),
                                                 ifelse(cum_indices >= (1 + margin),
                                                        cum_indices,
                                                        1))
         }

         if (margin_type == "SIC") {
            xl_indices_matrix[current_row,] <- c(rep(1, indexation_year - indices$Calendar_year[1]),
                                                 ifelse(cum_indices >= (1 + margin),
                                                        cum_indices/(1 + margin),
                                                        1))
         }
      }
   }

   rownames(xl_indices_matrix) <- first_orig_year:last_orig_year
   colnames(xl_indices_matrix) <- indices$Calendar_year

   # In some cases the needed origin years are older than first_orig_year. Those years have been used to generate
   # the xl_indices_matrix, but must now be deleted.
   xl_indices_per_claim <- xl_indices_matrix[c(as.character(claims_list$Origin_year)),
                                             colnames(xl_indices_matrix) >= first_orig_year,
                                             drop = FALSE]

   rownames(xl_indices_per_claim) <- claims_list$Claim_id
   return(xl_indices_per_claim)
}



#' Calculate reinsurer's payments for xl-treaty
#'
#' @description
#' Future and historic payments are binded per claim to calculate the reinsurer's payments. \cr
#' The marketwide very common indexation clause makes this slightly complicated, so
#' read the details section for further information.
#'
#' @param future_payments numeric matrix with one row per claim and 250 columns for future payments.
#' @param claims_list dataframe of claims generated with [generate_claims_list()].
#' @param history numeric matrix with one row per claim with historic payments, generated by [generate_history_per_claim()]. \cr
#' If generated in other ways, note that colnames must be the origin years.
#' @param reinsurance dataframe of reinsurance structures, see details.
#' @param indices dataframe for indexation, default NULL. See details of [prepare_data()].
#' Only one of `indices` and `xl_indices_per_claim` may be NULL.
#' @param xl_indices_per_claim numeric indexation matrix, default NULL. See details of [generate_xl_indices_per_claim()].
#' Only one of `indices` and `xl_indices_per_claim` may be NULL.
#' @param output desired output as character, one of `history`, `future` and `total`.
#'
#' @details
#'
#' This function uses historic and future payments per claim to derive the reinsurer's payments for the xl-treaty. \cr
#' The calculation becomes rather complicated as there are several common indexation clauses (also referred to as
#' Stability Clause or Inflation Clause), all of them with the idea of sharing the increase of claims payments due
#' to inflation during the long settlement of the claims between cedent and reinsurer. \cr
#' The basic types of clauses can be divided into European Index Clauses (EIC) and London Market Indexation Clause
#' (LMIC). This package does **not** support LMIC, but the most important EIC clauses! \cr \cr
#' For a layer C xs D and a large claim with payments \eqn{p_{i}} for development year i = 1,..., T and
#' reserves \eqn{r_{i}} at the end of development year i we define \eqn{a_{i} = z_{1} + … + z_{i} + r_{i}} as the
#' claims incurred at the end of development year i. For a given index series we call \eqn{I_{B}} the index for
#' base year B and \eqn{I_{i}} the index for development year i. The indexation clauses that are supported by this
#' package define the correction factors as follows: \cr \cr
#' 1. __APK__ (for german `Anpassungsklausel`) with margin \eqn{\alpha} (with \eqn{\alpha = 10%} in many cases):
#' \deqn{
#' f_{i} :=
#' \left\{
#'    \begin{array}{r}
#'       1 \qquad if \quad \frac{I_{i}}{I_{B}} < 1 + \alpha \\
#'       \frac{I_{B}}{I_{i}} \qquad if \quad  \frac{I_{i}}{I_{B}} \ge 1 + \alpha
#'    \end{array}
#' \right.
#' }
#' 2. __SIC__ (for `Severe Inflation Clause`) with margin \eqn{\alpha} (with \eqn{\alpha = 30%} in many cases):
#' \deqn{
#' f_{i} :=
#' \left\{
#'    \begin{array}{r}
#'       1 \qquad if \quad \frac{I_{i}}{I_{B}} < 1 + \alpha \\
#'       \frac{I_{B} \cdot (1+\alpha)}{I_{i}} \qquad if \quad  \frac{I_{i}}{I_{B}} \ge 1 + \alpha
#'    \end{array}
#' \right.
#' }

#' 3. __FIC__ (for `Full` or `Franchise Inflation Clause`):
#' \deqn{f_{i} := \frac{I_{B}}{I_{i}}}
#' In some cases FIC is also defined with a margin. But unlike in APK and SIC as soon as the cumulated index exceeds
#' the margin the index is applied to the former payments as well. As this happens in most claims in longtail lines of
#' business, only FIC without margin is implemented in this package which should be a reasonable simplification.
#'
#' With the payments and the latest reserve a stabilization factor per claim and per development year can be calculated by:
#' \deqn{F_{i} = \frac{z_{1} + \cdots + z_{i} + r_{i}}{f_{1}z_{1} + \cdots + f_{i}z_{i} + f_{i}r_{i}}}
#' and this leads to a stabilized prioririty and limit per claim and development year:
#' \deqn{C_{i}^{stab} \ xs \ D_{i}^{stab} \quad with \quad C_{i}^{stab} = F_{i}
#' \cdot C \ and \ D_{i}^{stab} = F_{i} \cdot D.}
#' __Note that this package does not forecast reserves (only reserve classes) and therefore reserves are not considered
#' in the application of the indexation clause.__ This leads to slight deviations from the real reinsurance cashflow during
#' the claim development but the effect should be not be significant. \cr \cr
#'
#' Considering these information the structure of the parameter dataframe `reinsurance` should be as follows: \cr
#' `reinsurance` must contain one row for each origin year that shall be considered even though no reinsurance may
#' have existed in some years and must consist of the following columns:
#'    + `Origin_year` \cr type: integer or numeric. Origin year of the reinsurance structure.
#'    + `Priority` \cr type: numeric. Unindexed priority (or retention) of the xl treaty.
#'    + `Limit` \cr type: numeric. Unindexed limit of the xl treaty.
#'    + `Base_year` \cr type: integer or numeric. Base_year for the application of the indexation margin.
#'    Set this column to 0 if no indexation shall be considered or set Margin_type to "none".
#'    + `Margin_type` \cr type: character. Either "FIC", "SIC", "APK" or "none" for no indexation.
#'    + `Margin` \cr type: numeric. Margin \eqn{\alpha}, e.g. 0.1 for a margin of 10%. If Margin_type equals `FIC`, this
#'    column will be set to 0. If Margin_type equals `none`, this column will be ignored.
#'    + `Quota_share` \cr type: numeric. If a quota share reinsurance is to be considered, this must be the reinsurance share,
#'    e.g. 0.2 if the reinsurance pays 20% of each claim.
#'
#'
#' @return numeric matrix with one row per claim containing reinsurer's payments.
#' The number of columns depends on the `output` parameter.
#' @export
#'
#' @examples
#' # this example uses data provided with this package
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
#' # generate history
#' history <- generate_history_per_claim(extended_claims_data, "Cl_payment_cal", 1989, 2023)
#'
#' # generate future payments with fictive constant payments
#' # of 1.000.000 for 10 years for claim in row 10
#' future_payments <- matrix(0, nrow = NROW(history), ncol = 250)
#' future_payments[10, 1:10] <- 1e6
#'
#' # output "history"
#' xl_cashflow(future_payments = future_payments,
#'             claims_list = large_claims_list,
#'             history = history,
#'             reinsurance = reinsurance_xmpl,
#'             indices = indices_xmpl,
#'             output = "history")[10,]
#'
#' # output "future"
#' xl_cashflow(future_payments = future_payments,
#'             claims_list = large_claims_list,
#'             history = history,
#'             reinsurance = reinsurance_xmpl,
#'             indices = indices_xmpl,
#'             output = "future")[10,]
#'
#' # simple example, only one claim from origin year 2020
#' future_payments <- rbind(c(rep(5e5, 5), rep(0, 245)))
#' history <- rbind(rep(1e6, 4))
#' colnames(history) <- 2020:2023
#' claims_list <- data.frame(Claim_id = "Test1",
#'                           Origin_year = 2020,
#'                           Exit_reserve_class = 1,
#'                           Dev_year_since_large = 4,
#'                           Dev_year_of_growing_large = 1,
#'                           Large_since = 2020,
#'                           Cl_reserve = 5000000)
#' reinsurance <- reinsurance_xmpl[reinsurance_xmpl$Origin_year > 2019,]
#' reinsurance$Margin_type[1] <- "none"
#' reinsurance$Quota_share[1] <- 0
#'
#' xl_cashflow(future_payments = future_payments,
#'             claims_list = claims_list,
#'             history = history,
#'             reinsurance = reinsurance,
#'             indices = indices_xmpl,
#'             output = "total")[1,1:10]
#'
xl_cashflow <- function(future_payments,
                        claims_list,
                        history,
                        reinsurance,
                        indices = NULL,
                        xl_indices_per_claim = NULL,
                        output = "future"){

   # if xl_indices_per_claim is not NULL it will be used in this function
   # otherwise it must be created with claims_list, reinsurance and indices
   if (is.null(indices) & is.null(xl_indices_per_claim)) {
      stop("One of the parameters indices and xl_indices_per_claim has to be not NULL")
   }

   first_orig_year <- min(as.integer(colnames(history)))
   last_orig_year <- max(as.integer(colnames(history)))

   if (is.null(xl_indices_per_claim)) {
      xl_indices_per_claim <- generate_xl_indices_per_claim(claims_list = claims_list,
                                                            reinsurance = reinsurance,
                                                            indices = indices,
                                                            first_orig_year = first_orig_year,
                                                            last_orig_year = last_orig_year)}

   # Expand history by sampled future payments
   expanded_history <- cbind(history, future_payments)

   # cumulate expanded_history and minimize to 0, because a negative sum would lead to an error
   cum_gross_cashflow <- pmax(inc2cum(expanded_history), 0)

   # same operation on indexed expanded history
   indexed_cum_gross_cashflow <- pmax(inc2cum(expanded_history/xl_indices_per_claim), 0)

   # further operations are only applied to elements > 0
   idx <- which(cum_gross_cashflow > 0)

   # Calculate adjustment factor for priority and retention
   adj_factor <- rep(1, nrow(expanded_history) * ncol(expanded_history))
   adj_factor[idx] <- cum_gross_cashflow[ idx ] / indexed_cum_gross_cashflow[ idx ]

   # look up row index of the origin years in dataframe reinsurance
   origin_years <- match(claims_list$Origin_year, reinsurance$Origin_year)

   # Calculate indexed priority and limit
   ind_prio <- reinsurance$Priority[origin_years] * adj_factor
   ind_limit <- reinsurance$Limit[origin_years] * adj_factor

   # apply quota share to get basis for xl
   cum_gross_cashflow_after_quota <- cum_gross_cashflow * (1 - reinsurance$Quota_share[origin_years])

   # Calculate xl
   xl <- cum2inc(pmax(pmin(cum_gross_cashflow_after_quota - ind_prio,
                            ind_limit),
                       0))

   if (!output %in% c("future", "history", "total")) {
      output == "future"
      warning("Parameter 'output' not valid and set to 'future'")
   }

   if (output == "future") {
      # Reduce ouptut to future calendar years
      return(xl[,(ncol(history) + 1):ncol(expanded_history), drop = FALSE])
   }

   if (output == "history") {
      # Reduce ouptut to expired calendar years
      return(xl[,1:ncol(history), drop = FALSE])
   } else {
      return(xl)
   }
}
