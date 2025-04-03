#' Skeleton for history triangle
#'
#' @description
#' Creates an empty matrix with each historic origin year as row and as calendar year in columns
#'
#'
#' @param first_orig_year desired first origin year
#' @param last_orig_year desired last origin year
#'
#' @return empty matrix containing claims history triangle
#' @export
#'
#' @examples skeleton_triangle(2010, 2015)
skeleton_triangle <- function(first_orig_year,
                              last_orig_year) {
   zeilen <- last_orig_year - first_orig_year + 1
   skeleton_triangle <- matrix(0, zeilen, zeilen)
   rownames(skeleton_triangle) <- first_orig_year:last_orig_year
   colnames(skeleton_triangle) <- first_orig_year:last_orig_year
   return(skeleton_triangle)
}


#' Skeleton for payments prediction
#'
#' @description
#' Creates an empty matrix with each historic origin year as row and 250 future calendar years in columns.
#'
#' @param first_orig_year desired first origin year
#' @param last_orig_year desired last origin year
#'
#' @return empty prediction matrix
#' @export
#'
#' @examples skeleton_prediction(2010, 2015)
skeleton_prediction <- function(first_orig_year,
                                last_orig_year) {
   zeilen <- last_orig_year - first_orig_year + 1
   skeleton_prediction <- matrix(0, zeilen, 250)
   rownames(skeleton_prediction) <- first_orig_year:last_orig_year
   colnames(skeleton_prediction) <- (last_orig_year + 1):(last_orig_year + 250)
   return(skeleton_prediction)
}

#' Cuts out the history part of a matrix with historic and future payments
#'
#' @param total_matrix numeric matrix with n rows and n + 250 columns. Rownames must be the origin years, colnames
#' must be the calendar years
#'
#' @return First n columns of total_matrix
#' @export
#'
#' @examples
#' total_matrix <- cbind(skeleton_triangle(2000,2010), skeleton_prediction(2000, 2010))
#' cut2history(total_matrix)
cut2history <- function(total_matrix) {
   total_matrix[,1:NROW(total_matrix), drop = FALSE]
}


#' Cuts out the prediction part of a matrix with historic and predicted payments
#'
#' @param total_matrix numeric matrix with n rows and n + 250 columns. Rownames must be the origin years, colnames
#' must be the calendar years
#'
#' @return Last 250 columns of total_matrix
#' @export
#'
#' @examples
#' total_matrix <- cbind(skeleton_triangle(2000,2010), skeleton_prediction(2000, 2010))
#' cut2prediction(total_matrix)
cut2prediction <- function(total_matrix) {
   total_matrix[,(NROW(total_matrix) + 1):NCOL(total_matrix), drop = FALSE]
}

#' Generating claims history triangle from dataframe
#'
#' @description
#' Creates a matrix with each historic origin year as row and as calendar year in columns and sums of col_name entrys in upper right triangle of the matrix
#'
#'
#' @param data dataframe with at least the columns 'Origin_year', 'Calendar_year' and col_name
#' @param col_name column to sum across
#' @param first_orig_year desired first origin year
#' @param last_orig_year desired last origin year
#' @param include_small_claims Default: FALSE. Set to FALSE to only include history as large claim. See details.
#' @details
#' last_orig_year will automatically be treated as the last observed calendar year, so the resulting matrix is quadratic /cr
#' if include_small_claims = FALSE, data must contain an additional column named "Dev_year_since_large to separate small from large claim history
#'
#' @return matrix containing claims history triangle
#' @export
#'
#' @examples
#' claims_data <- data.frame(Claim_id = c(rep("Claim1", 3), "Claim2", rep("Claim3", 2)),
#'                           Origin_year = c(rep(2010, 4), rep(2012, 2)),
#'                           Calendar_year = c(2010:2012, 2012, 2012, 2014),
#'                           Dev_year_since_large = c(0, 1, 2, 1, 1, 2),
#'                           Cl_payment_cal = rep(1000, 6))
#' print(claims_data)
#' generate_triangle(claims_data, "Cl_payment_cal", 2009, 2014)
generate_triangle <- function(data,
                              col_name,
                              first_orig_year,
                              last_orig_year,
                              include_small_claims = FALSE) {

   if (!col_name %in% colnames(data)) {
      stop(paste0("data does not contain column ", col_name))
   }

   if (include_small_claims == FALSE) {
      if (!"Dev_year_since_large" %in% colnames(data)) {
         stop("data must contain column 'Dev_year_since_large' to identify large claims")
      } else {
         data[col_name][data$Dev_year_since_large < 1,] <- 0 # set non-large claims history to 0
      }
   }

   ### add missing combinations of Calendar_year and Origin_year to prepare for xtabs usage
   orig_years <- cal_years <- seq(first_orig_year, last_orig_year)

   # create crosstable dataframe with all combinations
   all_comb <- merge(data.frame(Origin_year = orig_years),
                     data.frame(Calendar_year = cal_years),
                     by = NULL)
   data <- merge(all_comb, # merge with data
                 data,
                 by = c("Origin_year", "Calendar_year"),
                 all.x = TRUE)
   data[[col_name]][which(is.na(data[[col_name]]))] <- 0 # col_name in new rows to 0

   triangle <- xtabs(substitute(i ~ j,
                                list(i = as.name(col_name),
                                     j = quote(Origin_year + Calendar_year))),
                     data = data)

   rownames(triangle) <- orig_years
   colnames(triangle) <- cal_years
   return(triangle)
}


#' Generating triangle per development year from historic cashflow of single claims
#'
#' @description
#' Creates a matrix with each historic origin year as row and as calendar year in columns and sums in upper right triangle of the matrix
#'
#' @param cashflow matrix with one row per claim and columns first_orig_year:last_orig_year
#' @param claims_orig_years vector with origin years corresponding to claims in 'cashflow'
#' @param first_orig_year desired first origin year
#' @param last_orig_year desired last origin year
#' @details
#' last_orig_year will automatically be treated as the last observed calendar year, so the resulting matrix is quadratic
#' @return matrix containing claims history triangle
#' @export
#'
#' @examples
#' cashflow <- matrix(0, nrow = 5, ncol = 3)
#' rownames(cashflow) <- paste0("Claim", as.character(1:5))
#' colnames(cashflow) <- 2010:2012
#' cashflow[c(1, 2, 3, 6, 7, 12, 14, 15)] <- 1000
#' claims_orig_years <- c(2010, 2010, 2010, 2012, 2012)
#' print(cbind(cashflow, claims_orig_years))
#' convert_single2dev_year_triangle(cashflow, claims_orig_years, 2010, 2012)
convert_single2dev_year_triangle <- function(cashflow,
                                             claims_orig_years,
                                             first_orig_year,
                                             last_orig_year) {
   if (nrow(cashflow) != length(claims_orig_years)) {
      stop("cashflow must have same number of rows as claims_orig_years")
   }
   if (ncol(cashflow) != (last_orig_year - first_orig_year + 1)) {
      stop("cashflow must have one column for each origin year")
   }


   ergebnis_matrix <- skeleton_triangle(first_orig_year, last_orig_year)

   for (sj in first_orig_year:last_orig_year) {
      idx <- which(claims_orig_years == sj)
      if (NROW(idx) > 1) {
         ergebnis_matrix[as.character(sj),] <- colSums(cashflow[idx,])
      }
      if (NROW(idx) == 1) {
         ergebnis_matrix[as.character(sj),] <- cashflow[idx,]
      }
   }
   return(ergebnis_matrix)
}


#' Converting a prediction per single claim into a prediction per development year.
#'
#' @description
#' Summarizes single claim cashflow predictions to origin year cashflows predictions
#'
#' @param cashflow matrix with one row per claim and 250 future calendar years
#' @param claims_orig_years vector with origin years corresponding to claims in 'cashflow'
#' @param first_orig_year desired first origin year
#' @param last_orig_year desired last origin year
#'
#' @return sum of payments matrix with origin years as rows and 250 future calendar years as columns
#' @export
#'
#' @examples
#' cashflow <- matrix(0, nrow = 5, ncol = 250)
#' rownames(cashflow) <- paste0("Claim", as.character(1:5))
#' cashflow[c(1:10, 13:15, 18:20, 25)] <- 1000
#' claims_orig_years <- c(2010, 2010, 2010, 2012, 2012)
#' # printing only the first 5 of 250 columns
#' print(cbind(cashflow[,1:5], claims_orig_years))
#' convert_single2dev_year_prediction(cashflow, claims_orig_years, 2010, 2012)
convert_single2dev_year_prediction <- function(cashflow,
                                               claims_orig_years,
                                               first_orig_year,
                                               last_orig_year) {

   if (nrow(cashflow) != length(claims_orig_years)) {
      stop("cashflow must have same number of rows as claims_orig_years")
   }
   if (ncol(cashflow) != 250) {
      stop("cashflow must have 250 future calendar years as columns")
   }

   result_matrix <- skeleton_prediction(first_orig_year, last_orig_year)

   for (orig_year in first_orig_year:last_orig_year) {
      idx <- which(claims_orig_years == orig_year)
      if (NROW(idx) > 1) {
         result_matrix[as.character(orig_year),] <- colSums(cashflow[idx,])
      }
      if (NROW(idx) == 1) {
         result_matrix[as.character(orig_year),] <- cashflow[idx,]
      }
   }
   return(result_matrix)
}


#' Generating claims history from dataframe
#'
#' @description
#' Creates a matrix with one row per claim and one column per calendar year
#'
#'
#' @param data dataframe with at least the columns 'Claim_id', 'Calendar_year' and col_name
#' @param column column to sum across
#' @param first_orig_year desired first origin year, will be treated as the first observed calendar year
#' @param last_orig_year desired last origin year, will be treated as the last observed calendar year
#'
#' @return matrix containing claims history per claim
#' @export
#'
#' @examples
#' claims_data <- data.frame(Claim_id = c(rep("Claim1", 3), "Claim2", rep("Claim3", 2)),
#'                           Origin_year = c(rep(2010, 4), rep(2012, 2)),
#'                           Calendar_year = c(2010:2012, 2012, 2012, 2014),
#'                           Dev_year_since_large = c(0, 1, 2, 1, 1, 2),
#'                           Cl_payment_cal = rep(1000, 6))
#' print(claims_data)
#' generate_history_per_claim(claims_data, "Cl_payment_cal", 2009, 2014)
generate_history_per_claim <- function(data,
                                       column,
                                       first_orig_year,
                                       last_orig_year) {
   cal_years <- seq(first_orig_year, last_orig_year)
   data <- data[data$Origin_year >= first_orig_year,]
   claims_ids <- unique(data$Claim_id)

   # cal_years which are not included in data must be added separately
   if (any(!cal_years %in% data$Calendar_year & NROW(data) > 0)) {
      # Create cross table with each combination of claims_ids and cal_years
      additional_data <- merge(data.frame(Claim_id = claims_ids),
                               data.frame(Calendar_year = cal_years),
                               by = NULL)
      data <- merge(additional_data,
                    data,
                    by = c("Claim_id", "Calendar_year"),
                    all.x = TRUE)
      data[[column]][which(is.na(data[[column]]))] <- 0
   }
   # Calculate sum of payments for each combination of claims_ids and cal_years and convert to cross table
   history <- xtabs(substitute(i ~ j,
                               list(i = as.name(column),
                                    j = quote(Claim_id + Calendar_year))),
                    data = data)
   history <- history[claims_ids, ,drop = FALSE]
   if (NROW(data) > 1) {
      rownames(history) <- claims_ids
      colnames(history) <- first_orig_year:last_orig_year
   }
   return(history)
}


#' Indexing payments to another year
#'
#' @description
#' NULL
#'
#' @param payments numeric vector of payments to index
#' @param original_cal_years vector containing payments' original calendar years
#' @param new_cal_years vector containing calendar years to index to
#' @param indices dataframe containing transition factors for each element of original_cal_years and new_cal_years, see details
#'
#' @details
#' The Dataframe indices must contain at least the two columns 'Calendar_year' and 'Transition_factor'. \cr
#' The transition_factor for year j indexes a payment of year j to the niveau of payments in a fixed index year i.
#'
#' @return numeric vector with indexed payments
#' @export
#'
#' @examples
#' # for a constant inflation of 2% and index year 2022
#' indices <- data.frame(Calendar_year = 2015:2023,
#'                       Index_gross = rep(0.02, 9))
#' indices$Transition_factor <- cumprod(1 + indices$Index_gross) /
#'                                cumprod(1 + indices$Index_gross)[indices$Calendar_year == 2022]
#' indices
#' apply_index(1000, 2022, 2023, indices)
#' apply_index(payments = rep(1000, 5),
#'             original_cal_years = 2017:2021,
#'             new_cal_years = rep(2020, 5),
#'             indices = indices)
apply_index <- function(payments,
                        original_cal_years,
                        new_cal_years,
                        indices){
   if (any(!c(original_cal_years, new_cal_years) %in% indices$Calendar_year)) {
      stop("Each year of original_cal_years and new_cal_years must appear in indices")
   }

   index_factors_original <- indices$Transition_factor[match(original_cal_years, indices$Calendar_year)]
   index_factors_new <- indices$Transition_factor[match(new_cal_years, indices$Calendar_year)]

   index_ratio <- index_factors_new / index_factors_original
   return(payments * index_ratio)
}


#' Payment transition from first tail year to another development year
#'
#' @description
#' Vectorized helper function for applying the tail method. Melts down payment from first tail year to desired development year.
#'
#' @param dev_year desired development year
#' @param start_of_tail development year of tail start
#' @param end_of_tail development year of tail end
#'
#' @return numeric factor to apply to payment in first tail year
#' @export
#' @details
#' As data becomes thin in later development years, for each reserve class all payments between `start_of_tail` and `end_of_tail` are grouped and therefore scaled to the year `start_of_tail` and vice versa by
#' \deqn{Z_{M+j} = Z_{M+1} \cdot \frac{T - M - j + 1}{T - M}, \quad j = 1, \ldots, T-M}
#' @examples
#' tail_factor(25, 17, 50)
#' tail_factor(17, 17, 50)
#' tail_factor(50, 17, 50)
tail_factor <- function(dev_year,
                        start_of_tail,
                        end_of_tail){
   pmin(1,
        1 - pmin(1,
                 (dev_year - start_of_tail) /
                    (end_of_tail - start_of_tail + 1)
        )
   )
}


#' Transform calendar year triangle to development year triangle
#'
#' @param cal_year_triangle quadratic matrix with origin years as rows and calendar years as columns
#'
#' @return quadratic matrix with origin years as rows and development years as columns
#' @export
#'
#' @examples
#' cal_year_triangle <- matrix(c(1000, 0, 0, 200, 800, 0, 100, 100, 900), 3)
#' rownames(cal_year_triangle) <- colnames(cal_year_triangle) <- 2012:2014
#' print(cal_year_triangle)
#' cal_year2dev_year(cal_year_triangle)
cal_year2dev_year <- function(cal_year_triangle) {
   dimension <- nrow(cal_year_triangle)
   dev_year_triangle <- matrix(0, dimension, dimension)
   for (row in 1:dimension) {
      dev_year_triangle[row, 1:(dimension - row + 1)] <-
         cal_year_triangle[row, row:dimension]
   }
   rownames(dev_year_triangle) <- rownames(cal_year_triangle)
   colnames(dev_year_triangle) <- 1:dimension
   return(dev_year_triangle)
}


#' Transform development year triangle to calendar year triangle
#'
#' @param dev_year_triangle quadratic matrix with origin years as rows and development years as columns
#'
#' @return quadratic matrix with origin years as rows and calendar years as columns
#' @export
#'
#' @examples
#' dev_year_triangle <- matrix(c(1000, 800, 900, 200, 100, 0, 100, 0, 0), 3)
#' rownames(dev_year_triangle) <- 2012:2014
#' colnames(dev_year_triangle) <- 1:3
#' print(dev_year_triangle)
#' dev_year2cal_year(dev_year_triangle)
dev_year2cal_year <- function(dev_year_triangle) {
   dimension <- nrow(dev_year_triangle)
   cal_year_triangle <- matrix(0, dimension, dimension)
   for (row in 1:dimension) {
      cal_year_triangle[row, row:dimension] <-
         dev_year_triangle[row, 1:(dimension - row + 1)]
   }
   rownames(cal_year_triangle) <- rownames(dev_year_triangle)
   colnames(cal_year_triangle) <- rownames(dev_year_triangle)
   return(cal_year_triangle)
}


#' incremental to cumulative claims triangle
#' @description Transforms an incremental triangle or matrix into a cumulative triangle or matrix.
#' @param incr_triangle numeric matrix
#'
#' @return numeric matrix
#' @export
#'
#' @examples inc2cum(matrix(c(1000, 800, 1200, 200, 100, 0, 100, 0, 0), 3))
inc2cum <- function(incr_triangle) {
   cum_triangle <- incr_triangle <- rbind(incr_triangle)
   for (i in 1:NROW(incr_triangle)) {
      cum_triangle[i,] <- cumsum(incr_triangle[i,])
   }
   return(cum_triangle)
}


#' cumulative to incremental claims triangle
#' @description Transforms a cumulative triangle or matrix into an incremental triangle or matrix.
#' @param cum_triangle numeric matrix
#'
#' @return numeric matrix
#' @export
#'
#' @examples cum2inc(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1200), 3))
cum2inc <- function(cum_triangle){
   if (is.vector(cum_triangle)) {
      inc_triangle <- c(cum_triangle[1], diff(cum_triangle))
      return(inc_triangle)
   }

   inc_triangle <- cbind(cum_triangle[,1],
                        t(diff(t(cum_triangle))))
   colnames(inc_triangle) <- colnames(cum_triangle)
   return(inc_triangle)
}


#' transforms a matrix into a lower right triangle
#'
#' @description sets the upper left triangle including the diagonal to 0
#' @param matr numeric matrix
#' @return numeric matrix
#' @keywords internal
#'
#' @examples \dontrun{
#' matrix2triangle(matrix(c(1000, 800, 1200, 1200, 900, 1200, 1300, 900, 1250), 3))
#' }
matrix2triangle <- function(matr){
   matr * (col(matr) + row(matr) > (dim(matr)[1] +  1))
}



#' Extention to Sample function
#'
#' @description
#' Out of a list x with only one element n, sample(x, m) returns m random numbers between 1 and n.
#' This function returns rep(n, m) instead in this case and behaves like the sample-function in any other case.
#'
#' @param pool vector or list to choose from
#' @param i number of elements to choose
#' @keywords internal
#' @return vector of length i with elements drawn from pool
#' @examples \dontrun{
#' sample_1(1:10, 3)
#' sample_1(5, 2)
#' }
sample_1 <- function(pool, i){
   if (NROW(pool) == 1) {
      return(rep(pool, i))
   }
   else
   {return(sample(pool, i))}
}


#' Easy to read formatting for numbers
#'
#' @param number numeric
#' @param big.mark "." or ",". Default: "."
#' @param decimal.mark "." or ",". Default: ","
#'
#' @return character
#' @export
#'
#' @examples
#' nice_format(pi*1e6)
nice_format <- function(number, big.mark = ".", decimal.mark = ","){
   format(round(number, 2), nsmall = 2, big.mark = big.mark, decimal.mark = decimal.mark)
}
