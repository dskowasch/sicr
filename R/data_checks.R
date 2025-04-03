
#' Check column type
#'
#' @description
#' This is a helper function for data checks. It checks if a vector is of the desired type.
#'
#' @param column Vector.
#' @param type Character. One of `integer`, `numeric` or `character`.
#'
#' @returns TRUE if the vector is of specified type.
#' @export
#'
#' @examples
#' check_column_type(1:5, "integer")
#' check_column_type(paste0("test", 1:5), "character")
#' check_column_type(c(1:4, "notanumber"), "numeric")
check_column_type <- function(column, type){
   if (any(is.na(column))) return(FALSE)

   return(switch(type,
                 "integer" = if (is.numeric(column)) all(as.integer(column) == column) else FALSE,
                 "numeric" = is.numeric(column),
                 "character" = is.character(column),
                 FALSE))
}


#' Check structure of a dataframe
#'
#' @description
#' This is a helper function for data checks. It checks if the dataframe `df` contains all the columns
#' with names and types specified in `structure`.
#'
#' @param df Dataframe
#' @param structure Dataframe with two columns.
#' 1. `Colname` contains the colnames of `df`.
#' 2. `Coltype` contains the associated type to check for.
#'
#' @returns TRUE if `df` contains all the columns specified in `structure` with the correct types.
#' @export
#'
#' @examples
#' df <- data.frame(Col1 = 1:5, Col2 = paste0("test", 1:5))
#' structure <- data.frame(Colname = c("Col1", "Col2"), Coltype = c("integer", "character"))
#' check_dataframe_structure(df, structure)
check_dataframe_structure <- function(df, structure){
   p <- TRUE
   nonempty <- (NROW(df) > 0)
   for (column in 1:NROW(structure)) {
      col.name <- structure$Colname[column]
      col.type <- structure$Coltype[column]
      if (!(col.name %in% colnames(df))) {
         cat( paste("Column ", col.name, " is missing.", "\n", sep = ""))
         p <- FALSE
         next
      }
      if (nonempty & !check_column_type(df[[col.name]], col.type)) {
         cat(paste("Column ", col.name, " is not of type " , col.type, "\n", sep = ""))
         p <- FALSE
      }
   }
   return(p)
}



#' Checking active_annuities dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param active_annuities Dataframe of active annuities, see [active_annuities_xmpl].
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' active_annuities_ok(active_annuities_xmpl, 2023)
active_annuities_ok <- function(active_annuities, last_orig_year){
   structure_list <- list(
      Claim_id       = "character",
      Annuity_id     = "character",
      Origin_year    = "integer",
      Calendar_year  = "integer",
      Entering_year  = "integer",
      Annuity_start  = "integer",
      Annuity_end    = "integer",
      Birth_year     = "integer",
      Gender         = "character",
      Annual_payment = "numeric",
      Dynamic        = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- active_annuities
   p <- TRUE
   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.

   # dataframe check
   if (!is.data.frame(df)) {
      cat("active_annuities must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # return true if empty
   if (NROW(df) < 1) return(p)

   # check if all annuities are actually active
   if (any(df$Annuity_end <= df$Calendar_year & df$Annuity_end != 0)) {
      cat("At least one annuity's Annuity_end is smaller or equal to Calendar_year, hence is not active.\n")
      p <- FALSE
   }

   # check if calendar year is always equal to last_orig_year
   if (any(df$Calendar_year != last_orig_year)) {
      cat("At least one annuity's Calendar_year is not equal to last_orig_year.\n")
      p <- FALSE
   }

   # check if Annuity_start is smaller or equal to Annuity_end
   if (any(df$Annuity_start > df$Annuity_end & df$Annuity_end != 0)) {
      cat("At least one annuity's Annuity_start is larger than its Annuity_end.\n")
      p <- FALSE
   }

   # check if Birth_year is smaller or equal to Annuity_start
   if (any(df$Birth_year > df$Annuity_start)) {
      cat("At least one annuity's Birth_year is larger than its Annuity_start.\n")
      p <- FALSE
   }

   # check if Entering_year is smaller or equal to Annuity_start
   if (any(df$Entering_year > df$Annuity_start)) {
      cat("At least one annuity's Entering_year is larger than its Annuity_start.\n")
      p <- FALSE
   }

   # Gender always m, f or w (w for German version)
   if (any(!df$Gender %in% c("m", "f", "w"))) {
      cat("At least one annuity's Gender is not equal to m, f or w (german version).\n")
      p <- FALSE
   }

   # check for double entries with claim_id and annuity_id
   if (any(duplicated(df[,c("Claim_id", "Annuity_id")]))) {
      cat("At least one combination of Claim_id and Annuity_id is included more than once.\n")
      p <- FALSE
   }

   # check if Annual_payment larger 0
   if (any(df$Annual_payment <= 0)) {
      cat("At least one annuity's Annual_payment is not larger than 0.\n")
      p <- FALSE
   }

   # check if dynamic is between -1 and 1
   if (any(df$Dynamic <= -1 | df$Dynamic >= 1)) {
      cat("At least one annuity's Dynamic is not between -1 and 1.\n")
      p <- FALSE
   }


   return(p)
}


#' Checking pool_of_annuities dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param pool_of_annuities Dataframe of all historic annuities, see description of `pool_of_annuities` in
#' details of [prepare_data()].
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' pool_of_annuities_ok(pool_of_annuities_xmpl)
pool_of_annuities_ok <- function(pool_of_annuities){
   structure_list <- list(
      Claim_id       = "character",
      Annuity_id     = "character",
      Origin_year    = "integer",
      Calendar_year  = "integer",
      Entering_year  = "integer",
      Annuity_start  = "integer",
      Annuity_end    = "integer",
      Birth_year     = "integer",
      Gender         = "character",
      Annual_payment = "numeric",
      Dynamic        = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- pool_of_annuities
   p <- TRUE
   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.

   # dataframe check
   if (!is.data.frame(df)) {
      cat("pool_of_annuities must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # return true if empty
   if (NROW(df) < 1) return(p)

   # check if Annuity_start is smaller or equal to Annuity_end
   if (any(df$Annuity_start > df$Annuity_end & df$Annuity_end != 0)) {
      cat("At least one annuity's Annuity_start is larger than its Annuity_end.\n")
      p <- FALSE
   }

   # check if Birth_year is smaller or equal to Annuity_start
   if (any(df$Birth_year > df$Annuity_start)) {
      cat("At least one annuity's Birth_year is larger than its Annuity_start.\n")
      p <- FALSE
   }

   # check if Entering_year is smaller or equal to Annuity_start
   if (any(df$Entering_year > df$Annuity_start)) {
      cat("At least one annuity's Entering_year is larger than its Annuity_start.\n")
      p <- FALSE
   }

   # Gender always m, f or w (w for German version)
   if (any(!df$Gender %in% c("m", "f", "w"))) {
      cat("At least one annuity's Gender is not equal to m, f or w (german version).\n")
      p <- FALSE
   }

   # check for double entries with claim_id and annuity_id
   if (any(duplicated(df[,c("Claim_id", "Annuity_id")]))) {
      cat("At least one combination of Claim_id and Annuity_id is included more than once.\n")
      p <- FALSE
   }

   # check if Annual_payment larger 0
   if (any(df$Annual_payment <= 0)) {
      cat("At least one annuity's Annual_payment is not larger than 0.\n")
      p <- FALSE
   }

   # check if dynamic is between -1 and 1
   if (any(df$Dynamic <= -1 | df$Dynamic >= 1)) {
      cat("At least one annuity's Dynamic is not between -1 and 1.\n")
      p <- FALSE
   }

   return(p)
}


#' Checking claims_data dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param claims_data Dataframe, see details of [prepare_data()].
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' claims_data_ok(claims_data_xmpl, 2023)
claims_data_ok <- function(claims_data, last_orig_year){

   structure_list <- list(
      Claim_id       = "character",
      Origin_year    = "integer",
      Calendar_year  = "integer",
      Cl_payment_cal = "numeric",
      Cl_reserve     = "numeric",
      An_payment_cal = "numeric",
      An_reserve     = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- claims_data
   p <- TRUE

   # check if NULL
   if (is.null(df)) {
      cat("claims_data must not be NULL")
      return(FALSE)}

   # dataframe check
   if (!is.data.frame(df)) {
      cat("claims_data must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for data
   if (NROW(df) < 1) {
      cat("claims_data must not be empty.\n")
      return(FALSE)}

   #  check for double entries
   if (any(duplicated(df[,c("Claim_id", "Calendar_year")]))) {
      cat("At least one combination of Claim_id and Calendar_year is included more than once.\n")
      p <- FALSE
   }

   # check if Origin_year is smaller or equal to last_orig_year
   if (any(df$Origin_year > last_orig_year)) {
      cat("At least one claims' Origin_year is greater than last_orig_year.\n")
      p <- FALSE
   }

   # check if Calendar_year is greater or equal to Origin_year
   if (any(df$Calendar_year < df$Origin_year)) {
      cat("At least one claims' Calendar_year is smaller than its Origin_year.\n")
      p <- FALSE
   }

   # check if Calendar_year is smaller or equal to last_orig_year
   if (any(df$Calendar_year > last_orig_year)) {
      cat("At least one claims' Calendar_year is greater than last_orig_year.\n")
      p <- FALSE
   }

   return(p)
}


#' Checking exposure dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param exposure Dataframe that must contain one row for each origin year between `first_orig_year`
#' and `last_orig_year` and the columns `Origin_year` and `Exposure`.
#' @param first_orig_year First origin year.
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' exposure_ok(exposure_xmpl, 1989, 2023)
exposure_ok <- function(exposure, first_orig_year, last_orig_year) {
   structure_list <- list(
      Origin_year  = "integer",
      Exposure     = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- exposure

   p <- TRUE

   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.


   # dataframe check
   if (!is.data.frame(df)) {
      cat("exposure must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for double entries
   if (any(duplicated(df[,c("Origin_year")]))) {
      cat("At least one Origin_year has more than one entry in exposure.\n")
      p <- FALSE
   }

   # check if no origin year is missing
   if (!all(first_orig_year:last_orig_year %in% df$Origin_year)) {
      cat("At least one Origin_year between first_orig_year and last_orig_year is missing in exposure.\n")
      p <- FALSE
   }

   return(p)
}


#' Checking historic_indices dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param historic_indices Dataframe, see description of [expand_historic_indices()].
#' @param first_orig_year First origin year with full history.
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' historic_indices_ok(historic_indices_xmpl, 2015, 2023)
historic_indices_ok <- function(historic_indices, first_orig_year, last_orig_year) {
   structure_list <- list(
      Calendar_year  = "integer",
      Index_gross    = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- historic_indices

   p <- TRUE

   # check for NULL
   if (is.null(df)) {
      cat("dataframe historic_indices must not be NULL")
      return(FALSE)}

   # dataframe check
   if (!is.data.frame(df)) {
      cat("historic_indices must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for double entries
   if (any(duplicated(df[,c("Calendar_year")]))) {
      cat("At least one Calendar_year is included more than once in historic_indices.\n")
      p <- FALSE
   }

   # check for completeness
   if (!all(first_orig_year:last_orig_year %in% df$Calendar_year)) {
      cat("At least one Calendar_year between first_orig_year and last_orig_year is missing in historic_indices.\n")
      p <- FALSE
   }

   # check gross index values
   if (any(df$Index_gross <= -1 | df$Index_gross >= 1)) {
      cat("At least one Index_gross in historic_indices is not between -1 and 1.\n")
      p <- FALSE
   }

   # check reinsurance index values if existing
   if ("Index_re" %in% names(historic_indices)) {
      if (any(df$Index_re <= -1 | df$Index_re >= 1)) {
         cat("At least one Index_re in historic_indices is not between -1 and 1.\n")
         p <- FALSE
      }
   }


   return(p)
   }



#' Checking indices dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param first_orig_year First origin year.
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' indices_ok(indices_xmpl, 1989, 2023)
indices_ok <- function(indices, first_orig_year, last_orig_year) {

   structure_list <- list(
      Calendar_year = "integer",
      Index_gross   = "numeric",
      Index_re      = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   p <- TRUE

   df <- indices

   # check for NULL
   if (is.null(df)) {
      cat("indices must not be NULL.")
      return("FALSE")}

   # dataframe check
   if (!is.data.frame(df)) {
      cat("indices must be a dataframe.\n")
      return(FALSE)}

   # check for double entries
   if (any(duplicated(df[,c("Calendar_year")]))) {
      cat("At least one Calendar_year is included more than once.\n")
      p <- FALSE
   }

   # check if all calendar years + 250 future calendar years are included
   if (!all(first_orig_year:(last_orig_year + 250) %in% df$Calendar_year)) {
      cat("At least one Calendar_year between first_orig_year and last_orig_year + 250 is missing.\n")
      p <- FALSE
   }

   # check for correct order
   if (any(!df$Calendar_year == df$Calendar_year[order(df$Calendar_year)])) {
      cat("Calendar_year is in wrong order.\n")
      p <- FALSE
   }

   # check if index is between -1 and 1
   if (any(df$Index_re < -1 | df$Index_re > 1 | df$Index_gross < -1 | df$Index_gross > 1)) {
      cat("At least one index is not between -1 and 1.\n")
      p <- FALSE
   }

   # check for attribute index_year
   if (!is.numeric(attr(df, "index_year"))) {
      cat("Attribute index_year is missing. Please use predefined functions to create dataframe indices.")
      p <- FALSE
   }

   return(p)
}

#' Checking reinsurance dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param reinsurance Dataframe with reinsurance information, see details of [xl_cashflow()] or [reinsurance_xmpl].
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param first_orig_year First origin year.
#' @param last_orig_year Last origin year.
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' reinsurance_ok(reinsurance_xmpl, indices_xmpl, 1989, 2023)
reinsurance_ok <- function(reinsurance, indices, first_orig_year, last_orig_year) {

   structure_list <- list(
      Origin_year   = "integer",
      Priority      = "numeric",
      Base_year     = "integer",
      Margin_type   = "character",
      Margin        = "numeric",
      Quota_share   = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   p <- TRUE

   df <- reinsurance

   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.

   # dataframe check
   if (!is.data.frame(df)) {
      cat("reinsurance must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for double entries
   if (any(duplicated(reinsurance[,c("Origin_year")]))) {
      cat("At least one Origin_year is included more than once.\n")
      p <- FALSE
   }

   # check if one origin year is missing
   if (!all(first_orig_year:last_orig_year %in% df$Origin_year)) {
      cat("At least one Origin_year is missing in reinsurance.\n")
      p <- FALSE
   }

   # check Margin_type
   if (!all(df$Margin_type %in% c("APK", "SIC", "FIC", "none"))) {
      cat("At least one Origin_year has invalid Margin_type. Must be one of 'APK', 'SIC', 'FIC' or 'none'\n")
      p <- FALSE
   }

   # check Margin
   if (any(df$Margin < 0 | df$Margin > 1)) {
      cat("At least one Origin_year's Margin is not between 0 and 1.\n")
      p <- FALSE
   }

   # Check Quota_share
   if (any(df$Quota_share < 0 | df$Quota_share > 1)) {
      cat("At least one Origin_years' Quota_share is not between 0 and 1.\n")
      p <- FALSE
   }

   # check Base_year
   if (any(df$Base_year > last_orig_year)) {
      cat("At least one Origin_years' Base_year is greater than last_orig_year.\n")
      p <- FALSE
   }

   # check if all base_years occur in indices.
   # this is important as the base year of the first_orig_year may be smaller than first_orig_year. This has to be
   # considered in the dataframe indices
   if (!all((min(df$Base_year[df$Base_year > 0],
                 first_orig_year):last_orig_year) %in% indices$Calendar_year)) {
      cat("At least one Calendar_year is missing in indices.
          Make sure all Base_years of reinsurance are included in indices.\n")
      p <- FALSE
   }

   return(p)
}


#' Checking mortality dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param mortality Dataframe, see description of [mortality_xmpl].
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' mortality_ok(mortality_xmpl)
mortality_ok <- function(mortality) {
   structure_list <- list(
      Gender      = "character",
      Age         = "integer",
      Probability = "numeric"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- mortality

   p <- TRUE

   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.

   # dataframe check
   if (!is.data.frame(df)) {
      cat("mortality must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for double entries
   if (any(duplicated(df[,c("Gender", "Age")]))) {
      cat("At least one combination of Gender and Age is included more than once in mortality.\n")
      p <- FALSE
   }

   # check Gender
   if (!all(df$Gender %in% c("m", "f"))) {
      cat("At least one entry in mortality has invalid Gender. Must be one of 'f' and 'm'.\n")
      p <- FALSE
   }

   # check if all ages between 0 and max are included
   min_age_f <- min(df[df$Gender == "f", "Age"])
   min_age_m <- min(df[df$Gender == "m", "Age"])
   max_age_f <- max(df[df$Gender == "f", "Age"])
   max_age_m <- max(df[df$Gender == "m", "Age"])
   if (any(!all((min_age_f:max_age_f) %in% df[df$Gender == "f", "Age"]),
           !all((min_age_m:max_age_m) %in% df[df$Gender == "m", "Age"]))) {
      cat("Not every age between minimum and maximum age is included in mortality. Please check for gaps.\n")
      p <- FALSE
   }

   # check Probability
   if (any(df$Probabilty < 0 | df$Probabilty > 1)) {
      cat("At least one Probability in mortality is not between 0 and 1.\n")
      p <- FALSE
   }

   # check if maximum for m and f is 1.
   if (!max(df[df$Gender == "f","Probability"]) * max(df[df$Gender == "m","Probability"]) == 1) {
      cat("The maximum probability in mortality must be 1 for each female and male.\n")
      p <- FALSE
   }

   return(p)
}


#' Checking age_shift dataframe for possible mistakes
#'
#' @description
#' This function checks this input data for possible mistakes, implausible entries or missing or double rows.
#'
#' @param age_shift Dataframe, see description of [age_shift_xmpl].
#'
#' @returns TRUE if every check is passed, else FALSE.
#' @export
#'
#' @examples
#' age_shift_ok(age_shift_xmpl)
age_shift_ok <- function(age_shift) {
   structure_list <- list(
      Gender      = "character",
      Birth_year  = "integer",
      Age_shift   = "integer"
   )

   structure <- data.frame(Colname = names(structure_list), Coltype = unlist(structure_list))

   df <- age_shift

   p <- TRUE

   if (is.null(df)) {return(p)} #NULL is ok, dataframe will not be used then.

   # dataframe check
   if (!is.data.frame(df)) {
      cat("age_shift must be a dataframe.\n")
      return(FALSE)}

   # structure check
   if (!check_dataframe_structure(df, structure)) return(FALSE)

   # check for double entries
   if (any(duplicated(df[,c("Gender", "Birth_year")]))) {
      cat("At least one combination of Gender and Birth_year is included more than once in age_shift.\n")
      p <- FALSE
   }

   # check Gender
   if (!all(df$Gender %in% c("m", "f"))) {
      cat("At least one entry in age_shift has invalid Gender. Must be one of 'f' and 'm'.\n")
      p <- FALSE
   }

   # check if all ages between min and max are included
   min_birth_year_f <- min(df[df$Gender == "f", "Birth_year"])
   min_birth_year_m <- min(df[df$Gender == "m", "Birth_year"])
   max_birth_year_f <- max(df[df$Gender == "f", "Birth_year"])
   max_birth_year_m <- max(df[df$Gender == "m", "Birth_year"])
   if (any(!all((min_birth_year_f:max_birth_year_f) %in% df[df$Gender == "f", "Birth_year"]),
           !all((min_birth_year_m:max_birth_year_m) %in% df[df$Gender == "m", "Birth_year"]))) {
      cat("Not every Birth_year between minimum and maximum Birth_year is included in age_shift.
          Please check for gaps.\n")
      p <- FALSE
   }

   return(p)
}
