#' Minimal example of a claims data dataframe
#'
#' Dataframe contains one row for each calendar year per claim where at least one of the
#' columns `Cl_payment_cal`, `Cl_reserve`, `An_payment_cal`, `An_reserve` is not 0. \cr
#' Data belongs to the set of `minimal_claims_data_xmpl`, `minimal_active_annuities_xmpl` and `minimal_pool_of_annuities_xmpl`.
#'
#' @format ## `minimal_claims_data_xmpl`
#' A data frame with 995 rows and 7 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Cl_payment_cal}{Claim payments without annuities}
#'    \item{Cl_reserve}{Claim payments without annuities}
#'    \item{An_payment_cal}{Annuities' payments}
#'    \item{An_reserve}{Annuities' reserve}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"minimal_claims_data_xmpl"


#' Example of a claims data dataframe
#'
#' Dataframe contains one row for each calendar year per claim where at least one of the
#' columns `Cl_payment_cal`, `Cl_reserve`, `An_payment_cal`, `An_reserve` is not 0. \cr
#' Data belongs to the set of `claims_data_xmpl`, `active_annuities_xmpl` and `pool_of_annuities_xmpl`.
#'
#' @format ## `claims_data_xmpl`
#' A data frame with 49000 rows and 7 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Cl_payment_cal}{Claim payments without annuities}
#'    \item{Cl_reserve}{Claim payments without annuities}
#'    \item{An_payment_cal}{Annuities' payments}
#'    \item{An_reserve}{Annuities' reserve}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"claims_data_xmpl"


#' Minimal example of an active annuities dataframe
#'
#' Dataframe contains one row for each active annuity, that means for each annuity that has
#' not yet been canceled \cr
#' Data belongs to the set of `minimal_claims_data_xmpl`, `minimal_active_annuities_xmpl` and `minimal_pool_of_annuities_xmpl`.
#'
#'
#' @format ## `minimal_active_annuities_xmpl`
#' A data frame with 3 rows and 11 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Annuity_id}{Annuity ID for the case of multiple annuities in one claim}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Entering_year}{Year in which insurer and recipient have agreed on the annuity}
#'    \item{Annuity_start}{Year in which payment starts, may be a past or a future year}
#'    \item{Annuity_end}{Year in which payment ends, must be a future year as it is an active annuity}
#'    \item{Birth_year}{Birth year of recipient for assigning survival probability from mortality tables}
#'    \item{Gender}{Gender of recipient for assigning mortality table}
#'    \item{Annual_payment}{Agreed annual payment}
#'    \item{Dynamic}{If a dynamic increase of the annual payment is part of the agreement, it can be specified here, e.g. 0.02 for 2% annual payment increasement}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"minimal_active_annuities_xmpl"


#' Example of an active annuities dataframe
#'
#' Dataframe contains one row for each active annuity, that means for each annuity that has
#' not yet been canceled \cr
#' Data belongs to the set of `claims_data_xmpl`, `active_annuities_xmpl` and `pool_of_annuities_xmpl`.
#'
#'
#' @format ## `active_annuities_xmpl`
#' A data frame with 132 rows and 11 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Annuity_id}{Annuity ID for the case of multiple annuities in one claim}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Entering_year}{Year in which insurer and recipient have agreed on the annuity}
#'    \item{Annuity_start}{Year in which payment starts, may be a past or a future year}
#'    \item{Annuity_end}{Year in which payment ends, must be a future year as it is an active annuity}
#'    \item{Birth_year}{Birth year of recipient for assigning survival probability from mortality tables}
#'    \item{Gender}{Gender of recipient for assigning mortality table}
#'    \item{Annual_payment}{Agreed annual payment}
#'    \item{Dynamic}{If a dynamic increase of the annual payment is part of the agreement, it can be specified here, e.g. 0.02 for 2% annual payment increasement}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"active_annuities_xmpl"


#' Minimal example of a pool of annuities dataframe
#'
#' Dataframe contains one row for each annuity that has ever been agreed on regardless
#' of whether the annuity is still active or not \cr
#' Data belongs to the set of `minimal_claims_data_xmpl`, `minimal_active_annuities_xmpl` and `minimal_pool_of_annuities_xmpl`.
#'
#'
#' @format ## `minimal_pool_of_annuities_xmpl`
#' A data frame with 3 rows and 11 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Annuity_id}{Annuity ID for the case of multiple annuities in one claim}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Entering_year}{Year in which insurer and recipient have agreed on the annuity}
#'    \item{Annuity_start}{Year in which payment starts, may be a past or a future year}
#'    \item{Annuity_end}{Year in which payment ends, may be a past or a future year}
#'    \item{Birth_year}{Birth year of recipient for assigning survival probability from mortality tables}
#'    \item{Gender}{Gender of recipient for assigning mortality table}
#'    \item{Annual_payment}{Agreed annual payment}
#'    \item{Dynamic}{If a dynamic increase of the annual payment is part of the agreement, it can be specified here, e.g. 0.02 for 2% annual payment increasement}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"minimal_pool_of_annuities_xmpl"


#' Example of a pool of annuities dataframe
#'
#' Dataframe contains one row for each annuity that has ever been agreed on regardless
#' of whether the annuity is still active or not \cr
#' Data belongs to the set of `claims_data_xmpl`, `active_annuities_xmpl` and `pool_of_annuities_xmpl`.
#'
#'
#' @format ## `pool_of_annuities_xmpl`
#' A data frame with 208 rows and 11 columns:
#' \describe{
#'    \item{Claim_id}{Claim ID}
#'    \item{Annuity_id}{Annuity ID for the case of multiple annuities in one claim}
#'    \item{Origin_year}{Origin year of the claim}
#'    \item{Calendar_year}{Calendar year}
#'    \item{Entering_year}{Year in which insurer and recipient have agreed on the annuity}
#'    \item{Annuity_start}{Year in which payment starts, may be a past or a future year}
#'    \item{Annuity_end}{Year in which payment ends, may be a past or a future year}
#'    \item{Birth_year}{Birth year of recipient for assigning survival probability from mortality tables}
#'    \item{Gender}{Gender of recipient for assigning mortality table}
#'    \item{Annual_payment}{Agreed annual payment}
#'    \item{Dynamic}{If a dynamic increase of the annual payment is part of the agreement, it can be specified here, e.g. 0.02 for 2% annual payment increasement}
#'    }
#'
#' @source {A subset derived by reducing and anonymising an original dataset of an insurer.}
"pool_of_annuities_xmpl"


#' Example of a indices dataframe
#'
#' Dataframe contains one row for each historic calendar year and 250 future calendar years. \cr
#' This dataframe emerges from the dataframe `historic_indices_xmpl` and an expected future constant gross index of 3%
#' and an expected future constant reinsurance index of 2%.
#'
#' @format ## `indices_xmpl`
#' A data frame with 286 rows and 4 columns:
#'
#' \describe{
#'    \item{Calendar_year}{Calendar year}
#'    \item{Index_gross}{is the claim payment development from one year to another, e.g. 0.02 for 2% increase}
#'    \item{Index_re}{is the contractually fixed claim payment development that is to be used in special index clauses that are a common part of longtail xl resinsurance programs.}
#'    \item{Transition_factor}{The transition_factor for year j indexes a payment of year j to the niveau of payments in the fixed index_year.}
#'    }
#'
#' @source {
#' Dataframe indices_xmpl can be generated by:
#' ```
#' library(dplyr)
#' indices_xmpl <- expand_historic_indices(historic_indices_xmpl,
#'                                         first_orig_year = 1989,
#'                                         last_orig_year = 2023,
#'                                         index_gross_future = 0.03,
#'                                         index_re_future = 0.02) %>%
#'                 add_transition_factor(index_year = 2022)
#' ```
#' }
"indices_xmpl"


#' (unrealistic!) example of a mortality dataframe
#'
#' Dataframe contains one row for each combination of gender and age. \cr \cr
#' This data belongs to the set of `mortality_xmpl`, `age_shift_xmpl` and is totally made up to show functionalities of the package. \cr \cr
#' The age shift may lead to a negative technical age which must be considered in this dataframe.
#'
#' @format ## `mortality_xmpl`
#' A data frame with 264 rows and 3 columns:
#' \describe{
#'    \item{Gender}{"m" or "f" for male or female}
#'    \item{Age}{Age between -10 and 121.}
#'    \item{Probability}{mortality probabilities}
#'    }
#'
#' @source {Made up dataframe to show functionalities of the package.}
"mortality_xmpl"


#' (unrealistic!) example of an age shift dataframe
#'
#' Dataframe contains one row for each combination of gender and birth year. \cr \cr
#' As mortality tables are calibrated to a specific birth year, earlier or later birth years must be `shifted`. \cr \cr
#' Example: A shift of -3 for a 50 year old means that the mortality of a 47 year old will be used. \cr \cr
#' This data belongs to the set of `mortality_xmpl`, `age_shift_xmpl` and is totally made up to show functionalities of the package. \cr
#'
#' @format ## `age_shift_xmpl`
#' A data frame with 228 rows and 3 columns:
#' \describe{
#'    \item{Gender}{"m" or "f" for male or female}
#'    \item{Birth_year}{Birth year}
#'    \item{Age_shift}{Age shift}
#'    }
#'
#' @source {Made up dataframe to show functionalities of the package.}
"age_shift_xmpl"


#' Example of a reinsurance structures dataframe
#'
#' Dataframe contains one row per origin year and describes the reinsurance structure. \cr
#' Note: Only quota shares and XLs can be applied and no additional structures as AADs (aggregate annual deductable)
#' or AALs (aggregate annual limits). More details can be found in the details section of [xl_cashflow()].
#'
#'
#' @format ## `reinsurance_xmpl`
#' A data frame with 35 rows and 7 columns:
#' \describe{
#'    \item{Origin_year}{Origin year}
#'    \item{Priority}{Start of reinsurance coverage}
#'    \item{Limit}{Maximum reinsurance coverage}
#'    \item{Base_year}{The year from which the indexation shall start for this origin year}
#'    \item{Margin_type}{One of the standard margin types `FIC`, `APK` or `SIC` or `none` for no reinsurance}
#'    \item{Margin}{`APK` and `SIC` require a margin}
#'    \item{Quote_share}{Quota_share to consider before applying the xl construction}
#'    }
#'
#' @source {Made up to show package functionalities.}
"reinsurance_xmpl"


#' Example matrix with payments for all claims (e.g. small + large + special claims)
#'
#'
#' @format ## `all_claims_paid_xmpl`
#' A numeric matrix with 35 rows and 35 columns that contains all claims' payments.
#'
#' @source {Made up to show package functionalities.}
"all_claims_paid_xmpl"


#' Example matrix with reserves for all claims (e.g. small + large + special claims)
#'
#'
#' @format ## `all_claims_reserved_xmpl`
#' A numeric matrix with 35 rows and 35 columns that contains all claims' reserves.
#'
#' @source {Made up to show package functionalities.}
"all_claims_reserved_xmpl"


#' Example matrix with payments for all annuities (e.g. small + large + special claims)
#'
#'
#' @format ## `all_annuities_paid_xmpl`
#' A numeric matrix with 35 rows and 35 columns that contains all annuities' payments.
#'
#' @source { Made up to show package functionalities.}
"all_annuities_paid_xmpl"


#' Example matrix with reserves for all annuities (e.g. small + large + special claims)
#'
#'
#' @format ## `all_annuities_reserved_xmpl`
#' A numeric matrix with 35 rows and 35 columns that contains all annuities' reserves.
#'
#' @source {Made up to show package functionalities.}
"all_annuities_reserved_xmpl"


#' Example of an exposure dataframe
#'
#' Dataframe contains one row per origin year and one corresponding exposure
#' (here number of contracts of the german insurance market).
#'
#' @format ## `exposure_xmpl`
#' A data frame with 35 rows and 2 columns:
#' \describe{
#'    \item{Origin_year}{Origin year}
#'    \item{Exposure}{Numeric exposure, e.g. premiums or number of contracts}
#'    }
#'
#' @source https://www.gdv.de/gdv/statistik/statistiken-zur-deutschen-versicherungswirtschaft-uebersicht/schaden-und-unfallversicherung/anzahl-der-vertraege-und-schaeden-in-der-schaden-unfallversicherung-137946
"exposure_xmpl"


#' Example of a historic_indices dataframe
#'
#' Dataframe contains one row per calendar year from 1960 to 2023 and corresponding inflation of consumer prices
#' in germany that may be used for indexating gross and reinsurance payments and reserves.
#'
#' @format ## `historic_indices_xmpl`
#' A data frame with 64 rows and 3 columns:
#' \describe{
#'    \item{Calendar_year}{Calendar year}
#'    \item{Index_gross}{is the claim payment development from one year to another, e.g. 0.02 for 2% increase}
#'    \item{Index_re}{is the contractually fixed claim payment development that is to be used in special index clauses that are a common part of longtail xl resinsurance programs.}}
#'
#' @source \url{https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG}
"historic_indices_xmpl"
