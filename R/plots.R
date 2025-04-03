#' Generates plot to identify special claims
#'
#' @description
#' This function generates a plot that graphically compares the entry reserves of the historic observations
#' with the entry reserves of the claims that shall be simulated. It helps to identify claims without sufficient
#' similar historic claims for the simultaion.
#'
#' @param pools List as output of [generate_pools()].
#' @param extended_claims_data Prepared claims data dataframe, see details of [prepare_data()].
#' @param first_orig_year First desired origin year.
#' @param last_orig_year Last desired origin year.
#' @param selected_reserve_classes Reserve classes to be shown in the plot.
#' @param special_claims Dataframe with special claims with at least the column `Claim_id`. May be Null if
#' no special claim is identified. Default: NULL
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2023,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#' p <- plot_identify_special_claims(
#'          pools = pools,
#'          extended_claims_data = extended_claims_data,
#'          first_orig_year = 1989,
#'          last_orig_year = 2023,
#'          selected_reserve_classes = 4:5,
#'          special_claims = NULL)
#' @importFrom ggplot2 ggplot aes geom_point scale_fill_manual theme_minimal theme labs element_text element_blank
#' geom_hline geom_vline annotate scale_y_continuous scale_x_discrete position_jitter position_nudge
plot_identify_special_claims <- function(pools,
                                         extended_claims_data,
                                         first_orig_year,
                                         last_orig_year,
                                         selected_reserve_classes,
                                         special_claims = NULL) {
   # Setting arguments to NULL to prevent note in CMD check
   Claim_id <- Development_year <- Reserve <- Mapping <- NULL
   ### Transition pools to dataframe for easier processing
   pools_as_df <- pools2dataframe(pools)
   start_of_tail <- max(pools_as_df$Dev_year_since_large)
   reserve_classes <- attr(pools, "reserve_classes")

   # create extended_claims_data and claims_list for special claims
   special_extended_claims_data <- extended_claims_data[extended_claims_data$Claim_id %in% special_claims$Claim_id,]

   special_claims_list <- generate_claims_list(extended_claims_data = special_extended_claims_data,
                                               first_orig_year = first_orig_year,
                                               last_orig_year = last_orig_year)

   # create extended_claims_data and claims_list for large claims
   large_extended_claims_data <- extended_claims_data[!extended_claims_data$Claim_id %in% special_claims$Claim_id,]
   large_claims_list <- generate_claims_list(extended_claims_data = large_extended_claims_data,
                                             first_orig_year = first_orig_year,
                                             last_orig_year = last_orig_year)

   # add column that corresponds to pools_as_df$Dev_year_since_large for correct mapping
   special_claims_list$Dev_year_new <- pmin(special_claims_list$Dev_year_since_large + 1, start_of_tail)
   large_claims_list$Dev_year_new <- pmin(large_claims_list$Dev_year_since_large + 1, start_of_tail)

   # Reduce dataframes to selected reserve_classes
   pools_as_df_reduced <- pools_as_df[pools_as_df$Entry_reserve_class %in% selected_reserve_classes,]
   large_claims_list_reduced <- large_claims_list[large_claims_list$Exit_reserve_class %in% selected_reserve_classes,]
   special_claims_list_reduced <- special_claims_list[special_claims_list$Exit_reserve_class %in% selected_reserve_classes,]

   # combine dataframes for plotting with ggplot
   data <- data.frame(Development_year = numeric(),
                      Reserve = numeric(),
                      Mapping = character())
   if (NROW(pools_as_df_reduced) > 0) {
      data <- rbind(data,
                    data.frame(Development_year = pools_as_df_reduced$Dev_year_since_large,
                               Reserve = pools_as_df_reduced$Ind_entry_cl_reserve,
                               Mapping = "Pools"))
   }

   if (NROW(large_claims_list_reduced) > 0) {
      data <- rbind(data,
                    data.frame(Development_year = large_claims_list_reduced$Dev_year_new,
                               Reserve = large_claims_list_reduced$Ind_cl_reserve,
                               Mapping = "Large claim"))
   }
   if (NROW(special_claims_list_reduced) > 0) {
      data <- rbind(data, data.frame(Development_year = special_claims_list_reduced$Dev_year_new,
                                     Reserve = special_claims_list_reduced$Ind_cl_reserve,
                                     Mapping = "Special claim"))
   }

   # change to factor and sort
   data$Development_year <- factor(data$Development_year,
                                   levels = 2:start_of_tail)


   ggplot(data, aes(x = Development_year, y = Reserve)) +
      geom_point(data = subset(data, Mapping == "Pools"),
                 aes(fill = Mapping),
                 position = position_jitter(width = 0.175, height = 0),
                 shape = 21,
                 size = 4,
                 color = "#0d0d0d",
                 alpha = 0.8) +
      geom_point(data = subset(data, Mapping != "Pools"),
                 aes(fill = Mapping),
                 position = position_nudge(x = 0.45),
                 shape = 21,
                 size = 4,
                 color = "#0d0d0d") +
      scale_fill_manual(name = NULL, # no legend title
                        values = c("Large claim" = "#c1d7fe",
                                   "Special claim" = "#03368e",
                                   "Pools" = "#f87217"),
                        breaks = c("Pools", "Large claim", "Special claim")) +  #changes order
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5),
            panel.grid = element_blank(),
            axis.text.x = element_text(hjust = -.5)) +
      scale_x_discrete(limits = c(1, levels(data$Development_year)),
                       labels = c("",2:(start_of_tail - 1), "Tail")) +
      scale_y_continuous(limits = range(data$Reserve),
                         labels = function(x) format(x / 1e6, big.mark = ".", decimal.mark = ",", nsmall = 1)) +
      labs(x = "Development Year",
           y = "Indexed Entry Reserve (in Mio.)",
           title = "Observations in pools vs. claims to simulate",
           subtitle = "Indexed entry reserves") +
      geom_hline(yintercept = reserve_classes,
                 linetype = "dotted") +
      geom_vline(xintercept = 1:(start_of_tail - 1) + 0.65,
                 linetype = "dotted") +
      annotate(geom = "text",
               x = 1,
               y = reserve_classes,
               label = paste("class ", 1:length(reserve_classes)),
               vjust = -0.3)



}
