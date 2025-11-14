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


#' Generates plots to compare best estimates with reserves
#'
#' @description
#' This function generates two plots that graphically compare the entry reserves with the calculated best estimates.
#'
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#' @param sim_result Numeric array as a result of [sicr()].
#'
#' @returns list of two ggplot2 objects, first compares per reserve class, second compares per development year
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
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2023)
#' # smallest version
#' sim_result <- sicr(n = 1,
#'                    large_claims_list = large_claims_list,
#'                    first_orig_year = 1989,
#'                    last_orig_year = 2023,
#'                    pools = pools,
#'                    indices = indices_xmpl,
#'                    history = history)
#'
#' p <- plot_reserve_vs_be(
#'          large_claims_list = large_claims_list,
#'          sim_result = sim_result)
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual theme_minimal theme labs element_text element_blank
#' scale_y_continuous scale_x_continuous position_dodge
#' @importFrom tidyr pivot_longer
#' @importFrom stats aggregate
#' @importFrom grDevices rgb
plot_reserve_vs_be <- function(large_claims_list,
                               sim_result) {
   # Setting arguments to NULL to prevent note in CMD check
   Best_Estimate <- Cl_reserve <- Exit_reserve_class <- Value <- Variable <-
      Dev_year_since_large <- NULL

   large_claims_list$Best_Estimate <- rowSums(sim_result[1:NROW(large_claims_list),
                                                         ,
                                                         c("Claim_payments",
                                                           "Future_annuities_payments")])

   col1 <- rgb(4, 70, 190, maxColorValue = 255)
   col2 <- rgb(191, 191, 191, maxColorValue = 255)

   ## aggregate best estimates and reserves...
   # first per reserve class
   data_rc <- aggregate(cbind(Best_Estimate, Cl_reserve) ~ Exit_reserve_class,
                        large_claims_list,
                        sum)
   # then per development year
   data_dy <- aggregate(cbind(Best_Estimate, Cl_reserve) ~ Dev_year_since_large,
                        large_claims_list,
                        sum)

   ## find maximum reserve class and maximum development year
   max_reserve_class <- max(data_rc$Exit_reserve_class)
   max_dev_year <- max(data_dy$Dev_year_since_large)

   ## pivot longer for ggplot usage
   # first per reserve class
   data_rc_longer <- pivot_longer(data_rc,
                                  cols = c(Best_Estimate,
                                           Cl_reserve),
                                  names_to = "Variable",
                                  values_to = "Value")

   # then per development year
   data_dy_longer <- pivot_longer(data_dy,
                                  cols = c(Best_Estimate,
                                           Cl_reserve),
                                  names_to = "Variable",
                                  values_to = "Value")

   ## generate plots
   # first per reserve class
   plot_rc <-
      ggplot(data_rc_longer,
             aes(
                x = Exit_reserve_class,
                y = Value,
                fill = Variable
             )
      ) +
      geom_bar(
         stat = "identity",
         position = position_dodge()
      ) +
      labs(
         x = "Reserve class",
         y = "Reserves (EUR million)",
         fill = NULL,
         title = "Best estimates und reserves per reserve class"
      ) +
      scale_fill_manual(
         values = c("Best_Estimate" = col1,
                    "Cl_reserve" = col2),
         labels = c("Best_Estimate" = "Best Estimates",
                    "Cl_reserve" = "Claims reserves")
      ) +
      scale_x_continuous(
         labels = 0:(max_reserve_class + 1),
         breaks = 0:(max_reserve_class + 1)
      ) +
      scale_y_continuous(
         labels = function(x) format(x / 1e6,
                                     big.mark = ".",
                                     decimal.mark = ",",
                                     scientific = FALSE)
      ) +
      theme_minimal(
      ) +
      theme(
         legend.position = "right",
         plot.title = element_text(hjust = 0.5),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )

   plot_dy <-
      ggplot(data_dy_longer,
             aes(
                x = Dev_year_since_large,
                y = Value,
                fill = Variable
             )
      ) +
      geom_bar(
         stat = "identity",
         position = position_dodge()
      ) +
      labs(
         x = "Development Year",
         y = "Reserves (EUR million)",
         fill = NULL,
         title = "Best estimates und reserves per development year"
      ) +
      scale_fill_manual(
         values = c("Best_Estimate" = col1,
                    "Cl_reserve" = col2),
         labels = c("Best_Estimate" = "Best Estimates",
                    "Cl_reserve" = "Claims reserves")
      ) +
      scale_x_continuous(
         labels = 1:(max_dev_year + 1),
         breaks = 1:(max_dev_year + 1)
      ) +
      scale_y_continuous(
         labels = function(x) format(x / 1e6,
                                     big.mark = ".",
                                     decimal.mark = ",",
                                     scientific = FALSE)
      ) +
      theme_minimal(
      ) +
      theme(
         legend.position = "right",
         plot.title = element_text(hjust = 0.5),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )

   return(list(plot_rc = plot_rc,
               plot_dy = plot_dy))
}


#' Generates plots to compare best estimates with reserves on a single claim basis
#'
#' @description
#' This function generate two plots that compare best estimates with
#' claim reserves on a single claim basis, one plot for claims that have
#' not yet reached the tail (pretail) and one for claims during the tail.
#'
#' @param last_orig_year Last origin year.
#' @param pools List as output of [generate_pools()].
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param age_shift Dataframe, see description of [age_shift_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param mortality Dataframe, see description of [mortality_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param n Number of simulations. Default: 100
#'
#' @returns
#' List of two ggplot elements, first for pretail claims and second for tail claims.
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
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' p <- plot_reserve_vs_be_per_claim(
#'    last_orig_year = 2023,
#'    pools = pools,
#'    large_claims_list = large_claims_list,
#'    indices = indices_xmpl,
#'    age_shift = age_shift_xmpl,
#'    mortality = mortality_xmpl)
#' @importFrom ggplot2 ggplot aes geom_rect geom_segment geom_point position_jitter
#' scale_fill_manual scale_colour_manual scale_alpha_manual scale_y_reverse scale_x_continuous
#' labs theme theme_minimal element_text element_blank guides guide_legend
#' @importFrom grDevices rgb
plot_reserve_vs_be_per_claim <- function(last_orig_year,
                                         pools,
                                         large_claims_list,
                                         indices,
                                         age_shift = NULL,
                                         mortality = NULL,
                                         n = 100) {

   # Setting arguments to NULL to prevent note in CMD check
   Dev_year_since_large <- Cl_reserve <- xmin <- xmax <- fill <- height <-
      name <- x_start <- x_end <- y_start <- y_end <- Reserve_class <-
      Exit_reserve_class <- NULL

   # define color palette
   palette <- c(rgb(64, 64, 64, maxColorValue = 255),
                rgb(89, 89, 89, maxColorValue = 255),
                rgb(2, 35, 95, maxColorValue = 255),
                rgb(3, 53, 142, maxColorValue = 255),
                rgb(4, 70, 190, maxColorValue = 255),
                rgb(69, 134, 251, maxColorValue = 255),
                rgb(131, 174, 252, maxColorValue = 255),
                rgb(193, 215, 254, maxColorValue = 255),
                rgb(127, 127, 127, maxColorValue = 255),
                rgb(191, 191, 191, maxColorValue = 255),
                rgb(217, 217, 217, maxColorValue = 255))
   # extract parameters from pools
   index_year <- attr(pools, "index_year")
   pool_of_annuities <- pools[[3]]
   reserve_classes <- attr(pools, "reserve_classes")
   n_rc <- length(reserve_classes) + 1
   start_of_tail <- attr(pools, "start_of_tail")
   end_of_tail <- attr(pools, "end_of_tail")


   # prepare best estimates dataframe
   df_be <- sicr_single(dev_year = 1:end_of_tail,
                        reserve_class = 0:(n_rc-1),
                        last_orig_year = last_orig_year,
                        pools = pools,
                        indices = indices,
                        age_shift = age_shift,
                        mortality = mortality,
                        n = n)



   # prepare additional data
   df_be_pretail <- df_be[df_be$Dev_year < (start_of_tail - 1),]
   df_be_tail <- df_be[df_be$Dev_year >= (start_of_tail - 1),]

   large_claims_list_pretail <-
      large_claims_list[
         large_claims_list$Dev_year_since_large < (start_of_tail - 1) &
            large_claims_list$Exit_reserve_class > 0,]
   large_claims_list_tail <-
      large_claims_list[
         large_claims_list$Dev_year_since_large >= (start_of_tail - 1) &
            large_claims_list$Exit_reserve_class > 0,]

   x_dist_pretail <- max(large_claims_list_pretail$Cl_reserve) / 1000
   x_dist_tail <- max(large_claims_list_tail$Cl_reserve) / 1000

   intervals_pretail <- data.frame(
      Reserve_class = df_be_pretail$Reserve_class,
      x_start = pmax(0, df_be_pretail$Best_estimate - x_dist_pretail),
      x_end = pmax(0, df_be_pretail$Best_estimate + x_dist_pretail),
      y_start = df_be_pretail$Dev_year - 0.25,
      y_end = df_be_pretail$Dev_year + 0.25)

   intervals_tail <- data.frame(
      Reserve_class = df_be_tail$Reserve_class,
      x_start = pmax(0,df_be_tail$Best_estimate - x_dist_tail),
      x_end = pmax(0,df_be_tail$Best_estimate + x_dist_tail),
      y_start = df_be_tail$Dev_year - 0.25,
      y_end = df_be_tail$Dev_year + 0.25)


   df_rc <- data.frame(height = reserve_classes,
                       name = 1:(n_rc - 1))

   max_nonnull_rc_pretail <-
      findInterval(max(large_claims_list_pretail$Cl_reserve),
                   reserve_classes)

   max_nonnull_rc_tail <-
      findInterval(max(large_claims_list_tail$Cl_reserve),
                   reserve_classes)

   max_reserve_pretail <- if (length(reserve_classes) > max_nonnull_rc_pretail) {
      reserve_classes[max_nonnull_rc_pretail + 1]
   } else {
      2 * reserve_classes[max_nonnull_rc_pretail] - reserve_classes[max_nonnull_rc_pretail - 1]
   }

   max_reserve_tail <- if (length(reserve_classes) > max_nonnull_rc_tail) {
      reserve_classes[max_nonnull_rc_tail + 1]
   } else {
      2 * reserve_classes[max_nonnull_rc_tail] - reserve_classes[max_nonnull_rc_tail - 1]
   }


   rect_data_pretail <- data.frame(
      xmin = pmin(max_reserve_pretail, reserve_classes),
      xmax = pmin(max_reserve_pretail, c(reserve_classes[-1], max_reserve_pretail)),
      ymin = -0.1,
      ymax = 0.1,
      fill = c(1:(n_rc - 1)))

   rect_data_tail <- data.frame(
      xmin = pmin(max_reserve_tail, reserve_classes),
      xmax = pmin(max_reserve_tail, c(reserve_classes[-1], max_reserve_tail)),
      ymin = -0.1,
      ymax = 0.1,
      fill = c(1:(n_rc - 1)))

   plot_pretail <-
      ggplot(large_claims_list_pretail,
             aes(y = Dev_year_since_large,
                 x = Cl_reserve)) +
      geom_rect(data = rect_data_pretail,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = (start_of_tail - 1) - 0.5,
                    ymax = (start_of_tail - 1),
                    fill = factor(fill, levels = n_rc:1)),
                inherit.aes = FALSE) +
      geom_rect(data = rect_data_pretail,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = 0,
                    ymax = 0.5,
                    fill = factor(fill, levels = n_rc:1)),
                inherit.aes = FALSE) +
      geom_segment(data = df_rc,
                   aes(alpha = "Best Estimate",
                       x = height,
                       xend = height,
                       y = 0,
                       yend = (start_of_tail - 1),
                       colour = factor(name, levels = n_rc:1)),
                   linetype = 4) +
      geom_rect(data = intervals_pretail[intervals_pretail$Reserve_class > 0,],
                aes(xmin = x_start,
                    xmax = x_end,
                    ymin = y_start,
                    ymax = y_end,
                    colour = factor(Reserve_class, levels = n_rc:1)),
                linewidth = 1,
                inherit.aes = FALSE) +
      geom_point(position = position_jitter(width = 0,
                                            height = 0.2),
                 aes(alpha = "Reserve per claim",
                     colour = factor(Exit_reserve_class)),
                 size = 2,
                 shape = 1) +
      scale_fill_manual(values = palette) +
      scale_colour_manual(values = palette) +
      scale_y_reverse(limits = c((start_of_tail - 1), 0),
                      breaks = seq((start_of_tail - 1) - 1, 1, -1)) +
      scale_x_continuous(limits = c(0, max_reserve_pretail),
                         breaks = seq(0, max_reserve_pretail, by = 5e5),
                         labels = format(seq(0, max_reserve_pretail, by = 5e5) * 1e-6,
                                         nsmall = 1)) +
      labs(title = "Reserves overview",
           y = "Development year",
           x = "Reserves (EUR million)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_alpha_manual(name = NULL,
                         values = c(1, 1),
                         breaks = c("Reserve per claim", "Best Estimate"),
                         guide = guide_legend(override.aes = list(linetype = c(NA, "solid"),
                                                                  shape = c("circle open", NA),
                                                                  color = c("black", "black"),
                                                                  linewidth = c(3, 1.5)))) +
      guides(color = "none", fill = "none")


   plot_tail <-
      ggplot(large_claims_list_tail,
             aes(y = Dev_year_since_large,
                 x = Cl_reserve)) +
      geom_rect(data = rect_data_tail,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = end_of_tail - 0.5,
                    ymax = end_of_tail,
                    fill = factor(fill, levels = n_rc:1)),
                inherit.aes = FALSE) +
      geom_rect(data = rect_data_tail,
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = (start_of_tail - 1) - 1,
                    ymax = (start_of_tail - 1) - 0.5,
                    fill = factor(fill, levels = n_rc:1)),
                inherit.aes = FALSE) +
      geom_segment(data = df_rc,
                   aes(alpha = "Best Estimate",
                       x = height,
                       xend = height,
                       y = (start_of_tail - 1),
                       yend = end_of_tail,
                       colour = factor(name, levels = n_rc:1)),
                   linetype = 4) +
      geom_rect(data = intervals_tail[intervals_tail$Reserve_class > 0,],
                aes(xmin = x_start,
                    xmax = x_end,
                    ymin = y_start,
                    ymax = y_end,
                    colour = factor(Reserve_class, levels = n_rc:1)),
                linewidth = 1,
                inherit.aes = FALSE) +
      geom_point(position = position_jitter(width = 0,
                                            height = 0.2),
                 aes(alpha = "Reserve per claim",
                     colour = factor(Exit_reserve_class)),
                 size = 2,
                 shape = 1) +
      scale_fill_manual(values = palette) +
      scale_colour_manual(values = palette) +
      scale_y_reverse(limits = c(end_of_tail, (start_of_tail - 1) - 1),
                      breaks = seq(end_of_tail - 1, (start_of_tail - 1), -1)) +
      scale_x_continuous(limits = c(0, max_reserve_tail),
                         breaks = seq(0, max_reserve_tail, by = 5e5),
                         labels = format(seq(0, max_reserve_tail, by = 5e5) * 1e-6,
                                         nsmall = 1)) +
      labs(title = "Reserves overview",
           y = "Development year",
           x = "Reserves (EUR million)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_alpha_manual(name = NULL,
                         values = c(1, 1),
                         breaks = c("Reserve per claim", "Best Estimate"),
                         guide = guide_legend(override.aes = list(linetype = c(NA, "solid"),
                                                                  shape = c("circle open", NA),
                                                                  color = c("black", "black"),
                                                                  linewidth = c(3, 1.5)))) +
      guides(color = "none", fill = "none")

   return(list(plot_pretail = plot_pretail,
               plot_tail = plot_tail))
}


#' Four simple plots to visualize the pools and open claims
#'
#' @description
#' The function produces four simple ggplot objects to visualize the pools and
#' the distribution of open claims' reserves and open claims numbers to the
#' reserve classes.
#'
#' @param pools List as output of [generate_pools()].
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#'
#' @returns
#' List of four ggplot objects. \cr
#' - plot_obs_numbers shows the observation numbers in the pools by reserve class and development year
#' - plot_stack stacks the reserve classes
#' - plot_reserves_allocation shows how the reserves of open claims are distributed to the reserve classes
#' - plot_numbers_allocation shows how the numbers of open claims are distributed to the reserve classes
#' - 250 columns for future calendar years
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
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#' p <- plot_pools_overview(pools = pools,
#'                          large_claims_list = large_claims_list)
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_minimal theme
#' element_text element_blank geom_text position_stack scale_y_continuous coord_polar
#' @importFrom stats aggregate
#' @importFrom grDevices rgb
plot_pools_overview <- function(pools,
                                large_claims_list) {
   # Setting variables to NULL to prevent note in CMD check
    Development_year <- Observations <- Reserve_class <- Value <- Category <-
       Cl_reserve <- Exit_reserve_class <- NULL

   # define color palette
   palette <- c(rgb(64, 64, 64, maxColorValue = 255),
                rgb(89, 89, 89, maxColorValue = 255),
                rgb(2, 35, 95, maxColorValue = 255),
                rgb(3, 53, 142, maxColorValue = 255),
                rgb(4, 70, 190, maxColorValue = 255),
                rgb(69, 134, 251, maxColorValue = 255),
                rgb(131, 174, 252, maxColorValue = 255),
                rgb(193, 215, 254, maxColorValue = 255),
                rgb(127, 127, 127, maxColorValue = 255),
                rgb(191, 191, 191, maxColorValue = 255),
                rgb(217, 217, 217, maxColorValue = 255))

   # extract parameters from pools
   reserve_classes <- attr(pools, "reserve_classes")
   start_of_tail <- attr(pools, "start_of_tail")
   end_of_tail <- attr(pools, "end_of_tail")


   # pool sizes
   n_rc <- length(reserve_classes)
   n_dy <- start_of_tail - 2
   #pool_sizes <- matrix(nrow = n_dy, ncol = n_rc)
   df_pool_sizes <- data.frame(Reserve_class = factor(),
                               Development_year = factor(),
                               Observations = numeric())
   for (rc in n_rc:1) {
      for (dy in 1:n_dy) {
         df_pool_sizes <- rbind(df_pool_sizes,
                                data.frame(Reserve_class = factor(rc),
                                           Development_year = factor(dy + 1),
                                           Observations = NROW(pools[[1]][[rc + 1]][[dy]])))
      }
   }

   # numbers of observations overview
   plot_obs_numbers <-
      ggplot(
         df_pool_sizes,
         aes(x = Development_year,
             y = Observations,
             fill = Reserve_class)
      ) +
      geom_bar(
         stat = "identity",
         position = "stack"
      ) +
      scale_fill_manual(
         values = palette
      ) +
      labs(
         x = "Development year since large",
         y = NULL,
         title = "Number of historic observations"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(hjust = 0.5),
         panel.grid = element_blank()
      ) +
      geom_text(
         aes(label = Observations),
         position = position_stack(vjust = 0.5),
         col = "white",
         size = 3.5,
         fontface = "italic"
      )

   rc_stack <- diff(reserve_classes)
   rc_stack <- c(rc_stack, rc_stack[length(rc_stack)])

   data <- data.frame(Category = factor(paste("RC", c(1:n_rc))),
                      Value = rc_stack * 1e-6)

   # change levels
   data$Category <- factor(data$Category,
                           levels = rev(levels(data$Category)))

   # simple stack plot
   plot_stack <-
      ggplot(data,
             aes(x = 1,
                 y = Value,
                 fill = Category)
      ) +
      geom_bar(
         stat = "identity",
         position = "stack"
      ) +
      labs(
         x = NULL,
         y = "EUR million",
         title = NULL
      ) +
      scale_fill_manual(
         values = palette
      ) +
      theme_minimal() +
      scale_y_continuous(
         breaks = reserve_classes/1e6,
         labels = round(reserve_classes/1e6, 1)
      ) +
      geom_text(
         aes(label = Category),
         position = position_stack(vjust = 0.5),
         col = "white",
         size = 3.5,
         fontface = "bold"
      ) +
      theme(
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         panel.grid = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "none"
      )

   # Reserves allocation
   reserves_allocation <-
      aggregate(Cl_reserve ~ Exit_reserve_class,
                large_claims_list[large_claims_list$Cl_reserve > 0,],
                sum)

   reserves_allocation$Cl_reserve <-
      reserves_allocation$Cl_reserve / sum(reserves_allocation$Cl_reserve)
   reserves_allocation$Exit_reserve_class <-
      factor(paste("RC",
                   reserves_allocation$Exit_reserve_class,
                   "\n",
                   format(round(reserves_allocation$Cl_reserve * 100, 0), nsmall = 0),
                   "%"))
   reserves_allocation$Exit_reserve_class <-
      factor(reserves_allocation$Exit_reserve_class,
             levels = rev(levels(reserves_allocation$Exit_reserve_class)))

   plot_reserves_allocation <-
      ggplot(
         reserves_allocation,
         aes(x = "",
             y = Cl_reserve,
             fill = Exit_reserve_class)
      ) +
      geom_bar(
         stat = "identity",
         width = 1,
         linewidth = 1,
         color = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = palette) +
      geom_text(
         aes(label = Exit_reserve_class),
         position = position_stack(vjust = 0.5),
         col = "white",
         size = 3.5,
         fontface = "bold"
      ) +
      labs(
         x = NULL,
         y = NULL,
         title = "Reserves"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(hjust = 0.5),
         axis.text.x = element_blank(),
         panel.grid = element_blank(),
         legend.position = "none"
      )

   # Number allocation
   numbers_allocation <-
      aggregate(Cl_reserve/Cl_reserve ~ Exit_reserve_class,
                large_claims_list[large_claims_list$Cl_reserve > 0,],
                sum)
   numbers_allocation$Cl_reserve <-
      numbers_allocation$Cl_reserve / sum(numbers_allocation$Cl_reserve)

   numbers_allocation$Exit_reserve_class <-
      factor(paste("RC",
                   numbers_allocation$Exit_reserve_class,
                   "\n",
                   format(round(numbers_allocation$Cl_reserve * 100, 0), nsmall = 0),
                   "%"))

   numbers_allocation$Exit_reserve_class <-
      factor(numbers_allocation$Exit_reserve_class,
             levels = rev(levels(numbers_allocation$Exit_reserve_class)))


   plot_numbers_allocation <-
      ggplot(
         numbers_allocation,
         aes(
            x = "",
            y = Cl_reserve,
            fill = Exit_reserve_class)
      ) +
      geom_bar(
         stat = "identity",
         width = 1,
         linewidth = 1,
         color = "white"
      ) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = palette) +
      geom_text(
         aes(label = Exit_reserve_class),
         position = position_stack(vjust = 0.5),
         col = "white",
         size = 3.5,
         fontface = "bold"
      ) +
      labs(
         x = NULL,
         y = NULL,
         title = "Number of open claims"
      ) +
      theme_minimal() +
      theme(
         plot.title = element_text(hjust = 0.5),
         axis.text.x = element_blank(),
         panel.grid = element_blank(),
         legend.position = "none"
      )
   return(
      list(
         plot_obs_numbers = plot_obs_numbers,
         plot_stack = plot_stack,
         plot_reserves_allocation = plot_reserves_allocation,
         plot_numbers_allocation = plot_numbers_allocation))
}


#' Creating series of simulation results
#'
#' @description
#' This function executes sicr n times and returns a result series
#' of each realisation for the desired columns.
#' @param n Integer. Desired number of realisations.
#' @param columns Character vector containing the names of the desired information.
#' Valid names are the names of the matrices in the output of [sicr()], see Value.
#' Default: c("Claim_payments")
#' @param drop_ibnr_claims TRUE if only known claims shall appear in the output. Default: TRUE
#' @param large_claims_list Dataframe with one row per known large claim generated by [generate_claims_list()].
#' @param first_orig_year First origin year with complete history.
#' @param last_orig_year Last origin year.
#' @param pools List as output of [generate_pools()].
#' @param indices Dataframe for indexation, see details of [prepare_data()].
#' @param history numeric matrix with one row per claim with historic payments,
#' generated by [generate_history_per_claim()]. \cr
#' If generated in other ways, note that colnames must be the origin years. \cr
#' This matrix is needed to calculate ibnr payments and reinsurance if desired.
#' @param exposure Dataframe that must contain one row for each origin year between `first_orig_year`
#' and `last_orig_year` and the columns `Origin_year` and `Exposure`. \cr
#' Default: NULL. In this case, a constant exposure is assumed.
#' @param years_for_ibnr_pools Vector containing the calendar years that shall be used to build the pools. \cr
#' Default: NULL. In this case, all years are used for building ibnr pools.
#' @param active_annuities Dataframe of active annuities, see [active_annuities_xmpl]. Default: NULL.\cr
#' Only necessary if annuities shall be considered.
#' @param age_shift Dataframe, see description of [age_shift_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param mortality Dataframe, see description of [mortality_xmpl]. Default: NULL \cr
#' Only necessary if annuities shall be considered.
#' @param reinsurance Dataframe with reinsurance information, see details of [xl_cashflow()] or [reinsurance_xmpl]. Default: NULL\cr
#' Only necessary if reinsurance shall be considered.
#' @param seed Default: 12345. Setting a seed makes the simulation reproducible.
#' @param progress Default: TRUE. Set to FALSE if no progress bar shall be shown during simulation.
#'
#' @returns
#' Numeric three-dimensional array with \cr
#' - one row per known claim followed by one row per expected ibnr claim if desired
#' - 250 columns for future calendar years
#' - one matrix for each of the n desired realisations
#' The entries contain the sum of the desired information stated in `columns`.
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
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2023)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2023)
#'
#' series <- sicr_series(n = 3,
#'                       large_claims_list = large_claims_list,
#'                       first_orig_year = 1989,
#'                       last_orig_year = 2023,
#'                       pools = pools,
#'                       indices = indices_xmpl,
#'                       history = history)
sicr_series <- function(n,
                        columns = c("Claim_payments"),
                        drop_ibnr_claims = TRUE,
                        large_claims_list,
                        first_orig_year,
                        last_orig_year,
                        pools,
                        indices,
                        history,
                        exposure = NULL,
                        years_for_ibnr_pools = NULL,
                        active_annuities = NULL,
                        age_shift = NULL,
                        mortality = NULL,
                        reinsurance = NULL,
                        seed = 12345,
                        progress = TRUE) {

   # check if columns contains only valid names
   valid_column_names <- c("Gross_sum",
                           "Reinsurance_sum",
                           "Claim_payments",
                           "Active_annuities_payments",
                           "Future_annuities_payments",
                           "Reinsurance_xl",
                           "Reinsurance_quota_share",
                           "Reserve_class",
                           "Dev_year",
                           "Future_annuities_index1",
                           "Future_annuities_index2",
                           "Future_annuities_index3",
                           "Future_annuities_index4",
                           "Future_annuities_index5",
                           "Random_sampling_index")
   if (!all(columns %in% valid_column_names)) {
      stop("Invalid entry in columns, see sicr function help to find the names of the output matrices")
   }

   # Initialize A_list to save each sim_result
   A_list <- list()

   start_time <- Sys.time() # set start time to predict remaining time
   for (realisation in 1:n) {
      duration <- Sys.time() - start_time
      if (progress) {
         cat("\r", # print progress
             " ",
            realisation,
            " of ",
            n,
            " (",
            realisation/n * 100,
            "%",
            ")",
            " --- remaining time approx. ",
            sicr::nice_format(duration/(realisation - 1) * (n - realisation + 1)),
            "          ",
            sep = "")
      }

      # simulate
      sim_result <-
         sicr(n = 1,
              large_claims_list = large_claims_list,
              first_orig_year = first_orig_year,
              last_orig_year = last_orig_year,
              pools = pools,
              indices = indices,
              history = history,
              exposure = exposure,
              years_for_ibnr_pools = years_for_ibnr_pools,
              active_annuities = active_annuities,
              age_shift = age_shift,
              mortality = mortality,
              reinsurance = reinsurance,
              seed = seed - 1 + realisation,
              progress = FALSE,
              summary = FALSE,
              detailed_export = TRUE)

      # reduce to known claims if desired and reduce to desired matrices
      max_index <- ifelse(drop_ibnr_claims,
                          NROW(large_claims_list),
                          NROW(sim_result))
      sim_result <- sim_result[1:max_index, , columns, drop = FALSE]
      sim_result <- rowSums(sim_result, dims = 2)

      # save to A_list
      A_list[[realisation]] <- sim_result
   }

   result <- simplify2array(A_list) # transform to array

   # attach attributes for latter usage
   attr(result, "last_orig_year") <- last_orig_year
   attr(result, "columns") <- columns
   attr(result, "large_claims_list") <- large_claims_list
   dimnames(result)[[3]] <- seed:(seed + n - 1)

   return(result)
}



#' Compares expected and actual payments per reserve class
#'
#' @description
#' This function compares the expected payments with the actual payments that arose the next year per reserve class.
#'
#'
#' @param sim_result_series_old Object created by [sicr_series()] containing a series of possible realisations.
#' @param extended_claims_data_new Object created by [prepare_data()] at least one year later.
#'
#' @returns
#' ggplot2 object
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' claims_data_old <- dplyr::filter(claims_data_xmpl,
#'                                  Calendar_year <= 2022)
#'
#' historic_indices_old <- dplyr::filter(historic_indices_xmpl,
#'                                       Calendar_year <= 2022)
#'
#' indices_old <- expand_historic_indices(historic_indices_old,
#'                                        first_orig_year = 1989,
#'                                        last_orig_year = 2022,
#'                                        index_gross_future = 0.03,
#'                                        index_re_future = 0.02)
#'
#' indices_old <- add_transition_factor(indices_old, index_year = 2022)
#'
#' extended_claims_data <- prepare_data(claims_data = claims_data_old,
#'                                      indices = indices_old,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2022,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2022,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2022)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2022)
#'
#' series <- sicr_series(n = 10,
#'                       large_claims_list = large_claims_list,
#'                       first_orig_year = 1989,
#'                       last_orig_year = 2022,
#'                       pools = pools,
#'                       indices = indices_old,
#'                       history = history)
#'
#' extended_claims_data_new <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' g <- backtesting_1y_per_reserve_class(sim_result_series_old = series,
#'                                       extended_claims_data_new = extended_claims_data_new)
#' @importFrom stats quantile
#' @importFrom dplyr tibble bind_rows filter summarise pull mutate
#' @importFrom ggplot2 ggplot aes geom_bar geom_segment
#' scale_fill_manual guides guide_legend scale_color_manual
#' theme_minimal labs theme element_text element_blank
backtesting_1y_per_reserve_class <- function(sim_result_series_old,
                                             extended_claims_data_new) {

   # Setting arguments to NULL to prevent note in CMD check
   Claim_id <- Calendar_year <- Ind_cl_payment_cal <-
      Type <- Reserve_class <- Value <- NULL

   # define colors
   col1 <- "#0446BE"
   col2 <- "#4586FB"
   col3 <- "#C1D7FE"
   # pull additional objects from attributes
   last_orig_year <- attr(sim_result_series_old, "last_orig_year") + 1
   large_claims_list <- attr(sim_result_series_old, "large_claims_list")
   n_rc <- max(large_claims_list$Exit_reserve_class)

   # Create dataframe
   df_backtesting <- lapply(0:n_rc, function(rc) {
      v_idx <- which(large_claims_list$Exit_reserve_class == rc)

      # summarize per reserve class
      series_sum <- colSums(sim_result_series_old[v_idx, 1, , drop = FALSE])

      # calculate quantiles
      q <- quantile(series_sum, probs = c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99))

      # calculate actual value
      actual_val <- extended_claims_data_new %>%
         filter(Claim_id %in% large_claims_list$Claim_id[v_idx],
                Calendar_year == last_orig_year) %>%
         summarise(sum = sum(Ind_cl_payment_cal)) %>%
         pull(sum)

      # combine to tibble
      tibble(
         Reserve_class = as.character(rc),
         Type = c("Expected", "Q1", "Q10", "Q25", "Q75", "Q90", "Q99", "Actual"),
         Value = c(
            mean(series_sum),
            q[1],
            q[2] - q[1],
            q[3] - q[2],
            q[4] - q[3],
            q[5] - q[4],
            q[6] - q[5],
            actual_val
         )
      )
   }) %>%
      bind_rows() %>%
      mutate(
         Type = factor(Type,
                       levels = c("Q99",
                                  "Q90",
                                  "Q75",
                                  "Q25",
                                  "Q10",
                                  "Q1",
                                  "Expected",
                                  "Actual")),
         Reserve_class = factor(Reserve_class,
                                levels = as.character(0:n_rc))
      ) %>%
      as.data.frame()



   ggplot(df_backtesting, aes(x = as.factor(Reserve_class),
                              y = Value / 1e6,
                              fill = Type)) +
      geom_bar(
         data = filter(df_backtesting,
                       !(Type %in% c("Expected", "Actual"))),
         stat = "identity",
         alpha = 1,
         width = 0.8) +
      geom_segment(
         data = filter(df_backtesting,
                       Type %in% c("Expected", "Actual")),
         aes(
            x = as.numeric(Reserve_class) - 0.25,
            xend = as.numeric(Reserve_class) + 0.25,
            y = Value / 1e6,
            yend = Value / 1e6,
            color = Type
         ),
         linewidth = 1.5
      ) +
      scale_fill_manual(
         values = c(
            "Q1" = "NA",
            "Q10" = col3,
            "Q25" = col2,
            "Q75" = col1,
            "Q90" = col2,
            "Q99" = col3
         ),
         labels = c(
            "25% - 75%",
            "10% - 90%",
            "1% - 99%",
            "",
            "",
            ""
         )
      ) +
      scale_color_manual(
         name = "",
         values = c("Expected" = "black", "Actual" = "darkorange"),
         labels = c("Expected" = "Expected", "Actual" = paste("Actual", last_orig_year))
      ) +
      guides(
         fill = guide_legend(
            override.aes = list(
               fill = c(
                  col1,
                  col2,
                  col3,
                  "NA",
                  "NA",
                  "NA"
               ),
               order = 2
            ),
            title = "Quantiles"
         ),
         color = guide_legend(
            order = 1
         )
      ) +
      theme_minimal() +
      labs(
         x = "Reserve class",
         y = "Payments in mio.",
         title = "Actual vs. expected large claims payments"
      ) +
      theme(
         plot.title = element_text(hjust = 0.5),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )
}



#' Compares expected and actual payments per origin year
#'
#' @description
#' This function compares the expected payments with the actual payments that arose the next year per origin year.
#'
#'
#' @param sim_result_series_old Object created by [sicr_series()] containing a series of possible realisations.
#' @param extended_claims_data_new Object created by [prepare_data()] at least one year later.
#'
#' @returns
#' ggplot2 object
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' claims_data_old <- dplyr::filter(claims_data_xmpl,
#'                                  Calendar_year <= 2022)
#'
#' historic_indices_old <- dplyr::filter(historic_indices_xmpl,
#'                                       Calendar_year <= 2022)
#'
#' indices_old <- expand_historic_indices(historic_indices_old,
#'                                        first_orig_year = 1989,
#'                                        last_orig_year = 2022,
#'                                        index_gross_future = 0.03,
#'                                        index_re_future = 0.02)
#'
#' indices_old <- add_transition_factor(indices_old, index_year = 2022)

#' extended_claims_data <- prepare_data(claims_data = claims_data_old,
#'                                      indices = indices_old,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2022,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2022,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2022)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2022)
#'
#' series <- sicr_series(n = 10,
#'                       large_claims_list = large_claims_list,
#'                       first_orig_year = 1989,
#'                       last_orig_year = 2022,
#'                       pools = pools,
#'                       indices = indices_old,
#'                       history = history)
#'
#' extended_claims_data_new <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' g <- backtesting_1y_per_origin_year(sim_result_series_old = series,
#'                                     extended_claims_data_new = extended_claims_data_new)
#' @importFrom stats quantile
#' @importFrom dplyr tibble bind_rows filter summarise pull mutate
#' @importFrom ggplot2 ggplot aes geom_bar geom_segment
#' scale_fill_manual guides guide_legend theme_minimal
#' labs theme element_text element_blank scale_color_manual
backtesting_1y_per_origin_year <- function(sim_result_series_old,
                                           extended_claims_data_new) {

   # Setting arguments to NULL to prevent note in CMD check
   Claim_id <- Calendar_year <- Ind_cl_payment_cal <- Type <-
      Origin_year <- Value <- NULL

   # define colors
   col1 <- "#0446BE"
   col2 <- "#4586FB"
   col3 <- "#C1D7FE"

   # pull additional objects from attributes
   last_orig_year <- attr(sim_result_series_old, "last_orig_year") + 1
   large_claims_list <- attr(sim_result_series_old, "large_claims_list")
   orig_years <- min(large_claims_list$Origin_year):
      max(large_claims_list$Origin_year)

   # Create dataframe
   df_backtesting <- lapply(orig_years, function(orig_year) {
      v_idx <- which(large_claims_list$Origin_year == orig_year)

      # summarize per reserve class
      series_sum <- colSums(sim_result_series_old[v_idx, 1, , drop = FALSE])

      # calculate quantiles
      q <- quantile(series_sum, probs = c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99))

      # calculate actual value
      actual_val <- extended_claims_data_new %>%
         filter(Claim_id %in% large_claims_list$Claim_id[v_idx],
                Calendar_year == last_orig_year) %>%
         summarise(sum = sum(Ind_cl_payment_cal)) %>%
         pull(sum)

      # combine to tibble
      tibble(
         Origin_year = as.character(orig_year),
         Type = c("Expected", "Q1", "Q10", "Q25", "Q75", "Q90", "Q99", "Actual"),
         Value = c(
            mean(series_sum),
            q[1],
            q[2] - q[1],
            q[3] - q[2],
            q[4] - q[3],
            q[5] - q[4],
            q[6] - q[5],
            actual_val
         )
      )
   }) %>%
      bind_rows() %>%
      mutate(
         Type = factor(Type,
                       levels = c("Q99",
                                  "Q90",
                                  "Q75",
                                  "Q25",
                                  "Q10",
                                  "Q1",
                                  "Expected",
                                  "Actual")),
         Origin_year = factor(Origin_year,
                              levels = as.character(orig_years))
      ) %>%
      as.data.frame()



   ggplot(df_backtesting, aes(x = Origin_year,
                              y = Value / 1e6,
                              fill = Type)) +
      geom_bar(
         data = filter(df_backtesting,
                       !(Type %in% c("Expected", "Actual"))),
         stat = "identity",
         alpha = 1,
         width = 0.8) +
      geom_segment(
         data = filter(df_backtesting,
                       Type %in% c("Expected", "Actual")),
         aes(
            x = as.numeric(as.character(Origin_year)) - min(orig_years) + 1 - 0.25,
            xend = as.numeric(as.character(Origin_year)) - min(orig_years) + 1 + 0.25,
            y = Value / 1e6,
            yend = Value / 1e6,
            color = Type
         ),
         linewidth = 1.5
      ) +
      scale_fill_manual(
         values = c(
            "Q1" = "NA",
            "Q10" = col3,
            "Q25" = col2,
            "Q75" = col1,
            "Q90" = col2,
            "Q99" = col3
         ),
         labels = c(
            "25% - 75%",
            "10% - 90%",
            "1% - 99%",
            "",
            "",
            ""
         )
      ) +
      scale_color_manual(
         name = "",
         values = c("Expected" = "black", "Actual" = "darkorange"),
         labels = c("Expected" = "Expected", "Actual" = paste("Actual", last_orig_year))
      ) +
      guides(
         fill = guide_legend(
            override.aes = list(
               fill = c(
                  col1,
                  col2,
                  col3,
                  "NA",
                  "NA",
                  "NA"
               ),
               order = 2
            ),
            title = "Quantiles"
         ),
         color = guide_legend(
            order = 1
         )
      ) +
      theme_minimal() +
      labs(
         x = "Origin year",
         y = "Payments in mio.",
         title = "Actual vs. expected large claims payments"
      ) +
      theme(
         plot.title = element_text(hjust = 0.5),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )
}


#' Compares expected and actual payments for single claims
#'
#' @description
#' This function compares the expected payments with the actual payment that arose the next year on a single claim
#' basis.
#'
#' @param sim_result_series_old Object created by [sicr_series()] containing a series of possible realisations.
#' @param extended_claims_data_new Object created by [prepare_data()] at least one year later.
#' @param reserve_class Integer. Desired reserve class.
#' @param dev_years Integer vector for the development years (since turning large) to be included.
#' @param label Must be one of "Claim_id" or "Origin_year". Default: "Claim_id". Note that no labels will be shown
#' if ggrepel package is not installed.
#'
#' @returns
#' ggplot2 object
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' claims_data_old <- dplyr::filter(claims_data_xmpl,
#'                                  Calendar_year <= 2022)
#'
#' historic_indices_old <- dplyr::filter(historic_indices_xmpl,
#'                                       Calendar_year <= 2022)
#'
#' indices_old <- expand_historic_indices(historic_indices_old,
#'                                        first_orig_year = 1989,
#'                                        last_orig_year = 2022,
#'                                        index_gross_future = 0.03,
#'                                        index_re_future = 0.02)
#'
#' indices_old <- add_transition_factor(indices_old, index_year = 2022)

#' extended_claims_data <- prepare_data(claims_data = claims_data_old,
#'                                      indices = indices_old,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2022,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2022,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2022)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2022)
#'
#' series <- sicr_series(n = 10,
#'                       large_claims_list = large_claims_list,
#'                       first_orig_year = 1989,
#'                       last_orig_year = 2022,
#'                       pools = pools,
#'                       indices = indices_old,
#'                       history = history)
#'
#' extended_claims_data_new <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' g <- backtesting_1y_single_per_dev_year(sim_result_series_old = series,
#'                                         extended_claims_data_new = extended_claims_data_new,
#'                                         reserve_class = 3,
#'                                         dev_year = 2:16)
#' @importFrom ggplot2 ggplot aes geom_bar geom_segment geom_point
#' position_dodge2 scale_fill_manual guides guide_legend scale_color_manual
#' theme_minimal labs theme element_text element_blank
#' @importFrom dplyr sym tibble bind_rows mutate
#' @importFrom stats quantile
backtesting_1y_single_per_dev_year <- function(sim_result_series_old,
                                               extended_claims_data_new,
                                               reserve_class,
                                               dev_years,
                                               label = "Claim_id") {

   # Setting arguments to NULL to prevent note in CMD check
   Type <- Development_year <- Entry_reserve_class <- Calendar_year <-
      Claim_id <- Dev_year_since_large <- Value <- Cl_payment_cal <- NULL

   # prepare label name for ggplot
   label_var <-  if(label == "Claim_id") {
      sym("Claim_id")
   } else {
      sym("Origin_year")
   }

   # define colors
   col1 <- "#0446BE"
   col2 <- "#4586FB"
   col3 <- "#C1D7FE"

   # pull additional objects from attributes
   last_orig_year <- attr(sim_result_series_old, "last_orig_year") + 1
   large_claims_list <- attr(sim_result_series_old, "large_claims_list")

   # filter to relevant rows
   relevant_rows <- which(large_claims_list$Exit_reserve_class == reserve_class)
   sim_result_series_old <- sim_result_series_old[relevant_rows, 1, , drop = FALSE]
   large_claims_list <- large_claims_list[relevant_rows, , drop = FALSE]

   # Create dataframe
   df_backtesting <- lapply(dev_years, function(dev_year) {
      v_idx <- which(large_claims_list$Dev_year_since_large == (dev_year - 1))

      if (NROW(v_idx) == 0) {
         tibble(
            Development_year = rep(as.character(dev_year), 7),
            Type = c("Expected", "Q1", "Q10", "Q25", "Q75", "Q90", "Q99"),
            Value = rep(0, 7)
         )
      } else {
         # summarize per reserve class
         series_sum <- sim_result_series_old[v_idx, 1, , drop = TRUE]

         # calculate quantiles
         q <- quantile(series_sum, probs = c(0.01, 0.1, 0.25, 0.75, 0.9, 0.99))

         # combine to tibble
         tibble(
            Development_year = as.character(dev_year),
            Type = c("Expected", "Q1", "Q10", "Q25", "Q75", "Q90", "Q99"),
            Value = c(
               mean(series_sum),
               q[1],
               q[2] - q[1],
               q[3] - q[2],
               q[4] - q[3],
               q[5] - q[4],
               q[6] - q[5]
            )
         )
      }
   }) %>%
      bind_rows() %>%
      mutate(
         Type = factor(Type,
                       levels = c("Q99",
                                  "Q90",
                                  "Q75",
                                  "Q25",
                                  "Q10",
                                  "Q1",
                                  "Expected")),
         Development_year = factor(Development_year,
                                   levels = as.character(dev_years))
      ) %>%
      as.data.frame()

   # Filter new data to desired actual payments
   filtered_extended_claims_data <- filter(extended_claims_data_new,
                                           Entry_reserve_class == reserve_class,
                                           Calendar_year == last_orig_year,
                                           Claim_id %in% large_claims_list$Claim_id,
                                           Dev_year_since_large %in% dev_years)

   # plot
   p <-
      ggplot(df_backtesting,
             aes(
                x = as.factor(Development_year),
                y = Value / 1e6)
      ) +
      geom_bar(
         data = filter(df_backtesting,
                       !(Type %in% c("Expected"))
         ),
         aes(
            fill = Type
         ),
         stat = "identity",
         alpha = 1,
         width = 0.8
      ) +
      geom_segment(
         data = filter(df_backtesting,
                       Type %in% c("Expected")
         ),
         aes(
            x = as.numeric(Development_year) - 0.25,
            xend = as.numeric(Development_year) + 0.25,
            y = Value / 1e6,
            yend = Value / 1e6,
            color = Type
         ),
         linewidth = 1.5
      ) +
      geom_point(
         data = filtered_extended_claims_data,
         aes(
            x = as.factor(Dev_year_since_large),
            y = Cl_payment_cal / 1e6,
            color = "Actual"
         ),
         position = position_dodge2(0.2),
         size = 3,
         shape = 21,
         fill = "darkorange"
      )

   if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
         data = filtered_extended_claims_data,
         aes(
            label = !!label_var,
            x = as.factor(Dev_year_since_large),
            y = Cl_payment_cal / 1e6
         ),
         size = 3
      )
   }

   p <- p +
      scale_fill_manual(
         values = c(
            "Q1" = "NA",
            "Q10" = col3,
            "Q25" = col2,
            "Q75" = col1,
            "Q90" = col2,
            "Q99" = col3
         ),
         labels = c(
            "Q1" = "",
            "Q10" = "",
            "Q25" = "",
            "Q75" = "1% - 99% Quantil",
            "Q90" = "10% - 90% Quantil",
            "Q99" = "25% - 75% Quantil"
         )
      ) +
      guides(
         fill = guide_legend(
            override.aes = list(
               fill = c(col1,
                        col2,
                        col3,
                        "NA",
                        "NA",
                        "NA"
               )
            ),
            title = "Quantiles",
            order = 2),
         color = guide_legend(
            title = "",
            order = 1,
            override.aes = list(
               shape = c(19, 21),
               linetype = c(1, 1),
               color = c("black", "white"),
               fill = c("NA", "darkorange")
            )
         )
      ) +
      scale_color_manual(values = c("Expected" = "black",
                                    "Actual" = "darkorange")
      ) +
      theme_minimal(
      ) +
      labs(
         x = "Development year as large claim",
         y = "Payments in mio.",
         title = "Actual vs. expected for single large claims",
         subtitle = paste(sep = "",
                          "Reserve class ", reserve_class)
      ) +
      theme(
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )
   return(p)
}


#' Compares expected and actual payments for more than one year
#'
#' @description
#' This function compares expected and actual payments for desired reserve classes and development years
#' on a multi-year basis.
#'
#' @param sim_result_series_old Object created by [sicr_series()] containing a series of possible realisations.
#' @param extended_claims_data_new Object created by [prepare_data()] at least one year later.
#' @param reserve_classes Integer vector of desired reserve classes.
#' @param dev_years Integer vector of desired development years.
#'
#' @returns
#' ggplot2 object
#'
#' @export
#'
#' @examples
#' # this example uses data provided with this package
#' claims_data_old <- dplyr::filter(claims_data_xmpl,
#'                                  Calendar_year <= 2020)
#'
#' historic_indices_old <- dplyr::filter(historic_indices_xmpl,
#'                                       Calendar_year <= 2020)
#'
#' indices_old <- expand_historic_indices(historic_indices_old,
#'                                        first_orig_year = 1989,
#'                                        last_orig_year = 2020,
#'                                        index_gross_future = 0.03,
#'                                        index_re_future = 0.02)
#'
#' indices_old <- add_transition_factor(indices_old, index_year = 2022)

#' extended_claims_data <- prepare_data(claims_data = claims_data_old,
#'                                      indices = indices_old,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2020,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#' pools <- generate_pools(extended_claims_data = extended_claims_data,
#'                         reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                         years_for_pools = 2014:2020,
#'                         start_of_tail = 17,
#'                         end_of_tail = 50,
#'                         lower_outlier_limit = -Inf,
#'                         upper_outlier_limit = Inf,
#'                         pool_of_annuities = pool_of_annuities_xmpl)
#'
#' large_claims_list <- generate_claims_list(extended_claims_data = extended_claims_data,
#'                                           first_orig_year = 1989,
#'                                           last_orig_year = 2020)
#'
#' history <- generate_history_per_claim(data = extended_claims_data,
#'                                       column = "Cl_payment_cal",
#'                                       first_orig_year = 1989,
#'                                       last_orig_year = 2020)
#'
#' series <- sicr_series(n = 10,
#'                       large_claims_list = large_claims_list,
#'                       first_orig_year = 1989,
#'                       last_orig_year = 2020,
#'                       pools = pools,
#'                       indices = indices_old,
#'                       history = history)
#'
#' extended_claims_data_new <- prepare_data(claims_data = claims_data_xmpl,
#'                                      indices = indices_xmpl,
#'                                      threshold = 400000,
#'                                      first_orig_year = 1989,
#'                                      last_orig_year = 2023,
#'                                      expected_year_of_growing_large = 3,
#'                                      reserve_classes = c(1, 200001, 400001, 700001, 1400001),
#'                                      pool_of_annuities = pool_of_annuities_xmpl)
#'
#' g <- backtesting_multiyears(sim_result_series_old = series,
#'                             extended_claims_data_new = extended_claims_data_new,
#'                             reserve_classes = 3,
#'                             dev_year = 2:16)
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#' labs theme_minimal scale_y_continuous scale_x_continuous scale_fill_manual
#' scale_color_manual scale_shape_manual guides guide_legend
#' theme element_text element_blank
backtesting_multiyears <- function(sim_result_series_old,
                                   extended_claims_data_new,
                                   reserve_classes = NULL,
                                   dev_years = NULL) {

   # Setting arguments to NULL to prevent note in CMD check
   Year <- Expected <- NULL

   # define colors
   col1 <- "#0446BE"
   col2 <- "#4586FB"
   col3 <- "#C1D7FE"

   # pull additional objects from attributes
   reserving_year <- attr(sim_result_series_old, "last_orig_year")
   large_claims_list <- attr(sim_result_series_old, "large_claims_list")

   if (is.null(reserve_classes)) {reserve_classes <- unique(large_claims_list$Exit_reserve_class)}
   if (is.null(dev_years)) {dev_years <- unique(large_claims_list$Dev_year_since_large)}
   claim_ids <-
      large_claims_list$Claim_id[
         large_claims_list$Exit_reserve_class %in% reserve_classes &
            large_claims_list$Dev_year_since_large %in% dev_years]

   row_ind <- which(large_claims_list$Claim_id %in% claim_ids)
   n_years <- max(extended_claims_data_new$Calendar_year) - reserving_year

   # Fehler abfangen, dass dass years_count < 1

   expected <- q1 <- q10 <- q25 <- q50 <- q75 <- q90 <- q99 <- actual <- n_years
   for (year in 1:n_years) {
      values <- colSums(colSums(sim_result_series_old[row_ind, 1:year, , drop = FALSE]))
      expected[year] <- mean(values)
      q1[year] <- quantile(values, 0.01)
      q10[year] <- quantile(values, 0.1)
      q25[year] <- quantile(values, 0.25)
      q50[year] <- quantile(values, 0.5)
      q75[year] <- quantile(values, 0.75)
      q90[year] <- quantile(values, 0.9)
      q99[year] <- quantile(values, 0.99)
      actual[year] <-
         sum(
            extended_claims_data_new$Cl_payment_cal[
               extended_claims_data_new$Claim_id %in% claim_ids &
                  extended_claims_data_new$Calendar_year %in% (reserving_year + 1):(reserving_year + year)])
   }

   data <- data.frame(Year = (reserving_year + 1):(reserving_year + n_years),
                      Expected = expected,
                      Median = q50)
   # Erstellen des ggplot Diagramms
   ggplot(data,
          aes(
             x = Year,
             y = Expected)
   ) +
      geom_ribbon(
         aes(
            ymin = q1,
            ymax = q99,
            fill = "1% - 99%"
         ),
         alpha = 0.5) +
      geom_ribbon(
         aes(
            ymin = q10,
            ymax = q90,
            fill = "10% - 90%"
         ),
         alpha = 0.5) +
      geom_ribbon(
         aes(
            ymin = q25,
            ymax = q75,
            fill = "25% - 75%"
         ),
         alpha = 0.5) +
      geom_line(
         aes(
            y = q50,
            color = "Median"
         ),
         linewidth = 2) +
      geom_line(
         aes(
            y = expected,
            color = "Expected"
         ),
         linewidth = 2) +
      geom_point(
         aes(
            y = actual,
            shape = "Actual"),
         size = 6,
         stroke = 1.5,
         color = "black",
         fill = "darkorange",
         alpha = 1) +
      labs(
         x = "Year",
         y = "predicted payments in mio.",
         title = paste("Actual vs. Expected for reserve classes",
                       if (min(reserve_classes) == max(reserve_classes)) {
                          min(reserve_classes)
                       } else {
                          paste(min(reserve_classes), "to", max(reserve_classes))
                       },
                       "and dev. years",
                       if (min(dev_years) == max(dev_years)) {
                          min(dev_years)
                       } else {
                          paste(min(dev_years), "to", max(dev_years))
                       }
         ),
         subtitle = paste("Reserving year", reserving_year)
      ) +
      theme_minimal(
      ) +
      scale_y_continuous(
         labels = function(x) format(
            x / 1e6,
            big.mark = ".",
            decimal.mark = ",",
            nsmall = 1,
            trim = TRUE
         )
      ) +
      scale_x_continuous(
         labels = reserving_year + (1:n_years),
         breaks = reserving_year + (1:n_years)
      ) +
      scale_fill_manual(
         values = c("25% - 75%" = col1,
                    "10% - 90%" = col2,
                    "1% - 99%" = col3)
      ) +
      scale_color_manual(
         values = c("Median" = col3,
                    "Expected" = "black"),
         labels = c("Expected" = "Mean expected",
                    "Median" = "Median expected")
      ) +
      scale_shape_manual(
         values = c("Actual" = 21)) +
      guides(
         fill = guide_legend(
            title = "Quantile",
            order = 3),
         color = guide_legend(
            title = "",
            override.aes = list(shape = NA),
            order = 2),
         shape = guide_legend(
            title = "",
            order = 1)) +
      theme(
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank()
      )
}
