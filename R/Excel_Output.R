
#' Add formatted worksheet to a workbook
#'
#' @description
#' This function adds a properly formatted worksheet with payments and reserves information to an existing workbook.
#'
#' @param workbook Workbook created via `openxlsx2::wb_workbook()`.
#' @param sheetname Desired sheet name.
#' @param payments Numeric matrix of historic and future payments with origin years as rownames and origin years + 250
#' as calendar years as colnames.
#' @param reserved Default: NULL. Numeric matrix with historic payments. Not considered if NULL.
#'
#' @returns Workbook including the new sheet.
#' @export
#'
#' @examples
#' future_payments <- matrix(0, 35, 250)
#' colnames(future_payments) <- 2024:2273
#' future_payments[,1] <- 1e6 # some entries to show functionality
#'
#' payments <- cbind(all_claims_paid_xmpl, future_payments)
#'
#' workbook <- openxlsx2::wb_workbook()
#' workbook <- add_sicr_worksheet(workbook = workbook,
#'                                sheetname = "new_sheet",
#'                                payments = payments,
#'                                reserved = all_claims_reserved_xmpl)
#'
#' # either open with openxlsx2::wb_open(workbook) or save via openxlsx2::wb_save(workbook)
#' @importFrom dplyr "%>%"
#' @importFrom openxlsx2 wb_add_worksheet wb_color wb_set_col_widths wb_freeze_pane wb_set_base_font
#' wb_add_data wb_add_cell_style wb_dims wb_add_fill wb_add_numfmt wb_add_font wb_add_cell_style
#' create_hyperlink wb_add_formula
add_sicr_worksheet <- function(workbook, sheetname, payments, reserved = NULL) {

   ### define gradient_fills
   # header left
   gf_header_left <- '<gradientFill type="linear" degree="180">
                      <stop position="1"><color rgb="A6A6A6"/></stop>
                      <stop position="0"><color rgb="808080"/></stop>
                      </gradientFill>'

   # header top
   gf_header_top <- '<gradientFill type="linear" degree="90">
                     <stop position="1"><color rgb="A6A6A6"/></stop>
                     <stop position="0"><color rgb="808080"/></stop>
                     </gradientFill>'

   # future payments
   gf_future <- '<gradientFill type="linear" degree="180">
                 <stop position="1"><color rgb="FCE4D6"/></stop>
                 <stop position="0"><color rgb="F8CBAD"/></stop>
                 </gradientFill>'

   # historic payments and reserves
   gf_history <- '<gradientFill type="linear" degree="180">
                  <stop position="1"><color rgb="DDEBF7"/></stop>
                  <stop position="0"><color rgb="BDD7EE"/></stop>
                  </gradientFill>'


   ### extend payments by calculated columns
   years_count <- NROW(payments)
   min_year <- min(as.integer(rownames(payments)))
   max_year <- max(as.integer(rownames(payments)))
   reserved_vector <- if (is.null(reserved)) {rep(0, years_count)} else {reserved[, years_count]}

   data <- cbind(rowSums(payments[,1:years_count]), # cumulative payments
                 reserved_vector, # reserves
                 rowSums(payments[, 1:(years_count + 250)]), # ultimates
                 rowSums(payments[, (years_count + 1):(years_count + 250)]), # best estimates
                 payments)

   # insert sum row on top
   data <- rbind(colSums(data), data)

   # names
   dimnames(data) <- list(c("Dev. years   \u2215   Sum",
                            dimnames(payments)[[1]]),
                          c("Cum. payments",
                            "Reserve",
                            "Ultimate",
                            "Best Estimate",
                            dimnames(payments)[[2]]))


   ### prepare excel sheet
   cols_sum <- 5:8
   cols_history <- 9:(9 + years_count - 1)
   cols_future <- (9 + years_count):(9 + years_count + 249)
   cols_all <- c(cols_sum, cols_history, cols_future)
   rows <- 4:(4 + years_count)

   # add sheet
   options("openxlsx2.string_nums" = TRUE)
   workbook <- workbook %>% wb_add_worksheet(sheetname,
                                             grid_lines = FALSE,
                                             tab_color = wb_color(hex = "#074f69")) %>%


      # column widths
      wb_set_col_widths(sheetname,
                        cols = c(1:4, cols_all),
                        widths = c(rep(3,3),
                                   rep(18, 5),
                                   rep(13, years_count + 250))) %>%

      # freeze pane
      wb_freeze_pane(sheetname,
                     first_active_row = 5,
                     first_active_col = 9) %>%

      # set font
      wb_set_base_font(
         font_size = 11,
         font_color = wb_color(hex = "#404040"),
         font_name = "Calibri") %>%

      # add data
      wb_add_data(sheetname,
                  data,
                  start_row = 3,
                  start_col = 4,
                  col_names = TRUE,
                  row_names = TRUE) %>%

      # insert note for calendar years
      wb_add_data(sheetname,
                  "Cal. years --->",
                  start_col = 8,
                  start_row = 2,
                  col_names = FALSE,
                  row_names = FALSE) %>%

      # format note for calendar years
      wb_add_cell_style(sheetname,
                        dims = wb_dims(rows = 2, cols = 8),
                        horizontal = "right",
                        vertical = "center") %>%

      # add the four different gradient fills
      wb_add_fill(sheetname,
                  dims = wb_dims(rows = 3, cols = cols_all),
                  gradient_fill = gf_header_top) %>%
      wb_add_fill(sheetname,
                  dims = wb_dims(rows = rows, cols = 4),
                  gradient_fill = gf_header_left) %>%
      wb_add_fill(sheetname,
                  dims = wb_dims(rows = rows, cols = c(5, 6, cols_history)),
                  gradient_fill = gf_history) %>%
      wb_add_fill(sheetname,
                  dims = wb_dims(rows = rows, cols = c(7, 8, cols_future)),
                  gradient_fill = gf_future) %>%

      # set number formats
      wb_add_numfmt(sheetname,
                    dims = wb_dims(rows = rows, cols = 4),
                    numfmt = "0") %>%
      wb_add_numfmt(sheetname,
                    dims = wb_dims(rows = rows, cols = c(cols_sum, cols_history, cols_future)),
                    numfmt = "#,##0") %>%

      # set fonts (bold and white)
      wb_add_font(sheetname,
                  dims = wb_dims(rows = c(3), cols = cols_all),
                  size = "11",
                  name = "Calibri",
                  color = wb_color("white")) %>%
      wb_add_font(sheetname,
                  dims = wb_dims(rows = c(4), cols = c(4, cols_all)),
                  bold = TRUE,
                  size = "11",
                  name = "Calibri",
                  color = wb_color(hex = "#404040")) %>%
      wb_add_font(sheetname,
                  dims = wb_dims(rows = rows, cols = c(4)),
                  size = "11",
                  name = "Calibri",
                  color = wb_color("white")) %>%

      # align to center
      wb_add_cell_style(sheetname,
                        dims = wb_dims(rows = c(3), cols = cols_all),
                        horizontal = "center",
                        vertical = "center") %>%
      wb_add_cell_style(sheetname,
                        dims = wb_dims(rows = rows, cols = c(4)),
                        horizontal = "center",
                        vertical = "center") %>%
      # can't use create_hyperlink because the formula must not contain sheetname for the purpose of
      # copying the sheet later
      wb_add_formula(sheetname,
                     x = paste0('=HYPERLINK(\"#',
                                wb_dims(5, min(cols_future)),
                                '\", \"Jump to ',
                                max_year + 1,
                                '\"',
                                ')'),
                     dims = "I2") %>%
      wb_add_formula(sheetname,
                     x = paste0('=HYPERLINK(\"#',
                                wb_dims(5, 9),
                                '\", \"Jump to ',
                                min_year,
                                '\"',
                                ')'),
                     dims = wb_dims(rows = 2, cols = min(cols_future))) %>%
      return()

}


#' Create Excel sheet by adding or subtracting other sheets
#'
#' @description
#' This function can be used after creating excel worksheets with [add_sicr_worksheet()] to derive
#' worksheets as aggregate of these. Addition and subtraction are supported to allow for a net after reinsurance
#' perspective.
#'
#' @param workbook Workbook created via `openxlsx2::wb_workbook()`.
#' @param sheetname Desired sheet name.
#' @param sheets_to_add Vector with the names of the sheets that shall be added.
#' @param sheets_to_subtract Vector with the names of the sheets that shall be subtracted.
#'
#' @returns Workbook including the new sheet.
#' @export
#'
#' @examples
#' future_payments <- matrix(0, 35, 250)
#' colnames(future_payments) <- 2024:2273
#' future_payments[,1] <- 1e6 # some entries to show functionality
#'
#' payments <- cbind(all_claims_paid_xmpl, future_payments)
#'
#' workbook <- openxlsx2::wb_workbook()
#' workbook <- add_sicr_worksheet(workbook = workbook,
#'                                sheetname = "add1",
#'                                payments = payments,
#'                                reserved = all_claims_reserved_xmpl)
#' workbook <- add_sicr_worksheet(workbook = workbook,
#'                                sheetname = "add2",
#'                                payments = payments,
#'                                reserved = all_claims_reserved_xmpl)
#' workbook <- add_sicr_worksheet(workbook = workbook,
#'                                sheetname = "sub1",
#'                                payments = payments,
#'                                reserved = all_claims_reserved_xmpl)
#'
#' workbook <- add_sicr_aggr_worksheet(workbook = workbook,
#'                                    sheetname = "Sum_sheet",
#'                                    sheets_to_add = c("add1", "add2"),
#'                                    sheets_to_subtract = c("sub1"))
#'
#' # either open with openxlsx2::wb_open(workbook) or save via openxlsx2::wb_save(workbook)
#'
#' @importFrom openxlsx2 wb_get_sheet_names wb_read wb_clone_worksheet wb_add_data
add_sicr_aggr_worksheet <- function(workbook, sheetname, sheets_to_add = NULL, sheets_to_subtract = NULL) {
   # check if sheets_to_add and sheets_to_substract are NULL
   if (is.null(sheets_to_add) & is.null(sheets_to_subtract)) {
      stop("One of sheets_to_add and sheets_to_subtract must not be NULL.")
   }

   # check if all specified sheets exist
   sheets <- wb_get_sheet_names(workbook)
   if (!is.null(sheets_to_add)) {
      if (!all(sheets_to_add %in% sheets)) {
         stop("Every sheetname in sheets_to_add must exist in workbook if not NULL.")
      }
   }

   if (!is.null(sheets_to_subtract)) {
      if (!all(sheets_to_subtract %in% sheets)) {
         stop("Every sheetname in sheets_to_subtract must exist in workbook if not NULL.")
      }
   }

   # create list with all matrices that should be added and add with Reduce function
   if (is.null(sheets_to_add)) {pos <- 0} else {
      mat_list_to_add <- lapply(sheets_to_add, function(sheet) {
         as.matrix(wb_read(workbook, sheet, start_row = 4, start_col = 5, col_names = FALSE))
      })
      pos <- Reduce("+", mat_list_to_add)
   }

   if (is.null(sheets_to_subtract)) {neg <- 0} else {
      mat_list_to_subtract <- lapply(sheets_to_subtract, function(sheet) {
         as.matrix(wb_read(workbook, sheet, start_row = 4, start_col = 5, col_names = FALSE))
      })
      neg <- Reduce("+", mat_list_to_subtract)
   }

   # aggregate
   agg_matrix <- pos - neg

   # add new sheet and fill new sheet in workbook with result
   workbook <- wb_clone_worksheet(workbook,
                                  old = if (is.null(sheets_to_add)) sheets_to_subtract[1] else sheets_to_add[1],
                                  new = sheetname,
                                  from = workbook)

   workbook <- wb_add_data(workbook,
                           sheetname,
                           x = agg_matrix,
                           start_col = 5,
                           start_row = 4,
                           col_names = FALSE)

   return(workbook)
}
