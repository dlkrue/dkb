#' analyze_data
#'
#' @param inp input data.frame as provided by import data
#' @param time_col col.name of inp which contains temporal data
#' @param pattern used to subset the data, multiple input separated by "|"
#' @param pattern_col column to pattern should be applied
#'
#' @return dataframe with aggregated data
#' @export
#'
#' @examples
#' dd <- import_data()
#'
#' #' Example 1: Defaults --------------------------------------------------
#' dd2 <- analyze_data(dd)
#'
#' ggplot(dd2, aes(x = time, y = value, group = variable, col = variable),
#'        alpha = .6) +
#'   geom_line(lwd = 1) +
#'   geom_point(size = 3) +
#'   theme_dark() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = .5))
#'
#'
#' #' Example 2: Lohn --------------------------------------------------
#'
#' #'remove first + last month from data
#' dd_ <- subset(dd,!month %in% range(dd$month))
#'
#' dd2 <- analyze_data(dd_,pattern = "gfk|fraunhofer", time_col = "month")
#'
#'
#' ggplot(dd2, aes(x = time, y = value, group = variable, col = variable),
#'        alpha = .6) +
#'   geom_line(lwd = 1) +
#'   geom_point(size = 3) +
#'   geom_smooth(method = "lm", lwd = .5, lty = "dashed") +
#'   theme_dark() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = .5))
#'
#'
#' #' Example 3: Vodafone -----------------------------------------------
#' dd2 <- analyze_data(dd_,pattern = "vodafone", time_col = "month")
#'
#'
#' ggplot(dd2, aes(x = time, y = value, group = variable, col = variable),
#'        alpha = .6) +
#'   geom_line(lwd = 1) +
#'   geom_point(size = 3) +
#'   geom_smooth(method = "lm", lwd = .5, lty = "dashed") +
#'   theme_dark() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = .5))
#'
#'
#' #' Example 4: Hausgeld ---------------------------------------------
#' dd2 <- analyze_data(dd_,pattern = "FRIEDENSTRASSE", time_col = "month")
#'
#'
#' ggplot(dd2, aes(x = time, y = value, group = variable, col = variable),
#'        alpha = .6) +
#'   geom_line(lwd = 1) +
#'   geom_point(size = 3) +
#'   geom_smooth(method = "lm", lwd = .5, lty = "dashed") +
#'   theme_dark() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = .5))
analyze_data <- function(inp, time_col = "week", pattern = "aldi|rewe|edeka", pattern_col = "AuftraggeberBegunstigter") {
  dd <- subset(inp, grepl(pattern = tolower(pattern), x = tolower(inp[, pattern_col])))
  cat(round(100 * nrow(dd) / nrow(inp), 1), " % of all movements match pattern\n")

  dd2 <- dplyr::summarise(dplyr::group_by_(dd, pattern_col, time_col),
                   Betrag = sum(Betrag, na.rm = T))
  dd2 <- as.data.frame(dd2)
  all_pat <- unlist(stringr::str_split(string = pattern, pattern = "\\|"))
  #
  # comp_amount <- function(tt = dd2, pat, pattern_col = pattern_col, col_ = "Betrag") {
  #   tt <- as.data.frame((tt))
  #   tt[, pat] <- ifelse(grepl(pattern = tolower(pat), x = tolower(tt[, pattern_col])),
  #                       1, 0)
  #   return(tt)
  # }

  for (pat_ in all_pat) {
    dd2[, pat_] <- ifelse(grepl(pattern = tolower(pat_), x = tolower(dd2[, pattern_col])),
                          1, 0) * dd2$Betrag
  }

  dd3 <- reshape2::melt(data = dd2, id.vars = time_col, measure.vars = all_pat)
  names(dd3)[names(dd3) == time_col] <- "time"

  dd4 <- as.data.frame(dplyr::summarise(dplyr::group_by(dd3, time, variable),
                                 value = sum(value)
  ))

  return(dd4)

}
