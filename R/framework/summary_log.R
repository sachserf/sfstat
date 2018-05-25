#' write a log entry
#'
#' @description This function will summarize the information of a specific
#'   log-file.
#' @inheritParams write_log
#' @return A list of dataframes including information about duration of work per
#'   day, per nodename and both.
#' @note The results are not necessarily as intended: It is assumed that you work once a day without breaks. Optionally adjust the logfile as well as the function log_summary to fit your needs.
#' @importFrom magrittr "%>%"
#' @seealso \code{\link{write_log}}
#' @author Frederik Sachser
#' @export
summary_log <-
  function(filepath_log = "log.csv") {
    # read table
    df <-
      utils::read.table(filepath_log, header = TRUE, row.names = NULL)
    # convert posix
    df$POSIX <- as.POSIXct(df$POSIX)
    # create log summary
    per_DATE_NODENAME <- df %>%
      dplyr::group_by_("NODENAME", "DATE", "WEEKDAY") %>%
      dplyr::summarise_("MIN" = min("POSIX"), "MAX" = max("POSIX")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(
        DIFF_HOURS = difftime("MAX", "MIN", units = "hours"),
        DIFF_MINS = difftime("MAX", "MIN", units = "mins")
      ) %>%
      dplyr::arrange_("MIN")
    per_NODENAME <- per_DATE_NODENAME %>%
      dplyr::group_by_("NODENAME") %>%
      dplyr::summarise_("SUM_HOURS" = sum("DIFF_HOURS"), "SUM_MINS" = sum("DIFF_MINS"), "N_DATE" = length(unique("DATE"))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_("SUM_MINS")

    per_DATE <- per_DATE_NODENAME %>%
      dplyr::group_by_("DATE", "WEEKDAY") %>%
      dplyr::summarise_("SUM_HOURS" = sum("DIFF_HOURS"), "SUM_MINS" = sum("DIFF_MINS"), "N_NODENAME" = length(unique("NODENAME"))) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_("DATE")

    log_summaries <- list("per_DATE_NODENAME" = per_DATE_NODENAME, "per_NODENAME" = per_NODENAME, "per_DATE" = per_DATE)

    return(log_summaries)
  }
