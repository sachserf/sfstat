#' calculate memory usage
#'
#' @description Calculate the size of all objects within globalenv and baseenv.
#' @param units Character. Specify dimension of output. See ?utils::object.size for possible values.
#' @seealso \code{\link[utils]{object.size}}
#' @return A data frame with the sum of the size of all objects within the global environment and the base environment
#' @author Frederik Sachser
#' @export
summary_memory_usage <- function(units = "Mb") {
  inner_fun <- function(x) {
    return(utils::object.size(x = get(x)))
  }

  base_env_objects <- ls(envir = baseenv(), all.names = TRUE)
  global_env_objects <- ls(envir = globalenv(), all.names = TRUE)

  if (length(base_env_objects) > 0) {
    size_base_env <-
      sum(sapply(X = base_env_objects, FUN = inner_fun))
    class(size_base_env) <- "object_size"
    size_base_env <- format(size_base_env, units = units)

  }

  if (length(global_env_objects) > 0) {
    size_global_env <-
      sum(sapply(X = global_env_objects, FUN = inner_fun))
    class(size_global_env) <- "object_size"
    size_global_env <- format(size_global_env, units = units)
  } else {
    size_global_env <- 0
  }

  df_out <- data.frame("base_env" = size_base_env, "global_env" = size_global_env)
  row.names(df_out) <- ""
  message("\nmemory:")
  print(t(df_out))
}
