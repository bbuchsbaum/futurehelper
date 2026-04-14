#' Reset Future Plan to Sequential
#'
#' Resets the [future::plan()] to [future::sequential] and clears the
#' `future.globals.maxSize` option back to the default. If the active backend
#' was a package-managed `"mirai_cluster"`, its daemons are also stopped.
#'
#' @param globals_maxsize The value to reset `future.globals.maxSize` to.
#'   Default is `NULL`, which removes the option entirely (restoring the
#'   future package default of ~500 MiB).
#'
#' @return `NULL` (invisibly).
#' @export
#' @examples
#' \dontrun{
#' init_future(cores = 4, mem = 8)
#' # ... do work ...
#' reset_future()
#' }
reset_future <- function(globals_maxsize = NULL) {
  .set_sequential_plan(globals_maxsize = globals_maxsize)
}
