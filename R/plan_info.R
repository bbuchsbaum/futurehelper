#' Display Current Future Plan Information
#'
#' Prints a human-readable summary of the active [future::plan()], including
#' the backend, number of workers, nesting depth, and memory limit.
#'
#' @param verbose Logical. If `TRUE` (default), prints the summary. If `FALSE`,
#'   returns the info list silently without printing.
#'
#' @return A list (invisibly) with components:
#'   \describe{
#'     \item{`backend`}{Character name of the plan strategy.}
#'     \item{`workers`}{Integer number of workers.}
#'     \item{`nesting`}{Integer number of plan levels.}
#'     \item{`globals_maxsize_gib`}{Numeric memory limit in GiB, or `Inf`.}
#'     \item{`pid`}{Integer process ID.}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' init_future(cores = 4)
#' plan_info()
#'
#' info <- plan_info(verbose = FALSE)
#' info$workers
#' }
plan_info <- function(verbose = TRUE) {
  current <- plan()
  nlevels <- nbrOfFreeWorkers <- NULL

  strategy_names <- vapply(plan("list"), function(p) {
    cls <- class(p)
    cls <- setdiff(cls, c("FutureStrategy", "future", "function", "tweaked",
                          "multiprocess", "uniprocess"))
    if (length(cls) == 0L) "unknown" else cls[1L]
  }, character(1L))

  nlevels <- length(strategy_names)
  backend_str <- paste(strategy_names, collapse = " / ")

  n_workers <- tryCatch(
    parallelly::availableWorkers(),
    error = function(e) NA_integer_
  )
  if (is.character(n_workers)) n_workers <- length(n_workers)

  max_size <- getOption("future.globals.maxSize", default = Inf)
  max_size_gib <- if (is.infinite(max_size)) Inf else max_size / 1024^3

  info <- list(
    backend = backend_str,
    workers = as.integer(n_workers),
    nesting = nlevels,
    globals_maxsize_gib = round(max_size_gib, 2),
    pid = Sys.getpid()
  )

  if (verbose) {
    cat("Future Plan Summary\n")
    cat("-------------------\n")
    cat("Backend:      ", info$backend, "\n")
    cat("Workers:      ", info$workers, "\n")
    cat("Nesting:      ", info$nesting, " level(s)\n")
    cat("Max globals:  ",
        if (is.infinite(info$globals_maxsize_gib)) "unlimited"
        else paste0(info$globals_maxsize_gib, " GiB"),
        "\n")
    cat("PID:          ", info$pid, "\n")
  }

  invisible(info)
}
