#' Initialize a Future Plan
#'
#' Sets up a [future::plan()] with sensible defaults so you don't have to
#' remember the plan constructors and [future::tweak()] syntax. Configures the
#' global memory limit (`future.globals.maxSize`) and selects a parallel
#' backend.
#'
#' @param cores Integer number of worker processes. Defaults to
#'   [slurm_cores()], which detects SLURM allocation or falls back to
#'   [parallelly::availableCores()].
#' @param mem Numeric memory limit per worker in GiB. Sets
#'   `future.globals.maxSize` to `mem * 1024^3`. Default is `4` (4 GiB).
#' @param nested Logical. If `FALSE` (the default), sets a single-level
#'   parallel plan. If `TRUE`, sets a two-level plan with [future::sequential]
#'   as the outer layer and the parallel backend as the inner layer, which is
#'   useful for nested future calls.
#' @param type Character string selecting the parallel backend. One of
#'   `"multicore"`, `"multisession"`, `"callr"`, `"mirai_multisession"`, or
#'   `"mirai_cluster"`. Default is `"multicore"`.
#'   Note that `"multicore"` is not supported on Windows; the function will
#'   automatically fall back to `"multisession"` in that case.
#'
#' @return The plan (invisibly), as returned by [future::plan()].
#'
#' @details
#' The `"callr"` backend requires the \pkg{future.callr} package.
#' The `"mirai_multisession"` and `"mirai_cluster"` backends require the
#' \pkg{future.mirai} package. If the required package is not installed, the
#' function will stop with an informative error.
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' init_future()
#'
#' # Use 8 cores with 8 GiB memory limit
#' init_future(cores = 8, mem = 8)
#'
#' # Nested plan for inner parallelism
#' init_future(cores = 4, nested = TRUE)
#'
#' # Use the callr backend
#' init_future(type = "callr")
#'
#' # Use mirai backends (fast, low-overhead)
#' init_future(type = "mirai_multisession")
#' init_future(type = "mirai_cluster")
#' }
init_future <- function(cores = slurm_cores(),
                        mem = 4,
                        nested = FALSE,
                        type = c("multicore", "multisession", "callr",
                                 "mirai_multisession", "mirai_cluster")) {

  type <- match.arg(type)
  cores <- as.integer(cores)
  stopifnot(cores >= 1L)

  options(future.globals.maxSize = mem * 1024^3)

  # On Windows, multicore is not supported; fall back silently

  if (type == "multicore" && .Platform$OS.type == "windows") {
    message("multicore is not supported on Windows; using multisession instead.")
    type <- "multisession"
  }

  .require_pkg <- function(pkg, type) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The '", pkg, "' package is required for type=\"", type, "\". ",
           "Install it with: install.packages(\"", pkg, "\")",
           call. = FALSE)
    }
  }

  plan_fn <- switch(type,
    multicore    = future::multicore,
    multisession = future::multisession,
    callr = {
      .require_pkg("future.callr", type)
      future.callr::callr
    },
    mirai_multisession = {
      .require_pkg("future.mirai", type)
      future.mirai::mirai_multisession
    },
    mirai_cluster = {
      .require_pkg("future.mirai", type)
      future.mirai::mirai_cluster
    }
  )

  if (nested) {
    p <- future::plan(list(future::sequential, future::tweak(plan_fn, workers = cores)))
  } else {
    p <- future::plan(future::tweak(plan_fn, workers = cores))
  }

  invisible(p)
}
