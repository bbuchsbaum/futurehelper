#' Evaluate an Expression with Automatic Parallel Setup
#'
#' A high-level convenience wrapper that initializes a parallel
#' [future::plan()], evaluates `expr`, and restores the previous plan on exit
#' (even on error). Unlike [with_plan()], which requires a plan function, this
#' takes simple parameters like `cores` and `type` directly --- no need to
#' separately call [init_future()] + your code + [reset_future()].
#'
#' @param expr An expression to evaluate under the parallel plan.
#' @param cores Integer number of workers. Defaults to [slurm_cores()].
#' @param mem Numeric memory limit in GiB. Default `4`.
#' @param type Character backend type (see [init_future()]). Default
#'   `"multicore"`.
#' @param nested Logical. If `TRUE`, uses a nested plan. Default `FALSE`.
#'
#' @return The result of evaluating `expr`.
#'
#' @seealso [with_plan()] for the lower-level interface that takes a plan
#'   function directly, [init_future()] for persistent plan setup.
#'
#' @export
#' @examples
#' \dontrun{
#' # Parallel lapply with 4 cores --- one line, done
#' result <- with_parallel(
#'   future.apply::future_lapply(1:100, function(x) x^2),
#'   cores = 4
#' )
#'
#' # Use callr backend with 8 GiB memory limit
#' result <- with_parallel(
#'   future.apply::future_sapply(big_list, process_fn),
#'   cores = 8, mem = 8, type = "callr"
#' )
#'
#' # The plan is always restored afterward, even on error
#' future::plan(future::sequential)
#' with_parallel(long_computation(), cores = 4)
#' # back to sequential here
#' }
with_parallel <- function(expr,
                          cores = slurm_cores(),
                          mem = 4,
                          type = "multicore",
                          nested = FALSE) {
  old_plan <- plan()
  old_maxsize <- getOption("future.globals.maxSize")
  on.exit({
    plan(old_plan)
    options(future.globals.maxSize = old_maxsize)
  }, add = TRUE)

  init_future(cores = cores, mem = mem, nested = nested, type = type)
  force(expr)
}
