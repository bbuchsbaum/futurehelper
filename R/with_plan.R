#' Temporarily Evaluate an Expression Under a Different Plan
#'
#' Sets a [future::plan()], evaluates `expr`, then restores the previous plan.
#' Modeled after [withr::with_options()].
#'
#' @param plan_fn A future plan strategy (e.g., `future::multisession`) or a
#'   result from [future::tweak()].
#' @param expr An expression to evaluate under the temporary plan.
#' @param ... Additional arguments passed to [future::plan()] (e.g.,
#'   `workers = 4`).
#'
#' @return The result of evaluating `expr` (invisibly).
#'
#' @export
#' @examples
#' \dontrun{
#' future::plan(future::sequential)
#'
#' with_plan(future::multisession, workers = 4, {
#'   future.apply::future_lapply(1:10, sqrt)
#' })
#'
#' # plan is back to sequential here
#' }
with_plan <- function(plan_fn, expr, ...) {
  old_plan <- plan()
  on.exit(plan(old_plan), add = TRUE)
  plan(plan_fn, ...)
  force(expr)
}
