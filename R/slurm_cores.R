#' Detect Available Cores in a SLURM Environment
#'
#' Returns the number of CPUs allocated by SLURM via the
#' `SLURM_CPUS_PER_TASK` environment variable. Falls back to
#' [parallelly::availableCores()] when not running under SLURM.
#'
#' @return An integer: the number of available cores.
#' @export
#' @examples
#' slurm_cores()
slurm_cores <- function() {
  val <- Sys.getenv("SLURM_CPUS_PER_TASK", unset = "")
  if (nzchar(val)) {
    as.integer(val)
  } else {
    parallelly::availableCores()
  }
}
