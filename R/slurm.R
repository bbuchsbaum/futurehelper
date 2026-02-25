#' Detect SLURM Memory Allocation
#'
#' Returns the memory allocated by SLURM in GiB. Checks
#' `SLURM_MEM_PER_NODE` first, then `SLURM_MEM_PER_CPU` (multiplied by
#' the number of CPUs). Returns `NULL` when not running under SLURM.
#'
#' @return Numeric memory in GiB, or `NULL` if not in a SLURM environment.
#' @export
#' @examples
#' slurm_mem()
slurm_mem <- function() {
  per_node <- Sys.getenv("SLURM_MEM_PER_NODE", unset = "")
  if (nzchar(per_node)) {
    return(.parse_slurm_mem(per_node))
  }

  per_cpu <- Sys.getenv("SLURM_MEM_PER_CPU", unset = "")
  if (nzchar(per_cpu)) {
    ncpus <- slurm_cores()
    return(.parse_slurm_mem(per_cpu) * ncpus)
  }

  NULL
}

#' Parse a SLURM memory string to GiB
#'
#' SLURM memory values are in MB by default, or may have a suffix
#' (K, M, G, T).
#'
#' @param x Character string (e.g., "4096", "4G", "4096M").
#' @return Numeric value in GiB.
#' @keywords internal
.parse_slurm_mem <- function(x) {
  x <- toupper(trimws(x))
  suffix <- gsub("[0-9.]", "", x)
  val <- as.numeric(gsub("[^0-9.]", "", x))

  switch(suffix,
    "K" = val / 1024^2,
    "M" = val / 1024,
    "G" = val,
    "T" = val * 1024,
    val / 1024  # default: assume MB (covers "" and unknown suffixes)
  )
}


#' Get SLURM Job Information
#'
#' Returns a list of SLURM environment variables for the current job.
#' Returns `NULL` if not running under SLURM.
#'
#' @return A list with components `job_id`, `job_name`, `partition`,
#'   `nodelist`, `cpus`, `mem_gib`, and `time_limit`, or `NULL`
#'   if `SLURM_JOB_ID` is not set.
#' @export
#' @examples
#' slurm_job_info()
slurm_job_info <- function() {
  job_id <- Sys.getenv("SLURM_JOB_ID", unset = "")
  if (!nzchar(job_id)) return(NULL)

  list(
    job_id     = job_id,
    job_name   = Sys.getenv("SLURM_JOB_NAME", unset = NA_character_),
    partition  = Sys.getenv("SLURM_JOB_PARTITION", unset = NA_character_),
    nodelist   = Sys.getenv("SLURM_NODELIST", unset = NA_character_),
    cpus       = slurm_cores(),
    mem_gib    = slurm_mem(),
    time_limit = Sys.getenv("SLURM_TIMELIMIT", unset = NA_character_)
  )
}


#' Initialize Future Plan from SLURM Environment
#'
#' Auto-configures [init_future()] using SLURM environment variables for
#' cores and memory. Falls back to [init_future()] defaults when not
#' running under SLURM.
#'
#' @param type Character backend type, passed to [init_future()]. Default
#'   is `"multicore"`.
#' @param nested Logical, passed to [init_future()].
#' @param mem_fraction Numeric fraction (0, 1] of SLURM-allocated memory
#'   to use for `future.globals.maxSize`. Default is `0.8` to leave
#'   headroom for R overhead.
#'
#' @return The plan (invisibly), as returned by [init_future()].
#' @export
#' @examples
#' \dontrun{
#' init_future_slurm()
#' init_future_slurm(type = "mirai_multisession")
#' }
init_future_slurm <- function(type = "multicore",
                              nested = FALSE,
                              mem_fraction = 0.8) {
  cores <- slurm_cores()
  mem <- slurm_mem()

  if (is.null(mem)) {
    mem <- 4
  } else {
    mem <- mem * mem_fraction
  }

  init_future(cores = cores, mem = mem, nested = nested, type = type)
}
