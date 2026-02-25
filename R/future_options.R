#' Get or Set Future-Related Options
#'
#' A unified interface for managing all `future.*` options with human-friendly
#' names and validation. When called with no arguments, displays all current
#' settings. Memory sizes use GiB (not raw bytes), and invalid combinations
#' produce warnings.
#'
#' @param globals_maxsize Numeric maximum size of globals in GiB, or `NULL`
#'   to leave unchanged. Converted to bytes for `future.globals.maxSize`.
#' @param rng_onmisuse Character action when non-parallel-safe RNG is detected.
#'   One of `"ignore"`, `"warning"`, `"error"`, or `NULL` to leave unchanged.
#' @param globals_onreference Character action when globals contain non-
#'   exportable references (connections, external pointers, environments).
#'   One of `"ignore"`, `"warning"`, `"error"`, or `NULL`.
#' @param resolve_recursive Integer or logical controlling recursive resolution
#'   depth for futures, or `NULL`.
#' @param gc Logical. If `TRUE`, trigger garbage collection on the worker after
#'   resolving a future. Reduces memory pressure at a small speed cost.
#'   Or `NULL` to leave unchanged.
#' @param stdout Logical or `NA`. `TRUE` captures stdout from workers, `FALSE`
#'   discards it, `NA` skips capture entirely. Or `NULL` to leave unchanged.
#' @param conditions Character pattern of conditions to relay from workers
#'   (e.g., `"message"`, `"warning"`), or `NULL`.
#' @param fork_enable Logical. If `FALSE`, disables forked processing even
#'   on supported platforms. Or `NULL` to leave unchanged.
#' @param verbose Logical. If `TRUE` (default when called with no arguments),
#'   prints the current option values.
#'
#' @return A named list of all future option values (invisibly).
#'
#' @export
#' @examples
#' \dontrun{
#' # View current settings
#' future_options()
#'
#' # Set memory limit and RNG checking in one call
#' future_options(globals_maxsize = 8, rng_onmisuse = "warning")
#'
#' # Strict mode: error on references and RNG misuse
#' future_options(globals_onreference = "error", rng_onmisuse = "error")
#'
#' # Production mode: capture everything, GC aggressively
#' future_options(gc = TRUE, stdout = TRUE, rng_onmisuse = "error")
#' }
future_options <- function(globals_maxsize = NULL,
                           rng_onmisuse = NULL,
                           globals_onreference = NULL,
                           resolve_recursive = NULL,
                           gc = NULL,
                           stdout = NULL,
                           conditions = NULL,
                           fork_enable = NULL,
                           verbose = NULL) {

  # Determine if any setter arguments were supplied
  explicit <- !all(
    is.null(globals_maxsize),
    is.null(rng_onmisuse),
    is.null(globals_onreference),
    is.null(resolve_recursive),
    is.null(gc),
    is.null(stdout),
    is.null(conditions),
    is.null(fork_enable)
  )

  if (is.null(verbose)) verbose <- !explicit

  # --- Validation ---
  if (!is.null(rng_onmisuse)) {
    rng_onmisuse <- match.arg(rng_onmisuse, c("ignore", "warning", "error"))
  }
  if (!is.null(globals_onreference)) {
    globals_onreference <- match.arg(globals_onreference,
                                     c("ignore", "warning", "error"))
  }
  if (!is.null(globals_maxsize)) {
    stopifnot(is.numeric(globals_maxsize), length(globals_maxsize) == 1L,
              globals_maxsize > 0)
  }
  if (!is.null(fork_enable) && .Platform$OS.type == "windows" &&
      isTRUE(fork_enable)) {
    warning("Forked processing is not available on Windows; fork_enable ignored.",
            call. = FALSE)
    fork_enable <- NULL
  }

  # --- Set options ---
  if (!is.null(globals_maxsize))
    options(future.globals.maxSize = globals_maxsize * 1024^3)
  if (!is.null(rng_onmisuse))
    options(future.rng.onMisuse = rng_onmisuse)
  if (!is.null(globals_onreference))
    options(future.globals.onReference = globals_onreference)
  if (!is.null(resolve_recursive))
    options(future.resolve.recursive = resolve_recursive)
  if (!is.null(gc))
    options(future.gc = gc)
  if (!is.null(stdout))
    options(future.stdout = stdout)
  if (!is.null(conditions))
    options(future.conditions = conditions)
  if (!is.null(fork_enable))
    options(future.fork.enable = fork_enable)

  # --- Collect current values ---
  max_size_raw <- getOption("future.globals.maxSize")
  max_size_gib <- if (is.null(max_size_raw)) NULL
                  else if (is.infinite(max_size_raw)) Inf
                  else max_size_raw / 1024^3

  current <- list(
    globals_maxsize_gib = max_size_gib,
    rng_onmisuse        = getOption("future.rng.onMisuse"),
    globals_onreference = getOption("future.globals.onReference"),
    resolve_recursive   = getOption("future.resolve.recursive"),
    gc                  = getOption("future.gc"),
    stdout              = getOption("future.stdout"),
    conditions          = getOption("future.conditions"),
    fork_enable         = getOption("future.fork.enable")
  )

  if (verbose) {
    .print_future_options(current)
  }

  invisible(current)
}


# --- Formatting helpers (internal) ---

.print_future_options <- function(current) {
  cat("Future Options\n")
  cat(strrep("-", 50), "\n")
  .print_opt("globals_maxsize",     .fmt_gib(current$globals_maxsize_gib))
  .print_opt("rng_onmisuse",       .fmt_val(current$rng_onmisuse))
  .print_opt("globals_onreference", .fmt_val(current$globals_onreference))
  .print_opt("resolve_recursive",  .fmt_val(current$resolve_recursive))
  .print_opt("gc",                 .fmt_val(current$gc))
  .print_opt("stdout",             .fmt_val(current$stdout))
  .print_opt("conditions",         .fmt_val(current$conditions))
  .print_opt("fork_enable",        .fmt_val(current$fork_enable))
}

.print_opt <- function(name, value) {
  cat(sprintf("  %-22s %s\n", name, value))
}

.fmt_val <- function(x) {
  if (is.null(x)) "(default)" else as.character(x)
}

.fmt_gib <- function(x) {
  if (is.null(x)) "(default ~0.5 GiB)"
  else if (is.infinite(x)) "Inf"
  else paste0(round(x, 2), " GiB")
}
