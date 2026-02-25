#' System Parallel Capabilities Report
#'
#' Prints a comprehensive assessment of the current system's parallel
#' computing capabilities: available backends, CPU/memory resources, fork
#' safety, HPC scheduler detection, and potential issues. Useful for
#' debugging setup problems, onboarding new users, and pasting into
#' bug reports.
#'
#' @param verbose Logical. If `TRUE` (default), prints the report. If `FALSE`,
#'   returns the info list silently.
#'
#' @return A list (invisibly) with components:
#'   \describe{
#'     \item{`system`}{OS, R version, platform, PID.}
#'     \item{`cores`}{Available and total system cores.}
#'     \item{`ram_gib`}{Total system RAM in GiB (or `NA`).}
#'     \item{`fork_safe`}{Fork-safety assessment.}
#'     \item{`hpc`}{Detected HPC scheduler and job info.}
#'     \item{`backends`}{Data frame of installed/available backends.}
#'     \item{`plan`}{Current plan info (from [plan_info()]).}
#'     \item{`options`}{Current future options (from [future_options()]).}
#'     \item{`issues`}{Character vector of potential issues.}
#'   }
#'
#' @export
#' @examples
#' parallel_capabilities()
#'
#' # Capture without printing
#' caps <- parallel_capabilities(verbose = FALSE)
#' caps$backends
#' caps$cores
parallel_capabilities <- function(verbose = TRUE) {
  sys <- list(
    os        = .detect_os(),
    r_version = paste0(R.version$major, ".", R.version$minor),
    platform  = R.version$platform,
    pid       = Sys.getpid()
  )

  cores <- list(
    available = parallelly::availableCores(),
    system    = .detect_system_cores()
  )

  ram_gib   <- .detect_ram_gib()
  fork_safe <- .assess_fork_safety()
  hpc       <- .detect_hpc()
  backends  <- .available_backends()

  current_plan <- tryCatch(plan_info(verbose = FALSE), error = function(e) NULL)
  opts         <- tryCatch(future_options(verbose = FALSE), error = function(e) list())

  issues <- .detect_issues(fork_safe, backends, ram_gib, opts)

  result <- list(
    system    = sys,
    cores     = cores,
    ram_gib   = ram_gib,
    fork_safe = fork_safe,
    hpc       = hpc,
    backends  = backends,
    plan      = current_plan,
    options   = opts,
    issues    = issues
  )

  if (verbose) .print_capabilities(result)

  invisible(result)
}


# ---- Internal helpers --------------------------------------------------------

.detect_os <- function() {
  info <- Sys.info()
  paste0(info["sysname"], " ", info["release"])
}

.detect_system_cores <- function() {
  tryCatch(
    parallelly::availableCores(methods = "system"),
    error = function(e) NA_integer_
  )
}

.detect_ram_gib <- function() {
  tryCatch({
    if (.Platform$OS.type == "unix") {
      if (Sys.info()["sysname"] == "Darwin") {
        raw <- system("sysctl -n hw.memsize", intern = TRUE)
        as.numeric(raw) / 1024^3
      } else {
        lines <- readLines("/proc/meminfo", n = 1L)
        kb <- as.numeric(gsub("[^0-9]", "", lines))
        kb / 1024^2
      }
    } else {
      raw <- system("wmic ComputerSystem get TotalPhysicalMemory /value",
                     intern = TRUE)
      val_line <- grep("TotalPhysicalMemory", raw, value = TRUE)
      as.numeric(gsub("[^0-9]", "", val_line)) / 1024^3
    }
  }, error = function(e) NA_real_)
}

.assess_fork_safety <- function() {
  if (.Platform$OS.type == "windows") {
    return(list(supported = FALSE, in_rstudio = FALSE, recommended = FALSE,
                note = "Windows does not support forking"))
  }
  in_rstudio   <- nzchar(Sys.getenv("RSTUDIO"))
  fork_disabled <- identical(getOption("future.fork.enable"), FALSE)
  note <- if (in_rstudio) "Forking unreliable inside RStudio"
          else if (fork_disabled) "Disabled via future.fork.enable = FALSE"
          else NULL
  list(
    supported   = TRUE,
    in_rstudio  = in_rstudio,
    recommended = !in_rstudio && !fork_disabled,
    note        = note
  )
}

#' List Available Future Backends
#'
#' Checks which future backends are installed and returns their names
#' and package versions.
#'
#' @return A data frame with columns `backend`, `package`, `version`,
#'   and `available`.
#' @keywords internal
.available_backends <- function() {
  backends <- data.frame(
    backend = c("sequential", "multicore", "multisession",
                "callr", "mirai_multisession", "mirai_cluster"),
    package = c("future", "future", "future",
                "future.callr", "future.mirai", "future.mirai"),
    stringsAsFactors = FALSE
  )

  backends$available <- vapply(backends$package, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1L))

  backends$version <- vapply(seq_len(nrow(backends)), function(i) {
    if (backends$available[i]) {
      tryCatch(
        as.character(utils::packageVersion(backends$package[i])),
        error = function(e) NA_character_
      )
    } else {
      NA_character_
    }
  }, character(1L))

  # multicore not really available on Windows
  if (.Platform$OS.type == "windows") {
    backends$available[backends$backend == "multicore"] <- FALSE
  }

  backends
}

.detect_hpc <- function() {
  if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {
    return(list(scheduler = "SLURM", info = slurm_job_info()))
  }
  if (nzchar(Sys.getenv("PBS_JOBID"))) {
    return(list(scheduler = "PBS/Torque", info = list(
      job_id = Sys.getenv("PBS_JOBID"),
      nodes  = Sys.getenv("PBS_NUM_NODES", unset = NA),
      ppn    = Sys.getenv("PBS_NUM_PPN", unset = NA)
    )))
  }
  if (nzchar(Sys.getenv("JOB_ID"))) {
    return(list(scheduler = "SGE/UGE", info = list(
      job_id = Sys.getenv("JOB_ID"),
      pe     = Sys.getenv("PE", unset = NA),
      nslots = Sys.getenv("NSLOTS", unset = NA)
    )))
  }
  if (nzchar(Sys.getenv("LSB_JOBID"))) {
    return(list(scheduler = "LSF", info = list(
      job_id = Sys.getenv("LSB_JOBID"),
      queue  = Sys.getenv("LSB_QUEUE", unset = NA),
      hosts  = Sys.getenv("LSB_HOSTS", unset = NA)
    )))
  }
  list(scheduler = "none", info = NULL)
}

.detect_issues <- function(fork_safe, backends, ram_gib, opts) {
  issues <- character(0L)

  if (isTRUE(fork_safe$in_rstudio)) {
    issues <- c(issues,
      "RStudio detected: use multisession or callr instead of multicore")
  }

  if (!any(backends$available[backends$backend == "callr"])) {
    issues <- c(issues,
      "Consider installing future.callr for a robust isolated backend")
  }

  if (!any(backends$available[backends$backend == "mirai_multisession"])) {
    issues <- c(issues,
      "Consider installing future.mirai for high-performance async execution")
  }

  if (!is.null(opts$globals_maxsize_gib) && !is.na(ram_gib)) {
    if (is.finite(opts$globals_maxsize_gib) &&
        opts$globals_maxsize_gib > ram_gib * 0.8) {
      issues <- c(issues, sprintf(
        "globals_maxsize (%.1f GiB) exceeds 80%% of system RAM (%.1f GiB)",
        opts$globals_maxsize_gib, ram_gib))
    }
  }

  issues
}

.print_capabilities <- function(info) {
  cat("Parallel Capabilities Report\n")
  cat(strrep("=", 50), "\n\n")

  # System
  cat("System\n")
  cat(strrep("-", 50), "\n")
  cat("  OS:          ", info$system$os, "\n")
  cat("  R:           ", info$system$r_version, "\n")
  cat("  Platform:    ", info$system$platform, "\n")
  cat("  PID:         ", info$system$pid, "\n\n")

  # Resources
  cat("Resources\n")
  cat(strrep("-", 50), "\n")
  cat("  Cores:       ", info$cores$available)
  if (!is.na(info$cores$system) && info$cores$system != info$cores$available) {
    cat(" (", info$cores$system, " system total)", sep = "")
  }
  cat("\n")
  if (!is.na(info$ram_gib)) {
    cat("  RAM:         ", sprintf("%.1f GiB", info$ram_gib), "\n")
  }
  fork_str <- if (!info$fork_safe$supported) "No (Windows)"
              else if (info$fork_safe$recommended) "Yes"
              else paste0("Limited (", info$fork_safe$note, ")")
  cat("  Fork-safe:   ", fork_str, "\n\n")

  # HPC
  if (info$hpc$scheduler != "none") {
    cat("HPC Scheduler: ", info$hpc$scheduler, "\n")
    cat(strrep("-", 50), "\n")
    for (nm in names(info$hpc$info)) {
      val <- info$hpc$info[[nm]]
      if (!is.null(val) && !is.na(val)) {
        cat(sprintf("  %-12s  %s\n", nm, as.character(val)))
      }
    }
    cat("\n")
  } else {
    cat("HPC:            Not detected\n\n")
  }

  # Backends
  cat("Installed Backends\n")
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(info$backends))) {
    b <- info$backends[i, ]
    mark <- if (b$available) "+" else "-"
    ver  <- if (b$available) paste0("(", b$package, " ", b$version, ")")
            else "(not installed)"
    cat(sprintf("  [%s] %-22s %s\n", mark, b$backend, ver))
  }
  cat("\n")

  # Current plan
  if (!is.null(info$plan)) {
    cat("Current Plan\n")
    cat(strrep("-", 50), "\n")
    cat("  Backend:     ", info$plan$backend, "\n")
    cat("  Workers:     ", info$plan$workers, "\n")
    max_str <- if (is.null(info$options$globals_maxsize_gib))
                 "(default ~0.5 GiB)"
               else if (is.infinite(info$options$globals_maxsize_gib)) "Inf"
               else paste0(round(info$options$globals_maxsize_gib, 2), " GiB")
    cat("  Max globals: ", max_str, "\n\n")
  }

  # Issues
  if (length(info$issues) > 0L) {
    cat("Potential Issues\n")
    cat(strrep("-", 50), "\n")
    for (issue in info$issues) {
      cat("  * ", issue, "\n")
    }
    cat("\n")
  }
}
