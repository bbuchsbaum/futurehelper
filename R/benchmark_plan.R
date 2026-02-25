#' Benchmark an Expression Across Future Backends
#'
#' Runs `expr` under each specified backend and returns timings with
#' speedup and efficiency metrics. Useful for choosing the fastest
#' backend and optimal core count for a given workload.
#'
#' @param expr An expression to benchmark (unquoted).
#' @param types Character vector of backend types to test. Defaults to
#'   `c("sequential", "multicore", "multisession")`. Ignored when
#'   `auto_detect = TRUE`.
#' @param cores Integer number of workers for parallel backends. Defaults to
#'   [slurm_cores()].
#' @param times Integer number of repetitions per backend. Default is `1`.
#' @param auto_detect Logical. If `TRUE`, automatically discovers all
#'   installed backends and benchmarks each one. Default `FALSE`.
#' @param vary_cores Logical. If `TRUE`, tests multiple worker counts
#'   for each parallel backend (half, N-1, and N cores). Default `FALSE`.
#'
#' @return A data frame with columns `type`, `cores`, `rep`, `elapsed`
#'   (seconds), `speedup` (relative to sequential), and `efficiency`
#'   (percent: speedup / cores * 100). Returned invisibly; a formatted
#'   summary with a recommendation is printed.
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic comparison
#' benchmark_plan(
#'   Reduce("+", future.apply::future_lapply(1:200, function(x) rnorm(1e4))),
#'   types = c("sequential", "multicore", "multisession"),
#'   cores = 4
#' )
#'
#' # Auto-detect all installed backends and vary core counts
#' benchmark_plan(
#'   Reduce("+", future.apply::future_lapply(1:200, function(x) rnorm(1e4))),
#'   auto_detect = TRUE,
#'   vary_cores = TRUE
#' )
#' }
benchmark_plan <- function(expr,
                           types = c("sequential", "multicore", "multisession"),
                           cores = slurm_cores(),
                           times = 1L,
                           auto_detect = FALSE,
                           vary_cores = FALSE) {

  expr_q <- substitute(expr)
  old_plan <- plan()
  on.exit(plan(old_plan), add = TRUE)

  # Auto-detect available backends
  if (auto_detect) {
    all_backends <- .available_backends()
    types <- all_backends$backend[all_backends$available]
  }

  # Determine core counts to test
  if (vary_cores && cores > 2L) {
    core_counts <- sort(unique(as.integer(c(
      max(2L, cores %/% 2L),
      max(2L, cores - 1L),
      cores
    ))))
  } else {
    core_counts <- as.integer(cores)
  }

  results <- vector("list", length(types) * length(core_counts) * times)
  idx <- 0L

  for (type in types) {
    test_cores <- if (type == "sequential") 1L else core_counts

    for (nc in test_cores) {
      for (rep in seq_len(times)) {
        idx <- idx + 1L

        if (type == "sequential") {
          plan(sequential)
        } else {
          ok <- tryCatch({
            init_future(cores = nc, type = type)
            TRUE
          }, error = function(e) {
            message("Skipping ", type, " (", nc, " cores): ", e$message)
            FALSE
          })
          if (!ok) {
            results[[idx]] <- data.frame(
              type = type, cores = as.integer(nc), rep = rep,
              elapsed = NA_real_, stringsAsFactors = FALSE
            )
            next
          }
        }

        elapsed <- system.time(eval(expr_q, envir = parent.frame()))["elapsed"]
        results[[idx]] <- data.frame(
          type = type, cores = as.integer(nc), rep = rep,
          elapsed = as.numeric(elapsed), stringsAsFactors = FALSE
        )

        plan(sequential)
      }
    }
  }

  results <- results[seq_len(idx)]
  df <- do.call(rbind, results)

  # Compute speedup and efficiency
  seq_time <- mean(df$elapsed[df$type == "sequential"], na.rm = TRUE)
  if (is.na(seq_time) || seq_time == 0) seq_time <- NA_real_
  df$speedup <- round(seq_time / df$elapsed, 2)
  df$efficiency <- round(100 * df$speedup / pmax(df$cores, 1L), 1)
  df <- df[order(df$elapsed, na.last = TRUE), ]
  rownames(df) <- NULL

  # Aggregate by type + cores
  agg <- aggregate(elapsed ~ type + cores, data = df, FUN = mean, na.rm = TRUE)
  agg$speedup <- round(seq_time / agg$elapsed, 2)
  agg$efficiency <- round(100 * agg$speedup / pmax(agg$cores, 1L), 1)
  agg <- agg[order(agg$elapsed), ]

  # Print summary
  has_multi_cores <- length(unique(agg$cores[agg$type != "sequential"])) > 1L

  cat("Benchmark Results (", times, " rep(s))\n", sep = "")

  if (has_multi_cores) {
    cat(strrep("-", 65), "\n")
    cat(sprintf("  %-20s %5s %9s %8s %10s\n",
                "backend", "cores", "elapsed", "speedup", "efficiency"))
    cat(strrep("-", 65), "\n")
    for (i in seq_len(nrow(agg))) {
      cat(sprintf("  %-20s %5d %8.2fs %7.1fx %9.0f%%\n",
                  agg$type[i], agg$cores[i], agg$elapsed[i],
                  agg$speedup[i], agg$efficiency[i]))
    }
  } else {
    cat(strrep("-", 50), "\n")
    cat(sprintf("  %-20s %9s %8s\n", "backend", "elapsed", "speedup"))
    cat(strrep("-", 50), "\n")
    for (i in seq_len(nrow(agg))) {
      cat(sprintf("  %-20s %8.2fs %7.1fx\n",
                  agg$type[i], agg$elapsed[i], agg$speedup[i]))
    }
  }

  # Recommendation
  parallel_rows <- agg[agg$type != "sequential" & !is.na(agg$elapsed), ]
  if (nrow(parallel_rows) > 0L) {
    any_faster <- any(parallel_rows$speedup > 1.0, na.rm = TRUE)
    if (!isTRUE(any_faster)) {
      cat("\n-> Recommendation: sequential is fastest for this workload.",
          "Try a heavier payload.\n")
    } else if (has_multi_cores) {
      good <- parallel_rows[parallel_rows$speedup >= 1.5, ]
      if (nrow(good) > 0L) {
        best <- good[which.max(good$efficiency), ]
      } else {
        best <- parallel_rows[which.max(parallel_rows$speedup), ]
      }
      cat(sprintf("\n-> Recommendation: %s with %d cores (%.1fx, %.0f%% efficiency)\n",
                  best$type, best$cores, best$speedup, best$efficiency))
    } else {
      best <- parallel_rows[which.max(parallel_rows$speedup), ]
      cat(sprintf("\n-> Recommendation: %s (%.1fx speedup)\n",
                  best$type, best$speedup))
    }
  }

  invisible(df)
}
