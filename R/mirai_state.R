.futurehelper_state <- local({
  env <- new.env(parent = emptyenv())
  env$managed_mirai_cluster <- FALSE
  env$managed_mirai_cores <- NULL
  env$managed_mirai_prev_profile <- NULL
  env$managed_mirai_profile <- NULL
  env
})

.managed_mirai_profile <- function() {
  profile <- .futurehelper_state$managed_mirai_profile
  if (is.null(profile)) {
    profile <- paste0("futurehelper-", Sys.getpid())
    .futurehelper_state$managed_mirai_profile <- profile
  }
  profile
}

.mirai_state_env <- function() {
  get(".", envir = asNamespace("mirai"))
}

.get_mirai_current_profile <- function() {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    return(NULL)
  }

  .mirai_state_env()[["cp"]]
}

.set_mirai_current_profile <- function(profile) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    return(invisible(profile))
  }

  assign(
    "cp",
    if (length(profile)) profile else "default",
    envir = .mirai_state_env()
  )
  invisible(profile)
}

.clear_managed_mirai_cluster_state <- function() {
  .futurehelper_state$managed_mirai_cluster <- FALSE
  .futurehelper_state$managed_mirai_cores <- NULL
  .futurehelper_state$managed_mirai_prev_profile <- NULL
  invisible(NULL)
}

.start_managed_mirai_cluster <- function(cores) {
  cores <- as.integer(cores)
  stopifnot(length(cores) == 1L, !is.na(cores), cores >= 1L)

  profile <- .managed_mirai_profile()
  .futurehelper_state$managed_mirai_prev_profile <- .get_mirai_current_profile()

  mirai::daemons(cores, .compute = profile)
  .set_mirai_current_profile(profile)

  .futurehelper_state$managed_mirai_cluster <- TRUE
  .futurehelper_state$managed_mirai_cores <- cores
  invisible(TRUE)
}

.stop_managed_mirai_cluster <- function() {
  if (!isTRUE(.futurehelper_state$managed_mirai_cluster)) {
    return(invisible(FALSE))
  }

  if (requireNamespace("mirai", quietly = TRUE)) {
    mirai::daemons(0, .compute = .managed_mirai_profile())
    .set_mirai_current_profile(.futurehelper_state$managed_mirai_prev_profile)
  }

  .clear_managed_mirai_cluster_state()
  invisible(TRUE)
}

.capture_future_state <- function() {
  list(
    plan = plan(),
    globals_maxsize = getOption("future.globals.maxSize"),
    managed_mirai_cluster = isTRUE(.futurehelper_state$managed_mirai_cluster),
    managed_mirai_cores = .futurehelper_state$managed_mirai_cores
  )
}

.restore_future_state <- function(state) {
  .stop_managed_mirai_cluster()

  if (isTRUE(state$managed_mirai_cluster)) {
    .start_managed_mirai_cluster(state$managed_mirai_cores)
  }

  plan(state$plan)
  options(future.globals.maxSize = state$globals_maxsize)
  invisible(NULL)
}

.set_sequential_plan <- function(globals_maxsize = getOption("future.globals.maxSize")) {
  plan(sequential)
  .stop_managed_mirai_cluster()
  options(future.globals.maxSize = globals_maxsize)
  invisible(NULL)
}
