skip_if_not_installed("future.mirai")
skip_if_not_installed("mirai")

test_that("reset_future stops package-managed mirai_cluster daemons", {
  old_state <- futurehelper:::.capture_future_state()
  on.exit(futurehelper:::.restore_future_state(old_state), add = TRUE)

  futurehelper::reset_future()
  futurehelper::init_future(cores = 2, type = "mirai_cluster")

  expect_equal(unname(mirai::info()[["connections"]]), 2L)

  futurehelper::reset_future()

  expect_false(isTRUE(futurehelper:::.futurehelper_state$managed_mirai_cluster))
  expect_null(mirai::info())
  expect_match(futurehelper::plan_info(verbose = FALSE)$backend, "sequential")
})

test_that("with_parallel restores a prior package-managed mirai_cluster", {
  old_state <- futurehelper:::.capture_future_state()
  on.exit(futurehelper:::.restore_future_state(old_state), add = TRUE)

  futurehelper::reset_future()
  futurehelper::init_future(cores = 2, type = "mirai_cluster")

  futurehelper::with_parallel(
    {
      expect_match(futurehelper::plan_info(verbose = FALSE)$backend, "multisession")
      42
    },
    cores = 1,
    type = "multisession"
  )

  expect_true(isTRUE(futurehelper:::.futurehelper_state$managed_mirai_cluster))
  expect_equal(unname(mirai::info()[["connections"]]), 2L)
})
