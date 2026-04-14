skip_if_not_installed("mirai")

test_that("managed mirai cluster uses a dedicated compute profile", {
  old_state <- futurehelper:::.capture_future_state()
  old_profile <- futurehelper:::.get_mirai_current_profile()
  user_profile <- "futurehelper-user-test"

  on.exit(futurehelper:::.restore_future_state(old_state), add = TRUE)
  on.exit(futurehelper:::.set_mirai_current_profile(old_profile), add = TRUE)
  on.exit(try(mirai::daemons(0, .compute = user_profile), silent = TRUE), add = TRUE)

  futurehelper::reset_future()
  mirai::daemons(sync = TRUE, .compute = user_profile)
  futurehelper:::.set_mirai_current_profile(user_profile)

  expect_identical(futurehelper:::.get_mirai_current_profile(), user_profile)
  expect_equal(
    unname(mirai::with_daemons(user_profile, mirai::info())[["connections"]]),
    0
  )

  futurehelper:::.start_managed_mirai_cluster(1)

  expect_identical(
    futurehelper:::.get_mirai_current_profile(),
    futurehelper:::.managed_mirai_profile()
  )
  expect_equal(unname(mirai::info()[["connections"]]), 1L)
  expect_equal(
    unname(mirai::with_daemons(user_profile, mirai::info())[["connections"]]),
    0
  )

  futurehelper:::.stop_managed_mirai_cluster()

  expect_identical(futurehelper:::.get_mirai_current_profile(), user_profile)
  expect_error(
    mirai::with_daemons(futurehelper:::.managed_mirai_profile(), mirai::info()),
    "No daemons set"
  )
  expect_equal(
    unname(mirai::with_daemons(user_profile, mirai::info())[["connections"]]),
    0
  )
})
