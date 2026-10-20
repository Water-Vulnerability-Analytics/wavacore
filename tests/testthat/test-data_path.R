test_that("wava_data_dir() falls back to 'data'", {
  withr::local_options(wava.data_dir = NULL)
  withr::local_envvar(WAVA_DATA_DIR = NA)
  expect_equal(wava_data_dir(), "data")
})

test_that("wava_data_dir() reads WAVA_DATA_DIR", {
  withr::local_options(wava.data_dir = NULL)
  withr::local_envvar(WAVA_DATA_DIR = "/from/env")
  expect_equal(wava_data_dir(), "/from/env")
})

test_that("the wava.data_dir option beats the environment variable", {
  withr::local_options(wava.data_dir = "/from/option")
  withr::local_envvar(WAVA_DATA_DIR = "/from/env")
  expect_equal(wava_data_dir(), "/from/option")
})

test_that("wava_data_path() joins pieces below the data root", {
  withr::local_options(wava.data_dir = "/root")
  expect_equal(wava_data_path("rapid", "inputs"), "/root/rapid/inputs")
  expect_equal(wava_data_path(), "/root")
})
