test_that("create_demographic_baseparms works", {
  new <- create_demographic_baseparms()
  

  expect_error(create_demographic_baseparms(1949:2020))
  expect_error(create_demographic_baseparms(1950:2101))
  
  expect_error(create_demographic_baseparms(NA))
  expect_error(create_demographic_baseparms(NULL))
  expect_error(create_demographic_baseparms(LETTERS))
})
