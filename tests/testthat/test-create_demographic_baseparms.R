test_that("create_demographic_baseparms works", {
  new <- create_demographic_baseparms()
  
  expect_equal(digest::digest(new), "9275d71bf94c7038c190813bf54bbed5")
  
  expect_error(create_demographic_baseparms(1949:2020))
  expect_error(create_demographic_baseparms(1950:2101))
  
  expect_error(create_demographic_baseparms(NA))
  expect_error(create_demographic_baseparms(NULL))
  expect_error(create_demographic_baseparms(LETTERS))
})
