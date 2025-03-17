test_that("default_parameters works", {
  
  expect_equal(default_parameters("migrage",c(2,2)), c(0,0))
  expect_equal(default_parameters("migrage",c(2,1)), 0)

  expect_equal(default_parameters("propinitnat",c(2,2)), c(1,0))  
  expect_equal(default_parameters("propinitnat",c(2,3)), c(1,0,0))
  
  
  expect_error(default_parameters("foo"))
  
  
})

