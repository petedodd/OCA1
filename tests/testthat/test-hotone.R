test_that("hotone works", {
  n <- 1
  
  expect_equal(hotone(n), 1)
  
  n <- 2
  expect_equal(hotone(n), c(1,0))
  
  n <- 0
  
  expect_equal(hotone(n),1)
  
  ## negatives give an error
  n <- -1 
  expect_error(hotone(n))
  
  ## error if given a vector
  n <- 1:3
  expect_error(hotone(n))
})
