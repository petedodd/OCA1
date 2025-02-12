test_that("testing lenordim2", {
  
  ## vectors
  X <- 1
  expect_equal(lenordim2(X), 1)
  
  X <- 1:10
  expect_equal(lenordim2(X), 10)
  
  ## arrays
  X <- array(1:24, dim = c(3,2,4))
  expect_equal(lenordim2(X),2)  
  
  X <- array(1:12, dim = c(3,4))
  expect_equal(lenordim2(X),4)
  
  ## matrix
  
  X <- matrix(1:20, nrow=2,ncol=10)
  expect_equal(lenordim2(X),10)
  
  ## NULL
  
  X <- NULL
  expect_equal(lenordim2(X),0)
  
  
  ## empty
  X <- c()
  expect_equal(lenordim2(X),0)
  
  
})

