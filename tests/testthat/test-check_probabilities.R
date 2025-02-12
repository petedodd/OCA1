test_that("check_probabilities works", {
  expect_false(check_probabilities(0))
  expect_true(check_probabilities(1))
  
  expect_false(check_probabilities(c(1,1)))
  expect_false(check_probabilities(c(0,0)))  

  expect_true(check_probabilities(c(1,0)))    
  expect_true(check_probabilities(c(0,1)))
  
  expect_true(check_probabilities(c(0.5,0.5)))
  
  expect_false(check_probabilities(c(0.5,0.5,0.5)))
  
  expect_false(check_probabilities(NA))
  
  expect_false(check_probabilities("0"))

  expect_false(check_probabilities(c("0","1")))    
})
