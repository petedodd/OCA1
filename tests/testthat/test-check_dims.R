test_that("check_dims works", {
  
  dms <- c(2, 3, 4, 5, 6, 7)  # nsex, nnat, nrisk, npost, nstrain, nprotn  
  
  # migrage
  parlist <- list(migrage = 1:dms[2])
  expect_true(check_dims(parlist, dms)["migrage"])

  parlist <- list(migrage = 1:(dms[2] + 1))
  expect_false(check_dims(parlist, dms)["migrage"])
  
  parlist <- list(Pmigr_risk = matrix(1:(2 * dms[3]), nrow = 2, ncol = dms[3]))
  expect_true(check_dims(parlist, dms)["Pmigr_risk"])
  
  parlist <- list(Pmigr_risk = matrix(1:(2 * (dms[3] + 1)), nrow = 2, ncol = dms[3] + 1))
  expect_false(check_dims(parlist, dms)["Pmigr_risk"])
  

})
