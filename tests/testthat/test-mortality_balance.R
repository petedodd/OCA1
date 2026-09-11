test_that("treatment mortality does not inflate total population", {

  ## test bug where fromtreatmentL/fromtreatmentA divided by
  ## (1-mortality_treated) instead of multiplying, inflating flow
  ## out of Treat above treatmentends (double count)
  ## Detectable population inflation (empirically: RMSE
  ## 0.125  vs 0.0016 for w/ vs w/o bug).

  pms <- create_parms(
    tc = 1970:2020,
    tbparms = list(
      mortality_treated = 0.6,
      treatment_inversedurn = 4,
      relapse = 0.3,
      CDR_raw = array(0.95, dim = c(51, 17, 2, 1, 1, 1, 1, 1))
    )
  )
  out <- runmodel(pms)
  fit <- checkDemoFit(out)

  expect_lt(fit$RMSE, 0.02)
})
