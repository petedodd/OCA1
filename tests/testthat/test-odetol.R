## runmodel(): ODE solver tolerances (rtol, atol)
tc <- 1990:2010
pms <- create_parms(tc = tc, tbparms = list(staticfoi = 1, foi = 1e-2))
tot_asymp <- function(...) {
  out <- runmodel(pms, times = tc, raw = TRUE, ...)
  sum(out[nrow(out), grep("^Asymp", colnames(out))])
}

test_that("default tolerances are unchanged when rtol/atol are not given", {
  o0 <- runmodel(pms, times = tc, raw = TRUE)
  o1 <- runmodel(pms, times = tc, raw = TRUE, rtol = NULL, atol = NULL)
  expect_identical(o0, o1)
})

test_that("tighter tolerances run and agree closely with the default", {
  a0 <- tot_asymp()
  a1 <- tot_asymp(rtol = 1e-8, atol = 1e-8)
  expect_true(is.finite(a1))
  expect_equal(a1, a0, tolerance = 1e-4)
})

test_that("tolerances are passed to the solver", {
  ## a very loose tolerance gives a visibly different answer
  a1 <- tot_asymp(rtol = 1e-8, atol = 1e-8)
  a2 <- tot_asymp(rtol = 1e-2, atol = 1e-2)
  expect_false(isTRUE(all.equal(a1, a2, tolerance = 1e-10)))
})

test_that("tolerances work with data.table output", {
  out <- runmodel(pms, times = tc, rtol = 1e-8, atol = 1e-8)
  expect_true(all(unlist(lapply(out, is.data.frame))))
  expect_false(any(unlist(lapply(out, function(x) any(is.na(x$value))))))
})
