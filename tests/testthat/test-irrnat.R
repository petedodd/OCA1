## IRRnat: progression rate ratio by nativity class
tc <- 1990:2010
mk <- function(IRRnat = NULL, nnat = 3) {
  tb <- list(staticfoi = -1)
  if (!is.null(IRRnat)) tb$IRRnat <- IRRnat
  imm <- array(10, dim = c(length(tc), length(OCA1::agz), 2))
  create_parms(tc = tc, nnat = nnat, tbparms = tb,
               migrationdata = list(immigration = imm,
                                    migrage = c(0, 0.2, 0)[1:nnat]))
}
notes <- function(p, nat) {
  out <- runmodel(p, times = tc, raw = FALSE, singleout = FALSE)
  r <- out$rate[state == "rate_Incidence" & natcat == nat & t == max(tc)]
  sum(r$value)
}

test_that("IRRnat defaults to ones and is a no-op", {
  p0 <- mk()
  expect_equal(p0$IRRnat, rep(1, 3))
  p1 <- mk(IRRnat = rep(1, 3))
  o0 <- runmodel(p0, times = tc, raw = FALSE, singleout = FALSE)
  o1 <- runmodel(p1, times = tc, raw = FALSE, singleout = FALSE)
  expect_equal(o0$state$value, o1$state$value)
})

test_that("IRRnat > 1 for natcat 2 raises incidence there only", {
  base <- mk()
  up <- mk(IRRnat = c(1, 2, 1))
  expect_gt(notes(up, 2), notes(base, 2))
  ## UK born (natcat 1) is affected only through transmission, so the
  ## change there is much smaller than in natcat 2
  d1 <- abs(notes(up, 1) / notes(base, 1) - 1)
  d2 <- notes(up, 2) / notes(base, 2) - 1
  expect_lt(d1, d2 / 5)
})

test_that("IRRnat is checked for length and sign", {
  expect_false(isTRUE(all(check_dims(list(IRRnat = c(1, 1)),
                                     c(length(tc), 3, 1, 1, 1, 1)))))
  expect_error(mk(IRRnat = c(1, -1, 1)), "IRRnat")
})
