## progt_slow_raw / progt_fast_raw: time-varying progression multipliers
## by age and nativity class

tc <- 1990:2010
mk <- function(nnat = 3, ...) {
  tb <- list(staticfoi = -1, ...)
  imm <- array(10, dim = c(length(tc), length(OCA1::agz), 2))
  create_parms(
    tc = tc, nnat = nnat, tbparms = tb,
    migrationdata = list(
      immigration = imm,
      migrage = c(0, 0.2, 0)[1:nnat]
    )
  )
}
ones <- function(nnat = 3) {
  array(1, dim = c(length(tc), length(OCA1::agz), nnat))
}
## incidence (per 100k, summed over strata) by time and natcat
inc <- function(p) {
  out <- runmodel(p, times = tc, raw = FALSE, singleout = FALSE)
  out$rate[state == "rate_Incidence", .(value = sum(value)), by = .(t, natcat)]
}

test_that("progt defaults are all ones and a no-op", {
  p0 <- mk()
  expect_equal(p0$progt_slow_raw, ones())
  expect_equal(p0$progt_fast_raw, ones())
  p1 <- mk(progt_slow_raw = ones(), progt_fast_raw = ones())
  o0 <- runmodel(p0, times = tc, raw = FALSE, singleout = FALSE)
  o1 <- runmodel(p1, times = tc, raw = FALSE, singleout = FALSE)
  expect_equal(o0$state$value, o1$state$value)
})

test_that("a fall in reactivation from 2000 lowers incidence after only", {
  base <- inc(mk())
  ps <- ones()
  ps[tc >= 2000, , ] <- 0.5
  low <- inc(mk(progt_slow_raw = ps))
  m <- merge(base, low, by = c("t", "natcat"), suffixes = c("_b", "_l"))
  ## identical up to 1999 (linear interpolation from 1999 to 2000)
  expect_equal(m[t <= 1999, value_l], m[t <= 1999, value_b])
  expect_true(all(m[t >= 2001, value_l < value_b]))
})

test_that("progt acts only on the ages and natcats it is set for", {
  base <- inc(mk())
  ## fast progression doubled in natcat 2 only
  pf <- ones()
  pf[, , 2] <- 2
  up <- inc(mk(progt_fast_raw = pf))
  m <- merge(base, up, by = c("t", "natcat"), suffixes = c("_b", "_u"))[
    t == max(tc)
  ]
  r <- m[, value_u / value_b - 1]
  ## natcat 2 up; natcat 1 only through transmission, much smaller
  expect_gt(r[m$natcat == 2], 0)
  expect_lt(abs(r[m$natcat == 1]), r[m$natcat == 2] / 5)

  ## reactivation off at ages 65+ (agz 14:17): incidence there falls
  ps <- ones()
  ps[, 14:17, ] <- 0
  p <- mk(progt_slow_raw = ps)
  out <- runmodel(p, times = tc, raw = FALSE, singleout = FALSE)
  out0 <- runmodel(mk(), times = tc, raw = FALSE, singleout = FALSE)
  old <- function(o) {
    o$rate[state == "rate_Incidence" & t == max(tc) &
      AgeGrp %in% OCA1::agz[14:17], sum(value)]
  }
  young <- function(o) {
    o$rate[state == "rate_Incidence" & t == max(tc) &
      AgeGrp %in% OCA1::agz[1:13], sum(value)]
  }
  expect_lt(old(out), 0.5 * old(out0))
  expect_lt(abs(young(out) / young(out0) - 1), 0.1)
})

test_that("progt is checked for dimensions and sign", {
  dms <- c(length(tc), 3, 1, 1, 1, 1)
  expect_true(check_dims(list(progt_slow_raw = ones()), dms))
  expect_false(check_dims(list(progt_slow_raw = ones(2)), dms))
  expect_message(mk(progt_fast_raw = ones(2)), "dimension problems")
  bad <- ones()
  bad[1, 1, 1] <- -1
  expect_error(mk(progt_slow_raw = bad), "progt_slow_raw")
})
