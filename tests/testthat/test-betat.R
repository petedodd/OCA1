## time-varying transmission multiplier betat_raw

## NB OCA1::parms has staticfoi = 0.5 (static foi, where betat is
## deliberately ignored); hyperparms fixes staticfoi = -1 (dynamic).
## Tests of the effect therefore switch to dynamic transmission.
.dyn <- function(...) list(staticfoi = -1, ...)

## annual notifications and incidence (summed over strata) from a run
.betat_summary <- function(pms, tc) {
  out <- runmodel(pms, times = tc, raw = FALSE, singleout = FALSE)
  out$rate[, .(value = sum(value)), by = .(t, state)]
}

test_that("betat_raw default is all ones and a no-op", {
  tc <- 1970:2020
  expect_equal(default_parameters("betat_raw", c(length(tc), 1, 1, 1, 1, 1)),
               rep(1, length(tc)))
  p0 <- create_parms(tc = tc)
  expect_equal(p0$betat_raw, rep(1, length(tc)))
  p1 <- create_parms(tc = tc, tbparms = list(betat_raw = rep(1, length(tc))))
  expect_identical(.betat_summary(p0, tc), .betat_summary(p1, tc))
})

test_that("betat_raw is a no-op under dynamic transmission too", {
  tc <- 1970:2020
  p0 <- create_parms(tc = tc, tbparms = .dyn())
  p1 <- create_parms(tc = tc,
                     tbparms = .dyn(betat_raw = rep(1, length(tc))))
  expect_identical(.betat_summary(p0, tc), .betat_summary(p1, tc))
})

test_that("betat_raw is ignored under static foi", {
  tc <- 1970:2020
  p0 <- create_parms(tc = tc, tbparms = list(staticfoi = 0.5))
  p1 <- create_parms(tc = tc, tbparms = list(
    staticfoi = 0.5, betat_raw = rep(0.5, length(tc))
  ))
  expect_identical(.betat_summary(p0, tc), .betat_summary(p1, tc))
})

test_that("betat_raw lowers transmission", {
  tc <- 1970:2020
  base <- .betat_summary(create_parms(tc = tc, tbparms = .dyn()), tc)
  half <- .betat_summary(
    create_parms(tc = tc, tbparms = .dyn(betat_raw = rep(0.5, length(tc)))),
    tc
  )
  inc_b <- base[state == "rate_Incidence" & t > 1975, value]
  inc_h <- half[state == "rate_Incidence" & t > 1975, value]
  expect_true(all(inc_h < inc_b))

  ## switch transmission off from 2000: model identical before the
  ## switch starts (linear interpolation from 1999), lower after
  bt <- ifelse(tc >= 2000, 0, 1)
  off <- .betat_summary(
    create_parms(tc = tc, tbparms = .dyn(betat_raw = bt)), tc
  )
  pre_b <- base[state == "rate_Notification" & t <= 1999, value]
  pre_o <- off[state == "rate_Notification" & t <= 1999, value]
  expect_equal(pre_o, pre_b)
  post_b <- base[state == "rate_Notification" & t >= 2005, value]
  post_o <- off[state == "rate_Notification" & t >= 2005, value]
  expect_true(all(post_o < post_b))
})

test_that("betat_raw checks flag bad input", {
  tc <- 1970:2020
  dms <- c(length(tc), 1, 1, 1, 1, 1)
  expect_true(check_dims(list(betat_raw = rep(1, length(tc))), dms))
  expect_false(check_dims(list(betat_raw = rep(1, 3)), dms))
  expect_message(
    create_parms(tc = tc, tbparms = list(betat_raw = rep(1, 3))),
    "dimension problems"
  )
  expect_error(
    create_parms(tc = tc, tbparms = list(betat_raw = rep(-1, length(tc)))),
    "betat_raw"
  )
})
