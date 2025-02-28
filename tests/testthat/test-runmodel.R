test_that("running the basic model works", {
  
  
  ## basic example without nativity class used
  pms <- create_demographic_parms() #create UK parameters
  out <- runmodel(pms)     
  
  ## check that result is not NULL
  expect_false(is.null(out))

  ## is the result a data frame?
  expect_true(is.data.frame(out))

  expect_true(is.numeric(out$value))
  
  expect_false(any(is.na(out$value)))  
  expect_true(all(out$value >= 0 ))
  
  ## Should have 10 columns
  expect_equal(ncol(out), 10)
  
  ## Check column names
  expect_true(all(colnames(out) == c("t","value","state","AgeGrp",
                                     "sex","natcat","risk","post","strain","prot")))

  ## check sex levels
  expect_true(all(levels(out$sex) == c("M","F")))
  
  ## check states
  
  states <- out[!grepl("rate_",out$state)]
  
  expect_true(all(states$natcat==1))
  expect_true(all(states$risk==1))
  expect_true(all(states$post==1))
  expect_true(all(states$strain==1))
  expect_true(all(states$prot==1))
  
  times <- as.numeric(substr(out$t, 1,4))
  
  ## check model was run over expected time series
  expect_lt(max(times),2021)
  expect_gt(min(times), 1949)
  expect_true(all(1970:2020 %in% times))
  
})


test_that("running the model with 2 nativity classes", {
  
  ## version with 2 static nativity classes
  
  pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  out <- runmodel(pms,raw=FALSE)
  out
  
  ## check that result is not NULL
  expect_false(is.null(out))
  
  ## is the result a data frame?
  expect_true(is.data.frame(out))
  
  expect_true(is.numeric(out$value))
  
  expect_false(any(is.na(out$value)))  
  expect_true(all(out$value >= 0 ))
  
  
  ## Should have 10 columns
  expect_equal(ncol(out), 10)
  
  ## Check column names
  expect_true(all(colnames(out) == c("t","value","state","AgeGrp",
                                     "sex","natcat","risk","post","strain","prot")))
  
  ## check sex levels
  expect_true(all(levels(out$sex) == c("M","F")))
  
  ## check states
  
  states <- out[!grepl("rate_",out$state)]
  
  expect_true(all(states$natcat %in% c(1,2)))
  expect_true(all(states$risk==1))
  expect_true(all(states$post==1))
  expect_true(all(states$strain==1))
  expect_true(all(states$prot==1))
  
  times <- as.numeric(substr(out$t, 1,4))
  
  ## check model was run over expected time series
  expect_lt(max(times),2021)
  expect_gt(min(times), 1949)
  expect_true(all(1970:2020 %in% times))
  

})

test_that("running the model with 2 nativity classes and 2 static risk classes", {
  
  ## version with 2 static nativity classes
  
  ## version with 2 static nativity classes and 2 static risk classes
  pms <- create_demographic_parms(nnat = 2, nrisk = 2,
                                  migrationdata = list(propinitnat = c(0.9,0.1)),
                                  riskdata = list(propinitrisk = c(0.9,0.1)))
  out <- runmodel(pms)
  out
  
  ## check that result is not NULL
  expect_false(is.null(out))
  
  ## is the result a data frame?
  expect_true(is.data.frame(out))
  
  expect_true(is.numeric(out$value))
  
  expect_false(any(is.na(out$value)))  
  expect_true(all(out$value >= 0 ))
  
  ## Should have 10 columns
  expect_equal(ncol(out), 10)
  
  ## Check column names
  expect_true(all(colnames(out) == c("t","value","state","AgeGrp",
                                     "sex","natcat","risk","post","strain","prot")))
  
  ## check sex levels
  expect_true(all(levels(out$sex) == c("M","F")))
  
  ## check states
  
  states <- out[!grepl("rate_",out$state)]
  
  expect_true(all(states$natcat %in% c(1,2)))
  expect_true(all(states$risk %in% c(1,2)))
  expect_true(all(states$post==1))
  expect_true(all(states$strain==1))
  expect_true(all(states$prot==1))
  
  times <- as.numeric(substr(out$t, 1,4))
  
  ## check model was run over expected time series
  expect_lt(max(times),2021)
  expect_gt(min(times), 1949)
  expect_true(all(1970:2020 %in% times))
  
  
})



