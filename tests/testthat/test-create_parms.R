test_that("basic create_parms works", {
  
  pms <- create_parms()       #create UK parameters
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)
})

test_that("version with 2 static nativity classes create_parms works", {
  
  ## version with 2 static nativity classes
  pms <- create_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)  
})


test_that("version with 2 static nativity classes create_parms works", {
  
  ## version with 2 static nativity classes
  pms <- create_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)
})

test_that("version with 2 static nativity classes and 2 static risk classes create_parms works", {
  
  ## version with 2 static nativity classes
  pms <- create_parms(nnat = 2, nrisk = 2,
                      migrationdata = list(propinitnat = c(0.9,0.1)),
                      riskdata = list(propinitrisk = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)
})


test_that("version with 2 static nativity classes and 2 static risk classes create_parms works", {
  
  ## version with 2 static nativity classes
  pms <- create_parms(nnat = 2, nrisk = 2,
                      migrationdata = list(propinitnat = c(0.9,0.1)),
                      riskdata = list(propinitrisk = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)
})

test_that("version with all strata create_parms works", {
  
  pms <- create_parms(
    nnat = 2, nrisk = 2, npost = 2, nstrain = 2, nprot = 2,
    migrationdata = list(propinitnat = c(0.9,0.1)),
    riskdata = list(propinitrisk = c(0.9,0.1)),
    straindata = list(propinitstrain = c(0.9,0.1)),
    protdata = list(propinitprot = c(0.9,0.1)),
    verbose=FALSE)
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  expect_snapshot(pms)
})

