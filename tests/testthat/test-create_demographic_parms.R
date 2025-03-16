test_that("multiplication works", {
  
  ## try with default args
  new <- create_demographic_parms()
  
  calculated_md5 <- digest::digest(new)
  expect_snapshot(calculated_md5)

  
  expect_snapshot(new)
})

test_that("test creating parms with 2 static nativity classes", {
  

  ## version with 2 static nativity classes
  pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  
  
  expect_snapshot(pms)
})

test_that("test creating parms with all strata", {
  

  
  ## go big version with all strata to some degree:
  pms <- create_demographic_parms(
    nnat = 2, nrisk = 2, npost = 2, nstrain = 2, nprot = 2,
    migrationdata = list(propinitnat = c(0.9,0.1)),
    riskdata = list(propinitrisk = c(0.9,0.1)),
    straindata = list(propinitstrain = c(0.9,0.1)),
    protdata = list(propinitprot = c(0.9,0.1)))
  
  calculated_md5 <- digest::digest(pms)
  expect_snapshot(calculated_md5)
  
  
  expect_snapshot(pms) 
  
})

