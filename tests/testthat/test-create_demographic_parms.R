test_that("multiplication works", {
  
  ## try with default args
  new <- create_demographic_parms()
  message("Checking with default arguments")
  expect_equal(digest::digest(new), "11cc9cdb7b45111ac8bf488cf2c02507")

  ## version with 2 static nativity classes
  pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "f00bcb8ba8c52ab37fa4f060eb7ec18b")
  
  ## version with 2 static nativity classes and 2 static risk classes
  pms <- create_demographic_parms(nnat = 2, nrisk = 2,
                                  migrationdata = list(propinitnat = c(0.9,0.1)),
                                  riskdata = list(propinitrisk = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "fce31e03bd49deea7038156918b1dc32")
  
  
  ## go big version with all strata to some degree:
  pms <- create_demographic_parms(
    nnat = 2, nrisk = 2, npost = 2, nstrain = 2, nprot = 2,
    migrationdata = list(propinitnat = c(0.9,0.1)),
    riskdata = list(propinitrisk = c(0.9,0.1)),
    straindata = list(propinitstrain = c(0.9,0.1)),
    protdata = list(propinitprot = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "56baf449daa8017075720c485fd730c4")
  
})
