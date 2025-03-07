test_that("multiplication works", {
  
  ## try with default args
  new <- create_demographic_parms()
  message("Checking with default arguments")
  expect_equal(digest::digest(new), "1425f12ba0a23b4564a265e28802fc9f")

  ## version with 2 static nativity classes
  pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "7d693d8a319d201762404166aa6a145e")
  
  ## version with 2 static nativity classes and 2 static risk classes
  pms <- create_demographic_parms(nnat = 2, nrisk = 2,
                                  migrationdata = list(propinitnat = c(0.9,0.1)),
                                  riskdata = list(propinitrisk = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "aa55536e25a5612b8edfa52b2c559846")
  
  
  ## go big version with all strata to some degree:
  pms <- create_demographic_parms(
    nnat = 2, nrisk = 2, npost = 2, nstrain = 2, nprot = 2,
    migrationdata = list(propinitnat = c(0.9,0.1)),
    riskdata = list(propinitrisk = c(0.9,0.1)),
    straindata = list(propinitstrain = c(0.9,0.1)),
    protdata = list(propinitprot = c(0.9,0.1)))
  
  expect_equal(digest::digest(pms), "292f45532fa76512a11048d336900018")
  
})
