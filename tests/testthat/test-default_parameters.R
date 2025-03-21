test_that("default_parameters works", {
  
  ##make some dummy dimensions  
  dms <- c(sample(1:51,1),
           sample(1:10,1),
           sample(1:10,1),
           sample(1:10,1),
           sample(1:10,1),
           sample(1:10,1),
           sample(1:10,1))
  
  
  hotone <- function(n) {
    result <- rep(0, n)
    if (n > 0) {
      result[1] <- 1
    }
    return(result)
  }
  
  # Test 'migrage'
  expect_equal(length(default_parameters("migrage", dms)), dms[2])
  expect_true(all(default_parameters("migrage", dms) == 0))
  
  # Test 'propinitnat'
  expect_equal(length(default_parameters("propinitnat", dms)), dms[2])
  expect_equal(default_parameters("propinitnat", dms), hotone(dms[2]))
  
  
  # Test 'Pmigr_risk'
  expect_equal(dim(default_parameters("Pmigr_risk", dms)), c(2, dms[3]))
  expect_equal(default_parameters("Pmigr_risk", dms)[1, ], hotone(dms[3]))
  
  # Test 'progn_posttb'
  expect_equal(length(default_parameters("progn_posttb",dms)), dms[4])
  expect_true(all(default_parameters("progn_posttb",dms)==0))
  
  
  
  # Test 'Pmigr_risk', Pmigr_post, Pmigr_strain, Pmigr_prot
  
  X <-c(1,1)
  names(X) <- c("ans","ans")
  
  expect_equal(dim(default_parameters("Pmigr_risk", dms)), c(2, dms[3]))
  expect_equal(default_parameters("Pmigr_risk", dms)[,1], X)
  expect_true(all(default_parameters("Pmigr_risk", dms)[,-1]==0))
  
  expect_equal(dim(default_parameters("Pmigr_post", dms)), c(2, dms[4]))
  expect_equal(default_parameters("Pmigr_post", dms)[,1], X)
  expect_true(all(default_parameters("Pmigr_post", dms)[,-1]==0))
  
  expect_equal(dim(default_parameters("Pmigr_prot", dms)), c(2, dms[6]))
  expect_equal(default_parameters("Pmigr_prot", dms)[,1], X)
  expect_true(all(default_parameters("Pmigr_prot", dms)[,-1]==0))
  
  expect_equal(dim(default_parameters("Pmigr_strain", dms)), c(2, dms[5]))
  expect_equal(default_parameters("Pmigr_strain", dms)[,1], X)
  expect_true(all(default_parameters("Pmigr_strain", dms)[,-1]==0))
  
  
  # Test 'migr_TBD'
  
  X <- matrix(1, nrow = length(OCA1::agz),ncol=2)
  X[1:3,] <- 0
  X[4:nrow(X),] <- 0.001
  rownames(X) <- OCA1::agz
  colnames(X) <- c("M","F")
  dimnames(X) <- list("acat"=OCA1::agz, "sex"=c("M","F"))
  
  
  expect_equal(default_parameters("migr_TBD",dms),X)
  
  # Test 'migr_TBI'
  X <- matrix(1, nrow = length(OCA1::agz),ncol=2)
  X[1:3,] <- 0
  X[4:nrow(X),] <- 0.3
  rownames(X) <- OCA1::agz
  colnames(X) <- c("M","F")
  dimnames(X) <- list("acat"=OCA1::agz, "sex"=c("M","F"))
  
  expect_equal(default_parameters("migr_TBI",dms),X)
  
  
  # Test 'immigration'
  expect_equal(dim(default_parameters("immigration", dms)), c(dms[1], length(OCA1::agz), 2))
  expect_true(all(default_parameters("immigration", dms) == 0))
  
  
  # Test 'exmigrate'
  expect_equal(dim(default_parameters("exmigrate", dms)), c(dms[1], length(OCA1::agz), 2, dms[2]))
  expect_true(all(default_parameters("exmigrate", dms) == 0))
  
  # Test 'RiskHazardData'
  expect_equal(dim(default_parameters("RiskHazardData", dms)), c(dms[1], length(OCA1::agz), 2, dms[3]))
  expect_true(all(default_parameters("RiskHazardData", dms) == 0))
  
  # Test 'CDR_raw'
  expect_equal(dim(default_parameters("CDR_raw", dms)), c(dms[1], length(OCA1::agz), 2, dms[2],dms[3],dms[4],dms[5],dms[6]))
  expect_true(all(default_parameters("CDR_raw", dms) == 0.7))
  
  # Test 'progn_posttb'
  expect_equal(length(default_parameters("progn_posttb",dms)), dms[4])
  expect_true(all(default_parameters("progn_posttb",dms)==0))
  expect_error(default_parameters("foo"))
  
  
})

