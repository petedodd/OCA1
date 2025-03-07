test_that("running the basic model works", {
  
  
  ## basic example without nativity class used
  pms <- create_demographic_parms() #create UK parameters
  out <- runmodel(pms)     
  
  ## check that result is not NULL
  expect_false(is.null(out))

  ## is the result a data frame?
  expect_true(all(unlist(lapply(out, is.data.frame))))

  expect_true(all(unlist(lapply(out, function(x) is.numeric(x$value)))))

  expect_false(all(unlist(lapply(out, function(x) is.na(x$value)))))

  expect_true(all(unlist(lapply(out, function(x) x$value>=0))))
  
  
  
  ## Should have 10 columns

    expect_true(all(unlist(lapply(out, function(x) ncol(x == 10)))))
  ## Check column names
  expect_true(all(unlist(lapply(out, function(x) colnames(x) ==
                                  c("t","value","state","AgeGrp",
                                    "sex","natcat","risk","post","strain","prot")))))

  
  ## check sex levels
  expect_true(all(unlist(lapply(out, function(x) levels(x$sex) == c("M","F")))))


  ## check states
  
  expect_true(all(unlist(lapply(out, function(x) x$natcat==1))))
  expect_true(all(unlist(lapply(out, function(x) x$risk==1))))
  expect_true(all(unlist(lapply(out, function(x) x$post==1))))
  expect_true(all(unlist(lapply(out, function(x) x$strain==1))))
  expect_true(all(unlist(lapply(out, function(x) x$prot==1))))
  
  ## check model was run over expected time series
  
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) < 2021))))
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) > 1949))))
  expect_true(all(unlist(lapply(out, function(x) 1970:2020 %in% as.numeric(substr(x$t, 1,4))))))
  
})


test_that("running the model with 2 nativity classes", {
  
  ## version with 2 static nativity classes
  
  pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
  out <- runmodel(pms,raw=FALSE)
  out
  
  ## check that result is not NULL
  expect_false(is.null(out))
  
  ## is the result a data frame?
  expect_true(all(unlist(lapply(out, is.data.frame))))
  
  expect_true(all(unlist(lapply(out, function(x) is.numeric(x$value)))))
  
  expect_false(all(unlist(lapply(out, function(x) is.na(x$value)))))
  
  expect_true(all(unlist(lapply(out, function(x) x$value>=0))))
  
  
  ## Should have 10 columns
  
  expect_true(all(unlist(lapply(out, function(x) ncol(x == 10)))))
  ## Check column names
  expect_true(all(unlist(lapply(out, function(x) colnames(x) ==
                                  c("t","value","state","AgeGrp",
                                    "sex","natcat","risk","post","strain","prot")))))
  
  
  ## check sex levels
  expect_true(all(unlist(lapply(out, function(x) levels(x$sex) == c("M","F")))))
  
  

  ## check states
  
  expect_true(all(unlist(lapply(out, function(x) x$natcat %in% c(1,2)))))
  expect_true(all(unlist(lapply(out, function(x) x$risk==1))))
  expect_true(all(unlist(lapply(out, function(x) x$post==1))))
  expect_true(all(unlist(lapply(out, function(x) x$strain==1))))
  expect_true(all(unlist(lapply(out, function(x) x$prot==1))))
  
  ## check model was run over expected time series
  
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) < 2021))))
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) > 1949))))
  expect_true(all(unlist(lapply(out, function(x) 1970:2020 %in% as.numeric(substr(x$t, 1,4))))))
  
  

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
  expect_true(all(unlist(lapply(out, is.data.frame))))
  
  expect_true(all(unlist(lapply(out, function(x) is.numeric(x$value)))))
  
  expect_false(all(unlist(lapply(out, function(x) is.na(x$value)))))
  
  expect_true(all(unlist(lapply(out, function(x) x$value>=0))))
  
  
  ## Should have 10 columns
  
  expect_true(all(unlist(lapply(out, function(x) ncol(x == 10)))))
  ## Check column names
  expect_true(all(unlist(lapply(out, function(x) colnames(x) ==
                                  c("t","value","state","AgeGrp",
                                    "sex","natcat","risk","post","strain","prot")))))
  
  
  ## check sex levels
  expect_true(all(unlist(lapply(out, function(x) levels(x$sex) == c("M","F")))))
  
  
  
  ## check states
  
  expect_true(all(unlist(lapply(out, function(x) x$natcat %in% c(1,2)))))
  expect_true(all(unlist(lapply(out, function(x) x$risk %in% c(1,2)))))
  expect_true(all(unlist(lapply(out, function(x) x$post==1))))
  expect_true(all(unlist(lapply(out, function(x) x$strain==1))))
  expect_true(all(unlist(lapply(out, function(x) x$prot==1))))
  
  ## check model was run over expected time series
  
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) < 2021))))
  expect_true(all(unlist(lapply(out, function(x) as.numeric(substr(x$t, 1,4)) > 1949))))
  expect_true(all(unlist(lapply(out, function(x) 1970:2020 %in% as.numeric(substr(x$t, 1,4))))))
  
  
  
})



