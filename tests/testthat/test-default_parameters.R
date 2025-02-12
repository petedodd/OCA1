test_that("default_parameters works", {
  
  expect_equal(default_parameters("migrage",c(2,2)), c(0,0))
  expect_equal(default_parameters("migrage",c(2,1)), 0)

  expect_equal(default_parameters("propinitnat",c(2,2)), c(1,0))  
  expect_equal(default_parameters("propinitnat",c(2,3)), c(1,0,0))
  
  
  expect_error(default_parameters("foo"))
  
  
})




#if(parname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
#  ans <- hotone(dms[3]) #nrisk
#}
#if(parname %in% c("Pmigr_post","propinitpost")){
#  ans <- hotone(dms[4]) #npost
#}
#if(parname %in% c("Pmigr_strain","propinitstrain")){
#  ans <- hotone(dms[5]) #nstrain
#}
#if(parname %in% c("Pmigr_prot","propinitprot")){
#  ans <- hotone(dms[6]) #nprotn
#}
## these need bumping up to duplicate by sex
#if(parname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
#  ans <- rbind(ans,ans) #one for each sex
#}