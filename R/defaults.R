
## utility for defaults: [1,0,0,...] or just 1
hotone <- function(n){
  ans <- rep(0,n)
  ans[1] <- 1
  ans
}

## a utility to construct default values for unsupplied parameters/data
default_parameters <- function(parname, dms, verbose=FALSE){

  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  if(verbose) message("Using default value for ",parname,"...")
  ans <- NA
  if(parname %in% c("migrage")){
    ans <- rep(0,dms[2]) #nnat
  }
  if(parname %in% c("propinitnat")){
    ans <- hotone(dms[2]) #nnat
  }
  if(parname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
    ans <- hotone(dms[3]) #nrisk
  }
  if(parname %in% c("Pmigr_post","propinitpost")){
    ans <- hotone(dms[4]) #npost
  }
  if(parname %in% c("Pmigr_strain","propinitstrain")){
    ans <- hotone(dms[5]) #nstrain
  }
  if(parname %in% c("Pmigr_prot","propinitprot")){
    ans <- hotone(dms[6]) #nprotn
  }
  ## these need bumping up to duplicate by sex
  if(parname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
    ans <- rbind(ans,ans) #one for each sex
  }
  if(parname %in% c("migr_TBD","migr_TBI")){
    ans <- array(1,
                 dim = c(length(OCA1::agz), 2),
                 dimnames = list(
                   acat = OCA1::agz,
                   sex = c("M", "F")
                 )
                 )
    ans[1:3,] <- 0 #default nothing in children
    if(parname %in% c("migr_TBD")){
      ans <- (100/1e5) * ans #default prevalence
    }
    if(parname %in% c("migr_TBI")){
      ans <- 0.3 * ans #default prevalence
    }
  }
  if(parname %in% c("immigration")){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F")
      )
      )
  }
  if (parname %in% c("exmigrate")) {
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2]
      )
    )
  }
  if(parname == "RiskHazardData"){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[3]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        risk = 1:dms[3]
      )
    )
  }
  if (parname == "CDR_raw") {
    ans <- array(0.7,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2:6]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2],
        risk = 1:dms[3],
        post = 1:dms[4],
        strain = 1:dms[5],
        protn = 1:dms[6]
      )
    )
  }
  if (parname == "BETAage") {
    ans <- array(1.0,
      dim = c(length(OCA1::agz), length(OCA1::agz)),
      dimnames = list(
        acat = OCA1::agz,
        acat = OCA1::agz
      )
    )
  }
  if (parname == "BETAsex") {
    ans <- array(1.0,
      dim = c(2,2),
      dimnames = list(
        sex = c("M", "F"),
        sex = c("M", "F")
      )
    )
  }
  if (parname == "BETAnat") {
    ans <- array(1.0,
      dim = c(dms[2], dms[2]),
      dimnames = list(
        nativity = 1:dms[2],
        nativity = 1:dms[2]
      )
    )
  }
  if (parname == "BETArisk") {
    ans <- array(1.0,
      dim = c(dms[3], dms[3]),
      dimnames = list(
        risk = 1:dms[3],
        risk = 1:dms[3]
      )
    )
  }
  if (parname == "BETAstrain") {
    ans <- array(1.0,
      dim = c(dms[5],dms[5]),
      dimnames = list(
        strain = 1:dms[5],
        strain = 1:dms[5]
      )
    )
  }
  if (parname == "progn_posttb") {
    ans <- rep(0,dms[4])
  }
  if(parname %in% c("propinitE","propinitL","propinitA","propinitS","propinitT")){
    ## template
    L <- 1-exp(-seq(from=2.5,by=5,length.out = length(OCA1::agz)) * 2.5e-3) #distribute by FOI flat prev ~10%
    T <- array(1.0,
               dim = c(length(OCA1::agz), 2, dms[2:6]),
               dimnames = list(
                 acat = OCA1::agz,
                 sex = c("M", "F"),
                 nativity = 1:dms[2],
                 risk = 1:dms[3],
                 post = 1:dms[4],
                 strain = 1:dms[5],
                 protn = 1:dms[6]
               )
               )
    ELAST <- c(0.1-15e-5,0.9,5e-5,5e-5,5e-5) #props
    ## sum(ELAST) #check
    if(parname == "propinitE"){
      ans <- ELAST[1] * T
    }
    if(parname == "propinitL"){
      ans <- ELAST[2] * T
    }
    if(parname == "propinitA"){
      ans <- ELAST[3] * T
    }
    if(parname == "propinitS"){
      ans <- ELAST[4] * T
    }
    if(parname == "propinitT"){
      ans <- ELAST[5] * T
    }
  }
  if(parname %in% names(OCA1::parms)){ #basic TB parameters
    ans <- OCA1::parms[[parname]]
  }
  if(is.na(sum(ans))) stop("No default available for ",parname,"!\n") #sum to handle arrays
  ans
}

## loops over to add missing parameters as default
add_defaults_if_missing <- function(L, parnames, dms, verbose){
  for(pname in parnames){
    if (!pname %in% names(L)) {
      L[[pname]] <- default_parameters(pname, dms, verbose)
    } else {
      known_parameters(pname,quiet=TRUE) #test exists
      if (verbose) message("Using supplied value for ", pname, "...")
    }
  }
  L
}


