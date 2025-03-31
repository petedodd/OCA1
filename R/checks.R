

## a utility to check probabilities are valid
check_probabilities <- function(params, checksum = TRUE) {
  # 1. Check for numeric type:
  if (!all(is.numeric(params))) {
    message("All parameters must be numeric.")
    return(FALSE)
  }
  # 2. Check for values between 0 and 1 (inclusive):
  if (!all(params >= 0 & params <= 1)) {
    message("All parameters must be probabilities between 0 and 1.")
    return(FALSE)
  }
  if (checksum) {
    if (is.vector(params)) {
      # 3. Check if the sum is (approximately) equal to 1
      if (abs(sum(params) - 1) > .Machine$double.eps^0.5) {
        message("The sum of parameters must be approximately equal to 1.")
        return(FALSE)
      }
    } else if (is.matrix(params)) {
      test <- apply(params, 1, function(x) abs(sum(x) - 1) > .Machine$double.eps^0.5)
      if (any(test)) {
        return(FALSE)
      }
    }
  }
  return(TRUE) # Return TRUE if all checks pass
}

## utility: length if vector, dim[2] if array
lenordim2 <- function(X) ifelse(is.null(dim(X)), length(X), dim(X)[2])


## utility to check lengths
check_dims <- function(parlist, dms){
  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  parnames <- names(parlist)
  ans <- list()
  for(pname in parnames){
    ans[[pname]] <- FALSE
    if(pname %in% c("migrage","propinitnat")){
      ans[[pname]] <- (length(parlist[[pname]])==dms[2]) #nnat
    }
    if(pname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[3]) #nrisk
    }
    if(pname %in% c("Pmigr_post","propinitpost","progn_posttb")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[4]) #npost
    }
    if(pname %in% c("Pmigr_strain","propinitstrain", "IRRstrain")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[5]) #nstrain
    }
    if(pname %in% c("Pmigr_prot","propinitprot", "IRRprotn")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[6]) #nprotn
    }
    ## now test dim 1s:
    if(pname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
      if (ans[[pname]]) {                                       #passed on dim 2
        ans[[pname]] <- (dim(parlist[[pname]])[1] == 2) # nsex
      }
    }
    if(pname %in% c("migr_TBD","migr_TBI")){
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(length(OCA1::agz), 2))
    }
    if(pname %in% c("immigration")){
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2))
    }
    if (pname %in% c("exmigrate")) {
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2, dms[2]))
    }
    if(pname=="RiskHazardData"){
      ans[[pname]] <- all(dim(parlist[[pname]])==c(dms[1], length(OCA1::agz), 2, dms[3]))
    }
    if(pname=="CDR_raw"){
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2, dms[2:6]))
    }
    if(pname %in% c("propinitE","propinitL","propinitA","propinitS","propinitT")){ #ALL non-time dims
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(length(OCA1::agz), 2, dms[2:6]))
    }
    if (pname == "BETAage") {
      ans[[pname]] <- all(dim(parlist[[pname]]) == rep(length(OCA1::agz), 2))
    }
    if (pname == "BETAsex") {
      ans[[pname]] <- all(dim(parlist[[pname]]) == rep(2, 2))
    }
    if (pname == "BETAnat") {
      ans[[pname]] <- all(dim(parlist[[pname]]) == rep(dms[2], 2)) #nnat
    }
    if (pname == "BETArisk") {
      ans[[pname]] <- all(dim(parlist[[pname]]) == rep(dms[3], 2)) # nrisk
    }
    if (pname == "BETAstrain") {
      ans[[pname]] <- all(dim(parlist[[pname]]) == rep(dms[5], 2)) # nstrain
    }
  } #end loop over list names
  unlist(ans)
}
