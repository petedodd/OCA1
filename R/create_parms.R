


##' @title Creates parameters for model
##' @param tc TODO
##' @param nnat TODO
##' @param nrisk TODO
##' @param npost TODO
##' @param nstrain TODO
##' @param nprot TODO
##' @param migrationdata list of migration data see details
##' @param riskdata list of risk data see details
##' @param postdata TODO
##' @param straindata TODO
##' @param protdata TODO
##' @param tbparms TODO
##' @param verbose give more feedback
##' @return list of parameter for model
##' @author Pete Dodd
##' @export
create_parms <- function(tc = 1970:2020,
                         nnat = 1, nrisk = 1, npost = 1, nstrain = 1, nprot = 1,
                         migrationdata = list(),
                         riskdata = list(),
                         postdata = list(),
                         straindata = list(),
                         protdata = list(),
                         tbparms = list(),
                         verbose = FALSE) {

  ## === key dims
  nage <- length(OCA1::agz) # number of ages
  ntimes <- length(tc) # number of time data points
  dms <- c(ntimes, nnat, nrisk, npost, nstrain, nprot) # dimensions, bar age/sex which are always fixed

  ## === create demographic parameters
  P <- create_demographic_parms(
    tc = tc,
    nnat = nnat, nrisk = nrisk, npost = npost, nstrain = nstrain, nprot = nprot,
    migrationdata = migrationdata,
    riskdata = riskdata,
    postdata = postdata,
    straindata = straindata,
    protdata = protdata,
    verbose = verbose
  )

  ## === TB specific parameters following similar pattern to above
  ## TODO
  ## probably want a new function for default_tb_parameters (like default_parameters)
  ## perhaps use the same check_dims function
  ## pjd to start with migration x TB
  ## das to start with time/dependent CDR/hazard - NOTE let me know if this impacts speed

  ## tb parms
  tbparnames <- names(OCA1::parms)
  tbparnames <- c(tbparnames, "CDR_raw", "migr_TBD", "migr_TBI")
  ## defaults:
  tbparms <- add_defaults_if_missing(tbparms, tbparnames, dms, verbose)

  ## === TB parm checks
  ## prob checks
  if (verbose) message("\n")
  param_list <- list(
    "CDR_raw" = tbparms$CDR_raw
  )
  checks <- sapply(param_list, check_probabilities)
  if (all(checks)) {
    if (verbose) message("All TB parameters containing probabilities were correct\n")
  } else {
    stop("TB parameters with problems:", paste(names(param_list)[!checks], collapse = ", "))
  }

  ## check dimension
  checks <- check_dims(param_list, dms)
  if (all(checks)) {
    if (verbose) message("All TB input parameters dimensions were correct\n")
  } else {
    message("TB parameters with dimension problems:", paste(names(param_list)[!checks]))
  }

  ## === return combined demographic & TB parameters
  c(P, tbparms)
}

## CHECK
# pms <-create_parms()
