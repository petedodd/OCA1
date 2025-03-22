
##' A helper function
##'
##' See `known_parameters`
##'
##' @title A helper function for `known_parameters`
##' @param parname a specific parameter name (Default NULL)
##' @param parmlist one of the lists of parameters
##' @return no return only printing
##' @author Pete Dodd
##' @importFrom rlang as_label ensym
print_helper <- function(parname=NULL,parmlist){
  parmlistname <- rlang::as_label(rlang::ensym(parmlist))
  print_here <- ifelse(is.null(parname),TRUE,parname %in% names(parmlist))
  if(print_here){
    cat("\n")
    cat(parmlistname,":\n")
    if(length(parmlist)>0){
      if(is.null(parname)){
        for(nm in names(parmlist)){
          cat("\t",nm,": ",parmlist[[nm]][[1]],"[",parmlist[[nm]][[2]],"]\n")
        }
      } else{
        cat("\t",parname,": ",parmlist[[parname]][[1]],"[",parmlist[[parname]][[2]],"]\n")
      }
    }
  }
}

##' Print info or test known parameters
##'
##' This has no return value but can be used to either print info on known parameters (all, or specific parameters),
##' and will test for unknown parameters.
##'
##' Info is returned organised by the parameter list they should be part of, and a short description and the expected
##' dimensions are printed.
##'
##' @title Print/test known parameters
##' @param parname if supplied consider only this parameter name; otherwise (default) print info on all parameters
##' @param quiet if TRUE (default) do not print info, only test whether parameter name is known
##' @return no return value
##' @author Pete Dodd
##' @examples
##' known_parameters()                     ##print all info
##' known_parameters("nonsense")           ##throws error as name not known
##' known_parameters("CDR_raw")            ##knows this parameter, prints info on it
##' known_parameters("CDR_raw",quiet=TRUE) ##knows this parameter, does not print info
##' @export
known_parameters <- function(parname=NULL,quiet=FALSE){

  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  ## relevant data on parameters
  ## will be docstring + dim string
  migrationdata <- list(
    migrage=list("Migration ageing rates","nnat"),
    propinitnat=list("Initial population proportion in each natal layer","nnat"),
    Pmigr_risk=list("Proportion of in-migrants to each risk layer","nsex, nrisk"),
    propinitrisk=list("Initial population proportion in each risk layer","nrisk"),
    birthrisk=list("Proportions of births into each risk layer","nrisk"),
    Pmigr_post=list("Proportion of in-migrants to each post-TB layer","nsex, npost"),
    propinitpost=list("Initial population proportion in each post-TB layer","npost"),
    Pmigr_strain=list("Proportion of in-migrants to each strain layer","nsex, nstrain"),
    propinitstrain=list("Initial population proportion in each strain layer","nstrain"),
    Pmigr_prot=list("Proportion of in-migrants to each protection layer","nsex"),
    propinitprot=list("Initial population proportion in each protection layer","npost"),
    immigration=list("In-migration numbers over time","ntimes, nage, nsex"),
    exmigrate=list("Out-migration hazard over time","ntimes, nage, nsex, nnat")
  )
  riskdata <- list(
    RiskHazardData=list("Progression hazard from each risk layer over time (last 0)","ntimes, nage, nsex, nrisk")
  )
  ## TODO introduce checks on 1st/last 0 & maybe harminoze naming: migrage/progn_posttb
  postdata <- list(
    progn_posttb=list("Post-TB ageing rates (1st/last 0)","npost")
  )
  straindata <- list()
  protdata <- list()
  tbparms <- list(
    BETAage=list("Beta matrix for age (see vignette for parametrization)","nage, nage"),
    BETAsex=list("Beta matrix for sex (see vignette for parametrization)","nsex, nsex"),
    BETAnat=list("Beta matrix for nativity (see vignette for parametrization)","nnat, nnat"),
    BETArisk=list("Beta matrix for risk (see vignette for parametrization)","nrisk, nrisk"),
    BETAstrain=list("Beta matrix for strain (see vignette for parametrization)","nstrain, nstrain"),
    CDR_raw=list("CDR data in each stratum over time","ntimes, nage, nsex, nnat, nrisk, nopst, nstrain, nprot"),
    migr_TBD=list("TB disease prevalence for immigrants","nage, nsex"),
    migr_TBI=list("TB infection prevalence for immigrants","nage, nsex")
  )
  for(nm in names(OCA1::hyperparms)){
    tbparms[[nm]] <- list(hyperparms[[nm]][[3]],"scalar")
  }
  nmzall <- c(
    names(migrationdata),
    names(riskdata),
    names(postdata),
    names(straindata),
    names(protdata),
    names(tbparms)
  )

  ## test if recognized:
  if(!is.null(parname)){
    if(!parname %in% nmzall){
      ## NOTE TODO could try to suggest nearest parameter for case of typos
      stop("Input parameter '", parname,"' not recognized!")
    }
  }

  ## print things
  if(!quiet){
    if(is.null(parname)){ #no argument given: print mode
      cat("(No argument given to test: printing parameter info)\n")
      cat("\n")
    }
    print_helper(parname,migrationdata)
    print_helper(parname,riskdata)
    print_helper(parname,postdata)
    print_helper(parname,straindata)
    print_helper(parname,protdata)
    print_helper(parname,tbparms)
    cat("\n")
  }
}


## known_parameters()
## known_parameters("nonsense")
## known_parameters("CDR_raw")
## known_parameters("CDR_raw",quiet=TRUE)
