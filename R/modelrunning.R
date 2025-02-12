## code for running models

##' A Simple Wrapper to Run Model
##'
##' content for details here
##'
##'
##' @title runmodel
##' @param p parameter list
##' @param times vector of times to run the model over - if not given, will construct from p
##' @param raw return the raw matrix ODE output or a data.table? (default FALSE)
##' @return a matrix of timeseries
##' @author Pete Dodd
##' @useDynLib OCA1, .registration = TRUE
##' @import data.table
##' @export
runmodel <- function(p, times, raw = FALSE) {
  if (missing(times)) {
    times <- seq(from = min(p$ttp), to = max(p$ttp), by = 0.1) # default times if not given
  }
  ## create model
  mdl <- ocaode$new(user=p)
  ## run
  ans <- mdl$run(times)
  if (!raw) { # convert to data.table
    ans <- raw2datatable(ans)
  }
  ans # return
}


## utility to convert raw array output in to format data.table
raw2datatable <- function(ans) {
    ans <- data.table::as.data.table(ans)
    ans <- data.table::melt(ans, id = "t")
    ans[, variable := gsub("\\[|\\]|N", "", variable)]
    ans[, c("astring", "sexstring", "natstring","rskstring","poststring","strainstring","protstring") :=
            data.table::tstrsplit(variable, split = ",")]
    ans[, c("AgeGrp", "sex", "natcat","risk","post","strain","prot") := .(
            OCA1::agz[as.integer(astring)],
            c("M", "F")[as.integer(sexstring)],
            as.integer(natstring),
            as.integer(rskstring),
            as.integer(poststring),
            as.integer(strainstring),
            as.integer(protstring)
          )]
    ans[, c("astring", "sexstring", "natstring", "rskstring", "variable",
            "poststring", "strainstring", "protstring") := NULL]
    ## as before
    ans$sex <- factor(ans$sex,levels=c("M","F"))
    ans$AgeGrp <- factor(ans$AgeGrp,levels=OCA1::agz)
    ans
}
