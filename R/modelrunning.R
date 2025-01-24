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
    ans <- data.table::as.data.table(ans)
    ## names(ans) <- c("t",t(outer(c("M_", "F_"), OCA1::agz, paste0)))
    ## ans <- data.table::melt(ans,id="t")
    ## ans[,c("sex","AgeGrp"):=data.table::tstrsplit(variable,split="_")]
    ## ans[,variable:=NULL]
    ans <- data.table::melt(ans, id = "t")
    ans[, variable := gsub("\\[|\\]|N", "", variable)]
    ans[, c("astring", "sexstring", "natstring","rskstring") :=
            data.table::tstrsplit(variable, split = ",")]
    ans[, c("AgeGrp", "sex", "natcat","risk") := .(
            OCA1::agz[as.integer(astring)],
            c("M", "F")[as.integer(sexstring)],
            as.integer(natstring),
            as.integer(rskstring)
          )]
    ans[, c("astring", "sexstring", "natstring","rskstring","variable") := NULL]
    ## as before
    ans$sex <- factor(ans$sex,levels=c("M","F"))
    ans$AgeGrp <- factor(ans$AgeGrp,levels=OCA1::agz)
  }
  ans # return
}

