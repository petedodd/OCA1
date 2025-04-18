% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outputhandling.R
\name{plt_DemoGrowth}
\alias{plt_DemoGrowth}
\title{Visualising demographic growth over time}
\usage{
plt_DemoGrowth(outdata)
}
\arguments{
\item{outdata}{A data.table returned by \code{runmodel} with \code{raw=FALSE}}
}
\value{
A \code{ggplot2} plot object
}
\description{
\code{plt_DemoGrowth} takes an model output object and provides a visualisation to show how the population sizes change over time
}
\details{
The \code{ggplot2} package is used to construct a line and scatter plot for the population sizes for the time period used to run the model
}
\examples{
pms <- create_demographic_parms() #create UK parameters
out <- runmodel(pms)              #run model with these
out                               #inspect
## visualize
plt_DemoGrowth(out)  
}
\seealso{
\code{\link[=plt_DemoSnapshots]{plt_DemoSnapshots()}}

\code{\link[=plt_TBSnapshots]{plt_TBSnapshots()}}
}
\author{
Pete Dodd
}
