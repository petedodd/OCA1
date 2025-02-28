## functions for managing outputs
absspace <- function(x, ...) {
  format(abs(x), ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}


##' @title Visualising demographic growth over time
##' @description
##' `plt_DemoGrowth` takes an model output object and provides a visualisation to show how the population sizes change over time
##' @param outdat  a data.table returned by `runmodel` with `raw=FALSE`
##' @details
##' The `ggplot2` package is used to construct a line and scatter plot for the population sizes for the time period used to run the model
##' @return  a `ggplot2` plot object
##' @seealso [plt_DemoSnapshots()]
##' @seealso [plt_TBSnapshots()]
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_DemoGrowth(out)  
##' @import ggplot2
##' @import data.table
##' @export
plt_DemoGrowth <- function(outdat) {
  cntry <- "GBR"
  N <- OCA1::UKdemo$N
  tc <- seq(from = round(min(outdat$t)), to = round(max(outdat$t)), by = 1)
  ggplot2::ggplot(
    N[iso3 == cntry & Year %in% tc, .(pop = sum(PopTotal)), by = Year],
    aes(Year, pop)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = outdat[!grepl("rate", state), .(pop = sum(value)), by = t], aes(t, pop), col = 2) +
    ggplot2::ylab("Population (in thousands)") +
    ggplot2::ggtitle(cntry) +
    ggplot2::scale_y_continuous(labels = absspace) +
    ggplot2::theme_minimal()+
    ggplot2::expand_limits(y=c(0,NA))
}


##' @title Visualising Demographic snapshots
##' @description
##' A short description...
##' @details
##' Additional details...
##' @seealso [plt_DemoGrowth()]
##' @seealso [plt_TBSnapshots()]
##' @param outdat a data.table returned by `runmodel` with `raw=FALSE`
##' @return a `ggplot2` plot object
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_DemoSnapshots(out)  
##' @import ggplot2
##' @import ggpubr
##' @import data.table
##' @export
plt_DemoSnapshots <- function(outdat) {
  cntry <- "GBR"
  N <- OCA1::UKdemo$N
  tmz <- seq(from = round(min(outdat$t)), to = round(max(outdat$t)), by = 5)
  tmpo <- outdat[!grepl("rate", state), .(value = sum(value)), by = .(t, sex, AgeGrp)]
  PL <- list()
  for (i in 1:length(tmz)) {
    YR <- tmz[i]
    (cly <- outdat[which.min(abs(t - YR)), t])
    tmp <- data.table::melt(N[iso3 == cntry & Year == YR,
                              .(t = Year, AgeGrp, M = PopMale, F = PopFemale)],
      id.vars = c("t", "AgeGrp")
    )
    names(tmp)[3] <- "sex"
    PL[[i]] <- ggplot2::ggplot(tmp, aes(x = AgeGrp, col = sex, shape = sex)) +
      ggplot2::geom_point(data = tmp[sex == "M"], aes(y = -value), size = 2) +
      ggplot2::geom_point(data = tmp[sex == "F"], aes(y = value), size = 2) +
      ggplot2::geom_line(data = tmpo[t == cly & sex == "M"],aes(y = -value, group = 1), alpha = .5) +
      ggplot2::geom_line(data = tmpo[t == cly & sex == "F"],aes(y = value, group = 1), alpha = .5) +
      ggplot2::coord_flip() +
      ggplot2::ylab("Population (in thousands)") +
      ggplot2::xlab("Age group") +
      ggplot2::scale_y_continuous(labels = absspace) +
      ggplot2::ggtitle(YR) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
  }
  nr <- ceiling(length(tmz) / 4)
  ggpubr::ggarrange(plotlist = PL, ncol = 4, nrow = nr)
}




##' @title Visualising TB Snapshots
##' @description
##' A short description...
##' @details
##' Additional details...
##' 
##' @param outdat a data.table eg returned by `runmodel` with `raw=FALSE`
##' @examples
##' ##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_TBSnapshots(out)  
##' @returns `ggplot2` plot
##' @seealso [plt_DemoSnapshots()]
##' @seealso [plt_DemoGrowth()]
##' @export
plt_TBSnapshots <- function(outdat){
  mycols <- c("lightseagreen", "maroon3", "palevioletred4", "yellow", "palevioletred3", "plum2", "lightsalmon2", "deeppink", "lightblue")
  tmp <- outdat
  tmp <- tmp[t > 1970]
  # select every fifth year for population pyramid
  tmp <- tmp[t %% 5 == 0]
  tmp <- tmp[(state!= "Uninfected" & !grepl("rate", state)), .(value = sum(value)), by = .(t, AgeGrp, sex, natcat, state)]
  tmp[, state := factor(state, levels = c("Learly", "Llate", "Asymp", "Symp", "Treat"))]

  pl <- ggplot2::ggplot(tmp, aes(x = AgeGrp, fill = state)) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(data = tmp[sex == 'M'], stat = 'identity', aes(y = value)) +  # Males (positive values)
    ggplot2::geom_bar(data = tmp[sex == 'F'], stat = 'identity', aes(y = -value)) +  # Females (negative values on left)
    ggplot2::scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE, trim = TRUE)) +  # Format y-axis labels as absolute values
    ggplot2::scale_fill_manual(values = mycols) +
    ggplot2::facet_wrap(natcat ~ t, scales = 'free', nrow = 2) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   legend.position = "bottom") +
    ggplot2::geom_hline(yintercept = 0, col = 'grey') +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Population distribution by age, sex, nativity and TB status") +
    ggplot2::ylab("Population") +
    ggplot2::xlab("")

  pl
}

