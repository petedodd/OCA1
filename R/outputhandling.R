## functions for managing outputs
absspace <- function(x, ...) {
  format(abs(x), ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}


getStateFromOut <- function(outdata){
  
## function to get state information in robust manner depending on outdata type
  
  if(!is.data.frame(outdata)){
    
    if(!"state" %in% names(outdata)) stop("No state information found in the supplied outdata object. Was this created with raw = TRUE ?")
    
    outdat <- outdata$state  
    
  } else {
    
    if(!"state" %in% colnames(outdata)) stop("No state information found in the supplied outdata object. Was this created with raw = TRUE ?")
    outdat <- outdata
  }
  
  outdat 
}

getRateFromOut <- function(outdata){
  
  ## function to get state information in robust manner depending on outdata type
  
  if(!is.data.frame(outdata)){
    
    if(!"rate" %in% names(outdata)) stop("No state information found in the supplied outdata object. Was this created with raw = TRUE ?")
    
    outdat <- outdata$rate  
    
  } else {
    
    if(!"rate" %in% colnames(outdata)) stop("No state information found in the supplied outdata object. Was this created with raw = TRUE ?")
    outdat <- outdata
  }
  

  outdat 
}




##' @title Visualising demographic growth over time
##' @description
##' `plt_DemoGrowth` takes an model output object and provides a visualisation to show how the population sizes change over time
##' @param outdata  A data.table returned by `runmodel` with `raw=FALSE`
##' @details
##' The `ggplot2` package is used to construct a line and scatter plot for the population sizes for the time period used to run the model
##' @return  A `ggplot2` plot object
##' @seealso [plt_DemoSnapshots()]
##' @seealso [plt_TBSnapshots()]
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_DemoGrowth(out)  
##' @importFrom ggplot2 ggplot geom_point geom_line ylab ggtitle scale_y_continuous theme_bw expand_limits
##' @import data.table
##' @export
plt_DemoGrowth <- function(outdata) {
  
  outdat <- getStateFromOut(outdata)

  cntry <- "GBR"
  N <- OCA1::UKdemo$N
  tc <- seq(from = round(min(outdat$t)), to = round(max(outdat$t)), by = 1)
  ggplot(
    N[iso3 == cntry & Year %in% tc, .(pop = sum(PopTotal)), by = Year],
    aes(Year, pop)
  ) +
    geom_point() +
    geom_line(data = outdat[!grepl("rate", state), .(pop = sum(value)), by = t], aes(t, pop), col = 2) +
    ylab("Population (in thousands)") +
    ggtitle(cntry) +
    scale_y_continuous(labels = absspace) +
    theme_bw()+
    expand_limits(y=c(0,NA))
}

checkDemoFit <- function(outdata){
  
  outdat <- getStateFromOut(outdata)
  
  cntry <- "GBR"
  N <- OCA1::UKdemo$N
  tc <- seq(from = round(min(outdat$t)), to = round(max(outdat$t)), by = 1)
  
  x <- N[iso3 == cntry & Year %in% tc, .(pop = sum(PopTotal)), by = Year]
  y <- outdat[!grepl("rate", state), .(pop = sum(value)), by = t] |>
    dplyr::group_by(stringr::str_sub(t, 1, 4)) |>
    dplyr::summarise(pop  = mean(pop)) |>
    dplyr::rename(Year = 1) |>
    dplyr::mutate(Year = as.numeric(Year))
    
   n <- nrow(x)
  dplyr::left_join(x,y,by="Year") |>
    dplyr::mutate(error = (pop.x - pop.y)/pop.x, N=dplyr::n()) |>
    dplyr::summarise(sse = sum(error^2)) |>
    dplyr::mutate(RMSE = sqrt(sse / n))
  
}


##' @title Visualising Demographic snapshots
##' @description
##' A short description...
##' @details
##' Additional details...
##' @seealso [plt_DemoGrowth()]
##' @seealso [plt_TBSnapshots()]
##' @param outdata a data.table returned by `runmodel` with `raw=FALSE`
##' @return a `ggplot2` plot object
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_DemoSnapshots(out)  
##' @importFrom ggplot2 ggplot geom_point geom_line coord_flip ylab xlab scale_y_continuous theme_bw theme facet_wrap aes
##' @import data.table
##' @importFrom dplyr mutate group_by slice ungroup rename filter select left_join
##' @importFrom stringr str_replace_all
##' @importFrom tidyr pivot_longer
##' @export
plt_DemoSnapshots <- function(outdata) {
  
  outdat <- getStateFromOut(outdata)
  
  cntry <- "GBR"
  
  ## Get expected populations for males and females
  N <- OCA1::UKdemo$N |>
    select(t = Year, AgeGrp,PopMale,PopFemale) |>
    pivot_longer(contains("Pop"), values_to = "expected",names_to = "sex") |> 
    mutate(sex = str_replace_all(sex,"PopMale","M")) |>
    mutate(sex = str_replace_all(sex,"PopFemale","F"))
   
  ## calculate years at intervals of 5  
  tmz <- seq(from = round(min(outdat$t)), to = round(max(outdat$t)), by = 5)
  nr <- ceiling(length(tmz) / 4)
  
    ## get the calculated populations  
    tmpo <- outdat[!grepl("rate", state), .(value = sum(value)), by = .(t, sex, AgeGrp)] |>
    mutate(t = round(t)) |>
    group_by(t, sex, AgeGrp) |>
    slice(1) |>
    ungroup() |>
    rename("calculated" = value) |>
    mutate(AgeGrp = as.ordered(AgeGrp))
  
  
    ## make joined data frame for plotting
    
    combined <- left_join(tmpo,N, by = c("t","sex","AgeGrp")) |>
      mutate(calculated = ifelse(sex == "M", -calculated,calculated)) |> 
      mutate(expected = ifelse(sex == "M", -expected, expected)) |>
      filter(t %in% tmz)
    
    ggplot(combined, aes(x = AgeGrp, col=sex, shape=sex)) + 
      geom_point(aes(y = expected,group=sex)) + 
      geom_line(aes(y = calculated, group = sex)) +
      coord_flip() +
      ylab("Population (in thousands)") +
      xlab("Age group") +
      scale_y_continuous(labels = absspace) +
      theme_bw() +
      theme(legend.position = "none") + 
      facet_wrap(~t,ncol = nr)
      
  

}



##' @title Visualising Temporal Trends in Demographics and TB Status
##' @description
##' A short description...
##' @details
##' Additional details...
##' @seealso [plt_DemoGrowth()]
##' @seealso [plt_TBRates()]
##' @param outdata A data.table returned by `runmodel` with `raw=FALSE`
##' @param by_layer One of natcat, risk, post, strain, prot
##' @return A `ggplot2` plot object showing population distribution over time by age, sex, nativity and TB status
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_TBSnapshots(out, "natcat") 
##' @importFrom ggplot2 ggplot coord_flip geom_bar scale_y_continuous scale_fill_manual facet_wrap theme_bw theme geom_hline ggtitle ylab xlab aes element_text element_blank
##' @import data.table
##' @export

plt_TBSnapshots <- function(outdata, by_layer = "natcat") {
  
  outdat <- getStateFromOut(outdata)
  
  
  mycols <- c("lightseagreen", "maroon3", "palevioletred4", "yellow", 
              "palevioletred3", "plum2", "lightsalmon2", "deeppink", "lightblue")
  
  
  
  
  layer_names <- c(
    "natcat" = "Nativity",
    "risk" = "TB risk",
    "post" = "Post TB",
    "strain" = "TB strain",
    "prot" = "Protection"
  )
  
  # Ensure the provided `by_layer` is valid
  stopifnot(by_layer %in% names(layer_names))
  
  # Get the descriptive name for the selected layer
  new_layer_name <- layer_names[[by_layer]]
  
  # Define dyn_name specific to the selected layer
  #dyn_name <- paste(new_layer_name, "layer")
  dyn_name <- new_layer_name
  tmp <- outdat[t > 1970]
  
  tmp <- tmp[t %% 5 == 0]
  
  # Aggregate population values by year, age group, sex, TB state, and the selected layer
  tmp <- tmp[(state != "Uninfected" & !grepl("rate", state)), 
             .(value = sum(value)), by = c("t", "AgeGrp", "sex", "state", by_layer)]
  
  tmp[, (by_layer) := paste0(new_layer_name, get(by_layer))]
  setnames(tmp, by_layer, new_layer_name)
  
  tmp[, state := factor(state, levels = c("Learly", "Llate", "Asymp", "Symp", "Treat"))]
  
  # Generate the population pyramid plot
  pl <- ggplot(tmp, aes(x = AgeGrp, fill = state)) +
    coord_flip() +
    geom_bar(data = tmp[sex == 'M'], stat = 'identity', aes(y = value)) +
    geom_bar(data = tmp[sex == 'F'], stat = 'identity', aes(y = -value)) +
    scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_fill_manual(values = mycols) +
    facet_wrap(as.formula(paste0("`", new_layer_name, "` ~ t")), scales = 'free', nrow = 2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   legend.position = "bottom", legend.title = element_blank()) +
    geom_hline(yintercept = 0, col = 'grey') +
    ggtitle(paste("Population distribution by Age, Sex,", dyn_name, "and TB status")) +
    ylab("Population") +
    xlab("")
  
  pl
}

# plt_TBSnapshots(out, "natcat")
# plt_TBSnapshots(out, "risk")


##' @title Visualising TB incidence rates by age and sex over time
##' @description
##' A short description...
##' @details
##' Additional details...
##' @seealso [plt_DemoGrowth()]
##' @seealso [plt_TBSnapshots()]
##' @param outdata A data.table returned by `runmodel` with `raw=FALSE`
##' @param rate_type One of incidence, notification or mortality
##' @param by_layer One of natcat, risk, post, strain, prot
##' @return a `ggplot2` plot object showing changes in TB incidence by age and sex stratified by target layer over time
##' @author Pete Dodd
##' @examples
##' pms <- create_demographic_parms() #create UK parameters
##' out <- runmodel(pms)              #run model with these
##' out                               #inspect
##' ## visualize
##' plt_TBRates(out, rate_type = "incidence", by_layer = "natcat")
##' @importFrom ggplot2 ggplot facet_grid geom_line ylab ggtitle theme_light xlab theme guides aes element_blank guide_legend
##' @importFrom dplyr mutate group_by summarise recode
##' @import data.table
##' @export

plt_TBRates <- function(outdata, rate_type = "incidence", by_layer = "natcat") {
  outdat <- getRateFromOut(outdata)
  
  # Converuser-friendly rate types to colnames in the dataset
  state_map <- c(
    "incidence" = "rate_Incidence",
    "notification" = "rate_Notification",
    "mortality" = "rate_TBmortality"
  )
  
  # Mapping modeled layer names to their descriptive labels
  layer_names <- c(
    "natcat" = "Nativity layer",
    "risk" = "TB progression risk layer",
    "post" = "Post TB layer",
    "strain" = "TB strain layer",
    "prot" = "Protection layer"
  )
  
  # make sure rate_type and by_layer are valid input values
  stopifnot(rate_type %in% names(state_map), by_layer %in% names(layer_names))
  
  # select the target rate
  tmp <- outdat[state == state_map[[rate_type]]] |>
    mutate(state = recode(state,
                                        rate_Incidence = "Incidence rate",
                                        rate_Notification = "Notification rate",
                                        rate_TBmortality = "TB mortality rate")) |>
    
    group_by(t, state, AgeGrp, sex, .data[[by_layer]]) |>
    
    # Compute the mean value for each group, ignoring NA values
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    as.data.table()
  
  # Retrieve the maped layer namer for the selected layer
  new_layer_name <- layer_names[[by_layer]]
  
  # modify the the target layer to clear layer name
  tmp[, (by_layer) := paste(new_layer_name, get(by_layer))]
  
  # Rename the column to use the clear layer name
  setnames(tmp, by_layer, new_layer_name)
  
  # plot title based on the first available state name;
  plot_title <- if (nrow(tmp) > 0) tmp$state[1] else "No Data"
  
ggplot(tmp, aes(t, value, col = AgeGrp)) +
    # Create facet grid for stratification layer and sex (columns)
    facet_grid(as.formula(paste0("`", new_layer_name, "` ~ sex"))) + 
    geom_line() + 
    ylab("Rate per 100,000 population") +
    ggtitle(plot_title) +
    theme_light() + 
    xlab("") +
    
    # Adjust legend position and remove title
    theme(legend.position = "bottom", legend.title = element_blank()) +
    
    # Adjust legend to have 7 columns for better readability
    guides(col = guide_legend(ncol = 7))
}

# plt_TBRates(out, rate_type = "incidence", by_layer = "natcat")
# plt_TBRates(out,rate_type = "notification", by_layer = "risk")
# plt_TBRates(out,rate_type = "mortality", by_layer = "post")

