## rebuild.R
library(here)
library(odin)
library(devtools)
wdr <- here()
odin::odin_package(wdr)
devtools::document(wdr)
devtools::install(wdr, quick = TRUE)
devtools::test(wdr)
