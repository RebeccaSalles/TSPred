#Source all .R source files from package R folder
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir("C:/Users/Rebecca/OneDrive/Documentos/TSPred/R")

#Load all artifacts of the package to the environment
devtools::load_all()

#Resave data objects (datasets) as compressed files (required by CRAN)
path <- "C:/Users/Rebecca/OneDrive/Documentos/TSPred/data"
tools::resaveRdaFiles(path, compress="xz")

#Constructs package documentation based on roxygen2
devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))

#Check the package adequability to CRAN
#A thorough checking process is performed
#(for that, devtools::document is also called internally for building documentation)
devtools::check(args = c('--as-cran','--no-manual'), build_args = c('--resave-data=best','--no-manual','--no-manual'))

#Check the package in several environments and releases of R
devtools::check_rhub(args = c('--as-cran','--no-manual'), build_args = c('--resave-data=best','--no-manual','--no-manual'))
devtools::check_win_devel(args = c('--as-cran','--no-manual'), build_args = c('--resave-data=best','--no-manual','--no-manual'))
devtools::check_win_release(args = c('--as-cran','--no-manual'), build_args = c('--resave-data=best','--no-manual','--no-manual'))

#Check reverse dependencies of the package
devtools::revdep("TSPred")

#Build package source
devtools::build(args = c('--resave-data=best','--no-manual'))


#Release Package To CRAN.
#Run automated and manual tests, then post package to CRAN.
devtools::release(pkg = ".", check = FALSE, args = NULL)
