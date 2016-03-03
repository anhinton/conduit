### R language support

#' @describeIn internalInputScript prepare internal input script for R
#' language
internalInputScript.RSymbol <- function(symbol) {
    paste0(symbol, " <- readRDS(\"",
           paste0(symbol, internalExtension("R")), "\")")
}

#' @describeIn internalOutputScript create script to create internal
#' output for language = "R"
internalOutputScript.RSymbol <- function (symbol) {
    paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
}

command.RScript <- function(script) {
    list(command = "Rscript",
         args = script)
}
