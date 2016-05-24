### R language support

#' @describeIn prepareScriptInit
#'
#' Init script for R langage
prepareScriptInit.RModuleLanguage <- function(moduleLanguage) {
    initScript <- c(
        "#!/usr/bin/Rscript",
        "",
        "## CONDUIT: checking language version",
        paste0("version <- list(minVersion = \"", moduleLanguage$minVersion,
               "\","), 
        paste0("                maxVersion = \"", moduleLanguage$maxVersion,
               "\","),
        paste0("                version = \"", moduleLanguage$version,
               "\")"),
        "failMin <- getRversion() < numeric_version(version$minVersion,", 
        "                                           strict = FALSE)", 
        "failMax <- getRversion() > numeric_version(version$maxVersion,", 
        "                                           strict = FALSE)", 
        "failExact <- getRversion() != numeric_version(version$version,", 
        "                                              strict = FALSE)", 
        "## write results to .languageVersion. 1 for fail test, 0 for pass",
        "languageVersion <- c(as.character(getRversion()),",
        "                     as.integer(failMin), as.integer(failMax),", 
        "                     as.integer(failExact))",
        "languageVersion[is.na(languageVersion)] = 0", 
        "con <- file(description = \".languageVersion\", encoding = \"UTF-8\")", 
        "writeLines(text = languageVersion, con = con)",
        "close(con)",
        ""
    )
    initScript
}

#' @describeIn internalInputScript prepare internal input script for R
#'     language
#'
#' @export
internalInputScript.RSymbol <- function(symbol) {
    paste0(symbol, " <- readRDS(\"",
           paste0(symbol, internalExtension(moduleLanguage("R"))), "\")")
}

#' @describeIn internalOutputScript create script to create internal
#'     output for language = "R"
#'
#' @export
internalOutputScript.RSymbol <- function (symbol) {
    paste0("saveRDS(", symbol, ", file = \"", symbol, ".rds\")")
}

#' @describeIn command generate a command to execute an R language
#'     script
#'
#' @export
command.RScript <- function(script) {
    command <- list(command = "Rscript",
                    args = script)
    class(command) <- "command"
    command
}
