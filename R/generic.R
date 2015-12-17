#' Extract components from object
#'
#' @param x object
getComponents <- function(x) {
    UseMethod("getComponents")
}

#' Extract object name
#'
#' @param x object
getName <- function(x) {
    UseMethod("getName")
}

#' Extract pipes from object
#'
#' @param x object
getPipes <- function(x) {
    UseMethod("getPipes")
}

#' Extract object description
#'
#' @param x object
getDescription <- function(x) {
    UseMethod("getDescription")
}
