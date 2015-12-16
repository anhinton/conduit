#' Extract components from object
#'
#' @param x object
#'
#' @export
getComponents <- function(x) {
    UseMethod("components")
}

#' Extract object name
#'
#' @param x object
#'
#' @export
getName <- function(x) {
    UseMethod("getName")
}

#' Extract pipes from object
#'
#' @param x object
#'
#' @export
getPipes <- function(x) {
    UseMethod("getPipes")
}

#' Extract object description
#'
#' @param x object
#'
#' @export
getDescription <- function(x) {
    UseMethod("getDescription")
}
