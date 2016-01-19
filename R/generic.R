#' Extract components from object
#'
#' @param x object
#'
#' @export
getComponents <- function(x) {
    UseMethod("getComponents")
}

#' Extract object name
#'
#' @param x object
#'
#' @export
getName <- function(x) {
    UseMethod("getName")
}

#' @describeIn getName
#'
#' @export
getName <- function(x) {
    x$name
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

#' Retrieve object type
#'
#' @param x object
#'
#' @export
getType <- function(x) {
    UseMethod("getType")
}

#' Retrieve object vessel
#'
#' @param x object
#'
#' @export
getVessel <- function(x) {
    UseMethod("getVessel")
}

#' @describeIn getVessel
#'
#' @export
getVessel.default <- function(x) {
    x$vessel
}

#' Retrieve object value
#'
#' @param x object
#'
#' @export
getValue <- function(x) {
    UseMethod("getValue")
}

#' Retrieve object language
#'
#' @param x object
#'
#' @export
getLanguage <- function(x) {
    UseMethod("getLanguage")
}

#' Retrieve object location
#'
#' @param x object
#'
#' @export
getLocation <- function(x) {
    UseMethod("getLocation")
}
