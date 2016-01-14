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

#' Retrieve object type
#'
#' @param x object
getType <- function(x) {
    UseMethod("getType")
}

#' Retrieve object vessel
#'
#' @param x object
getVessel <- function(x) {
    UseMethod("getVessel")
}

#' Retrieve object value
#'
#' @param x object
getValue <- function(x) {
    UseMethod("getValue")
}

#' Retrieve object language
#'
#' @param x object
getLanguage <- function(x) {
    UseMethod("getLanguage")
}
