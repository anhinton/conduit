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
getName.default <- function(x) {
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

#' @describeIn getType
#'
#' @export
getType.default <- function(x) {
    x$type
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

#' Retrieve object format
#'
#' @param x object
#'
#' @export
getFormat <- function(x) {
    UseMethod("getFormat")
}

#' @describeIn getFormat
#'
#' @export
getFormat.default <- function(x) {
    x$format
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

#' @describeIn getLanguage
#'
#' @export
getLanguage.default <- function(x) {
    x$language
}

#' Retrieve object location
#'
#' @param x object
#'
#' @export
getLocation <- function(x) {
    UseMethod("getLocation")
}

#' Retrieve object ref
#'
#' @param x object
#'
#' @export
getRef <- function(x) {
    UseMethod("getRef")
}

#' @describeIn getRef
#'
#' @export
getRef.default <- function(x) {
    x$ref
}

#' Retrieve object result
#'
#' @param x object
#'
#' @export
getResult <- function(x) {
    UseMethod("getResult")
}

#' @describeIn getResult
#'
#' @export
getResult.default <- function(x) {
    x$result
}

#' Export OpenAPI objects
#'
#' Export OpenAPI objects for sharing.
#'
#' This function creates tarball bundles of OpenAPI objects to be
#' shared and loaded in other sessions.
#'
#' @param x object
#' @param targetDirectory file location
#'
#' @export
export <- function(x, targetDirectory = getwd()) {
    UseMethod("export")
}
