### Functions for loading, running and creating components

#' Create a component object
component <- function(name, ref=NULL, path=NULL, type, value=list()) {
    component <- list(name=name, ref=ref, path=path, type=type, value=value)
    class(component) <- "component"
}
