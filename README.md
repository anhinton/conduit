conduit
=======

`conduit` is a prototype R-based glue system for openapi.

Find out more about the openapi project at the website for a talk given
by Paul Murrell of the University of Auckland: 
<https://www.stat.auckland.ac.nz/~paul/Talks/OpenAPI2014/>.

Install conduit
---------------

`conduit` is an R package built using R version 3.1 on a 64-bit Linux
machine. It requires the following R packages: `XML`, `graph`, `RBGL`,
`RCurl`, and `tools`. The `devtools` package is required to install
`conduit` directly from github.

Source code for conduit is available at
<https://github.com/anhinton/conduit> for those who would like to
build and install the package manually.

To install conduit using `devtools`:

```
install.packages("devtools")
devtools::install_github("anhinton/conduit")
```
