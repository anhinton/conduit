conduit
=======

`conduit` is a prototype R-based glue system for OpenAPI.

You can find out more about the OpenAPI project in the technical
report, Introducing OpenAPI, at
<https://www.stat.auckland.ac.nz/~paul/Reports/openapi/introducingOpenapi/introducingOpenapi.html>.

Install conduit
---------------

`conduit` is an R package built using R version 3.2.3 on a 64-bit
Linux machine. It requires the following R packages: `XML`, `graph`,
`RBGL`, and `RCurl`. The `devtools` package is required to install
`conduit` directly from github.

Source code for conduit is available at
<https://github.com/anhinton/conduit> for those who would like to
build and install the package manually.

To install conduit using `devtools`:

```
install.packages("devtools")
devtools::install_github("anhinton/conduit")
```
