#!/usr/bin/Rscript

## CONDUIT: checking language version
version <- list(minVersion = "",
                maxVersion = "",
                version = "")
failMin <- getRversion() < numeric_version(version$minVersion,
                                           strict = FALSE)
failMax <- getRversion() > numeric_version(version$maxVersion,
                                           strict = FALSE)
failExact <- getRversion() != numeric_version(version$version,
                                              strict = FALSE)
## write results to .languageVersion. 1 for fail test, 0 for pass
languageVersion <- c(as.character(getRversion()),
                     as.integer(failMin), as.integer(failMax),
                     as.integer(failExact))
languageVersion[is.na(languageVersion)] = 0
con <- file(description = ".languageVersion", encoding = "UTF-8")
writeLines(text = languageVersion, con = con)
close(con)

