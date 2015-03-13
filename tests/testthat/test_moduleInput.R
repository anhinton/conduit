library(conduit)
context("create 'moduleInput' object")

internalInput <- moduleInput(name = "bigData", type = "internal",
                             symbol = "bigData", format = "R data frame")
## fileInput <- moduleInput(name = "myFile", type = "file",
##                          ref = "myFile.txt", path = "myPath",
##                          format = "text file")
## urlInput <- moduleInput(name = "myUrl", type = "url",
##                         ref = "http://stat.auckland.ac.nz",
##                         format = "HTML file")

test_that("class is \"moduleInput\"", {
    expect_match(class(internalInput), "^moduleInput$")
})

test_that("moduleInput has expected slots", {
    expect_match(names(internalInput), "^name$", all=F)
    expect_match(names(internalInput), "^type$", all=F)
    expect_match(names(internalInput), "^attributes$", all=F)
    expect_match(names(internalInput), "^format$", all=F)
    expect_match(names(internalInput), "^formatType$", all=F)
})

test_that("attributes object class corresponds to 'type'", {
    expect_match(class(internalInput$attributes), "^internalIO$")
    ## expect_match(class(fileInput$attributes), "^fileIO$")
    ## expect_match(class(urlInput$attributes), "^urlIO$")
})

