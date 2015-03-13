library(conduit)
context("create 'moduleInput' object")

internalInput <- moduleInput(name = "bigData", type = "internal",
                             symbol = "bigData", format = "R data frame")

test_that("class is \"moduleInput\"", {
    expect_match(class(internalInput), "^moduleInput$")
})
