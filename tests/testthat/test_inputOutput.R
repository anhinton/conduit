library(conduit)
context("Create input/output objects")

internalIO("mySym")

test_that("internalIO contains symbol element", {
    expect_match(names(internalIO("mySym")), "^symbol$", all=F)
})

