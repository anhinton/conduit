library(conduit)
context("Create input/output objects")

testInternal <- internalIO("mySym")

test_that("internalIO is named character with 'symbol' element", {
    expect_true(is.character(testInternal))
    expect_match(names(testInternal), "^symbol$")
})

test_that("internalIO has class 'internalIO'", {
    expect_match(class(testInternal), "^internalIO$")
})
