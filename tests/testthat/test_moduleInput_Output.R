library(conduit)
context("create 'moduleInput' and 'moduleOutput' objects")

myInput <- moduleInput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("moduleInput has correct class", {
    expect_match(class(myInput)[1], "moduleInput")
    expect_match(class(myInput)[2], "moduleIO")
})

myOutput <- moduleOutput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("moduleOutput has correct class", {
    expect_match(class(myOutput)[1], "moduleOutput")
    expect_match(class(myOutput)[2], "moduleIO")
})
