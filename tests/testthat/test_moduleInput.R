library(conduit)
context("create 'moduleInput' objects")

myInput <- moduleInput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("moduleInput has correct class", {
    expect_match(class(myInput)[1], "moduleInput")
    expect_match(class(myInput)[2], "moduleIO")
})
