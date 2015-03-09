library(conduit)
context("Create a module object")

testMod <- module("showY", platform = "R",
                  description = "displays the value of Y",
                  inputs = list(moduleInput(name = "y", type = "internal",
                      format = "R character string")),
                  sources = list(moduleSource(value = "print(y)")),
                  outputs = list(moduleOutput(name = "z", type = "internal",
                      format = "R character string")))
                   
test_that("name, platform, description are character vectors", {
    expect_true(is.character(class(testMod$name)))
    expect_true(is.character(class(testMod$platform)))
    expect_true(is.character(class(testMod$description)))
})

test_that("name, platform, module are length 1", {
    expect_equal(length(testMod$name), 1)
    expect_equal(length(testMod$platform), 1)
    expect_equal(length(testMod$description), 1)
})

test_that("inputs are moduleInputs", {
    expect_match(class(testMod$inputs[[1]]), "moduleInput")
})
