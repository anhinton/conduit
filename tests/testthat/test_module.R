library(conduit)
context("Create a module object")

testMod <- module("showY", platform = "R",
                  description = "displays the value of Y",
                  inputs = list(moduleInput(name = "y", type = "internal",
                      format = "R character string")),
                  sources = list(moduleSource(value = "print(y)")),
                  outputs = list(moduleOutput(name = "z", type = "internal",
                      format = "R character string")))
                   
test_that("class(module) is \"module\"", {
    expect_match(class(testMod), "^module$")
})

test_that("module has correct slots", {
    expect_match(names(testMod), "^name$", all = FALSE)
    expect_match(names(testMod), "^platform$", all = FALSE)
    expect_match(names(testMod), "^description$", all = FALSE)
    expect_match(names(testMod), "^inputs$", all = FALSE)
    expect_match(names(testMod), "^outputs$", all = FALSE)
    expect_match(names(testMod), "^sources$", all = FALSE)
})

test_that("class of inputs, outputs, sources correct", {
    expect_match(lapply(testMod$inputs, class), "moduleInput")
    expect_match(lapply(testMod$outputs, class), "moduleOutput")
    expect_match(lapply(testMod$sources, class), "moduleSource")
})


