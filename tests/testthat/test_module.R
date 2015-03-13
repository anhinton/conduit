library(conduit)
context("Create a 'module' object")

testMod <- module("showY", platform = "R",
                  description = "displays the value of Y",
                  inputs = list(moduleInput(name = "y", type = "internal",
                      symbol = "y", format = "R character string")),
                  sources = list(moduleSource(value = "print(y)")),
                  outputs = list(moduleOutput(name = "z", type = "internal",
                      format = "R character string")))
                   
test_that("class(module) is \"module\"", {
    expect_match(class(testMod), "^module$")
})

test_that("module has correct slot names", {
    expect_match(names(testMod), "^name$", all = FALSE)
    expect_match(names(testMod), "^platform$", all = FALSE)
    expect_match(names(testMod), "^description$", all = FALSE)
    expect_match(names(testMod), "^inputs$", all = FALSE)
    expect_match(names(testMod), "^outputs$", all = FALSE)
    expect_match(names(testMod), "^sources$", all = FALSE)
})

test_that("name, platform, description are character vectors", {
    expect_true(is.character(testMod$name))
    expect_true(is.character(testMod$platform))
    expect_true(is.character(testMod$description))
})

test_that("name, platform, description are length 1", {
    expect_equal(length(testMod$name), 1)
    expect_equal(length(testMod$description), 1)
    expect_equal(length(testMod$platform), 1)
})

test_that("inputs, outputs, sources are lists", {
    expect_true(is.list(testMod$inputs))
    expect_true(is.list(testMod$outputs))
    expect_true(is.list(testMod$sources))
})

test_that("class of objects in inputs, outputs, sources correct", {
    expect_match(lapply(testMod$inputs, class), "moduleInput")
    expect_match(lapply(testMod$outputs, class), "moduleOutput")
    expect_match(lapply(testMod$sources, class), "moduleSource")
})
