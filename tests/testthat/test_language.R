library(conduit)
context("test language tools")

test_that("execLanguage() returns correctly", {
    el1 <- execLanguage("R")
    expect_match(el1, "R")

    el2 <- execLanguage(language = "python", minVersion = "3.4")
    expect_match(el2, "python3")

    el3 <- execLanguage(language = "python", maxVersion = "2.8")
    expect_match(el3, "python2")

    el4 <- execLanguage(language = "python", version = "2.7.11+")
    expect_match(el4, "python2")

    el5 <- execLanguage("python")
    expect_match(el5, "python3")
})

test_that("internalExtension() returns correctly", {
    ml1 <- moduleLanguage(language = "perl")

    ## fail for invalid argumen
    expect_error(internalExtension(unclass(ml1)),
                 "moduleLanguage object required")

    ## fail for unsupported language
    expect_error(internalExtension(ml1),
                 "language")
})

test_that("scriptExtension() returns correctly", {
    ml1 <- moduleLanguage(language = "perl")

    ## fail for invalid argumen
    expect_error(scriptExtension(unclass(ml1)),
                 "moduleLanguage object required")

    ## fail for unsupported language
    expect_error(scriptExtension(ml1),
                 "language")
})

