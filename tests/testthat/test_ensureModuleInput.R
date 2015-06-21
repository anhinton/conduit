library(conduit)
context("ensure module inputs will be satisfied")

test_that("ensureInternalInput creates appropriate R script", {
    symbol1 <- "y"
    inputObject1 <- "~/datas/x.rds"
    language1 <- "R"
    script1 <- ensureInternalInput(inputObject = inputObject1,
                                   symbol = symbol1, language = language1)
    expect_identical(script1,
                     "y <- readRDS(\"~/datas/x.rds\")")
})

test_that("ensureFileInput has appropriate effects", {
    skip_on_cran()
    ref <- tempfile("ref")
    inputObject <- tempfile("inputObject")
    file.create(inputObject)
    script <- ensureFileInput(inputObject = inputObject, ref = ref)
    expect_true(file.exists(inputObject))
    expect_equal(file.size(ref), file.size(inputObject))
})

test_that("ensureModuleInput produces desired results", {
    input1 <- moduleInput("inp1", internalVessel("x"), ioFormat("R object"))
    outputObject1 <- tempfile()
    lang1 <- "R"
    script1 <- ensureModuleInput(input1, outputObject1, lang1)
    expect_false(is.null(script1))

    skip_on_cran()
    ref2 <- tempfile("ref")
    inputObject2 <- tempfile("inputObject")
    file.create(inputObject2)
    input2 <- moduleInput("inp2", fileVessel(ref2), ioFormat("file"))
    script2 <- ensureModuleInput(inputObject = inputObject2,
                                 input = input2, language = lang1)
    expect_null(script2)
})
