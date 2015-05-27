library(conduit)
context("ensure module inputs will be satisfied")

test_that("ensureInternalInput creates appropriate R script", {
    symbol1 <- "y"
    resource1 <- "~/datas/x.rds"
    language1 <- "R"
    script1 <- ensureInternalInput(symbol1, resource1, language1)
    expect_identical(script1,
                     "y <- readRDS(\"~/datas/x.rds\")")
})

test_that("ensureFileInput has appropriate effects", {
    skip_on_cran()
    ref <- tempfile()
    resource <- tempfile()
    system2("touch", ref)
    expect_error(ensureFileInput(ref, resource),
                 "input file already exists")    

    ref <- tempfile()
    resource <- tempfile()
    script <- ensureFileInput(ref, resource)
    expect_true(Sys.readlink(ref) == resource)    
})

test_that("ensureModuleInput produces desired results", {
    input1 <- moduleInput("inp1", internalVessel("x"), ioFormat("R object"))
    resource1 <- tempfile()
    lang1 <- "R"
    script1 <- ensureModuleInput(input1, resource1, lang1)
    expect_false(is.null(script1))

    skip_on_cran()
    ref2 <- tempfile()
    resource2 <- tempfile()
    input2 <- moduleInput("inp2", fileVessel(ref2), ioFormat("file"))
    script2 <- ensureModuleInput(input2, resource2, lang1)
    expect_null(script2)
})
