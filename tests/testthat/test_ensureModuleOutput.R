library(conduit)
context("ensure module outputs are created")

test_that("ensureInternalOutput creates appropriate R script", {
    skip_on_cran()
    symbol1 <- "y"
    language1 <- "R"
    script1 <- ensureInternalOutput(symbol1, language1)
    expect_identical(script1,
                     "saveRDS(y, file = \"y.rds\")")
})

test_that("ensureModuleOutput produces desired results", {
    output1 <- moduleOutput("out1", internalVessel("x"), ioFormat("R object"))
    lang1 <- "R"
    script1 <- ensureModuleOutput(output1, lang1)
    expect_false(is.null(script1))

    output2 <- moduleOutput("out2", fileVessel("file.csv"),
                            ioFormat("CSV file"))
    lang2 <- "python"
    script2 <- ensureModuleOutput(output2, lang2)
    expect_null(script2)
})
