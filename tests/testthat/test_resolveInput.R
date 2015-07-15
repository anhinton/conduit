library(conduit)
context("ensure module inputs will be satisfied")

test_that(
    "absolute fileVessel refs are resolved",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput(
                "great",
                fileVessel(
                    system.file("extdata", "simpleGraph", "createGraph.xml",
                                package = "conduit")),
                ioFormat("text file"))
        inputObjects <- NULL
        expect_true(resolveInput(input, inputObjects))
    })

test_that(
    "relative fileVessel refs are resolved",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput(
                "good",
                fileVessel("test2"),               
                ioFormat("text file"))
        inputObjects <-
            list(good = system.file(
                     "extdata", "simpleGraph", "createGraph.xml",
                     package = "conduit"))
        expect_true(resolveInput(input, inputObjects, host = NULL))
    })

test_that(
    "internalVessel inputs are resolved",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <- moduleInput("fantastic",
                             internalVessel("y"),
                             ioFormat("R numeric vector"))
        filename <- tempfile("testRDS", fileext=".rds")
        saveRDS(1:10, filename)
        inputObjects <- list(fantastic = filename)
        expect_true(resolveInput(input, inputObjects, host = NULL))
    })
