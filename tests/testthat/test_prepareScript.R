library(conduit)
context("prepare module script for execution")

test_that(
    "R script file is created",
    {
        skip_on_cran()

        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        
        ## create RDS file for input
        input_object <- 1:10
        filename <- tempfile()
        saveRDS(input_object, filename)
        inputObjects <- list(a = filename)

        ## create module
        module <-
            module(
                "testy",
                "R",
                inputs =
                    list(moduleInput(
                        "a",
                        internalVessel("onetoten"),
                        ioFormat("R numeric vector"))),
                sources =
                    list(moduleSource(
                        scriptVessel(
                            "top <- head(onetoten)"))),
                outputs =
                    list(moduleOutput(
                        "b",
                        internalVessel("top"),
                        ioFormat("R character vector"))))

        ## test script creation
        expect_match(prepareScript(module, inputObjects),
                     "script.R")
    })

