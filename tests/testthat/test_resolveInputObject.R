library(conduit)
context("ensure module inputs will be satisfied")

test_that(
    "absolute fileVessel refs are resolved",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput("great",
                        fileVessel("/home/ashley/Documents/comicsCount.txt"),
                        ioFormat("text file"))
        inputObject <- NULL
        expect_true(resolveInputObject(input, inputObject))
    })

test_that(
    "relative fileVessel refs are resolved",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <- moduleInput("good", fileVessel("test1"), ioFormat("text file"))
        inputObject <- normalizePath("~/Documents/SarahConnorHardware.txt")
        expect_true(resolveInputObject(input, inputObject))
    })

test_that(
    "internalVessel inputs are resolved",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <- moduleInput("fantastic",
                             internalVessel("y"),
                             ioFormat("R numeric vector"))
        filename <- tempfile("testRDS", fileext=".rds")
        saveRDS(1:10, filename)
        inputObject <- normalizePath(filename)
        expect_true(resolveInputObject(input, inputObject))
    })
