library(conduit)
context("run modules")

targ = tempdir()

createGraph <- loadModule(
    "createGraph",
    system.file("extdata", "simpleGraph", "createGraph.xml",
                package = "conduit"))
layoutGraph <- loadModule(
    "layoutGraph",
    system.file("extdata", "simpleGraph", "layoutGraph.xml",
                package = "conduit"))
    

test_that(
    "runModule() fails when not given a 'module' object",
    {
        fakemodule <- list(name ="fakemodule",
                           language = "R",
                           sources = list(
                               moduleSource(scriptVessel("1:10"))))
        expect_error(runModule(fakemodule, targetDirectory = targ),
                     "'module' is not a ")
    })

test_that(
    "runModule() fails when targetDirectory doesn't exist",
    {
        badTarget <- paste0(tempfile(), tempfile())
        expect_error(runModule(createGraph, targetDirectory = badTarget),
                     "no such target")
    })

test_that(
    "runModule() fails when input cannot be resolved",
    {
        badInput <- paste0(tempfile(), tempfile())
        module <- module(
            name = "fails",
            language = "R",
            inputs = list(
                moduleInput(
                    name = "nope",
                    vessel = fileVessel(badInput),
                    format = ioFormat("file"))),
            sources = list(
                moduleSource(vessel = scriptVessel(
                    paste0("exists <- file.exists(\"", badInput, "\")")))),
            outputs = list(
                moduleOutput(
                    name = "truth",
                    vessel = internalVessel("exists"),
                    format = ioFormat("R logical"))))
        expect_error(runModule(module, targetDirectory = targ),
                     "Input ")
    })

test_that(
    "runModule() succeeds for module with fileVessel input with absolute ref",
    {
        inputFile <- system.file("extdata", "simpleGraph", "createGraph.xml",
                                 package = "conduit")
        moduleName <- "absomod"
        language = "R"
        outputName <- "lines"
        outputType <- "internalVessel"
        outputObject <-
            file.path(targ, "modules", moduleName,
                      paste0(outputName, internalExtension(language)))
        absomod <- module(
            name = moduleName,
            language = language,
            inputs = list(
                moduleInput(
                    name = "file",
                    vessel = fileVessel(inputFile),
                    format = ioFormat("XML file"))),
            sources = list(
                moduleSource(
                    scriptVessel(
                        paste0(outputName, " <- readLines(\"", inputFile,
                               "\")")))),
            outputs = list(
                moduleOutput(
                    name = outputName,
                    vessel = internalVessel(outputName),
                    format = ioFormat("R character vector"))))
        output <- runModule(absomod, targetDirectory = targ)
        expect_match(output[[1]]$name, outputName)
        expect_match(output[[1]]$type, outputType)
        expect_true(file.exists(outputObject))
    })

test_that(
    "runModule() works",
    {
        skip_on_cran()
        ## run the createGraph module
        output1 <- createGraph$outputs[[1]]
        result1 <- runModule(createGraph, targetDirectory = targ)
        expect_match(result1[[1]]$name, output1$name)

        ## run the layoutGraph module, providing the output from
        ## createGraph as input
        inputObjects <- list(result1[[1]]$object)
        names(inputObjects) <- layoutGraph$inputs[[1]]$name
        output2 <- layoutGraph$outputs[[1]]
        result2 <- runModule(layoutGraph,
                             inputObjects = inputObjects,
                             targetDirectory = targ)
        expect_match(result2[[1]]$name, output2$name)
    })
