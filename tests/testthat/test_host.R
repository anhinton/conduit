library(conduit)
context("Check remote host functions work")

file1 <- tempfile()
system2("touch", file1)
host <- "conduit@127.0.0.1:2222"
host1 <- parseModuleHost(host)
host2 <- parseModuleHost("notreal@nosuch.server:22")

test_module <- module(
    name = "test_module",
    language = "R",
    host = host,
    sources = list(
        moduleSource(
            scriptVessel("x <- 1:10"))),
    outputs = list(
        moduleOutput("x", internalVessel("x"), ioFormat("R numeric vector"))))

## test createHostDirectory()
test_that(
    "createHostDirectory() works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        result1 <- createHostDirectory(host1)
        expect_equal(result1, 0)
        
        result2 <- createHostDirectory(host2)
        expect_more_than(result2, 0)
    })

## test fileToHost()
test_that(
    "fileToHost() works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        result1 <- fileToHost(file1, host1)
        expect_equal(result1, 0)

        result2 <- fileToHost(file1, host2)
        expect_more_than(result2, 0)
    })

## test resolveInput()
test_that(
    "resolveInput() works for remote module hosts",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        moduleInput1 <- moduleInput("x", internalVessel("x"),
                                    ioFormat("R data frame"))
        inputObjects <- list(x = file1)
        expect_true(resolveInput(moduleInput1, inputObjects, host1))
        expect_error(resolveInput(moduleInput1, inputObjects, host2),
                     "Unable to copy")
        moduleInput2 <- moduleInput("x", fileVessel("x"),
                                     ioFormat("text file"))
        inputObjects <- list(x = file1)
        expect_true(resolveInput(moduleInput2, inputObjects, host1))
        expect_error(resolveInput(moduleInput2, inputObjects, host2),
                     "Unable to copy")
    })

## test executeScript() on remote host
test_that(
    "executeScript.R works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module1 <-
            loadModule("module1",
                       system.file("extdata", "test_pipeline",
                                   "module1.xml",
                                   package = "conduit"))
        module1$host <- host
        inputObjects <- NULL
        script <- prepareScript(module1, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.python works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module2 <- module(
            name = "module2",
            language = "python",
            host = host,
            sources = list(
                moduleSource(
                    scriptVessel("x = [1, 2, 3, 5, 10]"))),
            outputs = list(
                moduleOutput(
                    "x",
                    internalVessel("x"),
                    ioFormat("python list"))))
        inputObjects <- NULL
        script <- prepareScript(module2, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.shell works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module3 <- module(
            name = "module3",
            language = "shell",
            host = host,
            sources = list(
                moduleSource(
                    scriptVessel("x=\"lemon duds\"]"))),
            outputs = list(
                moduleOutput(
                    "x",
                    internalVessel("x"),
                    ioFormat("shell environment variable"))))
        inputObjects <- NULL
        script <- prepareScript(module3, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

## test fetchFromHost
test_that(
    "fetchFromHost() works",
    {
        skip("requires conduit host at conduit@127.0.0.1:2222")
        skip_on_cran()
        mypath <- tempfile()
        dir.create(mypath)
        oldwd <- setwd(mypath)
        on.exit(oldwd)
        fileToHost(file1, host1)
        expect_equal(fetchFromHost(basename(file1), host1), 0)

        expect_more_than(fetchFromHost(basename(file1), host2), 0)
    })

test_that(
    "runModule() works",
    {
        targ = tempdir()
        createGraph <- loadModule(
            "createGraph",
            system.file("extdata", "simpleGraph", "createGraph.xml",
                        package = "conduit"))
        layoutGraph <- loadModule(
            "layoutGraph",
            system.file("extdata", "simpleGraph", "layoutGraph.xml",
                        package = "conduit"))
        createGraph$host <- layoutGraph$host
    

        skip("requires conduit host at conduit@127.0.0.1:2222")
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

