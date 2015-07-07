library(conduit)
context("execute a module script file")

test_that(
    "executeScript.R works",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module1 <-
            loadModule("module1",
                       system.file("extdata", "test_pipeline",
                                   "module1.xml",
                                   package = "conduit"))
        inputObjects <- NULL
        script <- prepareScript(module1, inputObjects)
        class(script) <- module1$language
        expect_equal(executeScript(script), 0)
    })

test_that(
    "executeScript.python works",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module2 <- module(
            "module2",
            "python",
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
        class(script) <- module2$language
        expect_equal(executeScript(script), 0)
    })

test_that(
    "executeScript.shell works",
    {
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module3 <- module(
            "module3",
            "shell",
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
        class(script) <- module3$language
        expect_equal(executeScript(script), 0)
    })
