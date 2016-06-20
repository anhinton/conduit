library(conduit)
context("manipulate module source scripts")

test_that("internalInputScript() returns sensible script fragment", {
    ## fails for invalid arguments
    expect_error(internalInputScript(as.character("cookies")),
                 "symbol object required")

    ## language-specific testing found in test_LANGUAGE.R files
    ## TODO(anhinton): write language-specific tests
})

test_that("prepareScriptInit() returns script fragment", {
    mlang1 <- moduleLanguage(language = "R")

    ## fail for invalid input
    expect_error(prepareScriptInit(moduleLanguage = unclass(mlang1)),
                 "moduleLanguage object required")

    ## language-specific testing found in test_LANGUAGE.R files
    ## TODO(anhinton): write language-specific tests    
})

test_that("prepareScriptInput() returns script fragment", {
    moduleInput1 <- moduleInput(name = "x",
                                vessel = internalVessel("X"),
                                format = ioFormat("language object"))
    moduleInput2 <- moduleInput(name = "y",
                                vessel = fileVessel("y.csv"),
                                format = ioFormat("CSV file"))
    
    ## fails for invalid arguments
    expect_error(prepareScriptInput(moduleInput = unclass(moduleInput1),
                                    moduleLanguage = moduleLanguage("R")),
                 "moduleInput object required")

    ## produces script for internalVessel
    scriptInput1 <- prepareScriptInput(moduleInput = moduleInput1,
                                       moduleLanguage = moduleLanguage("R"))
    expect_is(scriptInput1, "character")
    expect_true(length(scriptInput1) > 0)

    ## produces NULL for fileVessel
    scriptInput2 <- prepareScriptInput(moduleInput = moduleInput2,
                                       moduleLanguage = moduleLanguage("R"))
    expect_null(scriptInput2)
})

test_that("internalOutputScript() returns sensible script fragment", {
    ## fails for invalid arguments
    expect_error(internalOutputScript(as.character("cookies")),
                 "symbol object required")

    ## language-specific testing found in test_LANGUAGE.R files
    ## TODO(anhinton): write language-specific tests
})

test_that("prepareScriptOutput() returns script fragment", {
    moduleOutput1 <- moduleOutput(name = "x",
                                  vessel = internalVessel("X"),
                                  format = ioFormat("language object"))
    moduleOutput2 <- moduleOutput(name = "y",
                                  vessel = fileVessel("y.csv"),
                                  format = ioFormat("CSV file"))
    
    ## fails for invalid arguments
    expect_error(prepareScriptOutput(moduleOutput = unclass(moduleOutput1),
                                     moduleLanguage = moduleLanguage("R")),
                 "moduleOutput object required")

    ## produces script for internalVessel
    scriptOutput1 <- prepareScriptOutput(moduleOutput = moduleOutput1,
                                         moduleLanguage = moduleLanguage("R"))
    expect_is(scriptOutput1, "character")
    expect_true(length(scriptOutput1) > 0)

    ## produces NULL for fileVessel
    scriptOutput2 <- prepareScriptOutput(moduleOutput = moduleOutput2,
                                         moduleLanguage = moduleLanguage("R"))
    expect_null(scriptOutput2)
})

## test prepareScript
test_that("prepareScript() returns script file", {
    testDir <- tempfile("prepareScript")
    if (!dir.exists(testDir))
        dir.create(testDir)
    oldwd <- setwd(testDir)
    on.exit(setwd(oldwd))
    module1 <- module(
        name = "mod1",
        language = moduleLanguage("R"),
        inputs = list(
            moduleInput(name = "X",
                        vessel = internalVessel("X"),
                        format = ioFormat("language object"))),
        sources = list(
            moduleSource(
                scriptVessel("Y <- X * 2"))),
        outputs = list(
            moduleOutput(name = "Y",
                         vessel = internalVessel("Y"),
                         format = ioFormat("language object"))))

    ## fail for invalid input
    expect_error(prepareScript(module = unclass(module1)),
                 "module object required")

    ## creates script file    
    script1 <- prepareScript(module = module1)
    expect_true(file.exists(script1))
    expect_is(script1, "script")
    expect_is(script1, c(paste0(getLanguage(module1), "Script")))
})

test_that("command() returns appropriately", {
    ## fail for invalid arguments
    expect_error(command(tempfile()),
                 "script object required")

    ## see test_LANGUAGE.R for language-specific test
})

test_that("executeCommand() returns appropriately", {
    vagrantfile <- tempfile("vagrantfile")
    system2("touch", vagrantfile)
    moduleHost1 <- vagrantHost(vagrantfile = vagrantfile)
    outputLocation1 <- tempdir()
    class(outputLocation1) <- c("vagrantHostOutputLocation",
                                "outputLocation")
    command1 <- list(command = "echo", args = "$PWD")
    class(command1) <- "command"

    ## fail for invalid arguments
    expect_error(executeCommand(moduleHost = unclass(moduleHost1),
                                outputLocation = outputLocation1,
                                command = command1),
                 "moduleHost object required")
    expect_error(executeCommand(moduleHost = moduleHost1,
                                outputLocation = unclass(outputLocation1),
                                command = command1),
                 "outputLocation object required")
    expect_error(executeCommand(moduleHost = moduleHost1,
                                outputLocation = outputLocation1,
                                command = unclass(command1)),
                 "command object required")

    ## returns character vector with no status attribute
    execResult <- executeCommand(moduleHost = NULL, outputLocation = NULL,
                                 command = command1)
    expect_is(execResult, "character")
    expect_true(is.null(attr(execResult, "status")))
})

test_that("executeScript() returns correctly", {
    outDir <- tempfile("executeScript")
    if (!dir.exists(outDir))
        dir.create(outDir)
    oldwd <- setwd(outDir)
    on.exit(setwd(oldwd))
    mod1 <- loadModule(
        name = "mod1",
        ref = system.file("extdata", "test_pipeline", "module1.xml",
                          package = "conduit"))
    script1 <- prepareScript(mod1)
    vagrantfile1 <- tempfile("vagrantfile")
    system2("touch", vagrantfile1)
    moduleHost1 <- vagrantHost(vagrantfile = vagrantfile1)

    ## fail for invalid arguments
    expect_error(executeScript(script = unclass(script1),
                               moduleHost = NULL, outputLocation = NULL),
                 "script object required")
    expect_error(executeScript(script = script1,
                               moduleHost = unclass(moduleHost1),
                               outputLocation = NULL),
                 "moduleHost object required")
    expect_error(executeScript(script = script1,
                               moduleHost = moduleHost1,
                               outputLocation = c("/home", "/tmp")),
                 "outputLocation object required")
    
    ## valid return for no moduleHost
    execResult <- executeScript(script = script1, moduleHost = NULL,
                      outputLocation = NULL)
    expect_is(execResult, "character")
    expect_true(is.null(attr(execResult, "status")))
})

## test executeScript.LANGUAGE
## TODO(anhinton): move these test to test_LANGUAGE.R
test_that("can execute R scripts", {
    oldwd <- setwd(tempdir())
    on.exit(setwd(oldwd))
    module1 <-
        loadModule("module1",
                   system.file("extdata", "test_pipeline",
                               "module1.xml",
                               package = "conduit"))
    inputObjects <- NULL
    script <- prepareScript(module1)
    execResult <- executeScript(script = script, moduleHost = NULL,
                                outputLocation = NULL)
    expect_is(execResult, "character")
    expect_true(is.null(attr(execResult, "status")))
})

test_that("can execute python scripts", {
    oldwd <- setwd(tempdir())
    on.exit(setwd(oldwd))
    module2 <- module(
        "module2",
        moduleLanguage("python"),
        sources = list(
            moduleSource(
                scriptVessel("x = [1, 2, 3, 5, 10]"))),
        outputs = list(
            moduleOutput(
                "x",
                internalVessel("x"),
                ioFormat("python list"))))
    inputObjects <- NULL
    script <- prepareScript(module2)
    execResult <- executeScript(script = script, moduleHost = NULL,
                                outputLocation = NULL)
    expect_is(execResult, "character")
    expect_true(is.null(attr(execResult, "status")))
})

test_that("can execute bash scripts", {
    oldwd <- setwd(tempdir())
    on.exit(setwd(oldwd))
    module3 <- module(
        "module3",
        moduleLanguage("bash"),
        sources = list(
            moduleSource(
                scriptVessel("x=\"lemon duds\"]"))),
        outputs = list(
            moduleOutput(
                "x",
                internalVessel("x"),
                ioFormat("bash environment variable"))))
    inputObjects <- NULL
    script <- prepareScript(module3)
    execResult <- executeScript(script = script, moduleHost = NULL,
                                 outputLocation = NULL)
    expect_is(execResult, "character")
    expect_true(is.null(attr(execResult, "status")))
})
