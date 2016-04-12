library(conduit)
context("manipulate module source scripts")

test_that("internalInputScript() returns sensible script fragment", {
    ## fails for invalid arguments
    expect_error(internalInputScript(as.character("cookies")),
                 "symbol object required")

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
                                    language = "R"),
                 "moduleInput object required")

    ## produces script for internalVessel
    scriptInput1 <- prepareScriptInput(moduleInput = moduleInput1,
                                       language = "R")
    expect_is(scriptInput1, "character")
    expect_true(length(scriptInput1) > 0)

    ## produces NULL for fileVessel
    scriptInput2 <- prepareScriptInput(moduleInput = moduleInput2,
                                       language = "R")
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
                                     language = "R"),
                 "moduleOutput object required")

    ## produces script for internalVessel
    scriptOutput1 <- prepareScriptOutput(moduleOutput = moduleOutput1,
                                         language = "R")
    expect_is(scriptOutput1, "character")
    expect_true(length(scriptOutput1) > 0)

    ## produces NULL for fileVessel
    scriptOutput2 <- prepareScriptOutput(moduleOutput = moduleOutput2,
                                         language = "R")
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
        language = "R",
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
