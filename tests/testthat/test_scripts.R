library(conduit)
context("manipulate module source scripts")

test_that("internalInputScript() returns sensible script fragment", {
    ## fails for invalid arguments
    expect_error(internalInputScript(as.character("cookies")),
                 "symbol object required")

    ## language-specific testing found in test_LANGUAGE.R files
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
                 "moduleInput oject required")

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
