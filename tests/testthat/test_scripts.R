library(conduit)
context("manipulate module source scripts")

test_that("internalInputScript() returns sensible script fragment", {
    ## fails for invalid arguments
    expect_error(internalInputScript(as.character("cookies")),
                 "symbol object required")

    ## language-specific testing found in test_LANGUAGE.R files
})
