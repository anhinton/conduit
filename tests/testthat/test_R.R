library(conduit)
context("R language methods")

test_that("internalInputScript.RSymbol() returns sensible script fragment",
{
    RSymbol <- "obj"
    class(RSymbol) <- c("RSymbol", "symbol")
    script1 <- internalInputScript(symbol = RSymbol)
    expect_is(script1, "character")
    expect_true(grepl(RSymbol, script1))
})

