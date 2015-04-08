library(conduit)
context("test internal tools")

test_that("as_length1_char() works right", {
    expect_warning(as_length1_char(c("symbol1", "symbol2")),
                   "More than one value")
    expect_warning(as_length1_char(666),
                   "Non-character string")
    length1char <- as_length1_char("hello")
    expect_true(is.character(length1char))
    expect_equal(length(length1char), 1)
})
