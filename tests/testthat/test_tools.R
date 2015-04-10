library(conduit)
context("test internal tools")

myNumeric <- 666
twoCharacters <- c("first", "second")
oneCharacter <- "only"

test_that("is_length1_char() picks only length 1 character vectors", {
    expect_false(is_length1_char(myNumeric))
    expect_false(is_length1_char(twoCharacters))
    expect_true(is_length1_char(oneCharacter))
})
