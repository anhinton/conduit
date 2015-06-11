library(conduit)
context("create 'ioFormat' objects")

text_format <- ioFormat(value="R data frame", type="text")

test_that("ioFormat fails for invalid arguments", {
    expect_error(ioFormat(type=character(2), value="CSV file"),
                 "'type' is not a length 1 character")
    expect_error(ioFormat(type="text", value=list("CSV file")),
                 "'value' is not a length 1 character vector")
    expect_error(ioFormat(type="DRAGON", value="CSV file"),
                 "invalid 'type' provided")
})

test_that("'ioFormat' slots are correct type and length", {
    expect_true(is_length1_char(text_format$type))
    expect_true(is_length1_char(text_format$value))
})

test_that("'ioFormat' contains appropriate slots", {
    expect_match(names(text_format), "^type$", all=F)
    expect_match(names(text_format), "^value$", all=F)
})

test_that("'ioFormat' object has class \"ioFormat\"", {
    expect_match(class(text_format), "^ioFormat$")
})
