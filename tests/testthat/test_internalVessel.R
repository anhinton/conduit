library(conduit)
context("create 'internalVessel' objects")

testInternal <- internalVessel("important_data")

test_that("internalVessel stops when 'symbol' is not length 1 char", {
    expect_error(internalVessel(character(2)),
                 "'symbol' is not a length 1 character vector")
    expect_error(internalVessel(numeric(1)),
                 "'symbol' is not a length 1 character vector")
})

test_that("'internalVessel' slots are right type and length", {
    expect_true(is_length1_char(testInternal$symbol))
})

test_that("'internalVessel' objects contain appropriate slots", {
    expect_match(names(testInternal), "^symbol$", all=F)
})

test_that(paste0("'internalVessel' object has class",
                 "c(\"internalVessel\", \"vessel\")"), {
    expect_match(class(testInternal)[1], "^internalVessel$")
    expect_match(class(testInternal)[2], "^vessel$")
})
