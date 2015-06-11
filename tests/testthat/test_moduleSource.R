library(conduit)
context("create 'moduleSource' objects")

test_that("'moduleSource' fails for invalid arguments", {
    expect_error(moduleSource(vessel = c("x <- 1:10", "print(x)")),
                 "'vessel' is not a vessel")
    expect_error(moduleSource(vessel= internalVessel(symbol = "x")),
                 "'internalVessel' vessels not defined")
    expect_error(moduleSource(vessel = fileVessel("source.R"),
                              order = "16"),
                 "'order' is not numeric")
    expect_error(moduleSource(vessel = fileVessel("source.R"),
                              order = c(1,2,3)),
                 "more than one value")
})

src1 <- moduleSource(vessel = scriptVessel(value = "x <- 11"),
                     order = -1)
src2 <- moduleSource(vessel = fileVessel(ref = "filename.R"))

test_that("'moduleSource' slots are correct type and length", {
    expect_true(is.numeric(src1$order))
    expect_equal(length(src1$order), 1)
    expect_true(is.null(src2$order))
    expect_match(class(src1$vessel), "vessel", all=F)
    expect_match(class(src2$vessel), "vessel", all=F)
})

test_that("'moduleSource' contains appropriate slots", {
    expect_match(names(src1), "^vessel$", all=F)
    expect_match(names(src1), "^order$", all=F)
    expect_match(names(src2), "^vessel$", all=F)
    expect_match(names(src2), "^order$", all=F)
})

test_that("'moduleSource' object has class \"moduleSource\"", {
    expect_match(class(src1), "^moduleSource$")
    expect_match(class(src2), "^moduleSource$")
})
