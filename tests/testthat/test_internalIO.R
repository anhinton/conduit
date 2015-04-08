library(conduit)
context("create input/output objects")

testInternal <- internalIO("mySym")

test_that("internalIO warns when 'ref' or 'path' are not length 1 chars", {
    expect_warning(internalIO(c("symbol1", "symbol2")),
                 "'ref' is not a length 1")
    expect_warning(internalIO(666), "'path' is not a length 1")
})

test_that("set attributes are character vectors", {
    expect_true(is.character(testInternal$symbol))
    expect_true(is.character(testFile$ref))
    expect_true(is.character(testFile$path))
    expect_true(is.character(testFileNullPath$ref))
})

test_that("set attributes are length 1", {
    expect_equal(length(testInternal$symbol), 1)
    expect_equal(length(testFile$ref), 1)
    expect_equal(length(testFile$path), 1)
    expect_equal(length(testFileNullPath$ref), 1)
})

test_that("unset attributes are NULL", {
    expect_true(is.null(testFileNullPath$path))
})

test_that("IO objects contain appropriate elements", {
    expect_match(names(testInternal), "^symbol$")
    expect_match(names(testFile), "^ref$", all=F)
    expect_match(names(testFile), "^path$", all=F)
    expect_match(names(testFileNullPath), "^ref$", all=F)
    expect_match(names(testFileNullPath), "^path$", all=F)
})

test_that("objects have appropriate class", {
    expect_match(class(testInternal), "^internalIO$")
    expect_match(class(testFile), "^fileIO$")
    expect_match(class(testFileNullPath), "^fileIO$")
})
