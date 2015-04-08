library(conduit)
context("create 'fileIO' objects")

testFile <- fileIO("myFile.txt", "/home/hometime")
testFileNullPath <- fileIO("myFile.txt")

test_that("fileIO warns when 'ref' or 'path' are not length 1 chars", {
    expect_warning(fileIO(c("ref1", "ref1")),
                 "'ref' is not a length 1")
    expect_warning(fileIO(666), "'ref' is not a length 1")
    expect_warning(fileIO("ref", c("path1", "path2")),
                   "'path' is not a length 1")
    expect_warning(fileIO("ref", 666), "'path' is not a length 1")
})

test_that("'fileIO' slots are character vectors", {
    expect_true(is.character(testFile$ref))
    expect_true(is.character(testFile$path))
    expect_true(is.character(testFileNullPath$ref))
})

test_that("'fileIO' slots are length 1", {
    expect_equal(length(testFile$ref), 1)
    expect_equal(length(testFile$path), 1)
    expect_equal(length(testFileNullPath$ref), 1)
})

test_that("'path' slot is NULL when 'path' argument not given", {
    expect_true(is.null(testFileNullPath$path))
})

test_that("'fileIO' objects contain appropriate slots", {
    expect_match(names(testFile), "^ref$", all=F)
    expect_match(names(testFile), "^path$", all=F)
    expect_match(names(testFileNullPath), "^ref$", all=F)
    expect_match(names(testFileNullPath), "^path$", all=F)
})

test_that("'fileIO' object has appropriate class", {
    expect_match(class(testFile), "^fileIO$")
    expect_match(class(testFileNullPath), "^fileIO$")
})
