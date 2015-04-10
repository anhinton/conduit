library(conduit)
context("create 'fileVessel' objects")

testFile <- fileVessel("myFile.txt", "/home/hometime")
testFileNullPath <- fileVessel("myFile.txt")

test_that("fileVessel stops when 'ref' or 'path' are not length 1 chars", {
    expect_error(fileVessel(c("ref1", "ref1")),
                 "'ref' is not a length 1 character vector")
    expect_error(fileVessel(666),
                 "'ref' is not a length 1 character vector")
    expect_error(fileVessel("ref", c("path1", "path2")),
                 "'path' is not a length 1 character vector")
    expect_error(fileVessel("ref", 666),
                 "'path' is not a length 1 character vector")
})

test_that("'fileVector' slots are right type and length", {
    expect_true(is_length1_char(testFile$ref))
    expect_true(is_length1_char(testFile$path))
    expect_true(is_length1_char(testFileNullPath$ref))
    expect_true(is.null(testFileNullPath$path))
})

test_that("'fileVessel' objects contain appropriate slots", {
    expect_match(names(testFile), "^ref$", all=F)
    expect_match(names(testFile), "^path$", all=F)
    expect_match(names(testFileNullPath), "^ref$", all=F)
    expect_match(names(testFileNullPath), "^path$", all=F)
})

test_that("'fileVessel' object has class c(\"fileVessel\", \"vessel\")", {
    expect_match(class(testFile)[1], "^fileVessel$")
    expect_match(class(testFile)[2], "^vessel$")
    expect_match(class(testFileNullPath)[1], "^fileVessel$")
    expect_match(class(testFileNullPath)[2], "^vessel$")
})
