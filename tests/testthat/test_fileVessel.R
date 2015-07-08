library(conduit)
context("create 'fileVessel' objects")

testFile <- fileVessel("myFile.txt")

test_that("fileVessel stops when 'ref' is not length 1 char", {
    expect_error(fileVessel(c("ref1", "ref1")),
                 "'ref' is not a length 1 character vector")
    expect_error(fileVessel(666),
                 "'ref' is not a length 1 character vector")
})

test_that("'fileVector' slots are right type and length", {
    expect_true(is_length1_char(testFile$ref))
})

test_that("'fileVessel' objects contain appropriate slots", {
    expect_match(names(testFile), "^ref$", all=F)
})

test_that("'fileVessel' object has class c(\"fileVessel\", \"vessel\")", {
    expect_match(class(testFile)[1], "^fileVessel$")
    expect_match(class(testFile)[2], "^vessel$")
})
