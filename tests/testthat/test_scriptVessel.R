library(conduit)
context("create 'scriptVessel' objects")

testScript1 <- scriptVessel("important_data")
testScript2 <- scriptVessel(c("take a little code",
                              "add a little more",
                              "cross your fingers"))

test_that("scriptVessel stops when 'value' is not character vector", {
    expect_error(scriptVessel(numeric(2)),
                 "'value' is not a character vector")
    expect_error(scriptVessel(vector("list",1)),
                 "'value' is not a character vector")
})

test_that("'scriptVessel' slots are right type", {
    expect_true(is.character(testScript1$value))
    expect_true(is.character(testScript2$value))
})

test_that("'scriptVessel' objects contain appropriate slots", {
    expect_match(names(testScript1), "^value$", all=F)
    expect_match(names(testScript2), "^value$", all=F)
})

test_that(paste0("'scriptVessel' object has class",
                 "c(\"scriptVessel\", \"vessel\")"), {
    expect_match(class(testScript1)[1], "^scriptVessel$")
    expect_match(class(testScript1)[2], "^vessel$")
    expect_match(class(testScript2)[1], "^scriptVessel$")
    expect_match(class(testScript2)[2], "^vessel$")
})
