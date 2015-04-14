library(conduit)
context("create 'modulePlatform' objects")

platform1 <- modulePlatform(name = "R")

test_that("modulePlatform fails for invalid arguments", {
    expect_error(modulePlatform(name = character(2)),
                 "platform 'name' is not a")
    expect_error(modulePlatform(name = 16),
                 "platform 'name' is not a length")
})

test_that("'modulePlatform' slots are correct type and size", {
    expect_true(is_length1_char(platform1$name))
})

test_that("'modulePlatform' has expected slots", {
    expect_match(names(platform1), "^name$", all=F)
})

test_that("'modulePlatform' has object class \"modulePlatform\"", {
    expect_match(class(platform1), "^modulePlatform$")
})
