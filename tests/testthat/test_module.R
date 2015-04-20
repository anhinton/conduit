library(conduit)
context("create 'module' objects")

test_that("'module' fails for invalid arguments", {
    expect_error(module(name = character(2)),
                 "'name' is not a length 1 character vector")
    expect_error(module(name = 16),
                 "'name' is not a length 1 character vector")
    ## platform tests
    expect_error(module(name = "moddy", platform="R",
                        host = character(2)),
                 "'host' is not a length 1 character vector")
    expect_error(module(name = "moddy", platform="R",
                        , host = 16),
                 "'host' is not a length 1 character vector")
    expect_error(module(name = "moddy", platform = "R",
                        description = numeric(2)),
                 "'description' is not a character object")
    expect_error(module(name = "moddy", platform = "R",
                        description = list("my new module")),
                 "'description' is not a character object")
    ##expect_error(module(name = "moddy")
    
})
