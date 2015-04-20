library(conduit)
context("create 'module' objects")

mod1 <- module(name = "createGraph",
               platform = "R",
               host = "user@remothost:2222",
               description = "Lays out a graphNEL graph using the Rgraphviz package",
               inputs =
                   list(moduleInput(
                       name = "myGraph",
                       vessel = internalVessel("myGraph"),
                       format = ioFormat("R \"graphNEL\" object"))),
               outputs =
                   list(moduleOutput(
                       name = "Ragraph",
                       vessel = internalVessel("Ragraph"),
                       format = ioFormat("R \"Ragraph\" object"))),
               sources =
                   list(moduleSource(
                       scriptVessel(
                           c("library(Rgraphviz)",
                             "Ragraph <- agopen(myGraph, \"myGraph\")")))))
mod2 <- module(name = "blank",
               platform = "shell")
                       
myInput <- moduleInput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("'module' fails for invalid arguments", {
    expect_error(module(name = character(2)),
                 "'name' is not a length 1 character vector")
    expect_error(module(name = 16),
                 "'name' is not a length 1 character vector")
    ## 'platform' tests are sparse as this is properly tested in
    ## test_modulePlatform.R
    expect_error(module(name = "moddy", platform = character(2)))
    expect_error(module(name = "moddy", platform = numeric(1)))
    expect_error(module(name = "moddy", platform = "R",
                        host = character(2)),
                 "'host' is not a length 1 character vector")
    expect_error(module(name = "moddy", platform="R",
                        host = character(2)),
                 "'host' is not a length 1 character vector")
    expect_error(module(name = "moddy", platform="R",
                        host = numeric(1)),
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
    expect_error(module(name = "moddy", platform = "R",
                        inputs = character(2)),
                 "'inputs' is not a list object")
    expect_error(module(name = "moddy", platform = "R",
                        inputs = list(character(1))),
                 "not a 'moduleInput' object")
    expect_error(module(name = "moddy", platform = "R",
                        outputs = character(2)),
                 "'outputs' is not a list object")
    expect_error(module(name = "moddy", platform = "R",
                        outputs = list(character(1))),
                 "not a 'moduleOutput' object")
    expect_error(module(name = "moddy", platform = "R",
                        sources = character(2)),
                 "'sources' is not a list object")
    expect_error(module(name = "moddy", platform = "R",
                        sources = list(character(1))),
                 "not a 'moduleSource' object")
})

test_that("'module' slots are correct type and length", {
    expect_true(is_length1_char(mod1$name))
    expect_match(class(mod1$platform), "^modulePlatform$")
    expect_equal(length(mod1$platform), 1)
    expect_true(is.character(mod1$description))
    expect_true(is.null(mod2$desctription))
    expect_true(is.character(mod1$host))
    expect_equal(length(mod1$host), 1)
    expect_true(is.null(mod2$host))
    expect_true(is.list(mod1$inputs))
    expect_match(class(mod1$inputs[[1]]), "^moduleInput$", all=F)
    expect_match(class(mod1$inputs[[1]]), "^moduleIO$", all=F)
    expect_true(is.null(mod2$inputs))
    expect_true(is.list(mod1$outputs))
    expect_match(class(mod1$outputs[[1]]), "^moduleOutput$", all=F)
    expect_match(class(mod1$outputs[[1]]), "^moduleIO$", all=F)
    expect_true(is.null(mod2$outputs))
    expect_true(is.list(mod1$sources))
    expect_match(class(mod1$sources[[1]]), "^moduleSource$")
    expect_true(is.null(mod2$sources))
})

test_that("'module' object has appropriate slots", {
    expect_match(names(mod1), "^name$", all=F)
    expect_match(names(mod1), "^platform$", all=F)
    expect_match(names(mod1), "^host$", all=F)
    expect_match(names(mod1), "^description$", all=F)
    expect_match(names(mod1), "^inputs$", all=F)
    expect_match(names(mod1), "^outputs$", all=F)
    expect_match(names(mod1), "^sources$", all=F)
    expect_match(names(mod2), "^name$", all=F)
    expect_match(names(mod2), "^platform$", all=F)
    expect_match(names(mod2), "^host$", all=F)
    expect_match(names(mod2), "^description$", all=F)
    expect_match(names(mod2), "^inputs$", all=F)
    expect_match(names(mod2), "^outputs$", all=F)
    expect_match(names(mod2), "^sources$", all=F)
})

test_that("'module' object has class \"module\"", {
    expect_match(class(mod1), "^module$")
    expect_match(class(mod2), "^module$")
})
