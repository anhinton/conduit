library(conduit)
context("create module objects")

## create inputs and outputs
myInput <- moduleInput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("moduleInput has correct class", {
    expect_match(class(myInput)[1], "moduleInput")
    expect_match(class(myInput)[2], "moduleIO")
})

myOutput <- moduleOutput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("moduleOutput has correct class", {
    expect_match(class(myOutput)[1], "moduleOutput")
    expect_match(class(myOutput)[2], "moduleIO")
})

moduleInput <- moduleIO(name="comeIn", type="input",
                        vessel=fileVessel("comeIn.txt"),
                        format=ioFormat("tab-separated file"))
moduleOutput <- moduleIO(name="getOut", type="output",
                         vessel=internalVessel("going"),
                         format=ioFormat("R data frame"))

test_that("moduleIO fails for invalid arguments", {
    expect_error(moduleIO(name=character(2)),
                 "'name' is not a length 1 character")
    expect_error(moduleIO(name=66),
                 "'name' is not a length 1 character")
    expect_error(moduleIO(name="name", type=character(2)),
                 "'type' is not a length 1 character")
    expect_error(moduleIO(name="name", type=66),
                 "'type' is not a length 1 character")
    expect_error(moduleIO(name="name", type="input",
                          vessel=c("file.txt")),
                 "'vessel' is not")
    expect_error(moduleIO(name="name", type="input",
                          vessel=scriptVessel("a <- \"b\"")),
                 "'scriptVessel' vessels not defined for moduleIO objects")
    expect_error(moduleIO(name="name", type="input",
                          vessel=internalVessel("goodOne"),
                          format=c("clippers")),
                 "'format' is not")
    expect_error(moduleIO(name="name", type="throughput",
                          vessel=internalVessel("goodOne"),
                          format=ioFormat("steamer")),
                 "'type' must be")
})

test_that("'moduleIO' slots are correct type and length", {
    expect_true(is_length1_char(moduleInput$name))
    expect_true(is_length1_char(moduleInput$type))
    expect_true("vessel" %in% class(moduleInput$vessel))
    expect_true(class(moduleInput$format) == "ioFormat")
    expect_true(is_length1_char(moduleOutput$name))
    expect_true(is_length1_char(moduleOutput$type))
    expect_true("vessel" %in% class(moduleOutput$vessel))
    expect_true(class(moduleOutput$format) == "ioFormat")
})

test_that("'moduleIO' contains appropriate slots", {
    expect_match(names(moduleInput), "^name$", all=F)
    expect_match(names(moduleInput), "^type$", all=F)
    expect_match(names(moduleInput), "^vessel$", all=F)
    expect_match(names(moduleInput), "^format$", all=F)
    expect_match(names(moduleOutput), "^name$", all=F)
    expect_match(names(moduleOutput), "^type$", all=F)
    expect_match(names(moduleOutput), "^vessel$", all=F)
    expect_match(names(moduleOutput), "^format$", all=F)
})

test_that("'moduleIO' object has class \"moduleIO\"", {
    expect_match(class(moduleInput), "^moduleIO$", all=F)
    expect_match(class(moduleOutput), "^moduleIO$", all=F)
})

## create moduleSource objects
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

## create module objects

mod1 <- module(
    name = "createGraph",
    language = "R",
    host = vagrantHost("~/vagrant/vagrant-conduit/Vagrantfile"),
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
               language = "shell")
                       
myInput <- moduleInput(name="name", vessel=internalVessel("lobbo"),
                       format=ioFormat("R character vector"))

test_that("'module' fails for invalid arguments", {
    expect_error(module(name = character(2)),
                 "'name' is not a length 1 character vector")
    expect_error(module(name = 16),
                 "'name' is not a length 1 character vector")
    ## 'language' tests are sparse as this is properly tested in
    ## test_moduleLanguage.R
    expect_error(module(name = "moddy", language = character(2)),
                 "'language' is not a length 1 character vector")
    expect_error(module(name = "moddy", language = numeric(1)),
                 "'language' is not a length 1 character vector")
    expect_error(module(name = "moddy", language = "R",
                        host = character(2)),
                 "'host' is not moduleHost object")
    expect_error(module(name = "moddy", language = "R",
                        description = numeric(2)),
                 "'description' is not a character object")
    expect_error(module(name = "moddy", language = "R",
                        description = list("my new module")),
                 "'description' is not a character object")
    expect_error(module(name = "moddy", language = "R",
                        inputs = character(2)),
                 "'inputs' is not a list object")
    expect_error(module(name = "moddy", language = "R",
                        inputs = list(character(1))),
                 "not a 'moduleInput' object")
    expect_error(module(name = "moddy", language = "R",
                        outputs = character(2)),
                 "'outputs' is not a list object")
    expect_error(module(name = "moddy", language = "R",
                        outputs = list(character(1))),
                 "not a 'moduleOutput' object")
    expect_error(module(name = "moddy", language = "R",
                        sources = character(2)),
                 "'sources' is not a list object")
    expect_error(module(name = "moddy", language = "R",
                        sources = list(character(1))),
                 "not a 'moduleSource' object")
})

test_that("'module' slots are correct type and length", {
    skip("2016-02-21 vagrantHost whack-a-mole")
    expect_true(is_length1_char(mod1$name))
    expect_equal(length(mod1$language), 1)
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
    expect_match(names(mod1), "^language$", all=F)
    expect_match(names(mod1), "^host$", all=F)
    expect_match(names(mod1), "^description$", all=F)
    expect_match(names(mod1), "^inputs$", all=F)
    expect_match(names(mod1), "^outputs$", all=F)
    expect_match(names(mod1), "^sources$", all=F)
    expect_match(names(mod2), "^name$", all=F)
    expect_match(names(mod2), "^language$", all=F)
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
