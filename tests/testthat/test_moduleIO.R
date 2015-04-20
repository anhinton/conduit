library(conduit)
context("create 'moduleIO' objects")

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
    expect_match(class(moduleInput), "^moduleIO$")
    expect_match(class(moduleOutput), "^moduleIO$")
})
