library(conduit)
context("convert modules to XML")

test_that("moduleHostToXML() fails for invalid argument", {
    expect_error(moduleHostToXML(list(a = "b", c = "d")),
                 "moduleHost object required")
})

## convert ioFormat objects to XML

textValue <- "CSV file"
textFormat <- ioFormat(type = "text", value = textValue)
textFormatXML <- ioFormatToXML(textFormat)

test_that("ioFormatToXML fails for non-ioFormat objects", {
    expect_error(ioFormatToXML(vector("list", 6)),
                "'ioFormat' is not an")
})

test_that("ioFormatToXML outputs correct XML for formatType=\"text\"", {
    expect_match(xmlName(textFormatXML), "format")
    expect_match(xmlAttrs(textFormatXML), "text")
    expect_match(names(xmlAttrs(textFormatXML)), "formatType")
    formatValue <- xmlValue(xmlChildren(textFormatXML)[[1]])
    expect_true(textValue == formatValue)
})

## convert module inputs and outputs to XML
moduleInput <- moduleIO(name = "addresses",
                        type = "input",
                        vessel = internalVessel("x"),
                        format = ioFormat("R data frame"))
inputXML <- moduleIOToXML(moduleIO = moduleInput)
moduleOutput <- moduleIO(name = "good_data",
                         type = "output",
                         vessel = fileVessel(ref="good.csv"),
                         format = ioFormat("CSV file"))
outputXML <- moduleIOToXML(moduleIO = moduleOutput)

test_that("moduleIOToXML fails for non-moduleIO objects", {
    expect_error(moduleIOToXML(vector("list", 6)),
                "'moduleIO' is not a 'moduleIO' object")
})

test_that("moduleIOToXML outputs correct input XML", {
    expect_match(xmlName(inputXML), "input")
    attrs <- xmlAttrs(inputXML)
    expect_equal(length(attrs), 1)
    expect_match(names(attrs), "name")
    expect_match(attrs[1], "addresses")
    children <- xmlChildren(inputXML)
    expect_equal(length(children), 2)
    expect_match(names(children), "internal", all=F)
    expect_match(names(children), "format", all=F)
})

test_that("moduleIOToXML outputs correct output XML", {
    expect_match(xmlName(outputXML), "output")
    attrs <- xmlAttrs(outputXML)
    expect_equal(length(attrs), 1)
    expect_match(names(attrs), "name")
    expect_match(attrs[1], "good_data")
    children <- xmlChildren(outputXML)
    expect_equal(length(children), 2)
    expect_match(names(children), "file", all=F)
    expect_match(names(children), "format", all=F)
})

## convert moduleSource objects to XML
scriptSource <- moduleSource(
    vessel = scriptVessel(c("file.file", "x", "1172")),
    order = 10)
scriptXML <- moduleSourceToXML(scriptSource)
fileSource <- moduleSource(fileVessel("file.file"))
fileXML <- moduleSourceToXML(fileSource)

test_that("moduleSourceToXML fails for non-moduleSource objects", {
    expect_error(moduleSourceToXML(vector("list", 6)),
                "'moduleSource' is not a ")
})

test_that("moduleSourceToXML outputs correct script XML", {
    expect_match(xmlName(scriptXML), "source")
    attrs <- xmlAttrs(scriptXML)
    expect_equal(length(attrs), 1)
    expect_match(names(attrs)[1], "order")
    expect_match(attrs[1], "10")
    children <- xmlChildren(scriptXML)
    expect_equal(length(children), 1)
    expect_match(names(children), "script")
})

test_that("moduleSourceToXML outputs correct file XML", {
    expect_match(xmlName(fileXML), "source")
    attrs <- xmlAttrs(fileXML)
    expect_equal(length(attrs), 0)
    children <- xmlChildren(fileXML)
    expect_equal(length(children), 1)
    expect_match(names(children), "file")
})

## convert 'module' objects to XML
test_that("moduleToXML() creates correct output", {
    mod1 <- module(name = "setX", language = "R")
    mod2 <- module(name = "setY", language = "R",
                   host = vagrantHost("~/vagrant/vagrant-conduit/Vagrantfile"),
                   description = "a short description",
                   inputs = list(moduleInput("in1",
                                             internalVessel("y"),
                                             ioFormat("R data frame"))),
                   sources = list(moduleSource(
                       scriptVessel("x <- y"))),
                   outputs = list(moduleOutput("out1",
                                               internalVessel("x"),
                                               ioFormat("R data frame"))))

    ## fails for non-module objects"
    expect_error(moduleToXML(list(name="fake", language="R")),
                 "'module' is not a 'module'")

    ## minimal module
    mod1XML <- moduleToXML(mod1)
    expect_match(xmlName(mod1XML), "module")
    attrs1 <- xmlAttrs(mod1XML)
    expect_match(attrs1[["language"]], "R")
    children1 <- xmlChildren(mod1XML)
    expect_equal(length(children1), 0)

    ## module with vagrantHost
    mod2XML <- moduleToXML(mod2)
    expect_match(xmlName(mod2XML), "module")
    attrs2 <- xmlAttrs(mod2XML)
    expect_match(attrs2[["language"]], "R")
    children2 <- xmlChildren(mod2XML)
    expect_equal(length(children2), 5)
    expect_match(names(children2), "host", all=F)
    expect_match(names(children2), "description", all=F)
    expect_match(names(children2), "input", all=F)
    expect_match(names(children2), "source", all=F)
    expect_match(names(children2), "output", all=F)
})

## save module XML to file
mod2 <- module(name = "setY", language = "R",
               host = vagrantHost("~/vagrant/vagrant-conduit/Vagrantfile"),
               description = "a short description",
               inputs = list(moduleInput("in1",
                   internalVessel("y"),
                   ioFormat("R data frame"))),
               sources = list(moduleSource(
                   scriptVessel("x <- y"))),
               outputs = list(moduleOutput("out1",
                   internalVessel("x"),
                   ioFormat("R data frame"))))

test_that("saveModule fails for non-existent target directory", {
    expect_error(saveModule(module = mod2,
                            targetDirectory = tempfile(pattern="nope")),
                 "no such target directory")
})

test_that("saveModule produces appropriate XML file", {
    skip("2016-02-21 vagrantHost whack-a-mole")
    targ <- tempdir()
    name <- "lazerbeast.xml"
    xmlOutput1 <- saveModule(mod2, targ)
    expect_true(file.exists(xmlOutput1))
    expect_true(isValidXML(xmlOutput1, type = "module"))
    xmlOutput2 <- saveModule(mod2, targ, name)
    expect_true(file.exists(xmlOutput2))
    expect_true(isValidXML(xmlOutput2, type = "module"))
    expect_match(basename(xmlOutput2), name)
})
