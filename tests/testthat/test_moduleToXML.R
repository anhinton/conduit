library(conduit)
context("convert modules to XML")

test_that("moduleLanguageToXML() returns correctly", {
    #library(XML)
    language <- "R"
    minVersion <- "3.0.1"
    maxVersion <- "3.3.0"
    version <- "3.2.5"
    ml1 <- moduleLanguage(language = language)
    ml2 <- moduleLanguage(language, maxVersion = maxVersion)
    ml3 <- moduleLanguage(language, minVersion = minVersion)
    ml4 <- moduleLanguage(language, version = version)

    ## fails for invalid argumen
    expect_error(moduleLanguageToXML(unclass(ml1)),
                 "moduleLanguage object required")

    ## just language
    mlx1 <- moduleLanguageToXML(ml1)
    expect_match(xmlValue(mlx1), language)
    expect_null(getXMLAttr(mlx1, "minVersion"))
    expect_null(getXMLAttr(mlx1, "maxVersion"))
    expect_null(getXMLAttr(mlx1, "version"))

    ## maxVersion attribute
    mlx2 <- moduleLanguageToXML(ml2)
    expect_match(xmlValue(mlx2), language)
    expect_match(getXMLAttr(mlx2, "maxVersion"), maxVersion)
    expect_null(getXMLAttr(mlx2, "minVersion"))
    expect_null(getXMLAttr(mlx2, "version"))

    ## minVersion attribute
    mlx3 <- moduleLanguageToXML(ml3)
    attrs3 <- xmlAttrs(mlx3)
    expect_match(xmlValue(mlx3), language)
    expect_match(getXMLAttr(mlx3, "minVersion"), minVersion)
    expect_null(getXMLAttr(mlx3, "maxVersion"))
    expect_null(getXMLAttr(mlx3, "version"))

    ## version attribute
    mlx4 <- moduleLanguageToXML(ml4)
    attrs4 <- xmlAttrs(mlx4)
    expect_match(xmlValue(mlx4), language)
    expect_match(getXMLAttr(mlx4, "version"), version)
    expect_null(getXMLAttr(mlx4, "minVersion"))
    expect_null(getXMLAttr(mlx4, "maxVersion"))
})

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
    mod1 <- module(name = "setX", language = moduleLanguage("R"))
    mod2 <- module(
        name = "setY", language = moduleLanguage("R"),
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
    children1 <- xmlChildren(mod1XML)
    expect_equal(length(children1), 1)
    expect_match(xmlValue(getLanguage(children1)),
                 getLanguage(getLanguage(mod1)))

    ## module with vagrantHost
    mod2XML <- moduleToXML(mod2)
    expect_match(xmlName(mod2XML), "module")
    children2 <- xmlChildren(mod2XML)
    expect_match(xmlValue(getLanguage(children1)),
                 getLanguage(getLanguage(mod1)))
    expect_equal(length(children2), 6)
    expect_match(names(children2), "host", all=F)
    expect_match(names(children2), "description", all=F)
    expect_match(names(children2), "input", all=F)
    expect_match(names(children2), "source", all=F)
    expect_match(names(children2), "output", all=F)
})

## save module XML to file
test_that("saveModule() behaves correctly", {
    mod2 <- module(name = "setY", language = moduleLanguage("R"),
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

    ## fails for non-existent target directory
    expect_error(saveModule(module = mod2,
                            targetDirectory = tempfile(pattern="nope")),
                 "no such target directory")

    ## produces appropriate XML file
    targ <- tempdir()
    xmlOutput1 <- saveModule(mod2, targ)
    expect_true(file.exists(xmlOutput1))
    expect_true(isValidXML(xmlOutput1, type = "module"))

    ## explicitly name file
    name <- "lazerbeast.xml"
    xmlOutput2 <- saveModule(mod2, targ, name)
    expect_true(file.exists(xmlOutput2))
    expect_true(isValidXML(xmlOutput2, type = "module"))
    expect_match(basename(xmlOutput2), name)
})
