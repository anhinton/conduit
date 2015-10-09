library(conduit)
context("read module XML")

## read <format> XML
value1 <- "R data job"
type1 <- "text"
format1XML <- ioFormatToXML(ioFormat(value1, type1))

test_that("readIOFormatXML fails for incorrect node name", {
    notAFormat <- newXMLNode(name = "notAFormat")
    expect_error(readIOFormatXML(notAFormat),
                 "ioFormat XML is invalid")
})

test_that("readIOFormatXML creates appropriate ioFormat objects", {
    format1 <- readIOFormatXML(format1XML)
    expect_match(class(format1), "ioFormat")
    expect_match(format1$type, "text")
})

## read <input> and <output> XML
name1 <- "data"
type1 <- "input"
vessel1 <- internalVessel("myData")
format1 <- ioFormat("R data")
inputXML1 <- moduleIOToXML(moduleIO(name1, type1, vessel1, format1))

name2 <- "graph"
type2 <- "output"
vessel2 <- fileVessel("graph.dot")
format2 <- ioFormat("graphviz dot file")
outputXML2 <- moduleIOToXML(moduleIO(name2, type2, vessel2, format2))

name3 <- "url"
type3 <- "input"
vessel3 <- urlVessel("http://github.com/anhinton/conduit")
format3 <- ioFormat("html file")
inputXML3 <- moduleIOToXML(moduleIO(name3, type3, vessel3, format3))

test_that("readModuleIOXML fails for invalid named XML", {
    notIO <- XML::newXMLNode("notInput")
    expect_error(readModuleIOXML(notIO),
                 "moduleIO XML is invalid")
})

test_that("readModuleIOXML creates appropriate inputs", {
    input1 <- readModuleIOXML(inputXML1)
    expect_match(class(input1)[2], "moduleIO")
    expect_match(input1$type, type1)
    input3 <- readModuleIOXML(inputXML3)
    expect_match(class(input3)[2], "moduleIO")
    expect_match(input3$type, type3)
})

test_that("readModuleIOXML creates appropriate outputs", {
    output2 <- readModuleIOXML(outputXML2)
    expect_match(class(output2)[2], "moduleIO")
    expect_match(output2$type, type2)
})

## read <source> XML
file1 <- fileVessel("abs.csv")
order1 <- 9
sourceXML1 <- moduleSourceToXML(moduleSource(file1, order1))

script2 <- scriptVessel(c("x", "b", "92 / 2"))
sourceXML2 <- moduleSourceToXML(moduleSource(script2))

test_that("readModuleSourceXML fails for invalid XML", {
    notSource <- XML::newXMLNode("notSource")
    expect_error(readModuleSourceXML(notSource),
                 "moduleSource XML is invalid")
})

test_that("readModuleSourceXML creates appropriate objects", {
    fileSource <- readModuleSourceXML(sourceXML1)
    expect_match(class(fileSource), "moduleSource")
    expect_equal(fileSource$order, order1)
    scriptSource <- readModuleSourceXML(sourceXML2)
    expect_match(class(scriptSource), "moduleSource")
})

## read vessel XML - <internal>, <file>, <url>

test_that("readVesselXML fails for unknown type", {
    nonVessel <- XML::newXMLNode(name = "notAVessel")
    expect_error(readVesselXML(nonVessel),
                 "'vessel' xml unknown type")
})

test_that(
    "readVesselXML creates appropriate fileVessel objects", {
        ref1 <- "data.csv"
        fileXML1 <- vesselToXML(fileVessel(ref1))
        ref2 <- "different.csv"
        fileXML2 <- vesselToXML(fileVessel(ref2))
        ref3 <- "file.txt"
        path3 <- tempfile()
        pathXML <- vesselToXML(fileVessel(ref3, path3))
        ## fileXML1
        fileVessel1 <- readVesselXML(fileXML1)
        expect_match(class(fileVessel1), "fileVessel", all=F)
        expect_match(class(fileVessel1), "vessel", all=F)
        ## fileXML2
        fileVessel2 <- readVesselXML(fileXML2)
        expect_match(class(fileVessel2), "fileVessel", all=F)
        expect_match(class(fileVessel2), "vessel", all=F)
        ## pathXML
        pathVessel <- readVesselXML(pathXML)
        expect_match(class(pathVessel), "fileVessel", all=F)
        expect_match(class(pathVessel), "vessel", all=F)
    })

test_that(
    "readVesselXML creates appropriate internalVessel objects", {
        symbol <- "data_good"
        internalXML <- vesselToXML(internalVessel(symbol))
        internalVessel <- readVesselXML(internalXML)
        expect_match(class(internalVessel), "internalVessel", all=F)
        expect_match(class(internalVessel), "vessel", all=F)
    })

test_that(
    "readVesselXML creates appropriate urlVessel objects", {
        url <- "https://github.com/anhinton/conduit"
        urlXML <- vesselToXML(urlVessel(url))
        urlVessel <- readVesselXML(urlXML)
        expect_match(class(urlVessel), "urlVessel", all=F)
        expect_match(class(urlVessel), "vessel", all=F)
    })

test_that(
    "readVesselXML creates appropriate scriptVessel objects", {
        value <- c("x <- 1:10",
                   "y <- rnorm(10, 0, 1)",
                   "plot(x, y)")
        scriptXML <- vesselToXML(scriptVessel(value))
        scriptVessel <- readVesselXML(scriptXML)
        expect_match(class(scriptVessel), "scriptVessel", all=F)
        expect_match(class(scriptVessel), "vessel", all=F)
})

## read <module> XML
moduleXml <-
    moduleToXML(
        module(
            sources = list(moduleSource(scriptVessel("alpha"))),
            name = "setX",
            language = "R",
            description="your whole life",
            inputs=list(
                moduleInput("input", internalVessel("x"), ioFormat("names"))),
            outputs = list(
                moduleOutput("output", fileVessel("out.file"),
                             ioFormat("text file")))))

test_that("readModuleXML fails for invalid XML", {
    notModule <- XML::newXMLNode("notModule")
    expect_error(readModuleXML(xml = notModule),
                 "module XML is invalid")
})

test_that("readModuleXML creates appropriate module object", {
    module <- readModuleXML(name = "first", xml = moduleXml)
    expect_match(class(module), "module")
    expect_match(module$name, "first")
})

## load module from XML file
test_that("loadModule() fails for non-existent file", {
    skip_on_cran()
    expect_error(
        loadModule(
            name = "failtest",
            ref = tempfile(pattern = "doesnotexits",
                tmpdir = tempdir())),
        "Unable to load module")
})

test_that("loadModule() handles ref, no path", {
    skip_on_cran()
    ref1 <- system.file("extdata", "simpleGraph", "createGraph.xml",
                        package = "conduit")
    dir1 <- dirname(ref1)
    mod1 <- loadModule(name = "mod1",
                       ref = ref1)
    expect_match(class(mod1), "module")
    expect_match(attr(mod1, "location"), dir1)
})

test_that("loadModule() handles ref and path", {
    skip_on_cran()
    ref2 <- "layoutGraph.xml"
    path2 <- system.file("extdata", "simpleGraph", package = "conduit")
    dir2 <-  system.file("extdata", "simpleGraph", package = "conduit")
    mod2 <- loadModule("mod2", ref2, path2)
    expect_match(class(mod2), "module")
    expect_match(attr(mod2, "location"), dir2)
})
