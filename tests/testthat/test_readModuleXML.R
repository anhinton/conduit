library(conduit)
context("read module XML")

test_that("readIOFormatXML fails for incorrect node name", {
    library(XML)
    notAFormat <- newXMLNode(name = "notAFormat")
    expect_error(readIOFormatXML(notAFormat),
                 "ioFormat XML is invalid")
})

test_that("readIOFormatXML creates appropriate ioFormat objects", {
    value1 <- "R data job"
    type1 <- "text"
    format1XML <- ioFormatToXML(ioFormat(value1, type1))
    format1 <- readIOFormatXML(format1XML)
    expect_match(class(format1), "ioFormat")
    expect_match(format1$type, "text")
})

## read <input> and <output> XML



test_that("readModuleIOXML fails for invalid named XML", {
    notIO <- XML::newXMLNode("notInput")
    expect_error(readModuleIOXML(notIO),
                 "moduleIO XML is invalid")
})

test_that("readModuleIOXML creates appropriate inputs", {
    ## internalVessel
    name1 <- "data"
    type1 <- "input"
    vessel1 <- internalVessel("myData")
    format1 <- ioFormat("R data")
    inputXML1 <- moduleIOToXML(moduleIO(name1, type1, vessel1, format1))
    input1 <- readModuleIOXML(inputXML1)
    expect_match(class(input1)[2], "moduleIO")
    expect_match(input1$type, type1)

    ## urlVessel
    name3 <- "url"
    type3 <- "input"
    vessel3 <- urlVessel("http://github.com/anhinton/conduit")
    format3 <- ioFormat("html file")
    inputXML3 <- moduleIOToXML(moduleIO(name3, type3, vessel3, format3))
    input3 <- readModuleIOXML(inputXML3)
    expect_match(class(input3)[2], "moduleIO")
    expect_match(input3$type, type3)
})

test_that("readModuleIOXML creates appropriate outputs", {
    name2 <- "graph"
    type2 <- "output"
    vessel2 <- fileVessel("graph.dot")
    format2 <- ioFormat("graphviz dot file")
    outputXML2 <- moduleIOToXML(moduleIO(name2, type2, vessel2, format2))
    output2 <- readModuleIOXML(outputXML2)
    expect_match(class(output2)[2], "moduleIO")
    expect_match(output2$type, type2)
})

## read <source> XML

test_that("readModuleSourceXML fails for invalid XML", {
    notSource <- XML::newXMLNode("notSource")
    expect_error(readModuleSourceXML(notSource),
                 "moduleSource XML is invalid")
})

test_that("readModuleSourceXML creates appropriate objects", {
    ## fileVessel
    file1 <- fileVessel("abs.csv")
    order1 <- 9
    sourceXML1 <- moduleSourceToXML(moduleSource(file1, order1))
    fileSource <- readModuleSourceXML(sourceXML1)
    expect_match(class(fileSource), "moduleSource")
    expect_equal(fileSource$order, order1)

    ## scriptVessel
    script2 <- scriptVessel(c("x", "b", "92 / 2"))
    sourceXML2 <- moduleSourceToXML(moduleSource(script2))
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

test_that("readModuleXML creates appropriate module object", {
    ## minimal module
    minModXML <- moduleToXML(module(
        name = "minMod", language = "R"))
    minMod <- readModuleXML(name = "minMod", xml = minModXML)
    expect_is(minMod, "module")
    expect_match(getName(minMod), "minMod")
    expect_match(getLanguage(minMod), "R")
    expect_null(minMod$host)
    expect_null(minMod$description)
    expect_null(minMod$inputs)
    expect_null(minMod$outputs)
    expect_null(minMod$sources)
    
    ## moduleXML with the works
    sources = list(moduleSource(scriptVessel("alpha")))
    name = "setX"
    language = "R"
    host = vagrantHost(
        vagrantfile = "~/vagrant/vagrant-conduit/Vagrantfile")
    description="your whole life"
    inputs=list(
        moduleInput("input", internalVessel("x"),
                    ioFormat("names")))
    outputs = list(
        moduleOutput("output", fileVessel("out.file"),
                     ioFormat("text file")))
    module1Xml <- moduleToXML(module(
        name = name, language = language, host = host,
        description = description, inputs = inputs, sources = sources,
        outputs = outputs))
    module1 <- readModuleXML(name = "first", xml = module1Xml)
    expect_is(module1, "module")
    expect_match(getName(module1), "first")
    expect_match(getLanguage(module1), "R")
    expect_identical(module1$host, host)
    expect_match(module1$description, description)
    expect_true(!is.null(module1$inputs))
    expect_true(!is.null(module1$sources))
    expect_true(!is.null(module1$outputs))
})

## read <host> xml
test_that("readModuleHostXML() returns correctly", {
    ## <vagrant/> host XML
    vagrantfile <- "~/vagrant/vagrant-conduit/Vagrantfile"
    vhXML1 <- newXMLNode(
        name = "vagrant",
        attrs = list(vagrantfile = vagrantfile))
    vh1 <- readModuleHostXML(vhXML1)
    expect_is(vh1, "moduleHost")
})

## load module from XML file
test_that("loadModule() fails for non-existent file", {
    expect_error(
        loadModule(
            name = "failtest",
            ref = tempfile(pattern = "doesnotexits",
                tmpdir = tempdir())),
        "Unable to load module")
})

test_that("loadModule() fails for invalid module XML", {
    pipeline <- system.file("extdata", "invalidPipeline.xml",
                            package = "conduit")
    expect_error(loadModule("pipeline", ref = pipeline),
                 "module XML is invalid")
    invMod <- system.file("extdata", "invalidModule.xml",
                          package = "conduit")
    expect_error(loadModule("invMod", ref = invMod),
                 "module XML is invalid")
})

test_that("loadModule() handles ref, no path", {
    ref1 <- system.file("extdata", "simpleGraph", "createGraph.xml",
                        package = "conduit")
    dir1 <- dirname(ref1)
    mod1 <- loadModule(name = "mod1",
                       ref = ref1)
    expect_match(class(mod1), "module")
    expect_match(getLocation(mod1), dir1)
})

test_that("loadModule() handles ref and path", {
    ref2 <- "layoutGraph.xml"
    path2 <- system.file("extdata", "simpleGraph", package = "conduit")
    dir2 <-  system.file("extdata", "simpleGraph", package = "conduit")
    mod2 <- loadModule("mod2", ref2, path2)
    expect_match(class(mod2), "module")
    expect_match(getLocation(mod2), dir2)
})

