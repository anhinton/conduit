library(conduit)
context("read module XML")

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
