library(conduit)
context("read moduleIO XML")

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

test_that("readModuleIOXML fails for invalid named XML", {
    notIO <- XML::newXMLNode("notInput")
    expect_error(readModuleIOXML(notIO),
                 "moduleIO XML is invalid")
})

test_that("readModuleIOXML creates appropriate inputs", {
    input1 <- readModuleIOXML(inputXML1)
    expect_match(class(input1), "moduleIO")
    expect_match(input1$type, "input")
})

test_that("readModuleIOXML creates appropriate outputs", {
    output2 <- readModuleIOXML(outputXML2)
    expect_match(class(output2), "moduleIO")
    expect_match(output2$type, "output")
})
