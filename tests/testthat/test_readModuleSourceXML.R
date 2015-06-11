library(conduit)
context("read moduleSource XML")

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
