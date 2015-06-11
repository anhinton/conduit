library(conduit)
context("convert 'moduleSource' objects to XML")

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
