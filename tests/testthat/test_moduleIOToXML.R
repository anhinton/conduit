library(conduit)
context("convert 'moduleIO' objects to XML")

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
    children <- xmlChildren(inputXML)
    expect_equal(length(children), 2)
    expect_match(names(children), "internal", all=F)
    expect_match(names(children), "format", all=F)
})

test_that("moduleIOToXML outputs correct output XML", {
    expect_match(xmlName(outputXML), "output")
    children <- xmlChildren(outputXML)
    expect_equal(length(children), 2)
    expect_match(names(children), "file", all=F)
    expect_match(names(children), "format", all=F)
})
