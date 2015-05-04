library(conduit)
context("convert vessels to XML")

testInternal <- internalVessel("important_data")
internalXML <- vesselToXML(testInternal)
testFile1 <- fileVessel(ref="file.csv")
fileXML1 <- vesselToXML(testFile1)
testFile2 <- fileVessel(ref="file.csv", path="/home/user/folder")
fileXML2 <- vesselToXML(testFile2)
rawScript <- c("x <- 1:10", "y <- rnorm(10, 0, 1)", "plot(x, y)")
testScript <- scriptVessel(rawScript)
scriptXML <- vesselToXML(testScript)
testUndefined <- vector("list", 6)
class(testUndefined) <- c("undefined", "vessel")

test_that("vesselToXML fails for non-vessels", {
    expect_error(vesselToXML(vector("list", 6)),
                "'vessel' is not a 'vessel'")
    expect_error(vesselToXML(testUndefined),
                 "'vessel' is of unknown type")
})

test_that("vesselToXML returns correct class of object", {
    expect_match(class(internalXML), "XMLInternalElementNode", all=F)
    expect_match(class(internalXML), "XMLInternalNode", all=F)
    expect_match(class(internalXML), "XMLAbstractNode", all=F)
    expect_match(class(fileXML1), "XMLInternalElementNode", all=F)
    expect_match(class(fileXML1), "XMLInternalNode", all=F)
    expect_match(class(fileXML1), "XMLAbstractNode", all=F)
    expect_match(class(fileXML2), "XMLInternalElementNode", all=F)
    expect_match(class(fileXML2), "XMLInternalNode", all=F)
    expect_match(class(fileXML2), "XMLAbstractNode", all=F)
    expect_match(class(scriptXML), "XMLInternalElementNode", all=F)
    expect_match(class(scriptXML), "XMLInternalNode", all=F)
    expect_match(class(scriptXML), "XMLAbstractNode", all=F)
})

test_that("vesselToXML outputs correct XML", {
    expect_match(xmlName(internalXML), "internal")
    expect_match(xmlAttrs(internalXML), "important_data")
    expect_match(names(xmlAttrs(internalXML)), "symbol")
    expect_match(xmlName(fileXML1), "file")
    expect_match(xmlAttrs(fileXML1), "file.csv")
    expect_match(names(xmlAttrs(fileXML1)), "ref")
    expect_match(xmlName(fileXML2), "file")
    expect_match(xmlAttrs(fileXML2), "file.csv", all=F)
    expect_match(names(xmlAttrs(fileXML2)), "ref", all=F)
    expect_match(xmlAttrs(fileXML2), "/home/user/folder", all=F)
    expect_match(names(xmlAttrs(fileXML2)), "path", all=F)
    expect_match(xmlName(scriptXML), "script")
    scriptValue <- xmlChildren(scriptXML)[[1]]
    expect_match(class(scriptValue), "XMLInternalCDataNode",
                 all=F)
    expect_match(class(scriptValue), "XMLInternalNode",
                 all=F)
    expect_match(class(scriptValue), "XMLAbstractNode",
                 all=F)
    parsedValue <- readLines(textConnection(xmlValue(scriptValue)))
    expect_true(all(rawScript == parsedValue))
})
