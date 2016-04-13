library(conduit)
context("create vessel objects")

file <- system.file("extdata", "simpleGraph", "createGraph.xml",
                    package = "conduit")

testFile <- fileVessel(ref = file) # just a ref
testPath <- fileVessel(ref = basename(file),
                       path = dirname(file)) # ref and path
testURL <-
    urlVessel(ref = "https://raw.githubusercontent.com/anhinton/conduit/master/README.md")
testScript1 <- scriptVessel("important_data")
testScript2 <- scriptVessel(c("take a little code",
                              "add a little more",
                              "cross your fingers"))
text_format <- ioFormat(value="R data frame", type="text")

## internalVessel creation

test_that("'internalVessel' objects correctly created", {
    testInternal <- internalVessel(symbol = "important_data")

    ## fail for invalid arguments
    expect_error(internalVessel(character(2)),
                 "'symbol' is not a length 1 character vector")
    expect_error(internalVessel(numeric(1)),
                 "'symbol' is not a length 1 character vector")

    ## correctly constructed
    expect_match(names(testInternal), "^symbol$", all=F)
    expect_is(testInternal$symbol, "symbol")
    expect_true(is_length1_char(testInternal$symbol))
    expect_is(testInternal, "internalVessel")
    expect_is(testInternal, "vessel")
})

## fileVessel creation
test_that("fileVessel stops for invalid arguments", {
    expect_error(fileVessel(c("ref1", "ref1")),
                 "'ref' is not a length 1 character vector")
    expect_error(fileVessel(664),
                 "'ref' is not a length 1 character vector")
    expect_error(fileVessel(ref = "ref", path = c("ref1", "ref1")),
                 "'path' is not a length 1 character vector")
    expect_error(fileVessel(ref = "ref", path = 666),
                 "'path' is not a length 1 character vector")
})

test_that("'fileVessel' slots are right type and length", {
    expect_true(is_length1_char(testFile$ref))
    expect_true(is_length1_char(testPath$path))
})

test_that("'fileVessel' objects contain appropriate slots", {
    expect_match(names(testFile), "^ref$", all=F)
    expect_match(names(testPath), "^ref$", all=F)
    expect_match(names(testPath), "^path$", all=F)
})

test_that("'fileVessel' object has class c(\"fileVessel\", \"vessel\")", {
    expect_match(class(testFile)[1], "^fileVessel$")
    expect_match(class(testFile)[2], "^vessel$")
    expect_match(class(testPath)[1], "^fileVessel$")
    expect_match(class(testPath)[2], "^vessel$")
})

## urlVessel creation
test_that("urlVessel stops when 'ref' is not length 1 char", {
    expect_error(urlVessel(c("ref1", "ref1")),
                 "'ref' is not a length 1 character vector")
    expect_error(urlVessel(666),
                 "'ref' is not a length 1 character vector")
})

test_that("'urlVessel' slots are right type and length", {
    expect_true(is_length1_char(testURL$ref))
})

test_that("urlVessel objects contain appropriate slots", {
    expect_match(names(testURL), "^ref$", all=F)
})

test_that("urlVessel object has class c(\"urlVessel\", \"vessel\")", {
    expect_match(class(testURL)[1], "^urlVessel$")
    expect_match(class(testURL)[2], "^vessel$")
})

## scriptVessel creation
test_that("scriptVessel stops when 'value' is not character vector", {
    expect_error(scriptVessel(numeric(2)),
                 "'value' is not a character vector")
    expect_error(scriptVessel(vector("list",1)),
                 "'value' is not a character vector")
})

test_that("'scriptVessel' slots are right type", {
    expect_true(is.character(testScript1$value))
    expect_true(is.character(testScript2$value))
})

test_that("'scriptVessel' objects contain appropriate slots", {
    expect_match(names(testScript1), "^value$", all=F)
    expect_match(names(testScript2), "^value$", all=F)
})

test_that(paste0("'scriptVessel' object has class",
                 "c(\"scriptVessel\", \"vessel\")"), {
    expect_match(class(testScript1)[1], "^scriptVessel$")
    expect_match(class(testScript1)[2], "^vessel$")
    expect_match(class(testScript2)[1], "^scriptVessel$")
    expect_match(class(testScript2)[2], "^vessel$")
})

## create vessel XML

test_that(
    "vesselToXML fails for non-vessels", {
        testUndefined <- vector("list", 6)
        class(testUndefined) <- c("undefined", "vessel")
        expect_error(vesselToXML(vector("list", 6)),
                     "'vessel' is not a 'vessel'")
        expect_error(vesselToXML(testUndefined),
                     "'vessel' is of unknown type")
    })

test_that(
    "vesselToXML outputs correct internalVessel XML", {
        testInternal <- internalVessel("important_data")
        internalXML <- vesselToXML(testInternal)
        expect_match(xmlName(internalXML), "internal")
        expect_match(xmlAttrs(internalXML), "important_data")
        expect_match(names(xmlAttrs(internalXML)), "symbol")
    })

test_that(
    "vesselToXML outputs correct fileVessel XML", {
        testFile1 <- fileVessel(ref="file.csv")
        fileXML1 <- vesselToXML(testFile1)
        testFile2 <- fileVessel(ref="file.csv")
        fileXML2 <- vesselToXML(testFile2)
        expect_match(xmlName(fileXML1), "file")
        expect_match(xmlAttrs(fileXML1), "file.csv")
        expect_match(names(xmlAttrs(fileXML1)), "ref")
        expect_match(xmlName(fileXML2), "file")
        expect_match(xmlAttrs(fileXML2), "file.csv", all=F)
})

test_that(
    "vesselToXML outputs correct urlVessel XML", {
        testURL1 <- urlVessel(ref="https://github.com/anhinton/conduit")
        urlXML1 <- vesselToXML(testURL1)
        expect_match(xmlName(urlXML1), "url")
        expect_match(xmlAttrs(urlXML1), "https://github.com/anhinton/conduit")
        expect_match(names(xmlAttrs(urlXML1)), "ref")
})

test_that(
    "vesselToXML outputs correct scriptVessel XML", {
        rawScript <- c("x <- 1:10", "y <- rnorm(10, 0, 1)", "plot(x, y)")
        testScript <- scriptVessel(rawScript)
        scriptXML <- vesselToXML(testScript)
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

## create ioFormat objects
test_that("ioFormat fails for invalid arguments", {
    expect_error(ioFormat(type=character(2), value="CSV file"),
                 "'type' is not a length 1 character")
    expect_error(ioFormat(type="text", value=list("CSV file")),
                 "'value' is not a length 1 character vector")
    expect_error(ioFormat(type="DRAGON", value="CSV file"),
                 "invalid 'type' provided")
})

test_that("'ioFormat' slots are correct type and length", {
    expect_true(is_length1_char(text_format$type))
    expect_true(is_length1_char(text_format$value))
})

test_that("'ioFormat' contains appropriate slots", {
    expect_match(names(text_format), "^type$", all=F)
    expect_match(names(text_format), "^value$", all=F)
})

test_that("'ioFormat' object has class \"ioFormat\"", {
    expect_match(class(text_format), "^ioFormat$")
})

test_that("fetchVessel returns correctly", {
    ## error for incorrect object
    expect_error(fetchVessel(file), "not a vessel object")

    ## fileVessel
    result1 <- fetchVessel(testFile)
    expect_is(result1, "character")
    expect_match(attr(result1, "location"), dirname(file))
    result2 <- fetchVessel(testPath)
    expect_is(result2, "character")
    expect_match(attr(result2, "location"), dirname(file))

    ## urlVessel
    result3 <- fetchVessel(testURL)
    expect_is(result3, "character")
})
