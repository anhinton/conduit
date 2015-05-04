library(conduit)
context("convert ioFormat objects to XML")

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
