library(conduit)
context("read ioFormat XML")

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
