library(conduit)
context("read vessel XML")

ref1 <- "data.csv"
fileXML1 <- vesselToXML(fileVessel(ref1))
ref2 <- "different.csv"
path2 <- "~/Documents/data"
fileXML2 <- vesselToXML(fileVessel(ref2, path2))
symbol <- "data_good"
internalXML <- vesselToXML(internalVessel(symbol))
value <- c("x <- 1:10",
           "y <- rnorm(10, 0, 1)",
           "plot(x, y)")
scriptXML <- vesselToXML(scriptVessel(value))

test_that("readVesselXML fails for unknown type", {
    nonVessel <- XML::newXMLNode(name = "notAVessel")
    expect_error(readVesselXML(nonVessel),
                 "'vessel' xml unknown type")
})

test_that("readVesselXML creates appropriate vessel objects", {
    ## fileXML1
    fileVessel1 <- readVesselXML(fileXML1)
    expect_match(class(fileVessel1), "fileVessel", all=F)
    expect_match(class(fileVessel1), "vessel", all=F)
    fileVessel2 <- readVesselXML(fileXML2)
    expect_match(class(fileVessel2), "fileVessel", all=F)
    expect_match(class(fileVessel2), "vessel", all=F)
    internalVessel <- readVesselXML(internalXML)
    expect_match(class(internalVessel), "internalVessel", all=F)
    expect_match(class(internalVessel), "vessel", all=F)
    scriptVessel <- readVesselXML(scriptXML)
    expect_match(class(scriptVessel), "scriptVessel", all=F)
    expect_match(class(scriptVessel), "vessel", all=F)
})
