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

test_that("readVesselXML reads fileVessel XML", {
    ## fileXML1
    fileVessel1 <- readVesselXML(fileXML1)
    expect_match(class(fileVessel1), "fileVessel", all=F)
    expect_match(class(fileVessel1), "vessel", all=F)
    expect_equal(length(fileVessel1), 2)
    expect_match(names(fileVessel1), "ref", all=F)
    expect_match(fileVessel1$ref, ref1)
    expect_match(names(fileVessel1), "path", all=F)
    expect_null(fileVessel1$path)
    
    ## fileXML2
    fileVessel2 <- readVesselXML(fileXML2)
    expect_match(class(fileVessel2), "fileVessel", all=F)
    expect_match(class(fileVessel2), "vessel", all=F)
    expect_equal(length(fileVessel2), 2)
    expect_match(names(fileVessel2), "ref", all=F)
    expect_match(fileVessel2$ref, ref2)
    expect_match(names(fileVessel2), "path", all=F)
    expect_match(fileVessel2$path, path2)    
})

test_that("readVesselXML reads internalVessel XML", {
    internalVessel <- readVesselXML(internalXML)
    expect_match(class(internalVessel), "internalVessel", all=F)
    expect_match(class(internalVessel), "vessel", all=F)
    expect_equal(length(internalVessel), 1)
    expect_match(names(internalVessel), "symbol", all=F)
    expect_match(internalVessel$symbol, symbol)    
})

test_that("readVesselXML reads scriptVessel XML", {
    scriptVessel <- readVesselXML(scriptXML)
    expect_match(class(scriptVessel), "scriptVessel", all=F)
    expect_match(class(scriptVessel), "vessel", all=F)
    expect_equal(length(scriptVessel), 1)
    expect_match(names(scriptVessel), "value", all=F)
    expect_true(all(scriptVessel$value == value))
})
