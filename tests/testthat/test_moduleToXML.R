library(conduit)
context("convert 'module' objects to XML")

mod1 <- module(name = "setX", platform = "R")
mod1XML <- moduleToXML(mod1)
mod2 <- module(name = "setY", platform = "R",
               host = "127.0.0.1",
               description = "a short description",
               inputs = list(moduleInput("in1",
                   internalVessel("y"),
                   ioFormat("R data frame"))),
               sources = list(moduleSource(
                   scriptVessel("x <- y"))),
               outputs = list(moduleOutput("out1",
                   internalVessel("x"),
                   ioFormat("R data frame"))))
mod2XML <- moduleToXML(mod2)

test_that("moduleToXML fails for non-module objects", {
    expect_error(moduleToXML(list(name="fake", platform="R")),
                 "'module' is not a 'module'")
})

test_that("moduleToXML outputs correct mod1 XML", {
    expect_match(xmlName(mod1XML), "module")
    attrs <- xmlAttrs(mod1XML)
    expect_equal(length(attrs), 1)
    expect_match(names(attrs)[1], "platform")
    expect_match(attrs[1], "R")
    children <- xmlChildren(mod1XML)
    expect_equal(length(children), 1)
    expect_match(names(children), "description")
})

test_that("moduleToXML outputs correct mod2 XML", {
    expect_match(xmlName(mod2XML), "module")
    attrs <- xmlAttrs(mod2XML)
    expect_equal(length(attrs), 2)
    expect_match(names(attrs)[1], "platform")
    expect_match(attrs[1], "R")
    expect_match(names(attrs)[2], "host")
    expect_match(attrs[2], "127.0.0.1")
    children <- xmlChildren(mod2XML)
    expect_equal(length(children), 4)
    expect_match(names(children), "description", all=F)
    expect_match(names(children), "input", all=F)
    expect_match(names(children), "source", all=F)
    expect_match(names(children), "output", all=F)
})
