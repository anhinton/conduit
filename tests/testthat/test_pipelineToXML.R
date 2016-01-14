library(conduit)
context("convert pipelines to XML")

m1 <- module("m1", "R")
c1 <- component(value = m1)
p1 <- pipeline("p1", components = list(m1))
c2 <- component(value = p1)
fv <- fileVessel("pipeline.xml")
c3 <- component(vessel = fv, value = p1)

test_that("componentToXML() creates appropriate XML", {
    ## module component
    c1XML <- componentToXML(c1)
    child1 <- xmlChildren(c1XML)[[1]]
    expect_match(xmlName(c1XML), "component")
    expect_match(getName(c1), xmlAttrs(c1XML)[["name"]])
    expect_match(xmlName(child1), getType(c1))

    ## pipeline component
    c2XML <- componentToXML(c2)
    child2 <- xmlChildren(c2XML)[[1]]
    expect_match(xmlName(c2XML), "component")
    expect_match(getName(c2), xmlAttrs(c2XML)[["name"]])
    expect_match(xmlName(child2), getType(c2))

    ## component with vessel
    c3XML <- componentToXML(c3)
    child3 <- xmlChildren(c3XML)[[1]]
    expect_match(xmlName(c3XML), "component")
    expect_match(getName(c3), xmlAttrs(c3XML)[["name"]])
    expect_match(getType(c3), xmlAttrs(c3XML)[["type"]])
    expect_match(xmlName(child3), "file")
})
