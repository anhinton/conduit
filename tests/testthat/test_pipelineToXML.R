library(conduit)
context("convert pipelines to XML")

library(XML)
m1 <- module("m1", "R")
c1 <- component(value = m1)
desc <- "mock pipeline"
pipe1 <- pipe("a", "b", "1", "2")
p1 <- pipeline("p1", description = desc, components = list(m1),
               pipes = list(pipe1))
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

test_that("pipeToXML() works as expected", {
    pipeXML <- pipeToXML(pipe1)
    children1 <- xmlChildren(pipeXML)
    startAttrs <- xmlAttrs(children1[[1]])
    endAttrs <- xmlAttrs(children1[[2]])
    expect_match(xmlName(pipeXML), "pipe")
    expect_match(names(children1), "start", all = FALSE)
    expect_match(names(children1), "end", all = FALSE)
    expect_match(startAttrs[["component"]], start(pipe1)$component)
    expect_match(startAttrs[["output"]], start(pipe1)$output)
    expect_match(endAttrs[["component"]], end(pipe1)$component)
    expect_match(endAttrs[["input"]], end(pipe1)$input)
})

test_that("pipelineToXML() works as expected", {
    pipelineXML <- pipelineToXML(p1)
    expect_match(xmlName(pipelineXML), "pipeline")
    expect_equal(length(getComponents(p1)),
                 length(getNodeSet(pipelineXML, "/pipeline/component")))
    expect_equal(length(getPipes(p1)),
                 length(getNodeSet(pipelineXML, "/pipeline/pipe")))
    expect_equal(
        length(xmlValue(getNodeSet(pipelineXML, "/pipeline/description")[[1]])),
        1)
    expect_match(
        xmlValue(getNodeSet(pipelineXML, "/pipeline/description")[[1]]),
        getDescription(p1))
})

test_that("savePipeline() produces valid pipeline XML file", {
    xmlFile <- savePipeline(pipeline = p1, targetDirectory = tempdir())
    expect_true(isValidXML(file = xmlFile, type = "pipeline"))
})
