library(conduit)
context("read pipeline XML")

library(XML)

startComponent <- "a"
startOutput <- "b"
endComponent <- "c"
endInput <- "d"
pipeXML <- c("<pipe>",
             paste0("<start component=\"", startComponent, "\" output=\"",
                    startOutput, "\"/>"),
             paste0("<end component=\"", endComponent, "\" input=\"",
                    endInput, "\"/>"),
             "</pipe>")
pipeXML <- xmlRoot(xmlParse(pipeXML))

inlineModuleXML <- c("<component name=\"a\">",
                     "<module language = \"R\"/>",
                     "</component>")
inlineModuleXML <- xmlRoot(xmlParse(inlineModuleXML))
pipelineXML <- newXMLNode("pipeline", inlineModuleXML, pipeXML)
inlinePipelineXML <- newXMLNode("component", pipelineXML,
                                attrs = list(name = "x"))

moduleFile <- system.file("extdata", "simpleGraph", "createGraph.xml",
                          package = "conduit")
fileModuleXML <- c("<component name=\"createGraph\" type=\"module\">",
                   paste0("<file ref=\"", moduleFile, "\"/>"),
                   "</component>")
fileModuleXML <- xmlRoot(xmlParse(fileModuleXML))

pipelineFile <- system.file("extdata", "simpleGraph", "pipeline.xml",
                            package = "conduit")
filePipelineXML <- c("<component name=\"simpleGraph\" type=\"pipeline\">",
                   paste0("<file ref=\"", pipelineFile, "\"/>"),
                   "</component>")
filePipelineXML <- xmlRoot(xmlParse(filePipelineXML))

pipelineXML <- xmlRoot(xmlParse(
    c("<pipeline>",
      "<component name=\"a\">",
      "<module language=\"R\"/>",
      "</component>",
      "<component name=\"b\">",
      "<module language=\"R\"/>",
      "</component>",
      "<pipe>",
      "<start component=\"a\" output=\"out\"/>",
      "<end component=\"b\" input=\"in\"/>",
      "</pipe>",
      "</pipeline>")))

test_that("pipe nodes read correctly", {
    result <- readPipeXML(pipeXML)
    expect_true(inherits(result, "pipe"))
    expect_match(start(result)$component, startComponent)
    expect_match(start(result)$output, startOutput)
    expect_match(end(result)$component, endComponent)
    expect_match(end(result)$input, endInput)
})

test_that("readComponentNode() works correctly", {
    ## inline module
    result <- readComponentNode(inlineModuleXML)
    expect_true(inherits(result, "component"))
    expect_match(getType(result), "module")
    expect_match(getName(result), "a")
    expect_null(getVessel(result))

    ## file module
    result <- readComponentNode(fileModuleXML)
    expect_true(inherits(result, "component"))
    expect_match(getType(result), "module")
    expect_match(getName(result), "createGraph")
    expect_identical(getVessel(result), fileVessel(moduleFile))

    ## inline pipeline
    result <- readComponentNode(inlinePipelineXML)
    expect_true(inherits(result, "component"))
    expect_match(getType(result), "pipeline")
    expect_match(getName(result), "x")
    expect_null(getVessel(result))

    ## file pipeline
    result <- readComponentNode(filePipelineXML)
    expect_true(inherits(result, "component"))
    expect_match(getType(result), "pipeline")
    expect_match(getName(result), "simpleGraph")
    expect_identical(getVessel(result), fileVessel(pipelineFile))
})

test_that("readPipelineXML() works correctly", {
    location <- tempdir()
    result <- readPipelineXML(name = "danny", xml = pipelineXML,
                              location = location)
    expect_true(inherits(result, "pipeline"))
    expect_match(getName(result), "danny")
    expect_match(getLocation(result), location)
    componentNames <- names(getComponents(result))
    expect_equal(length(componentNames), 2)
    expect_match(componentNames, "a", all = FALSE)
    expect_match(componentNames, "b", all = FALSE)
    pipes <- getPipes(result)
    expect_equal(length(pipes), 1)
    expect_match(start(pipes[[1]])[[1]], "a")
    expect_match(start(pipes[[1]])[[2]], "out")
    expect_match(end(pipes[[1]])[[1]], "b")
    expect_match(end(pipes[[1]])[[2]], "in")
})

test_that("loadPipeline() works correctly", {
    ## just ref
    result <- loadPipeline("simpleGraph", ref = pipelineFile)
    expect_true(inherits(result, "pipeline"))
    expect_match(getName(result), "simpleGraph")
    expect_match(getLocation(result),
                 dirname(pipelineFile))
})
