library(conduit)
context("convert pipelines to XML")

m1 <- module("m1", "R")
c1 <- component(value = m1)
desc <- "mock pipeline"
pipe1 <- pipe("a", "b", "1", "2")
p1 <- pipeline("p1", description = desc, components = list(m1),
               pipes = list(pipe1))
c2 <- component(value = p1)
p2file <- system.file("extdata", "simpleGraph",
                      "pipeline.xml",
                      package = "conduit")
fv <- fileVessel(p2file)
p2 <- loadPipeline("p2", p2file) 
c3 <- component(vessel = fv, value = p1)
c4 <- component(vessel = urlVessel("http://openapi.org"),
                value = module("m4", "R"))

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
        length(xmlValue(getNodeSet(pipelineXML,
                                   "/pipeline/description")[[1]])),
        1)
})

test_that("savePipeline() produces valid pipeline XML file", {
    xmlFile1 <- savePipeline(pipeline = p1, targetDirectory = tempdir())
    expect_match(basename(xmlFile1), "pipeline.xml")
    expect_true(isValidXML(file = xmlFile1, type = "pipeline"))
    xmlFile2 <- savePipeline(pipeline = p2, targetDirectory = tempdir(),
                             filename = "acoolfile.xml")
    expect_match(basename(xmlFile2), "acoolfile.xml")
    expect_true(isValidXML(file = xmlFile1, type = "pipeline"))
})

test_that("exportComponent() generates appropriate objects", {
    targetDirectory <- tempfile("exportComponent")
    if (!dir.exists(targetDirectory)) dir.create(targetDirectory)

    ## fails for non-existent targetDirectory
    expect_error(exportComponent(c1, tempfile()),
                 "no such target")

    ## component with no vessel
    newC1 <- exportComponent(c1, targetDirectory)
    expect_is(newC1, "component")
    expect_match(getType(c1), getType(newC1))
    expect_true(file.exists(
        file.path(targetDirectory, paste0(getName(newC1), ".xml"))))
    expect_true(!is.null(getVessel(newC1)))

    ## component with fileVessel
    newC3 <- exportComponent(c3, targetDirectory)
    expect_is(newC3, "component")
    expect_match(getType(c3), getType(newC3))
    expect_true(file.exists(
        file.path(targetDirectory, paste0(getName(newC3), ".xml"))))
    expect_true(!is.null(getVessel(newC3)))

    ## component with urlVessel
    newC4 <- exportComponent(c4, targetDirectory)
    expect_is(newC4, "component")
    expect_match(getType(c4), getType(newC4))
    expect_true(!file.exists(
        file.path(targetDirectory, paste0(getName(newC4), ".xml"))))
    expect_true(!is.null(getVessel(newC4)))
    expect_identical(getVessel(c4), getVessel(newC4))
})

test_that("exportPipeline() behaves right", {
    targ <- tempfile("exportPipeline")
    if (!dir.exists(targ)) dir.create(targ)

    ## fails for non-existent targetDirectory
    expect_error(exportPipeline(p2, tempfile()),
                 "Target directory")

    ## exported XML file is full of the right things
    exportedXML <- exportPipeline(pipeline = p2, targetDirectory = targ)
    expect_true(file.exists(exportedXML))
    expect_true(isValidXML(exportedXML, "pipeline"))
    expect_null({
        reloaded <- loadPipeline("reloaded", ref = exportedXML)
        warnings()
    })
    expect_true(!identical(getLocation(p2), getLocation(reloaded)))
    expect_match(getLocation(reloaded),
                 file.path(targ, getName(p2)))
    expect_identical(names(getComponents(p2)),
                     names(getComponents(reloaded)))
    expect_identical(getPipes(p2), getPipes(reloaded))
    expect_identical(getDescription(p2), getDescription(reloaded))
})
