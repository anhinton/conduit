library(conduit)
context("save 'module' objects to XML file")

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

test_that("saveModule fails for non-existent target directory", {
    skip_on_cran()
    expect_error(saveModule(module = mod2,
                            targetDirectory = tempfile(pattern="nope")),
                 "no such target directory")
})

test_that("saveModule produces appropriate XML file", {
    skip_on_cran()
    targ <- tempdir()
    name <- "lazerbeast.xml"
    xmlOutput1 <- saveModule(mod2, targ)
    expect_true(file.exists(xmlOutput1))
    xmlOutput2 <- saveModule(mod2, targ, name)
    expect_true(file.exists(xmlOutput2))
    expect_match(basename(xmlOutput2), name)
})
