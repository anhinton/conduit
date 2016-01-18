library(conduit)
context("test validity checks")

createGraph.xml <- system.file("extdata", "simpleGraph", "createGraph.xml",
                               package = "conduit")
layoutGraph.xml <- system.file("extdata", "simpleGraph", "layoutGraph.xml",
                               package = "conduit")
pipeline.xml <- system.file("extdata", "simpleGraph",
                                          "pipeline.xml",
                                          package = "conduit")
invalidPipeline <- system.file("extdata", "invalidPipeline.xml",
                               package = "conduit")


test_that("isValidXML() works for known valid files", {
    expect_true(isValidXML(file = createGraph.xml, type = "module"))
    expect_true(isValidXML(file = layoutGraph.xml, type = "module"))
    expect_true(isValidXML(file = pipeline.xml, type = "pipeline"))
})

test_that("isValidXML() fails for invalid files", {
    expect_false(isValidXML(invalidPipeline, type = "pipeline"))
})

test_that("isValidXML() fails for incorrect type", {
    expect_false(isValidXML(pipeline.xml, type = "module"))
})
