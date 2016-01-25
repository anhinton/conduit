library(conduit)
context("test validity checks")

createGraph.xml <- system.file("extdata", "simpleGraph", "createGraph.xml",
                               package = "conduit")
layoutGraph.xml <- system.file("extdata", "simpleGraph", "layoutGraph.xml",
                               package = "conduit")
pipeline.xml <- system.file("extdata", "simpleGraph",
                                          "pipeline.xml",
                                          package = "conduit")
simpleGraph <- loadPipeline("simpleGraph", pipeline.xml)
pipeList <- getPipes(simpleGraph)
componentList <- getComponents(simpleGraph)
invalidPipeline <- system.file("extdata", "invalidPipeline.xml",
                               package = "conduit")


test_that("validatePipe() returns correctly", {
    ## error for invalid input
    expect_error(validatePipe(unclass(pipeList[[1]]), componentList),
                 "pipe object required")
    notAllComponents <- {
        x <- componentList
        x[[2]] <- unclass(x[[2]])
        x
    }
    expect_error(validatePipe(pipeList[[1]], notAllComponents),
                 "list of component objects required")

    ## fails for missing startComponent
    pipe1 <- pipeList[[1]]
    pipe1$start$component <- tempfile()
    expect_warning(valid1 <- validatePipe(pipe1, componentList),
                   "Start component")
    expect_false(valid1)

    ## fails for missing startOutput
    pipe2 <- pipeList[[1]]
    pipe2$start$output <- tempfile()
    expect_warning(valid2<- validatePipe(pipe2, componentList),
                   "Start output")
    expect_false(valid2)

    ## fails for missing endComponent
    pipe3 <- pipeList[[1]]
    pipe3$end$component <- tempfile()
    expect_warning(valid3 <- validatePipe(pipe3, componentList),
                   "End component")
    expect_false(valid3)

    ## fails for missing endInput
    pipe4 <- pipeList[[1]]
    pipe4$end$input <- tempfile()
    expect_warning(valid4 <- validatePipe(pipe4, componentList),
                   "End input")
    expect_false(valid4)

    ## works when valid
    expect_true(all(sapply(pipeList, validatePipe,
                           componentList = componentList)))
})

test_that("validatePipeline() returns correctly", {
    ## check for error when inputs incorrect
    expect_error(validatePipeline(unclass(simpleGraph)))

    ## FALSE for invalid pipeline
    componentList <- getComponents(simpleGraph)
    pipeList <- getPipes(simpleGraph)
    pipeList[[1]]$end$component <- basename(tempfile())
    badPipeline <- pipeline("bad", components = componentList,
                            pipes = pipeList)
    expect_warning(valid5 <- validatePipeline(badPipeline),
                   "End component")
    expect_false(valid5)
})

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
