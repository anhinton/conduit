library(conduit)
context("execute pipelines")

## skip tests which fail strangely when doing R CMD check
skipCheck <- TRUE

lang = "R"
outdir <- tempdir()
symbol <- "x"
internal_output <- moduleOutput(
    "internal", internalVessel(symbol), ioFormat("nonsense"))
url <- "https://github.com/anhinton/conduit"
url_output <- moduleOutput(
    "url", urlVessel(url), ioFormat("HTML file"))
file <- "output.csv"
file_output <- moduleOutput(
    "file", fileVessel(file), ioFormat("CSV file"))
mod1 <- module("mod1", language = lang,
               outputs = list(internal_output, url_output, file_output))
comp1 <- component(value = mod1)
p2 <- pipeline("p2", components = list(comp1))
comp2 <- component(value = p2)
mod3 <- module(
    name = "A", language = "R",
    outputs = list(
        moduleOutput("B", fileVessel("myfile"),
                     ioFormat("text file")),
        moduleOutput("C", urlVessel("http://localhost"),
                     ioFormat("html file"))))
comp3 <- component(value = mod3)
simpleGraph <- loadPipeline(
    name = "simpleGraph",
    system.file("extdata", "simpleGraph", "pipeline.xml", package = "conduit"))
                                 

test_that("calculateOutputs() produces correct output", {
    ## fails for incorrect object
    expect_error(calculateOutputs(unclass(comp1)),
                 "component object")

    ## module-type component
    outputs <- calculateOutputs(comp1, outdir)
    expect_true(all(sapply(outputs, inherits, what = "output")))
    expect_match(names(outputs), internal_output$name, all = FALSE)
    expect_match(names(outputs), file_output$name, all = FALSE)
    expect_match(names(outputs), url_output$name, all = FALSE)

    ## pipeline-type component
    expect_error(calculateOutputs(comp2, outdir),
                 "component type not supported")
})

test_that("componentPath() returns correct output", {
    ## fails for incorrect object
    expect_error(calculateOutputs(unclass(comp1)),
                 "component object")

    ## module-type component
    path1 <- componentPath(comp1, outdir)
    expect_identical(path1, file.path(outdir, getName(comp1)))

    ## pipeline-type component
    path2 <- componentPath(comp2, outdir)
    expect_identical(path2, file.path(outdir, getName(comp2)))
})

test_that("input() produces appropriate object", {
    pipe1 <- pipe("A", "C", "mod1", "url")
    pipe2 <- pipe("A", "X", "mod1", "file")
    pipe3 <- pipe("legs", "C", "mod1", "url")
    outdir <- tempfile("inputTest")
    outputList <- lapply(list(mod1 = comp1, A = comp3),
                         calculateOutputs,
                         outdir)
    
    ## fail for incorrect input
    expect_error(input(unclass(pipe1), outputList), "pipe object required")

    ## fails for missing component or ouptut
    expect_error(input(pipe2, outputList), "start output does not exist")
    expect_error(input(pipe3, outputList), "start component does not exist")

    ## returns correct object
    input1 <- input(pipe1, outputList)
    expect_true(inherits(input1, "input"))
    expect_match(input1, getVessel(getValue(comp3)$outputs$C)$ref)
})

test_that("calculateInputs() produces appropriate object", {
    pipeList <- getPipes(simpleGraph)
    componentList <- getComponents(simpleGraph)
    pipelinePath <- file.path(tempfile("calculateInputs"), "simpleGraph")

    ## error on incorrect inputs
    badPipes <- lapply(pipeList, unclass)
    badComps <- lapply(componentList, unclass)
    expect_error(calculateInputs(badPipes, componentList,
                                 pipelinePath),
                 "pipeList must contain pipe objects")
    expect_error(calculateInputs(pipeList, badComps,
                                 pipelinePath),
                 "componentList must contain component objects")

    ## correct output object
    inputList <- calculateInputs(pipeList, componentList, pipelinePath)
    expect_true(all(sapply(inputList, inherits, what = "input")))
    ## list objects correctly named for pipe component.input
    expect_true(all(sapply(
        pipeList,
        function (x, inputList) {
            name <- paste(end(x)$component, end(x)$input, sep = ".")
            name %in% names(inputList)
        },
        inputList)))
})

test_that("graphPipeline() produces appropriate object", {
    ## fails for invalid input
    expect_error(graphPipeline(unclass(simpleGraph)),
                 "pipeline object required")

    ## expected output
    graph1 <- graphPipeline(simpleGraph)
    expect_true(inherits(graph1, "graphNEL"))
    expect_equal(graph::numNodes(graph1),
                 length(getComponents(simpleGraph)))
    expect_equal(graph::numEdges(graph1),
                 length(getPipes(simpleGraph)))
    expect_match(RBGL::tsort(graph1)[1],
                 "createGraph")
    expect_match(RBGL::tsort(graph1)[3],
                 "plotGraph")
})

test_that("runComponent() returns correctly", {
    skip_on_cran()
    componentList <- getComponents(simpleGraph)
    pipelinePath <- tempfile("runComponent")
    if (!dir.exists(pipelinePath))
        dir.create(pipelinePath)
    
    ## fails for invalid input
    expect_error(runComponent(unclass(componentList[[1]]),
                              pipelinePath = pipelinePath))

    ## component with no inputs
    result1 <- runComponent(componentList[["createGraph"]],
                            pipelinePath = pipelinePath)
    expect_equal(length(result1$objects), 1)
    expect_true(inherits(result1$objects[[1]], "output"))
    expect_true(file.exists(getResult(result1$objects[[1]])))
                            
    ## component with inputs
    result2 <- runComponent(component = componentList[["layoutGraph"]],
                            inputList = list(
                                myGraph = getResult(result1$objects[[1]])),
                            pipelinePath = pipelinePath)
    expect_equal(length(result2$objects), 1)
    expect_true(inherits(result2$objects[[1]], "output"))
    expect_true(file.exists(getResult(result2$objects[[1]])))
})

test_that("runPipeline() produces expected results", {
    targetDirectory <- tempfile("runPipeline")
    if (!dir.exists(targetDirectory))
        dir.create(targetDirectory)
    
    ## fail for invalid input
    expect_error(runPipeline(unclass(simpleGraph), tempdir()),
                 "pipeline object required")
    expect_error(runPipeline(simpleGraph, tempfile()),
                 "no such target directory")

    ## correct output
    if (skipCheck) {
        skip(paste("2016-01-25 strange issues around R CMD check,",
                   "system2, Rscript, and what is returned."))
    }
    output1 <- runPipeline(simpleGraph, targetDirectory)
    expect_equal(length(output1), 3)
    expect_true(all(sapply(output1,
                           function(x) inherits(x$objects[[1]], "output"))))
    expect_true(all(sapply(output1,
                           function(x) file.exists(getResult(x$objects[[1]])))))
})

