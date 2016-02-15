library(conduit)
context("test module and pipeline return components")

modulePath <- tempfile("moduleResult")
if (!dir.exists(modulePath))
    dir.create(modulePath)
f1 <- moduleOutput("f1", fileVessel("f1.txt"),
                   ioFormat("text file"))
fileoutput <- output(f1,
                     language = "R",
                     outputDirectory = modulePath)
u1 <- moduleOutput("u1", urlVessel("http://openapi.net"),
                                 ioFormat("HTML file"))
urloutput <- output(u1,
                    language = "python",
                    outputDirectory = modulePath)
i1 <- moduleOutput("i1", internalVessel("x"),
                   ioFormat("python array"))
internaloutput <- output(i1,
                         language = "python",
                         outputDirectory = modulePath)
pipelinePath <- tempfile("pipelineResult")
if (!dir.exists(pipelinePath))
    dir.create(pipelinePath)
moduleComp <- component(value = module("m1", "R"),
                        vessel = fileVessel("./m1/m1.xml"))
compRes1 <- runComponent(moduleComp, pipelinePath = pipelinePath)

test_that("resultInput() returns correctly", {
    ## fails for invalid arguments
    expect_error(resultInput(unclass(fileoutput), modulePath),
                 "output object required")
    expect_error(resultInput(fileoutput, tempfile()),
                 "modulePath does not exist")

    ## returns correctly for file output
    res1 <- resultInput(fileoutput, modulePath)
    expect_true(inherits(res1, "moduleInput"))
    expect_match(getVessel(res1)$ref, getVessel(fileoutput)$ref)

    ## returns correctly for url output
    res2 <- resultInput(urloutput, modulePath)
    expect_null(res2)

    ## returns correctly for internal output
    res3 <- resultInput(internaloutput, modulePath)
    expect_true(inherits(res3, "moduleInput"))
    expect_match(getType(getVessel(res3)), "fileVessel")
    expect_match(getVessel(res3)$ref,
                 paste0(getVessel(internaloutput)$ref,
                       internalExtension(getLanguage(internaloutput))))
})

test_that("resultSource() returns correctly", {
    ## fails for invalid arguments
    expect_error(resultSource(unclass(fileoutput), modulePath),
                 "output object required")
    expect_error(resultSource(fileoutput, tempfile()),
                 "modulePath does not exist")

    ## returns correctly for file output
    res1 <- resultSource(fileoutput, modulePath)
    expect_null(res1)

    ## returns correctly for url output
    res2 <- resultSource(urloutput, modulePath)
    expect_null(res2)

    ## returns correctly for internal output
    res3 <- resultSource(internaloutput, modulePath)
    expect_true(inherits(res3, "moduleSource"))
})

test_that("resultOutput() returns correctly", {
    ## fail for invalid arguments
    expect_error(resultOutput(unclass(fileoutput)),
                 "output object required")

    ## returns correctly for file output
    res1 <- resultOutput(fileoutput)
    expect_true(inherits(res1, "moduleOutput"))
    expect_match(getType(getVessel(res1)),
                 getType(getVessel(fileoutput)))

    ## returns correctly for url output
    res2 <- resultOutput(urloutput)
    expect_true(inherits(res2, "moduleOutput"))
    expect_match(getType(getVessel(res2)),
                 getType(getVessel(urloutput)))

    ## returns correctly for internal output
    res3 <- resultOutput(internaloutput)
    expect_true(inherits(res3, "moduleOutput"))
    expect_match(getType(getVessel(res3)),
                 getType(getVessel(internaloutput)))
})

test_that("moduleResult() returns correctly", {
    objects <- list(fileoutput, urloutput, internaloutput)
    module <- module("m1", "R", outputs = list(f1, u1, i1))

    ## fail for invalid arguments
    expect_error(moduleResult(list(unclass(objects[[1]]),
                                   objects[[2]], objects[[3]]),
                              modulePath,
                              module),
                 "outputList must be 'output' objects")
    expect_error(moduleResult(objects,
                              tempfile(),
                              module),
                 "modulePath does not exist")
    expect_error(moduleResult(objects,
                              modulePath,
                              unclass(module)),
                 "module object required")

    ## correct output
    res1 <- moduleResult(objects, modulePath, module)
    expect_true(inherits(res1, "moduleResult"))

    ## module XML file
    expect_match(names(res1), "file", all = FALSE)
    expect_true(file.exists(res1$file))

    ## module object
    expect_match(names(res1), "component", all = FALSE)
    expect_true(inherits(res1$component, "module"))

    ## output objects
    expect_match(names(res1), "outputList", all = FALSE)
    expect_true(all(sapply(res1$objects, inherits, what = "output")))
})

test_that("resultComponent() returns correctly", {
    ## fail for invalid arguments
    expect_error(resultComponent(unclass(compRes1), pipelinePath))
    expect_error(resultComponent(compRes1, tempfile()))

    ## module component
    comp1 <- resultComponent(compRes1, pipelinePath)
    expect_true(inherits(comp1, "component"))
})
