library(conduit)
context("test module and pipeline result objects")

modulePath <- tempfile("moduleResult")
if (!dir.exists(modulePath))
    dir.create(modulePath)
f1 <- moduleOutput("f1", fileVessel("f1.txt"),
                   ioFormat("text file"))
fileoutput <- output(f1,
                     moduleLanguage = moduleLanguage("R"),
                     outputDirectory = modulePath)
u1 <- moduleOutput("u1", urlVessel("http://openapi.net"),
                                 ioFormat("HTML file"))
urloutput <- output(u1,
                    moduleLanguage = moduleLanguage("python"),
                    outputDirectory = modulePath)
i1 <- moduleOutput("i1", internalVessel("x"),
                   ioFormat("python array"))
internallang = moduleLanguage("python")
internaloutput <- output(i1,
                         moduleLanguage = internallang,
                         outputDirectory = modulePath)
pipelinePath <- tempfile("pipelineResult")
if (!dir.exists(pipelinePath))
    dir.create(pipelinePath)
moduleComp <- component(value = module("m1", moduleLanguage("R")),
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
    expect_is(res1, "moduleInput")
    expect_match(getVessel(res1)$ref, getVessel(fileoutput)$ref)

    ## returns correctly for url output
    res2 <- resultInput(urloutput, modulePath)
    expect_null(res2)

    ## returns correctly for internal output
    res3 <- resultInput(internaloutput, modulePath)
    expect_is(res3, "moduleInput")
    expect_match(getType(getVessel(res3)), "fileVessel")
    expect_match(getVessel(res3)$ref,
                 paste0(getVessel(internaloutput)$ref,
                        internalExtension(internallang)))
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
    expect_is(res3, "moduleSource")
})

test_that("resultOutput() returns correctly", {
    ## fail for invalid arguments
    expect_error(resultOutput(unclass(fileoutput)),
                 "output object required")

    ## returns correctly for file output
    res1 <- resultOutput(fileoutput)
    expect_is(res1, "moduleOutput")
    expect_match(getType(getVessel(res1)),
                 getType(getVessel(fileoutput)))

    ## returns correctly for url output
    res2 <- resultOutput(urloutput)
    expect_is(res2, "moduleOutput")
    expect_match(getType(getVessel(res2)),
                 getType(getVessel(urloutput)))

    ## returns correctly for internal output
    res3 <- resultOutput(internaloutput)
    expect_is(res3, "moduleOutput")
    expect_match(getType(getVessel(res3)),
                 getType(getVessel(internaloutput)))
})

test_that("moduleResult() returns correctly", {
    objects <- list(fileoutput, urloutput, internaloutput)
    module <- module("m1", moduleLanguage("R"), outputs = list(f1, u1, i1))

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
    expect_is(res1, "moduleResult")
    expect_is(res1, "componentResult")

    ## name
    expect_match(names(res1), "name", all = FALSE)
    expect_match(getName(res1), getName(module))

    ## module XML file
    expect_match(names(res1), "file", all = FALSE)
    expect_true(file.exists(res1$file))

    ## module object
    expect_match(names(res1), "component", all = FALSE)
    expect_is(res1$component, "module")

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
    expect_is(comp1, "component")
})

test_that("pipelineResult() returns correctly", {
    outputList1 <- list(fileoutput)
    module1 <- module("m1", moduleLanguage("R"), outputs = list(f1))
    modRes1 <- moduleResult(outputList1, modulePath, module1)
    outputList2 <- list(urloutput)
    module2 <- module("m2", moduleLanguage("python"), outputs = list(u1))
    modRes2 <- moduleResult(outputList2, modulePath, module2)
    componentResultList <- list(modRes1, modRes2)
    pipeline <- pipeline("p1", components = list(module1, module1))

    ## fail for invalid arguments
    expect_error(pipelineResult(list(unclass(modRes1),
                                     modRes2),
                                pipelinePath,
                                pipeline),
                 "componentResultList must contain componentResult objects")
    expect_error(pipelineResult(componentResultList, tempfile(), pipeline),
                 "pipelinePath does not exist")
    expect_error(pipelineResult(componentResultList, pipelinePath,
                                unclass(pipeline)),
                 "pipeline must be a pipeline object")

    ## correct output
    res1 <- pipelineResult(componentResultList, pipelinePath,
                           pipeline)
    expect_is(res1, "pipelineResult")
    expect_is(res1, "componentResult")

    ## name
    expect_match(names(res1), "name", all = FALSE)
    expect_match(getName(res1), getName(pipeline))

    ## file
    expect_match(names(res1), "file", all = FALSE)
    expect_true(file.exists(res1$file))

    ## component
    expect_match(names(res1), "component", all = FALSE)
    expect_is(res1$component, "pipeline")

    ## componentResultList
    expect_match(names(res1), "componentResultList", all = FALSE)
    expect_true(all(sapply(X = res1$componentResultList,
                           FUN = inherits,
                           what = "componentResult")))
    
    ## outputList
    expect_match(names(res1), "outputList", all = FALSE)
    expect_true(all(sapply(
        X = res1$outputList,
        FUN = function (x) {
            sapply(X = x, FUN = inherits, what = "output")
        })))
})

test_that("can export and import componentResult objects", {
    ## skip these tests on CRAN cause of lots of file and archive reading
    ## and writing
    skip_on_cran()
    
    p1 <- loadPipeline("p1", system.file("extdata", "simpleGraph",
                                         "pipeline.xml",
                                         package = "conduit"))
    pipelinetarg <- tempfile("exportComponentList")
    res1 <- runPipeline(p1, targetDirectory = tempdir())
    res2 <- res1$componentResultList[[1]]
    targ <- tempfile("export.componentResult")
    if (!dir.exists(targ))
        dir.create(targ)

    ## fail for invalid arguments
    expect_error(export(res1, tempfile()),
                 "targetDirectory does not exist")

    ## pipelineResult
    tar1 <- export(res1, targ)
    expect_true(file.exists(tar1))
    impdir <- tempfile("import")

    ## moduleResult
    tar2 <- export(res2, targ)
    expect_true(file.exists(tar2))

    ## successfully import from pipeline tarfile
    importDir <- tempfile("importTest")
    if (!dir.exists(importDir))
        dir.create(importDir)
    import1 <- importPipeline(tar1)
    expect_is(import1, "pipeline")
    expect_match(getName(import1), getName(p1))

    ## set pipeline name
    impName2 <- tempfile("rondtondly")
    import2 <- importPipeline(tarfile = tar1, name = impName2)
    expect_is(import2, "pipeline")
    expect_match(getName(import2), impName2)

    ## successfully import from module tarfile
    import3 <- importModule(tar2)
    expect_is(import3, "module")
    expect_match(getName(import3), getName(res2$component))

    ## set module name
    impName4 <- tempfile("alabluxo")
    import4 <- importModule(tarfile = tar2, name = impName4)
    expect_is(import4, "module")
    expect_match(getName(import4), impName4)
})
