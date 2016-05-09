library(conduit)
context("create and use moduleInputHost objects")

name <- "myHost"

test_that("moduleInputHost() constructor works right", {
    ## fail for invalid arguments
    expect_error(moduleInputHost(name = c("/data", "/moduleInput")),
                 "name must be a length one character string")

    mih1 <- moduleInputHost(name = name)
    expect_is(mih1, "moduleInputHost")
    expect_is(mih1, "moduleHost")
    expect_match(names(mih1), "name", all = FALSE)
    expect_match(name, mih1$name)
})

test_that("readModuleInputHostXML() returns correctly", {
    library(XML)
    mihXML1 <- newXMLNode(
        name = "moduleInput",
        attrs = list(name = name))

    ## fail for invalid argument
    expect_error(readModuleInputHostXML(newXMLNode("mooduleInputHost")),
                 "moduleInput element required")

    mih1 <- readModuleInputHostXML(mihXML1)
    expect_is(mih1, "moduleInputHost")
    expect_match(mih1$name, name)
})

test_that("moduleHostToXML.moduleInputHost() creates correct XML", {
    library(XML)
    mih1 <- moduleInputHost(name = name)

    hostNode1 <- moduleHostToXML(mih1)
    child1 <- xmlChildren(hostNode1)[[1]]
    attrs1 <- xmlAttrs(child1)
    expect_is(hostNode1, "XMLInternalElementNode")
    expect_match(xmlName(hostNode1), "host")
    expect_match(xmlName(child1), "moduleInput")
    expect_match(attrs1[["name"]], name)
})

## test_that("prepareModuleHost.moduleInputHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a moduleInputHost running at",
##                    "~/moduleInput/moduleInput-conduit/ModuleInputfile"))
##     }
    
##     moduleInputHost1 <- moduleInputHost(moduleInputfile = moduleInputfile)
##     moduleName1 <- "mod1"
##     modulePath1 <- tempfile("modulePath")
##     if (!dir.exists(modulePath1))
##         dir.create(modulePath1)
##     inputs1 <- sapply(1:3, function(x) tempfile(tmpdir = modulePath1))
##     system2("touch", args = inputs1)
##     outputLocation1 <-
##         prepareModuleHost(moduleHost = moduleInputHost1,
##                           moduleName = moduleName1,
##                           modulePath = modulePath1)
##     realLocation1 <- file.path(moduleInputHost1$hostdir, outputLocation1)
##     expect_true(dir.exists(realLocation1))
##     expect_is(outputLocation1, "moduleInputHostOutputLocation")
##     expect_is(outputLocation1, "outputLocation")
##     ## correct defined as contents of realLocation1 now same as
##     ## modulePath1
##     lapply(list.files(modulePath1),
##            function (x) {
##                expect_match(list.files(realLocation1), x, all = FALSE)
##            })
## })

## test_that("executeCommand.moduleInputHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a moduleInputHost running at",
##                    "~/moduleInput/moduleInput-conduit/ModuleInputfile"))
##     }

##     moduleInputHost1 <- moduleInputHost(moduleInputfile = moduleInputfile)
##     mod1 <- module(name = "mod1",
##                    language = "R",
##                    host = moduleInputHost1,
##                    sources = list(moduleSource(scriptVessel("x <- 1:10"))),
##                    outputs = list(moduleOutput(
##                        name = "x",
##                        vessel = internalVessel("x"),
##                        format = ioFormat("R numeric vector"))))
##     modulePath1 <- tempfile("modulePath")
##     if (!dir.exists(modulePath1))
##         dir.create(modulePath1)
##     oldwd <- setwd(modulePath1)
##     on.exit(setwd(modulePath1))
##     command1 <- command(prepareScript(mod1))
##     outputLocation1 <- prepareModuleHost(moduleHost = moduleInputHost1,
##                                         moduleName = getName(mod1),
##                                         modulePath = modulePath1)
##     exec_result <- executeCommand.moduleInputHost(moduleHost = moduleInputHost1,
##                                               outputLocation = outputLocation1,
##                                               command = command1)
##     ## correct defined as not getting error message from wrapped
##     ## system2 call
##     expect_equal(exec_result, 0)
## })

## test_that("retrieveModuleHost.moduleInputHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a moduleInputHost running at",
##                    "~/moduleInput/moduleInput-conduit/ModuleInputfile"))
##     }
##     moduleInputHost1 <- moduleInputHost(moduleInputfile = moduleInputfile)
##     mod1 <- loadModule(name = "mod1",
##                        ref = system.file(
##                            "extdata", "test_pipeline",
##                            "module1.xml",
##                            package = "conduit"))
##     mod1$host <- moduleInputHost1
##     modulePath1 <- tempfile("modulePath")
##     if (!dir.exists(modulePath1))
##         dir.create(modulePath1)
##     oldwd <- setwd(modulePath1)
##     on.exit(setwd(modulePath1))
##     command1 <- command(prepareScript(mod1))
##     outputLocation1 <- prepareModuleHost(moduleHost = moduleInputHost1,
##                                         moduleName = getName(mod1),
##                                         modulePath = modulePath1)
##     realLocation1 <- file.path(moduleInputHost1$hostdir, outputLocation1)
##     exec_result <- executeCommand.moduleInputHost(moduleHost = moduleInputHost1,
##                                               outputLocation = outputLocation1,
##                                               command = command1)
##     x <- retrieveModuleHost.moduleInputHost(moduleHost = moduleInputHost1,
##                                         outputLocation = outputLocation1,
##                                         modulePath = modulePath1)
##     expect_null(x)
##     ## correct defined as contents of modulePath1 and realLocation
##     ## being the same, assuming they were not before execution
##     lapply(list.files(realLocation1),
##            function (x) {
##                expect_match(list.files(modulePath1), x, all = FALSE)
##            })
## })
