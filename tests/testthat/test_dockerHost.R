library(conduit)
context("Check dockerHost functions work")

## skip tests which require a dockerHost image rocker/r-base
skipHost <- FALSE

dockerImage <- "rocker/r-base"
guestdir <- "/data/dir"

test_that("dockerHost() constructor works right", {
    ## fail for invalid arguents
    expect_error(dockerHost(image = c(dockerImage, "string")),
                 "image must be a length one character string")
    expect_error(dockerHost(image = dockerImage,
                            guestdir = c("/data", "/vagrant")),
                 "guestdir must be a length one character string")

    ## automatic guestdir
    dh1 <- dockerHost(image = dockerImage)
    expect_is(dh1, "dockerHost")
    expect_is(dh1, "dockerHost")
    expect_match(names(dh1), "image", all = FALSE)
    expect_match(names(dh1), "guestdir", all = FALSE)
    expect_match(dh1$guestdir, "/home/conduit")

    ## explicit guestdir
    dh2 <- dockerHost(image = dockerImage,
                      guestdir = guestdir)
    expect_is(dh2, "dockerHost")
    expect_is(dh2, "dockerHost")
    expect_match(names(dh2), "image", all = FALSE)
    expect_match(names(dh2), "guestdir", all = FALSE)
    expect_match(dh2$guestdir, guestdir)
})

test_that("readDockerHostXML() returns correctly", {
    library(XML)
    dhXML1 <- newXMLNode(
        name = "docker",
        attrs = list(image = dockerImage))
    dhXML2 <- newXMLNode(
        name = "docker",
        attrs = list(
            image = dockerImage,
            guestdir = guestdir))

    ## fail for invalid argument
    expect_error(readDockerHostXML(newXMLNode("doocker")),
                 "docker element required")

    ## image only
    dh1 <- readDockerHostXML(dhXML1)
    expect_is(dh1, "dockerHost")
    expect_match(dh1$image, dockerImage)
    expect_match(dh1$guestdir, "/home/conduit")

    ## image and guestdir
    dh2 <- readDockerHostXML(dhXML2)
    expect_is(dh2, "dockerHost")
    expect_match(dh2$image, dockerImage)
    expect_match(dh2$guestdir, guestdir)
})

## test_that("moduleHostToXML.vagrantHost() creates correct XML", {
##     library(XML)
##     vh1 <- vagrantHost(vagrantfile)
##     vh2 <- vagrantHost(vagrantfile, hostdir)
##     vh3 <- vagrantHost(vagrantfile, hostdir, guestdir)

##     ## just vagrantfile
##     hostNode1 <- moduleHostToXML(vh1)
##     child1 <- xmlChildren(hostNode1)[[1]]
##     attrs1 <- xmlAttrs(child1)
##     expect_is(hostNode1, "XMLInternalElementNode")
##     expect_match(xmlName(hostNode1), "host")
##     expect_match(xmlName(child1), "vagrant")
##     expect_match(attrs1[["vagrantfile"]], normalizePath(vagrantfile))
##     expect_match(attrs1[["hostdir"]], dirname(vagrantfile))
##     expect_match(attrs1[["guestdir"]], "/vagrant")

##     ## vagrantfile and hostdir
##     hostNode2 <- moduleHostToXML(vh2)
##     child2 <- xmlChildren(hostNode2)[[1]]
##     attrs2 <- xmlAttrs(child2)
##     expect_is(hostNode2, "XMLInternalElementNode")
##     expect_match(xmlName(hostNode2), "host")
##     expect_match(xmlName(child2), "vagrant")
##     expect_match(attrs2[["vagrantfile"]], normalizePath(vagrantfile))
##     expect_match(attrs2[["hostdir"]], normalizePath(hostdir))
##     expect_match(attrs2[["guestdir"]], "/vagrant")

##     ## vagrantfile hostdir guestdir
##     hostNode3 <- moduleHostToXML(vh3)
##     child3 <- xmlChildren(hostNode3)[[1]]
##     attrs3 <- xmlAttrs(child3)
##     expect_is(hostNode3, "XMLInternalElementNode")
##     expect_match(xmlName(hostNode3), "host")
##     expect_match(xmlName(child3), "vagrant")
##     expect_match(attrs3[["vagrantfile"]], normalizePath(vagrantfile))
##     expect_match(attrs3[["hostdir"]], normalizePath(hostdir))
##     expect_match(attrs3[["guestdir"]], guestdir)
## })

## test_that("prepareModuleHost.vagrantHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a vagrantHost running at",
##                    "~/vagrant/vagrant-conduit/Vagrantfile"))
##     }
    
##     vagrantHost1 <- vagrantHost(vagrantfile = vagrantfile)
##     moduleName1 <- "mod1"
##     modulePath1 <- tempfile("modulePath")
##     if (!dir.exists(modulePath1))
##         dir.create(modulePath1)
##     inputs1 <- sapply(1:3, function(x) tempfile(tmpdir = modulePath1))
##     system2("touch", args = inputs1)
##     outputLocation1 <-
##         prepareModuleHost(moduleHost = vagrantHost1,
##                           moduleName = moduleName1,
##                           modulePath = modulePath1)
##     realLocation1 <- file.path(vagrantHost1$hostdir, outputLocation1)
##     expect_true(dir.exists(realLocation1))
##     expect_is(outputLocation1, "vagrantHostOutputLocation")
##     expect_is(outputLocation1, "outputLocation")
##     ## correct defined as contents of realLocation1 now same as
##     ## modulePath1
##     lapply(list.files(modulePath1),
##            function (x) {
##                expect_match(list.files(realLocation1), x, all = FALSE)
##            })
## })

## test_that("executeCommand.vagrantHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a vagrantHost running at",
##                    "~/vagrant/vagrant-conduit/Vagrantfile"))
##     }

##     vagrantHost1 <- vagrantHost(vagrantfile = vagrantfile)
##     mod1 <- module(name = "mod1",
##                    language = "R",
##                    host = vagrantHost1,
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
##     outputLocation1 <- prepareModuleHost(moduleHost = vagrantHost1,
##                                         moduleName = getName(mod1),
##                                         modulePath = modulePath1)
##     exec_result <- executeCommand.vagrantHost(moduleHost = vagrantHost1,
##                                               outputLocation = outputLocation1,
##                                               command = command1)
##     ## correct defined as not getting error message from wrapped
##     ## system2 call
##     expect_equal(exec_result, 0)
## })

## test_that("retrieveModuleHost.vagrantHost() returns correctly", {
##     if (skipHost) {
##         skip(paste("tests requires a vagrantHost running at",
##                    "~/vagrant/vagrant-conduit/Vagrantfile"))
##     }
##     vagrantHost1 <- vagrantHost(vagrantfile = vagrantfile)
##     mod1 <- loadModule(name = "mod1",
##                        ref = system.file(
##                            "extdata", "simpleGraph",
##                            "createGraph.xml",
##                            package = "conduit"))
##     mod1$host <- vagrantHost1
##     modulePath1 <- tempfile("modulePath")
##     if (!dir.exists(modulePath1))
##         dir.create(modulePath1)
##     oldwd <- setwd(modulePath1)
##     on.exit(setwd(modulePath1))
##     command1 <- command(prepareScript(mod1))
##     outputLocation1 <- prepareModuleHost(moduleHost = vagrantHost1,
##                                         moduleName = getName(mod1),
##                                         modulePath = modulePath1)
##     realLocation1 <- file.path(vagrantHost1$hostdir, outputLocation1)
##     exec_result <- executeCommand.vagrantHost(moduleHost = vagrantHost1,
##                                               outputLocation = outputLocation1,
##                                               command = command1)
##     retrieveModuleHost.vagrantHost(moduleHost = vagrantHost1,
##                                    outputLocation = outputLocation1,
##                                    modulePath = modulePath1)
##     ## correct defined as contents of modulePath1 and realLocation
##     ## being the same, assuming they were not before execution
##     lapply(list.files(realLocation1),
##            function (x) {
##                expect_match(list.files(modulePath1), x, all = FALSE)
##            })
## })
