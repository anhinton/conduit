library(conduit)
context("create and use dockerHost objects")

## skip tests which require a dockerHost image rocker/r-base
skipHost <- TRUE

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
    #library(XML)
    dhXML1 <- newXMLNode(
        name = "docker",
        attrs = list(image = dockerImage),
        addFinalizer = TRUE)
    dhXML2 <- newXMLNode(
        name = "docker",
        attrs = list(
            image = dockerImage,
            guestdir = guestdir),
        addFinalizer = TRUE)

    ## fail for invalid argument
    expect_error(readDockerHostXML(newXMLNode("doocker", addFinalizer = TRUE)),
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

test_that("moduleHostToXML.dockerHost() creates correct XML", {
    #library(XML)
    dh1 <- dockerHost(image = dockerImage)
    dh2 <- dockerHost(image = dockerImage, guestdir = guestdir)

    ## just image
    hostNode1 <- moduleHostToXML(dh1)
    child1 <- xmlChildren(hostNode1)[[1]]
    attrs1 <- xmlAttrs(child1)
    expect_is(hostNode1, "XMLInternalElementNode")
    expect_match(xmlName(hostNode1), "host")
    expect_match(xmlName(child1), "docker")
    expect_match(attrs1[["image"]], dockerImage)
    expect_match(attrs1[["guestdir"]], "/home/conduit")

    ## image and guestdir
    hostNode2 <- moduleHostToXML(dh2)
    child2 <- xmlChildren(hostNode2)[[1]]
    attrs2 <- xmlAttrs(child2)
    expect_is(hostNode2, "XMLInternalElementNode")
    expect_match(xmlName(hostNode2), "host")
    expect_match(xmlName(child2), "docker")
    expect_match(attrs2[["image"]], dockerImage)
    expect_match(attrs2[["guestdir"]], guestdir)
})

test_that("prepareModuleHost.dockerHost() returns correctly", {
    dockerHost1 <- dockerHost(image = dockerImage)
    moduleName1 <- "mod1"
    modulePath1 <- tempfile("modulePath")
    if (!dir.exists(modulePath1))
        dir.create(modulePath1)
    outputLocation1 <-
        prepareModuleHost(moduleHost = dockerHost1,
                          moduleName = moduleName1,
                          modulePath = modulePath1)
    expect_is(outputLocation1, "dockerHostOutputLocation")
    expect_is(outputLocation1, "outputLocation")
    ## correct defined as an empty string
    expect_match("", outputLocation1)
})

test_that("executeCommand.dockerHost() returns correctly", {
    if (skipHost) {
        skip(paste("test requires the R user be in the docker group,",
                   "with dockerHost image \"rocker/r-base\""))
    }

    dockerHost1 <- dockerHost(image = dockerImage)
    mod1 <- module(name = "mod1",
                   language = "R",
                   host = dockerHost1,
                   sources = list(moduleSource(scriptVessel("x <- 1:10"))),
                   outputs = list(moduleOutput(
                       name = "x",
                       vessel = internalVessel("x"),
                       format = ioFormat("R numeric vector"))))
    modulePath1 <- tempfile("modulePath")
    if (!dir.exists(modulePath1))
        dir.create(modulePath1)
    oldwd <- setwd(modulePath1)
    on.exit(setwd(modulePath1))
    command1 <- command(prepareScript(mod1))
    outputLocation1 <- prepareModuleHost(moduleHost = dockerHost1,
                                         moduleName = getName(mod1),
                                         modulePath = modulePath1)
    exec_result <- executeCommand.dockerHost(moduleHost = dockerHost1,
                                             outputLocation = outputLocation1,
                                             command = command1)
    ## correct defined as not getting error message from wrapped
    ## system2 call
    expect_is(exec_result, "character")
    expect_true(is.null(attr(exec_result, "status")))
})

test_that("retrieveModuleHost.dockerHost() returns correctly", {
    if (skipHost) {
        skip(paste("test requires the R user be in the docker group,",
                   "with dockerHost image \"rocker/r-base\""))
    }
    
    dockerHost1 <- dockerHost(image = dockerImage)
    mod1 <- loadModule(name = "mod1",
                       ref = system.file(
                           "extdata", "test_pipeline",
                           "module1.xml",
                           package = "conduit"))
    mod1$host <- dockerHost1
    modulePath1 <- tempfile("modulePath")
    if (!dir.exists(modulePath1))
        dir.create(modulePath1)
    oldwd <- setwd(modulePath1)
    on.exit(setwd(modulePath1))
    command1 <- command(prepareScript(mod1))
    outputLocation1 <- prepareModuleHost(moduleHost = dockerHost1,
                                         moduleName = getName(mod1),
                                         modulePath = modulePath1)
    realLocation1 <- file.path(dockerHost1$hostdir, outputLocation1)
    exec_result <- executeCommand.dockerHost(moduleHost = dockerHost1,
                                             outputLocation = outputLocation1,
                                             command = command1)
    x <- retrieveModuleHost.dockerHost(moduleHost = dockerHost1,
                                  outputLocation = outputLocation1,
                                  modulePath = modulePath1)
    ## correct defined as returning NULL as this function actually
    ## does nothing
    expect_null(x)
})
