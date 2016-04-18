library(conduit)
context("Check vagrantHost functions work")

## skip tests which require a vagrantHost be available with
## Vagrantfilr at ~/vagrant/vagrant-conduit/Vagrantfile
skipHost <- FALSE

vagrantfile <- "~/vagrant/vagrant-conduit/Vagrantfile"
hostdir <- tempdir()
guestdir <- "/data/conduit"

test_that("vagrantHost() consructor works right", {
    ## fail for invalid arguments
    expect_error(vagrantHost(tempfile()),
                 "vagrantfile does not exist")
    expect_error(vagrantHost(vagrantfile, tempfile()),
                 "hostdir does not exist")
    expect_error(vagrantHost(vagrantfile, guestdir = c("/data", "/vagrant")),
                 "guestdir must be a length one character string")

    ## automatic hostdir
    vh1 <- vagrantHost(vagrantfile)
    expect_is(vh1, "vagrantHost")
    expect_is(vh1, "moduleHost")
    expect_match(names(vh1), "vagrantfile", all = FALSE)
    expect_match(normalizePath(vagrantfile), vh1$vagrantfile)
    expect_match(names(vh1), "hostdir", all = FALSE)
    expect_match(vh1$hostdir, dirname(vagrantfile))
    expect_match(names(vh1), "guestdir", all = FALSE)
    expect_match(vh1$guestdir, "/vagrant")

    ## explicit hostdir and guestdir
    vh2 <- vagrantHost(vagrantfile = vagrantfile, hostdir = hostdir,
                       guestdir = guestdir)
    expect_is(vh2, "vagrantHost")
    expect_is(vh2, "moduleHost")
    expect_match(names(vh2), "vagrantfile", all = FALSE)
    expect_match(normalizePath(vagrantfile), vh2$vagrantfile)
    expect_match(names(vh2), "hostdir", all = FALSE)
    expect_match(vh2$hostdir, normalizePath(hostdir))
    expect_match(names(vh2), "guestdir", all = FALSE)
    expect_match(vh2$guestdir, guestdir)
})

test_that("readVagrantHostXML() returns correctly", {
    library(XML)
    vhXML1 <- newXMLNode(
        name = "vagrant",
        attrs = list(vagrantfile = vagrantfile))
    vhXML2 <- newXMLNode(
        name = "vagrant",
        attrs = list(
            vagrantfile = vagrantfile,
            hostdir = hostdir))
    vhXML3 <- newXMLNode(
        name = "vagrant",
        attrs = list(
            vagrantfile = vagrantfile,
            hostdir = hostdir,
            guestdir = guestdir))

    ## fail for invalid argument
    expect_error(readVagrantHostXML(newXMLNode("voogrant")),
                 "vagrant element required")

    ## vagrantfile only
    vh1 <- readVagrantHostXML(vhXML1)
    expect_is(vh1, "vagrantHost")
    expect_match(vh1$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh1$hostdir, dirname(vagrantfile))
    expect_match(vh1$guestdir, "/vagrant")

    ## vagrantfile and hostdir
    vh2 <- readVagrantHostXML(vhXML2)
    expect_is(vh1, "vagrantHost")
    expect_match(vh2$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh2$hostdir, normalizePath(hostdir))
    expect_match(vh2$guestdir, "/vagrant")

    ## vagrantfile, hostdir, guestdir
    vh3 <- readVagrantHostXML(vhXML3)
    expect_is(vh3, "vagrantHost")
    expect_match(vh3$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh3$hostdir, normalizePath(hostdir))
    expect_match(vh3$guestdir, guestdir)
})

test_that("moduleHostToXML.vagrantHost() creates correct XML", {
    library(XML)
    vh1 <- vagrantHost(vagrantfile)
    vh2 <- vagrantHost(vagrantfile, hostdir)
    vh3 <- vagrantHost(vagrantfile, hostdir, guestdir)

    ## fail for invalid argument
    expect_error(moduleHostToXML.vagrantHost(unclass(vh1)),
                 "vagrantHost object required")

    ## just vagrantfile
    hostNode1 <- moduleHostToXML(vh1)
    child1 <- xmlChildren(hostNode1)[[1]]
    attrs1 <- xmlAttrs(child1)
    expect_is(hostNode1, "XMLInternalElementNode")
    expect_match(xmlName(hostNode1), "host")
    expect_match(xmlName(child1), "vagrant")
    expect_match(attrs1[["vagrantfile"]], normalizePath(vagrantfile))
    expect_match(attrs1[["hostdir"]], dirname(vagrantfile))
    expect_match(attrs1[["guestdir"]], "/vagrant")

    ## vagrantfile and hostdir
    hostNode2 <- moduleHostToXML(vh2)
    child2 <- xmlChildren(hostNode2)[[1]]
    attrs2 <- xmlAttrs(child2)
    expect_is(hostNode2, "XMLInternalElementNode")
    expect_match(xmlName(hostNode2), "host")
    expect_match(xmlName(child2), "vagrant")
    expect_match(attrs2[["vagrantfile"]], normalizePath(vagrantfile))
    expect_match(attrs2[["hostdir"]], normalizePath(hostdir))
    expect_match(attrs2[["guestdir"]], "/vagrant")

    ## vagrantfile hostdir guestdir
    hostNode3 <- moduleHostToXML(vh3)
    child3 <- xmlChildren(hostNode3)[[1]]
    attrs3 <- xmlAttrs(child3)
    expect_is(hostNode3, "XMLInternalElementNode")
    expect_match(xmlName(hostNode3), "host")
    expect_match(xmlName(child3), "vagrant")
    expect_match(attrs3[["vagrantfile"]], normalizePath(vagrantfile))
    expect_match(attrs3[["hostdir"]], normalizePath(hostdir))
    expect_match(attrs3[["guestdir"]], guestdir)
})

test_that("prepareModuleHost.vagrantHost() returns correctly", {
    vagrantHost1 <- vagrantHost(vagrantfile = vagrantfile)
    moduleName1 <- "mod1"
    modulePath1 <- tempfile("modulePath")
    if (!dir.exists(modulePath1))
        dir.create(modulePath1)
    inputs1 <- sapply(1:3, function(x) tempfile(tmpdir = modulePath1))
    system2("touch", args = inputs1)

    ## fail for invalid arguments
    expect_error(
        prepareModuleHost.vagrantHost(
            moduleHost = unclass(vagrantHost1),
            moduleName = moduleName1,
            modulePath = modulePath1),
        "vagrantHost object required")
    expect_error(
        prepareModuleHost.vagrantHost(
            moduleHost = vagrantHost1,
            moduleName = c("two", "names"),
            modulePath = modulePath1),
        "moduleName is not length 1 character")
    expect_error(
        prepareModuleHost.vagrantHost(
            moduleHost = vagrantHost1,
            moduleName = moduleName1,
            modulePath = tempfile()),
        "modulePath does not exist")

    ## success
    outputLocation1 <-
        prepareModuleHost(moduleHost = vagrantHost1,
                          moduleName = moduleName1,
                          modulePath = modulePath1)
    realLocation1 <- file.path(vagrantHost1$hostdir, outputLocation1)
    expect_true(dir.exists(realLocation1))
    expect_is(outputLocation1, "vagrantHostOutputLocation")
    expect_is(outputLocation1, "outputLocation")
    lapply(list.files(modulePath1),
           function (x) {
               expect_match(list.files(realLocation1), x, all = FALSE)
           })
})
