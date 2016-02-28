library(conduit)
context("Check vagrantHost functions work")

## skip tests which require a module host machine
## requires conduit host at vagrant@127.0.0.1:2222
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
    expect_is(hostNode1, "XMLInternalElementNode")
    expect_match(xmlName(hostNode1), "host")
    expect_match(xmlName(child1), "vagrant")
})
