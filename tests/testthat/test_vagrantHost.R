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
    expect_true(inherits(vh1, "vagrantHost"))
    expect_true(inherits(vh1, "moduleHost"))
    expect_match(names(vh1), "vagrantfile", all = FALSE)
    expect_match(normalizePath(vagrantfile), vh1$vagrantfile)
    expect_match(names(vh1), "hostdir", all = FALSE)
    expect_match(vh1$hostdir, dirname(vagrantfile))
    expect_match(names(vh1), "guestdir", all = FALSE)
    expect_match(vh1$guestdir, "/vagrant")

    ## explicit hostdir and guestdir
    vh2 <- vagrantHost(vagrantfile = vagrantfile, hostdir = hostdir,
                       guestdir = guestdir)
    expect_true(inherits(vh2, "vagrantHost"))
    expect_true(inherits(vh2, "moduleHost"))
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
    expect_true(inherits(vh1, "vagrantHost"))
    expect_match(vh1$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh1$hostdir, dirname(vagrantfile))
    expect_match(vh1$guestdir, "/vagrant")

    ## vagrantfile and hostdir
    vh2 <- readVagrantHostXML(vhXML2)
    expect_true(inherits(vh1, "vagrantHost"))
    expect_match(vh2$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh2$hostdir, normalizePath(hostdir))
    expect_match(vh2$guestdir, "/vagrant")

    ## vagrantfile, hostdir, guestdir
    vh3 <- readVagrantHostXML(vhXML3)
    expect_true(inherits(vh3, "vagrantHost"))
    expect_match(vh3$vagrantfile, normalizePath(vagrantfile))
    expect_match(vh3$hostdir, normalizePath(hostdir))
    expect_match(vh3$guestdir, guestdir)
})
