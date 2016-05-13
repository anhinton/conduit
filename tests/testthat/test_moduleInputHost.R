library(conduit)
context("create and use moduleInputHost objects")

## skip tests which require a vagrantHost be available with
## Vagrantfile at ~/vagrant/vagrant-conduit/Vagrantfile
skipVagrantHost <- TRUE

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

test_that("conduit can run a module with a moduleInputHost", {
    if (skipVagrantHost) {
        skip(paste("tests requires a vagrantHost running at",
                   "~/vagrant/vagrant-conduit/Vagrantfile"))
    }
    mod1 <- loadModule("createHost",
                       system.file("extdata", "moduleInputHostDemo",
                                   "createHost.xml",
                                   package = "conduit"))
    mod2 <- loadModule("executeHost",
                       system.file("extdata", "moduleInputHostDemo",
                                   "executeHost.xml",
                                   package = "conduit"))
    res1 <- runModule(module = mod1, targetDirectory = tempdir())
    inputObjects1 <- list(vagrantHost = res1$outputList$vagrantHost$ref)

    res2 <- runModule(module = mod2, targetDirectory = tempdir(),
                      inputObjects = inputObjects1)
    expect_is(res2, "moduleResult")
    lapply(names(mod2$outputs), function(n) {
        expect_match(names(res2$outputList), n, all = FALSE)
    })
})
