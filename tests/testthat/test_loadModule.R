library(conduit)
context("load module from XML file")

test_that("loadModule() fails for non-existent file", {
    skip_on_cran()
    expect_error(
        loadModule(
            name = "failtest",
            ref = tempfile(pattern = "doesnotexits",
                tmpdir = tempdir())),
        "Unable to load module")
})

test_that("loadModule() handles ref, no path", {
    skip_on_cran()
    ref1 <- system.file("extdata", "simpleGraph", "createGraph.xml",
                        package = "conduit")
    dir1 <- dirname(ref1)
    mod1 <- loadModule(name = "mod1",
                       ref = ref1)
    expect_match(class(mod1), "module")
    expect_match(attr(mod1, "location"), dir1)
})

test_that("loadModule() handles ref and path", {
    skip_on_cran()
    ref2 <- "layoutGraph.xml"
    path2 <- system.file("extdata", "simpleGraph", package = "conduit")
    dir2 <-  system.file("extdata", "simpleGraph", package = "conduit")
    mod2 <- loadModule("mod2", ref2, path2)
    expect_match(class(mod2), "module")
    expect_match(attr(mod2, "location"), dir2)
})
