library(conduit)
context("execute pipelines")

lang = "R"
outdir <- tempdir()
symbol <- "x"
internal_output <- moduleOutput(
    "internal", internalVessel(symbol), ioFormat("nonsense"))
url <- "https://github.com/anhinton/conduit"
url_output <- moduleOutput(
    "url", urlVessel(url), ioFormat("HTML file"))
file <- "output.csv"
file_output <- moduleOutput(
    "file", fileVessel(file), ioFormat("CSV file"))
mod1 <- module("mod1", language = lang,
               outputs = list(internal_output, url_output, file_output))
comp1 <- component(value = mod1)
p1 <- pipeline("p1", components = list(comp1))
comp2 <- component(value = p1)
                                 

test_that("calculateOutputs() produces correct output", {
    ## fails for incorrect object
    expect_error(calculateOutputs(unclass(comp1)),
                 "component object")

    ## module-type component
    outputs <- calculateOutputs(comp1, outdir)
    for (i in seq_along(outputs)) {
        expect_true(inherits(outputs[[i]], "output"))
    }
    expect_match(names(outputs), internal_output$name, all = FALSE)
    expect_match(names(outputs), file_output$name, all = FALSE)
    expect_match(names(outputs), url_output$name, all = FALSE)

    ## pipeline-type component
    expect_error(calculateOutputs(comp2, outdir),
                 "component type not supported")
})

test_that("componentPath() returns correct output", {
    ## fails for incorrect object
    expect_error(calculateOutputs(unclass(comp1)),
                 "component object")

    ## module-type component
    path1 <- componentPath(comp1, outdir)
    expect_identical(path1, file.path(outdir, getName(comp1)))

    ## pipeline-type component
    path2 <- componentPath(comp2, outdir)
    expect_identical(path2, file.path(outdir, getName(comp2)))
})
