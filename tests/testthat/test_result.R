library(conduit)
context("test module and pipeline return components")

modulePath <- tempfile("moduleResult")
if (!dir.exists(modulePath)) dir.create(modulePath)
fileoutput <- output(moduleOutput("f1", fileVessel("f1.txt"),
                                  ioFormat("text file")),
                     language = "R",
                     outputDirectory = modulePath)
urloutput <- output(moduleOutput("u1", urlVessel("http://openapi.net"),
                                 ioFormat("HTML file")),
                    language = "python",
                    outputDirectory = modulePath)
internaloutput <- output(moduleOutput("i1", internalVessel("x"),
                                      ioFormat("python array")),
                         language = "python",
                         outputDirectory = modulePath)

test_that("resultInput() returns correctly", {
    ## fails for invalid arguments
    expect_error(resultInput(unclass(fileoutput), modulePath),
                 "output object required")
    expect_error(resultInput(fileoutput, tempfile()),
                 "modulePath does not exist")

    ## returns correctly for file output
    res1 <- resultInput(fileoutput, modulePath)
    expect_true(inherits(res1, "moduleInput"))
    expect_match(getVessel(res1)$ref, getVessel(fileoutput)$ref)

    ## returns correctly for url output
    res2 <- resultInput(urloutput, modulePath)
    expect_null(res2)

    ## returns correctly for internal output
    res3 <- resultInput(internaloutput, modulePath)
    expect_true(inherits(res3, "moduleInput"))
    expect_match(getVessel(res3)$ref,
                 paste0(getVessel(internaloutput)$ref,
                       internalExtension(getLanguage(internaloutput))))
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
    expect_true(inherits(res3, "moduleSource"))
})
