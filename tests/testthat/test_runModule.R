library(conduit)
context("execute modules")

targ = tempdir()
createGraph <- loadModule(
    "createGraph",
    system.file("extdata", "simpleGraph", "createGraph.xml",
                package = "conduit"))
layoutGraph <- loadModule(
    "layoutGraph",
    system.file("extdata", "simpleGraph", "layoutGraph.xml",
                package = "conduit"))

## test extractModuleSource()
test_that(
    "extractModuleSource() works",
    {
        script <- c("Sys.info()", "sessionInfo()", "getwd()")
        file <- tempfile()
        
        ## scriptVessel source
        inline_source <- moduleSource(
            scriptVessel(script))
        class(inline_source) <- class(inline_source$vessel)
        source_script <- extractModuleSource(inline_source)
        expect_equal(length(source_script), 3)
        expect_match(class(source_script), "character")
        expect_match(source_script[2], "^sessionInfo[(][)]")

        ##
        writeLines(script, file)
        file_source <- moduleSource(
            fileVessel(file))
        class(file_source) <- class(file_source$vessel)
        source_script <- extractModuleSource(file_source)
        expect_equal(length(source_script), 3)
        expect_match(class(source_script), "character")
        expect_match(source_script[3], "^getwd[(][)]")        
    })

test_that("extractModuleSource() works for <url> sources", {
    skip("requires active internet connection")
    url_source <- moduleSource(
        urlVessel("https://raw.githubusercontent.com/anhinton/conduit/master/README.md"))
    localReadme <- readLines(system.file("README.md", package = "conduit"))
    class(url_source) <- class(url_source$vessel)
    source_script <- extractModuleSource(url_source)
    expect_match(class(source_script), "character")
    expect_match(source_script[1], localReadme[1])
})

test_that("prepareInternalInput() returns correct file path", {
    input <- tempfile()
    system2("touch", input)
    symbol <- "x"
    language = moduleLanguage("python")
    outputDirectory <- tempfile("prepareInternalInput")
    if (!dir.exists(outputDirectory))
        dir.create(outputDirectory)

    ## unable to copy
    expect_error(
        suppressWarnings(prepareInternalInput(input, symbol, language,
                                              tempfile())),
        "unable to copy input into outputDirectory")

    ## success 
    internalInput <-
        prepareInternalInput(input, symbol, language, outputDirectory)
    expect_true(file.exists(internalInput))
})

test_that("prepareFileInput() works with input = NULL", {
    outputDirectory <- tempfile("prepareFileInput")
    if (!dir.exists(outputDirectory))
        dir.create(outputDirectory)
    location <- tempdir()
    ref1 <- tempfile("ref1")
    ref2 <- file.path(location, "relativeToModule.file")
    system2("touch", args = c(ref1, ref2))
    vessel1 <- fileVessel(ref = basename(ref1))
    vessel2 <- fileVessel(ref = basename(ref2))
    
    ## fails if referenced file not found
    expect_error(prepareFileInput(input = NULL,
                                  vessel = fileVessel(ref = tempfile()),
                                  outputDirectory = outputDirectory,
                                  location = location),
                 "unable to locate input file")
    ## fails if unable to copy file
    expect_error(
        suppressWarnings(
            prepareFileInput(input = NULL,
                             vessel = vessel1,
                             outputDirectory = tempfile(),
                             location = location)),
        "unable to copy input into outputDirectory")

    ## success for absolute file ref
    fileInput1 <- prepareFileInput(input = NULL,
                                   vessel = fileVessel(ref = ref1),
                                   outputDirectory = outputDirectory,
                                   location = location)
    expect_true(file.exists(fileInput1))
                                   
    ##success for relative file ref
    fileInput2 <- prepareFileInput(input = NULL,
                                   vessel = vessel2,
                                   outputDirectory = outputDirectory,
                                   location = location)
    expect_true(file.exists(fileInput2))
})

test_that("prepareFileInput() succeeds given input" , {
    outputDirectory <- tempfile("prepareFileInput")
    if (!dir.exists(outputDirectory))
        dir.create(outputDirectory)
    location <- tempdir()
    input <- tempfile("input")
    ref1 <- tempfile("ref")
    system2("touch", args = c(input, ref1))
    vessel1 <- fileVessel(ref = basename(ref1))

    ## fails if path from resolved vessel does not match input
    expect_error(prepareFileInput(input = input,
                                  vessel = fileVessel(ref = ref1),
                                  outputDirectory = outputDirectory,
                                  location = location),
                 "input does not match path given in fileVessel")
    ## fails if unable to copy file
    expect_error(
        suppressWarnings(prepareFileInput(input = input,
                                          vessel = vessel1,
                                          outputDirectory = tempfile(),
                                          location = location)),
        "unable to copy input into outputDirectory")

    ## succeeds for absolute file ref
    fileInput1 <- prepareFileInput(input = ref1,
                                   vessel = fileVessel(ref = ref1),
                                   outputDirectory = outputDirectory,
                                   location = location)
    expect_true(file.exists(fileInput1))

    ## succeeds for relative file ref
    fileInput2 <- prepareFileInput(input = input,
                                   vessel = vessel1,
                                   outputDirectory = outputDirectory,
                                   location = location)
    expect_true(file.exists(fileInput2))
})

test_that("prepareURLInput() returns correct URL",
{
    ## fails for mismatched URLs
    expect_error(
        prepareURLInput(vessel = urlVessel("http://cran.stat.auckland.ac.nz"),
                        input = "https://github.com/anhinton/conduit"),
        "input does not match URL given in urlVessel")
    
    ## start from URL
    vessel1 <- urlVessel("http://cran.stat.auckland.ac.nz")
    urlInput1 <- prepareURLInput(vessel = vessel1, input = NULL)
    expect_match(getRef(vessel1), urlInput1)

    ## match the input
    urlInput2 <- prepareURLInput(vessel = vessel1, input = getRef(vessel1))
    expect_match(getRef(vessel1), urlInput2)
})

test_that("prepareInput() returns resolved input objects", {
    fileInput <- tempfile("fileInput")
    internalInput <- tempfile("internalInput")
    system2("touch", args = c(fileInput, internalInput))
    outputDirectory <- tempfile("prepareInput")
    if (!dir.exists(outputDirectory))
        dir.create(outputDirectory)
    language = moduleLanguage("R")
    location <- tempdir()
    urlInput <- "https://cran.stat.auckland.ac.nz/"
    inputList <- list(file = fileInput, internal = internalInput,
                      url = urlInput)

    ## fails for unknown vessel type
    fakeVessel <- "nope"
    class(fakeVessel) <- c("fakeVessel", "vessel")
    moduleInputBad <- moduleInput(name = "input1",
                               vessel = fakeVessel,
                               format = ioFormat("XML file"))
    expect_error(
        prepareInput(moduleInput = moduleInputBad, inputList, outputDirectory,
                     language, location),
        "unknown vessel type")

    ## internal input
    moduleInput1 <- moduleInput(name = "internal",
                                vessel = internalVessel("obj"),
                                format = ioFormat("language object"))
    input1 <- prepareInput(moduleInput = moduleInput1,
                           inputList = inputList,
                           outputDirectory = outputDirectory,
                           moduleLanguage = language,
                           location = location)
    expect_true(file.exists(input1))

    ## file input
    moduleInput2 <- moduleInput(name = "file",
                                vessel = fileVessel("file.fle"),
                                ioFormat("fle file"))
    input2 <- prepareInput(moduleInput = moduleInput2,
                           inputList = inputList,
                           outputDirectory = outputDirectory,
                           moduleLanguage = language,
                           location = location)
    expect_true(file.exists(input2))

    ## url input
    moduleInput3 <- moduleInput(name = "url",
                                vessel = urlVessel(urlInput),
                                ioFormat("HTML"))
    input3 <- prepareInput(moduleInput = moduleInput3,
                           inputList = inputList,
                           outputDirectory = outputDirectory,
                           moduleLanguage = language,
                           location = location)
    expect_match(input3, urlInput)
})

test_that("prepareModuleHost() behaves correctly", {
    vagrantfile <- tempfile()
    system2("touch", vagrantfile)        
    vagrantHost <- vagrantHost(vagrantfile = vagrantfile)
    name = "mod1"
    modulePath <- tempdir()

    ## fail for invalid arguments
    expect_error(prepareModuleHost(moduleHost = unclass(vagrantHost),
                                   moduleName = name,
                                   modulePath = modulePath),
                 "moduleHost object required")
    expect_error(prepareModuleHost(moduleHost = vagrantHost,
                                   moduleName = c("two", "names"),
                                   modulePath = modulePath),
                 "moduleName is not length 1 character")
    expect_error(prepareModuleHost(moduleHost = vagrantHost,
                                   moduleName = name,
                                   modulePath = tempfile()),
                 "modulePath does not exist")

    ## see test_HOST_TYPE.R for host specific tests
})

test_that("retrieveModuleHost() behaves correctly", {
    vagrantfile <- tempfile()
    system2("touch", vagrantfile)        
    vagrantHost <- vagrantHost(vagrantfile = vagrantfile)
    outputLocation1 = tempdir()
    class(outputLocation1) <- c("vagrantHostOutputLocation",
                                "outputLocation")
    modulePath <- tempdir()
    
    ## fail for invalid arguments
    expect_error(retrieveModuleHost(moduleHost = unclass(vagrantHost),
                                    outputLocation = outputLocation1,
                                    modulePath = modulePath),
                 "moduleHost object required")
    expect_error(retrieveModuleHost(moduleHost = vagrantHost,
                                    outputLocation = unclass(outputLocation1),
                                    modulePath = modulePath),
                 "outputLocation object required")
    expect_error(retrieveModuleHost(moduleHost = vagrantHost,
                                    outputLocation = outputLocation1,
                                    modulePath = tempfile()),
                 "modulePath does not exist")

    ## see test_HOSTTYPE.R for host specific tests
})

test_that(
    "output() behaves",
    {
        ## outputObject(output, language, outputDirectory)
        lang = moduleLanguage("R")
        outdir <- tempdir()

        ## fails when not given 'moduleOutput' object
        expect_error(output(list(name = "a", type = "output",
                                 vessel = urlVessel("http://www.openapi.org"),
                                 format = "website")),
                     "moduleOutput object required")
        
        ## works for internalVessel
        symbol <- "x"
        internal_output <- moduleOutput(
            "internal", internalVessel(symbol), ioFormat("nonsense"))
        output1 <- output(internal_output, lang, outdir)
        expect_is(output1, "output")
        expect_match(
            getRef(output1),
            file.path(outdir, paste0(symbol, internalExtension(lang))))

        ## works for urlVessel
        url <- "https://github.com/anhinton/conduit"
        url_output <- moduleOutput(
            "url", urlVessel(url), ioFormat("HTML file"))
        output2 <- output(url_output, lang, outdir)
        expect_is(output2, "output")
        expect_match(getRef(output2), url)
        
        ## works for fileVessel
        file <- "output.csv"
        file_output <- moduleOutput(
            "file", fileVessel(file), ioFormat("CSV file"))
        output3 <- output(file_output, lang, outdir)
        expect_is(output3, "output")
        expect_match(getRef(output3), file.path(outdir, file))
        
        ## fails for unknown vessel type
        not_a_real_output <- internal_output
        class(not_a_real_output$vessel)[1] <- "dudeVssl"
        expect_error(output(not_a_real_output,
                                  lang, outdir),
                     "vessel type not defined")
    })

## resolveOutput() successes are tested implicitly by runModule()
## the following tests failures
test_that(
    "resolveOutput() works on local machine",
    {
        lang = moduleLanguage("R")
        outdir <- tempdir()
        symbol <- basename(tempfile())
        internal_output <- moduleOutput(
            "internal", internalVessel(symbol), ioFormat("nonsense"))
        file <- basename(tempfile())
        file_output <- moduleOutput(
            "file", fileVessel(file), ioFormat("CSV file"))

        ## TODO(anhinton): write check for URL outputs
        ## internalVessel
        expect_error(resolveOutput(moduleOutput = internal_output,
                                   moduleLanguage = lang,
                                   outputDirectory = outdir),
                     "output object '")
        ## fileVessel
        expect_error(resolveOutput(moduleOutput = file_output,
                                   moduleLanguage = lang,
                                   outputDirectory = outdir),
                     "output object '") 
    })

## test runModule()
test_that(
    "runModule() fails when not given a 'module' object",
    {
        fakemodule <- list(name ="fakemodule",
                           language = "R",
                           sources = list(
                               moduleSource(scriptVessel("1:10"))))
        expect_error(runModule(fakemodule, targetDirectory = targ),
                     "'module' is not a ")
    })

test_that(
    "runModule() fails when targetDirectory doesn't exist",
    {
        badTarget <- paste0(tempfile(), tempfile())
        expect_error(runModule(createGraph, targetDirectory = badTarget),
                     "no such target")
    })

test_that(
    "runModule() succeeds for module with fileVessel input with absolute ref",
    {
        absRef <- system.file("extdata", "simpleGraph", "createGraph.xml",
                              package = "conduit")
        moduleName <- "absomod"
        moduleLanguage = moduleLanguage("R")
        outputName <- "lines"
        outputType <- "internalVessel"
        outputObject <-
            file.path(targ, moduleName,
                      paste0(outputName,
                             internalExtension((moduleLanguage))))
        absomod <- module(
            name = moduleName,
            language = moduleLanguage,
            inputs = list(
                moduleInput(
                    name = "file",
                    vessel = fileVessel(absRef),
                    format = ioFormat("XML file"))),
            sources = list(
                moduleSource(
                    scriptVessel(
                        paste0(outputName, " <- readLines(\"", absRef,
                               "\")")))),
            outputs = list(
                moduleOutput(
                    name = outputName,
                    vessel = internalVessel(outputName),
                    format = ioFormat("R character vector"))))
        result <- runModule(absomod, targetDirectory = targ)
        expect_match(getName(result$outputList[[1]]), outputName)
        expect_true(file.exists(outputObject))
    })

test_that(
    "runModule() works",
    {
        ## run the createGraph module
        output1 <- createGraph$outputs[[1]]
        result1 <- runModule(createGraph, targetDirectory = targ)
        expect_match(result1$outputList[[1]]$name, output1$name)
        expect_is(result1, "moduleResult")
        expect_true(file.exists(getRef(result1$outputList[[1]])))
        
        ## run the layoutGraph module, providing the output from
        ## createGraph as input
        inputObjects <- list(getRef(result1$outputList[[1]]))
        names(inputObjects) <- layoutGraph$inputs[[1]]$name
        output2 <- layoutGraph$outputs[[1]]
        result2 <- runModule(layoutGraph,
                             inputObjects = inputObjects,
                             targetDirectory = targ)
        expect_is(result2, "moduleResult")
        expect_match(result2$outputList[[1]]$name, output2$name)
        expect_true(file.exists(getRef(result2$outputList[[1]])))
    })

## conduit should give warnings when a module was executed in a
## version of the language which did not meet its language version
## requirements
test_that("runModule() warns for language version violations", {
    ## warn for exact version failure
    mod1 <- module(name = "mod1",
                   language = moduleLanguage(
                       language = "R",
                       version = "2.4.6"))
    expect_warning(
        mod1res <- runModule(module = mod1, targetDirectory = tempdir(),
                             warnVersion = TRUE),
        "was not exactly")
    
    ## warn for minVersion failure
    mod2 <- module(name = "mod2",
                   language = moduleLanguage(
                       language = "R",
                       minVersion = "999.999"))
    expect_warning(
        mod2res <- runModule(module = mod2, targetDirectory = tempdir(),
                             warnVersion = TRUE),
        "was less than minVersion")
    
    ## warn for maxVersion failure
    mod3 <- module(name = "mod3",
                   language = moduleLanguage(
                       language = "R",
                       maxVersion = "0.1.999"))
    expect_warning(
        mod3res <- runModule(module = mod3, targetDirectory = tempdir(),
                             warnVersion = TRUE),
        "was greater than maxVersion")
})

## TODO(anhinton): runModule() works for a module with a moduleHost
