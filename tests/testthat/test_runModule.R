library(conduit)
context("execute modules")

## skip tests which require a module host machine
## requires conduit host at conduit@127.0.0.1:2222
skipHost <- TRUE

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

test_that(
    "extractModuleSource() works for <url> sources",
    {
        if (skipHost)
            skip("requires test conduit web server at http://127.0.0.1:8080/")
        url_source <- moduleSource(
            urlVessel("http://127.0.0.1:8080/urlTesting/season1_html.R"))
        class(url_source) <- class(url_source$vessel)
        source_script <- extractModuleSource(url_source)
        expect_equal(length(source_script), 10)
        expect_match(class(source_script), "character")
        expect_match(source_script[1], "^library[(]R2HTML[)]")
    })

## test prepareScript
test_that(
    "R script file is created",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        
        ## create RDS file for input
        input_object <- 1:10
        filename <- tempfile()
        saveRDS(input_object, filename)
        inputObjects <- list(a = filename)

        ## create module
        module <-
            module(
                "testy",
                "R",
                inputs =
                    list(moduleInput(
                        "a",
                        internalVessel("onetoten"),
                        ioFormat("R numeric vector"))),
                sources =
                    list(moduleSource(
                        scriptVessel(
                            "top <- head(onetoten)"))),
                outputs =
                    list(moduleOutput(
                        "b",
                        internalVessel("top"),
                        ioFormat("R character vector"))))

        ## test script creation
        expect_match(prepareScript(module),
                     "script.R")

        skip("2016-02-03 changing host handling")
        ## module with remote host uses relative refs for serialized
        ## internalVessel input files
        module$host <- "conduit@127.0.0.1:2222"
        script <- prepareScript(module)
        inputLine <- readLines(script)[1]
        expect_true(grepl(basename(inputObjects[[1]]), inputLine))
        expect_false(grepl(dirname(inputObjects[[1]]), inputLine))
    })

test_that("prepareInternalInput() returns correctly", {
    input <- tempfile()
    system2("touch", input)
    symbol <- "x"
    language = "python"
    outputDirectory <- tempfile("prepareInternalInput")
    if (!dir.exists(outputDirectory))
        dir.create(outputDirectory)

    ## unable to copy
    expect_error(prepareInternalInput(input, symbol, language, tempfile()),
                 "unable to copy input into outputDirectory")

    ## success 
    internalInput <-
        prepareInternalInput(input, symbol, language, outputDirectory)
    expect_true(file.exists(internalInput))
})

## test resolveInput()
test_that(
    "absolute fileVessel refs are resolved",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput(
                "great",
                fileVessel(
                    system.file("extdata", "simpleGraph", "createGraph.xml",
                                package = "conduit")),
                ioFormat("text file"))
        inputObjects <- NULL
        skip("2016-02-03 changing host handling")
        expect_true(resolveInput(input, inputObjects))
    })

test_that(
    "relative fileVessel refs are resolved",
    {
        skip("2016-02-03 changing host handling")
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput(
                "good",
                fileVessel("test2"),               
                ioFormat("text file"))
        inputObjects <-
            list(good = system.file(
                     "extdata", "simpleGraph", "createGraph.xml",
                     package = "conduit"))
        expect_true(resolveInput(input, inputObjects, host = NULL))
    })

test_that(
    "fileVessel inputs with search paths are resolved",
    {
        skip("2016-02-03 changing host handling")
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <-
            moduleInput("okay",
                        fileVessel(ref = "layoutGraph.xml",
                                   path = system.file("extdata", "simpleGraph",
                                                      package = "conduit")),
                        ioFormat("text file"))
        inputObjects <-
            list(okay = system.file(
                     "extdata", "simpleGraph", "createGraph.xml",
                     package = "conduit"))
        expect_true(resolveInput(input, list(), host = NULL,
                                 location = getwd()))
    })

test_that(
    "internalVessel inputs are resolved",
    {
        skip("2016-02-03 changing host handling")
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        input <- moduleInput("fantastic",
                             internalVessel("y"),
                             ioFormat("R numeric vector"))
        filename <- tempfile("testRDS", fileext=".rds")
        saveRDS(1:10, filename)
        inputObjects <- list(fantastic = filename)
        expect_true(resolveInput(input, inputObjects, host = NULL))
    })
test_that(
    "urlVessel inputs are resolved",
    {
        skip("2016-02-03 changing host handling")
        moduleInput1 <- moduleInput(
            "inp1",
            urlVessel("http://cran.stat.auckland.ac.nz/"),
            ioFormat("HTML file"))
        inputObjects1 <- list()
        expect_true(resolveInput(moduleInput1, inputObjects1, host = NULL))

        moduleInput2 <- moduleInput(
            "inp1",
            urlVessel("NOT.A.REAL.URL"),
            ioFormat("HTML file"))
        expect_false(resolveInput(moduleInput2, inputObjects1, host = NULL))
        
        moduleInput3 <- moduleInput(
            "inp1",
            urlVessel("http://cran.stat.auckland.ac.nz/"),
            ioFormat("HTML file"))
        inputObjects3 <-
            list(inp1 = "http://cran.stat.auckland.ac.nz/SUBFOLDER")
        expect_error(resolveInput(moduleInput3, inputObjects3, host = NULL),
                     "url")
     })

## test executeScript
test_that(
    "executeScript.R works",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module1 <-
            loadModule("module1",
                       system.file("extdata", "test_pipeline",
                                   "module1.xml",
                                   package = "conduit"))
        inputObjects <- NULL
        script <- prepareScript(module1)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.python works",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module2 <- module(
            "module2",
            "python",
            sources = list(
                moduleSource(
                    scriptVessel("x = [1, 2, 3, 5, 10]"))),
            outputs = list(
                moduleOutput(
                    "x",
                    internalVessel("x"),
                    ioFormat("python list"))))
        inputObjects <- NULL
        script <- prepareScript(module2)
        skip("2016-02-03 changing host handling")
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.shell works",
    {
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module3 <- module(
            "module3",
            "shell",
            sources = list(
                moduleSource(
                    scriptVessel("x=\"lemon duds\"]"))),
            outputs = list(
                moduleOutput(
                    "x",
                    internalVessel("x"),
                    ioFormat("shell environment variable"))))
        inputObjects <- NULL
        skip("2016-02-03 changing host handling")
        script <- prepareScript(module3)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "output() behaves",
    {
        ## outputObject(output, language, outputDirectory)
        lang = "R"
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
            getResult(output1),
            file.path(outdir, paste0(symbol, internalExtension(lang))))

        ## works for urlVessel
        url <- "https://github.com/anhinton/conduit"
        url_output <- moduleOutput(
            "url", urlVessel(url), ioFormat("HTML file"))
        output2 <- output(url_output, lang, outdir)
        expect_is(output2, "output")
        expect_match(getResult(output2), url)
        
        ## works for fileVessel
        file <- "output.csv"
        file_output <- moduleOutput(
            "file", fileVessel(file), ioFormat("CSV file"))
        output3 <- output(file_output, lang, outdir)
        expect_is(output3, "output")
        expect_match(getResult(output3), file.path(outdir, file))
        
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
        lang = "R"
        skip("2016-02-03 changing host handling")
        host = parseModuleHost("cronduit@not.a.real.server:11")
        outdir <- tempdir()
        symbol <- basename(tempfile())
        internal_output <- moduleOutput(
            "internal", internalVessel(symbol), ioFormat("nonsense"))
        file <- basename(tempfile())
        file_output <- moduleOutput(
            "file", fileVessel(file), ioFormat("CSV file"))
        url <- "http://not.a.real.server/at/all"
        url_output <- moduleOutput(
            "url", urlVessel(url), ioFormat("html file"))

        ## throws error when object does not exist
        ## urlVessel
        expect_error(resolveOutput(url_output, lang, NULL, outdir),
                     "output object '")
        ## internalVessel
        expect_error(resolveOutput(internal_output, lang, NULL, outdir),
                     "output object '")
        ## fileVessel
        expect_error(resolveOutput(file_output, lang, NULL, outdir),
                     "output object '")        
        
        ## throws error when unable to fetch from host
        ## internalVessel
        expect_error(resolveOutput(internal_output, lang, host, outdir),
                     "Unable to fetch ")
        ## fileVessel
        expect_error(resolveOutput(file_output, lang, host, outdir),
                     "Unable to fetch ")
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
    "runModule() fails when input cannot be resolved",
    {
        skip("2016-02-03 changing host handling")
        badInput <- paste0(tempfile(), tempfile())
        module <- module(
            name = "fails",
            language = "R",
            inputs = list(
                moduleInput(
                    name = "nope",
                    vessel = fileVessel(badInput),
                    format = ioFormat("file"))),
            sources = list(
                moduleSource(vessel = scriptVessel(
                    paste0("exists <- file.exists(\"", badInput, "\")")))),
            outputs = list(
                moduleOutput(
                    name = "truth",
                    vessel = internalVessel("exists"),
                    format = ioFormat("R logical"))))
        expect_error(runModule(module, targetDirectory = targ),
                     "Input ")
    })

test_that(
    "runModule() succeeds for module with fileVessel input with absolute ref",
    {
        absRef <- system.file("extdata", "simpleGraph", "createGraph.xml",
                              package = "conduit")
        moduleName <- "absomod"
        language = "R"
        outputName <- "lines"
        outputType <- "internalVessel"
        outputObject <-
            file.path(targ, moduleName,
                      paste0(outputName, internalExtension(language)))
        absomod <- module(
            name = moduleName,
            language = language,
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
        expect_true(file.exists(getResult(result1$outputList[[1]])))
        
        ## run the layoutGraph module, providing the output from
        ## createGraph as input
        inputObjects <- list(getResult(result1$outputList[[1]]))
        names(inputObjects) <- layoutGraph$inputs[[1]]$name
        output2 <- layoutGraph$outputs[[1]]
        result2 <- runModule(layoutGraph,
                             inputObjects = inputObjects,
                             targetDirectory = targ)
        expect_is(result2, "moduleResult")
        expect_match(result2$outputList[[1]]$name, output2$name)
        expect_true(file.exists(getResult(result2$outputList[[1]])))
    })
