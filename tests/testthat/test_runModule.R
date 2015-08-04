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
        skip_on_cran()
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
        skip("requires test conduit web server at http://127.0.0.1:8080/")
        skip_on_cran()
        url_source <- moduleSource(
            urlVessel("http://127.0.0.1:8080/testing/season1_html.R"))
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
        skip_on_cran()

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
        expect_match(prepareScript(module, inputObjects),
                     "script.R")

        ## module with remote host uses relative refs for serialized
        ## internalVessel input files
        module$host <- "conduit@127.0.0.1:2222"
        script <- prepareScript(module, inputObjects)
        inputLine <- readLines(script)[1]
        expect_true(grepl(basename(inputObjects[[1]]), inputLine))
        expect_false(grepl(dirname(inputObjects[[1]]), inputLine))
    })

## test parseModuleHost
test_that(
    "parseModuleHost() works",
    {
        host <- "conduit@server:666"
        parsedHost <- parseModuleHost(host)
        expect_match(names(parsedHost), "user", all=F)
        expect_match(names(parsedHost), "address", all=F)
        expect_match(names(parsedHost), "port", all=F)
        expect_match(names(parsedHost), "directory", all=F)
        expect_match(names(parsedHost), "idfile", all=F)
        expect_match(parsedHost$user, "conduit")
        expect_match(parsedHost$address, "server")
        expect_match(parsedHost$port, "666")
        expect_match(parsedHost$directory,
                     paste0("^/tmp/", get("sessionID",
                                          envir = .conduit.global)))
        expect_match(parsedHost$idfile,
                     get("defaultIdfile", envir = .conduit.global))

        ## no username or host given
        host <- "6.6.6.6"
        parsedHost <- parseModuleHost(host)
        expect_match(parsedHost$user, "conduit")
        expect_match(parsedHost$address, "6.6.6.6")
        expect_match(parsedHost$port, "22")
    })
          

## test resolveInput()
test_that(
    "absolute fileVessel refs are resolved",
    {
        skip_on_cran()
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
        expect_true(resolveInput(input, inputObjects))
    })

test_that(
    "relative fileVessel refs are resolved",
    {
        skip_on_cran()
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
    "internalVessel inputs are resolved",
    {
        skip_on_cran()
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
        skip_on_cran()
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
        skip_on_cran()
        oldwd <- setwd(tempdir())
        on.exit(setwd(oldwd))
        module1 <-
            loadModule("module1",
                       system.file("extdata", "test_pipeline",
                                   "module1.xml",
                                   package = "conduit"))
        inputObjects <- NULL
        script <- prepareScript(module1, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.python works",
    {
        skip_on_cran()
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
        script <- prepareScript(module2, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "executeScript.shell works",
    {
        skip_on_cran()
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
        script <- prepareScript(module3, inputObjects)
        expect_equal(executeScript(script = script, host = NULL), 0)
    })

test_that(
    "outputObject() behaves",
    {
        ## outputObject(output, language, outputDirectory)
        lang = "R"
        outdir <- tempdir()
        
        ## works for internalVessel
        symbol <- "x"
        internal_output <- moduleOutput(
            "internal", internalVessel(symbol), ioFormat("nonsense"))
        expect_match(outputObject(internal_output, lang, outdir),
                     file.path(outdir,
                               paste0(symbol,
                                      internalExtension(lang))))

        ## works for urlVessel
        url <- "https://github.com/anhinton/conduit"
        url_output <- moduleOutput(
            "url", urlVessel(url), ioFormat("HTML file"))
        expect_match(outputObject(url_output, lang, outdir),
                     url)
        
        ## works for fileVessel
        file <- "output.csv"
        file_output <- moduleOutput(
            "file", fileVessel(file), ioFormat("CSV file"))
        expect_match(outputObject(file_output, lang, outdir),
                     file.path(outdir, file))
        
        ## fails for unknown vessel type
        not_a_real_output <- internal_output
        class(not_a_real_output$vessel)[1] <- "dudeVssl"
        expect_error(outputObject(not_a_real_output,
                                  lang, outdir),
                     "vessel type not defined")
    })

## resolveOutput() successes are tested implicitly by runModule()
## the following tests failures
test_that(
    "resolveOutput() works on local machine",
    {
        skip_on_cran()
        lang = "R"
        host = parseModuleHost("cronduit@not.a.real.server:11")
        outdir <- tempdir()
        symbol <- tempfile()
        internal_output <- moduleOutput(
            "internal", internalVessel(symbol), ioFormat("nonsense"))
        file <- tempfile()
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
        inputFile <- system.file("extdata", "simpleGraph", "createGraph.xml",
                                 package = "conduit")
        moduleName <- "absomod"
        language = "R"
        outputName <- "lines"
        outputType <- "internalVessel"
        outputObject <-
            file.path(targ, "modules", moduleName,
                      paste0(outputName, internalExtension(language)))
        absomod <- module(
            name = moduleName,
            language = language,
            inputs = list(
                moduleInput(
                    name = "file",
                    vessel = fileVessel(inputFile),
                    format = ioFormat("XML file"))),
            sources = list(
                moduleSource(
                    scriptVessel(
                        paste0(outputName, " <- readLines(\"", inputFile,
                               "\")")))),
            outputs = list(
                moduleOutput(
                    name = outputName,
                    vessel = internalVessel(outputName),
                    format = ioFormat("R character vector"))))
        output <- runModule(absomod, targetDirectory = targ)
        expect_match(output[[1]]$name, outputName)
        expect_match(output[[1]]$type, outputType)
        expect_true(file.exists(outputObject))
    })

test_that(
    "runModule() works",
    {
        skip_on_cran()
        ## run the createGraph module
        output1 <- createGraph$outputs[[1]]
        result1 <- runModule(createGraph, targetDirectory = targ)
        expect_match(result1[[1]]$name, output1$name)

        ## run the layoutGraph module, providing the output from
        ## createGraph as input
        inputObjects <- list(result1[[1]]$object)
        names(inputObjects) <- layoutGraph$inputs[[1]]$name
        output2 <- layoutGraph$outputs[[1]]
        result2 <- runModule(layoutGraph,
                             inputObjects = inputObjects,
                             targetDirectory = targ)
        expect_match(result2[[1]]$name, output2$name)
    })
