library(conduit)
context("create pipeline objects")

mod1 <- module("mod1", "R")
pipeStart <- list(component = "mod1", output = "a")
pipeEnd <- list(component = "mod2", input = "b")
pipe1 <- pipe(pipeStart[[1]], pipeStart[[2]], pipeEnd[[1]], pipeEnd[[2]])
plineName <- "pline1"
plineDesc <- c("a model", "pipeline")
pline1 <- pipeline(plineName, description = plineDesc,
                   components = list(mod1), pipes = list(pipe1))

test_that("pipeline creation fails in expected ways", {
    ## name is length 1 char
    expect_error(pipeline(name = 3), "'name' is not")
    expect_error(pipeline(name = c("two", "names")),
                 "'name' is not")

    ## description is char
    expect_error(pipeline(name = "ppl1", description = 3),
                 "'description' is not")

    ## components list of modules and pipelines
    expect_error(pipeline(name = "ppl1", components = mod1),
                 "components must be provided")
    expect_error(pipeline(name = "ppl1", components = list()),
                 "no component")
    expect_error(pipeline(name = "ppl", components = list("a", 2, "d")),
                 "components must be module")

    ## pipes list of pipes
    expect_error(pipeline(name = "ppl", components = list(mod1),
                          pipes = pipe1),
                 "pipes must be provided")
    expect_error(pipeline(name = "ppl", components = list(mod1),
                          pipes = list("a", 2, "d")),
                 "pipes must be pipe")
})

test_that("pipeline object is correctly formed", {
    ## class
    expect_true(inherits(pline1, "pipeline"))

    ## element names
    elementNames <- names(pline1)
    expect_match(elementNames, "^name$", all = FALSE)
    expect_match(elementNames, "^description$", all = FALSE)
    expect_match(elementNames, "^components$", all = FALSE)
    expect_match(elementNames, "^pipes$", all = FALSE)

    ## element classes
    expect_true(inherits(getName(pline1), "character"))
    expect_true(inherits(getComponents(pline1), "list"))
    expect_true(inherits(getDescription(pline1), "character"))
    expect_true(inherits(getPipes(pline1), "list"))

    ## element values
    expect_identical(getName(pline1), plineName)
    expect_identical(getDescription(pline1), plineDesc)
    expect_identical(getComponents(pline1), list("mod1" = mod1))
    expect_identical(getPipes(pline1), list(pipe1))
})

test_that("pipe creation fails in expected ways", {
    ## missing arg
    expect_error(pipe("one", "two", "three"))

    ## args are length 1 char
    expect_error(pipe("a", 2, "d", "4"), "arguments should be")
    expect_error(pipe(c("a", "b"), "c", "d", "e"), "arguments should be")
})

test_that("pipe object is correctly formed", {
    ## class
    expect_true(inherits(pipe1, "pipe"))

    ## element names
    elementNames <- names(pipe1)
    expect_match(elementNames, "^start$", all = FALSE)
    expect_match(elementNames, "^end$", all = FALSE)

    ## element classes
    expect_true(inherits(start(pipe1), "list"))
    expect_true(inherits(end(pipe1), "list"))

    ## element values
    expect_equal(length(end(pipe1)), 2)
    expect_equal(length(start(pipe1)), 2)
    expect_identical(start(pipe1), pipeStart)
    expect_identical(end(pipe1), pipeEnd)
})
