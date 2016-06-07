library(conduit)
context("create pipeline objects")

mod1 <- module("mod1", moduleLanguage("R"))
pipeStart <- list(component = "mod1", output = "a")
pipeEnd <- list(component = "mod2", input = "b")
pipe1 <- pipe(pipeStart[[1]], pipeStart[[2]], pipeEnd[[1]], pipeEnd[[2]])
plineName <- "pline1"
plineDesc <- c("a model", "pipeline")
pline1 <- pipeline(plineName, description = plineDesc,
                   components = list(mod1), pipes = list(pipe1))
pline2 <- pipeline(plineName, plineDesc, list(mod1, pline1))
pline3 <- pipeline(plineName, plineDesc,
                   list(mod1, pline1,
                        component("c1", value = mod1)))
fv <- fileVessel("some.file")
uv <- urlVessel("http://cran.stat.auckland.ac.nz")
comp1 <- component(name = getName(mod1), value = mod1)
comp2 <- component(value = pline1, vessel = fv)
comp3 <- component(name = getName(pline1), value = pline1, vessel = uv)

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
    expect_is(pline1, "pipeline")

    ## element names
    elementNames <- names(pline1)
    expect_match(elementNames, "^name$", all = FALSE)
    expect_match(elementNames, "^description$", all = FALSE)
    expect_match(elementNames, "^components$", all = FALSE)
    expect_match(elementNames, "^pipes$", all = FALSE)

    ## element classes
    expect_is(getName(pline1), "character")
    expect_is(getComponents(pline1), "list")
    expect_is(getDescription(pline1), "character")
    expect_is(getPipes(pline1), "list")

    ## element values
    expect_identical(getName(pline1), plineName)
    expect_identical(getDescription(pline1), plineDesc)
    expect_true(all(sapply(getComponents(pline1),
                           inherits, what = "component")))
    expect_true(all(sapply(getComponents(pline2),
                           inherits, what = "component")))
    expect_true(all(sapply(getComponents(pline3),
                           inherits, what = "component")))
    expect_identical(getType(getComponents(pline3)[[1]]), "module")
    expect_identical(getType(getComponents(pline3)[[2]]), "pipeline")
    expect_identical(getType(getComponents(pline3)[[3]]), "module")
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
    expect_is(pipe1, "pipe")

    ## element names
    elementNames <- names(pipe1)
    expect_match(elementNames, "^start$", all = FALSE)
    expect_match(elementNames, "^end$", all = FALSE)

    ## element classes
    expect_is(start(pipe1), "list")
    expect_is(end(pipe1), "list")

    ## element values
    expect_equal(length(end(pipe1)), 2)
    expect_equal(length(start(pipe1)), 2)
    expect_identical(start(pipe1), pipeStart)
    expect_identical(end(pipe1), pipeEnd)
})

test_that("component creation fails in expected ways", {
    ## name
    expect_error(component(name = 1), "'name' is not a length")
    expect_error(component(name = c("two", "names")),
                 "'name' is not a length")

    ## invalid value
    expect_error(component(name = "c1", value = list()), "invalid 'value'")

    ## invalid vessel
    expect_error(component(name = "c1", value = mod1,
                           vessel = scriptVessel("a")),
                 "invalid 'vessel'")
    expect_error(component(name = "c1", value = mod1,
                           vessel = internalVessel("a")),
                 "invalid 'vessel'")
})

test_that("component object is correctly formed", {
    ## class
    expect_is(comp1, "component")

    ## element names
    elementNames <- names(comp1)
    expect_match(elementNames, "^name$", all = FALSE)
    expect_match(elementNames, "^vessel$", all = FALSE)
    expect_match(elementNames, "^value$", all = FALSE)

    ## element values
    expect_identical(getName(comp1), getName(mod1))
    expect_identical(getName(comp2), getName(pline1))
    expect_null(getVessel(comp1))
    expect_identical(getVessel(comp2), fv)
    expect_identical(getVessel(comp3), uv)
    expect_identical(getValue(comp1), mod1)
    expect_identical(getValue(comp2), pline1)
    expect_match(getType(comp1), class(mod1))
    expect_match(getType(comp2), class(pline1))
})

test_that("amending a pipeline works correctly", {
    ## add a module
    p1 <- addComponent(newComponent = mod1, pipeline = pline1)
    expect_equal(length(getComponents(pline1)) + 1,
                 length(getComponents(p1)))
    expect_match(names(getComponents(p1)), getName(mod1), all = FALSE)

    ## add a pipeline
    p2 <- addComponent(newComponent = pline2, pipeline = p1)
    expect_equal(length(getComponents(p1)) + 1,
                 length(getComponents(p2)))
    expect_match(names(getComponents(p2)), getName(pline2), all = FALSE)

    ## add a component
    p3 <- addComponent(newComponent = comp1, pipeline = p2)
    expect_equal(length(getComponents(p2)) + 1,
                 length(getComponents(p3)))
    expect_match(names(getComponents(p3)), getName(comp1), all = FALSE)

    ## add a pipe
    pipe1 <- pipe("a", "b", "d", "f")
    p4 <- addPipe(newPipe = pipe1, pipeline = p3)
    expect_equal(length(getPipes(p3)) + 1,
                 length(getPipes(p4)))
    expect_true(any(sapply(getPipes(p4), identical, pipe1)))
})
