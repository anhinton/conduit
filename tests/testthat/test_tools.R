library(conduit)
context("test internal tools")

myNumeric <- 666
twoCharacters <- c("first", "second")
oneCharacter <- "only"

test_that("is_length1_char() picks only length 1 character vectors", {
    expect_false(is_length1_char(myNumeric))
    expect_false(is_length1_char(twoCharacters))
    expect_true(is_length1_char(oneCharacter))
})

test_that("topologicalSort() returns correctly", {
    ## fail for cycle
    cycle <- list(A = "B", B = "C", C = "A")
    expect_error(topologicalSort(cycle),
                 "graph has at least one cycle")

    ## fail for edge to non-existent vertex
    missingVertex <- list(A = c("B", "C"), B = "C", C = "D")
    expect_error(topologicalSort(missingVertex),
                 "vertex")

    ## simple order
    edges1 <- list(A = c("B", "C"), B = "C", C = character())
    expect_identical(topologicalSort(edges1),
                     c("A", "B", "C"))

    ## everything comes before "C"
    edges2 <- list(A = "C", B = "C", C = character(), D = "C")
    order2 <- topologicalSort(edges2)
    expect_equal(which("C" == order2), length(order2))

    ## everything comes after "A"
    edges3 <- list(B = character(), C = character(), D = character(),
                   E = character(), A = c("C", "B", "D", "E"))
    order3 <- topologicalSort(edges3)
    expect_equal(which("A" == order3), 1)

    ## "A" first, "E" last
    edges4 <- list(B = "E", C = "E", D = "E", E = character(),
                   A = c("D", "B", "C"))
    order4 <- topologicalSort(edges4)
    expect_equal(which("A" == order4), 1)
    expect_equal(which("E" == order4), length(order4))
})
