library(graph)
nodes <- c("a", "b", "c", "d")
edgeList <- list(a=list(edges=c("b")),
                 b=list(edges=c("c")),
                 c=list(edges=c("d")),
                 d=list(edges=c("a")))
directedGraph <- new("graphNEL", nodes=nodes, edgeL=edgeList, 
                     edgemode="directed")
