install.packages("genlasso")
library(genlasso)
set.seed(0)
edges = c(1,2,1,3,1,5,2,4,2,5,3,6,3,7,3,8,6,7,6,8)
gr = graph(edges=edges,directed=FALSE)
plot(gr)
y = c(1,1,0,1,1,0,0,0) + rnorm(8,0.1)
a1 = fusedlasso(y, graph = gr)
D = getDgSparse(gr)
a2 = fusedlasso(y, D=D)
plot(a1, numbers = TRUE)
?fusedlasso2d


# y is numeric response vector or precip
# X obs along rows and variables along columns
#   so here the year is a row and columns is
#   a grid point.
#   pur X definitely will have more columns than
#   rows, warning given
# dim1 number of rows in grid
# dim2 number of cols in grid

edges = c(1,2,1,3,1,4,2,3,2,4,3,4)
gr = graph(edges = edges, directed = FALSE)
plot(gr)
y <- c(1,2,3,4) + rnorm(4,0.1)

X = matrix(c(1,2,3,4,1,1,1,1,2,2,2,2,4,3,2,2,4,4,3,3),
           nrow = 4, ncol = 4, byrow = FALSE)
dim1 <- 2
dim2 <- 2
f <- fusedlasso2d(y,X,dim1,dim2)
plot(f)
