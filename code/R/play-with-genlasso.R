#install.packages("genlasso")
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


edges = c(1,2,1,4,1,5,2,3,2,4,2,5,2,6,3,5,3,6,4,5,5,6)
gr = graph(edges = edges, directed = FALSE)
plot(gr)
# I will need to build a network from the grid data
# actually will need to take the matrix and make an edge
# from every point to its neighbours if it has no NA
# !! maybe dont event need to do the graph
# but how to use NA/ land areas?
?fusedlasso2d

# example matrix
m <- matrix(c(1,2,3,4,NA,6,7,8,9), ncol = 3, byrow = TRUE)
is.na(m)
# make vertical and horizontal edges for every node
is.na(m[1,])
# node is row number plus position in row vector - 1, 1 + 1 -1 = 1
# append edges vector, node at pos + node next for rows its simply +1
# for col its row + row length, so that next node is under it
is.na(c(m[1,1],m[1,2]))
e <- c()
e <- append(e, c(1,2))
# maybe keep going until there is no more NA
# or check where is NA and create rows for all other nodes
edges_true <- c(1,2,1,4,2,3,3,6,4,7,6,9,7,8,8,9,
                )
gr = graph(edges = edges_true, directed = FALSE)
plot(gr)
m
# use this maybe for network
plot(graph_from_literal(A:B:C:D -- A:B:C:D))
# use weights for diagonal edges?
# use zero weights for NA connections?
