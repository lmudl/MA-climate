get_weights <- function(lattice_graph) {
  dist_latt <- distances(lattice_graph)
  dist_latt[upper.tri(dist_latt)] <- 0
  w <- (dist_latt[dist_latt!=0])^(-1)
  return(w)
}

create_pen_mat <- function() {}
# take raster obj
# create lattice
# get weights
# create full
# add weights
# delete land
# get dg sparse from graph
# multiply rows with weight
# give D as penalty matrix to fusedlasso
# check with test
library(raster)
raster_object <- brick("data/interim/sst/ersst_setreftime.nc", varname = "sst")
dims <- dim(raster_object)[1:2]
dims <- rev(dims)
g <- make_lattice(dims)
ec <- create_coords(dims)
V(g)$x <- ec$x_vec
V(g)$y <- rev(ec$y_vec)
w <- get_weights(g)
fg <- make_full_graph(dims[1]*dims[2])
V(fg)$x <- ec$x_vec
V(fg)$y <- rev(ec$y_vec)
E(fg)$weights <- w
vals <- values(raster_object[[1]])
land <- which(is.na(vals))
fg <- delete_vertices(fg, land)
plot(fg, vertex.label = NA, edge.width =0.00002)

real_g,vertex.size = 1, edge.width=0.00001,
vertex.label = NA, vertex.color = cl$membership*1
create_weighted_graph <- function() {
  
}

  
# how does getdg work?
g1 <- make_lattice(c(3,3))
plot(g1)
d1 <- getDg(g1)
dim(d1)

crossprod(d1)

g1f <- make_full_graph(9)
dims <- c(3,3)
ec <- create_coords(dims)
V(g1f)$x <- ec$x_vec
V(g1f)$y <- ec$y_vec
plot(g1f)

w <- get_weights(g1)
E(g1f)$weight <- w
is_weighted(g1f) 

d1f <- getDg(g1f)
length(w)
dim(d1f)[1]

for(i in 1:dim(d1f)[1]) {
  d1f[i,] <- d1f[i,]*w[i]
}
d1f

# create a graph
g <- make_lattice(c(3,3))
?make_lattice
as_adj(g)
plot(g)
scale(as_adj(g))
?as_adj
distance_table(g)

distances(g)

g2 <- make_full_graph(5)
#?make_full_graph
plot(g2, layout = layout.grid)
?plot.igraph()

distances(g2)

as_adj(g)

?fusedlasso

getDgSparse(g)


plot(g)
ad <- as_adj(g)
ad2 <- apply(ad, 1, function(x) scale(x, center = TRUE,
                                      scale = FALSE))
plot(graph_from_adjacency_matrix(ad2))


g <- make_ring(10)
plot(g)
graph(laplacian_matrix(g, norm = TRUE))


gg <- graph.laplacian(g)
graph.adjacency(gg)
?clusters

# we can give D instead of g
setwd("Repos/MA-climate/")
real_g <- readRDS("data/processed/graph_sst.rds")
library(igraph)
library(genlasso)
clusters(real_g)
plot(clusters(real_g))
?clusters

D2 <- getDgSparse(g2)


l2 <- graph.laplacian(g2)
getDgSparse(l2)
?getD1dSparse

graph.adjacency(g2)

# they usse graph.laplcacian(graph)
# and t(D) %*% D

?graph.laplacian
# we can give the graph weights
plot(g)
weights(g2) <- c()

# possibility 1
# drop small clusters
# possibility 2
# add fully

# a make lattica
# b make full
# use dist in a to weight in b
a <- make_lattice(c(2,2))
plot(a)
b <- make_full_graph(4)
plot(b, layout = layout.grid)

distances(a)
distances(b)

ecount(b)
dist_a <- distances(a)
dist_a
dist_a[lower.tri(dist_a)] <- 0
w <- dist_a[dist_a!=0]
E(b)$weight <- w^(-1)

plot(b, edge.color=w)






debug(get_weights)
l <- make_lattice(c(5,5))
dims <- c(5,5)
ec <- create_coords(dims)
V(l)$x <- ec$x_vec
V(l)$y <- ec$y_vec
ws <- get_weights(l)

f0 <- make_full_graph(25)
f <- make_full_graph(25)
V(f)$x <- ec$x_vec
V(f)$y <- ec$y_vec
plot(f)
E(f)$weight <- ws
is_weighted(f)
f <- set_vertex_attr(f, "name", value=LETTERS[1:25])
plot(f, edge.width=f$weight*10, layout=layout.grid)
f2 <- delete.vertices(f, c("G","H","I","L","N","Q","R","S"))
plot(f2, edge.width=E(f2)$weight*5)

d0 <-  getDgSparse(f0)
d <- getDgSparse(f)
all(d==d0)

(any(graph.laplacian(f) != t(d)%*%d))  
l0 = abs(crossprod(d0))
diag(l0) = 0
gr0 = graph.adjacency(l0, mode="upper")
is_weighted(gr0)
plot(gr0, layout=layout.grid)
cl0 = clusters(gr0)

l = abs(crossprod(d))
diag(l) = 0
gr = graph.adjacency(l, mode="upper", weighted = TRUE)

is_weighted(gr)
plot(gr, layout.g)
cl = clusters(gr)
?graph.adjacency()



# for our graph
D <- getDgSparse(real_g)
L = abs(crossprod(D))
diag(L) = 0
gr = graph.adjacency(L,mode="upper") # Underlying graph
cl = clusters(gr)

# poss 1 delete all cl instead if number 1
# poss 2 do weighted
plot(cl)

V(real_g)$cl_memb <- cl$membership
plot(real_g,vertex.size = 1, edge.width=0.00001,
     vertex.label = NA, vertex.color = cl$membership*1)


# TESTING MORE ################################################################


