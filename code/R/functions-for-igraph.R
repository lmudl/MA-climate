# check vignettes of genlasso
library(genlasso)
vignette("genlasso")
browseVignettes()

set.seed(1)
n=100
i=1:n
y=(i > 20 & i < 30) + 5*(i > 50 & i < 70) + rnorm(n, sd = 0.1)
out = fusedlasso1d(y)
plot(out, lambda=1, style = "path")

set.seed(1)
y = matrix(runif(256), 16, 16)
i = (row(y) - 8.5)^2 + (col(y) - 8.5)^2 <= 4.2
y[i] = y[i] + 1
out = fusedlasso2d(y)
co = coef(out, nlam = 5)
plot(out)
?fusedlasso

y = 1:10
X = cbind(1:10,)

# Fused lasso on a custom graph
set.seed(0)
edges = c(1,2,1,3,1,5,2,4,2,5,3,6,3,7,3,8,6,7,6,8)
gr = graph(edges=edges,directed=FALSE)
plot(gr)
y = c(1,1,0,1,1,0,0,0) + rnorm(8,0.1)

# Can either pass the graph object directly, or
# first construct the penalty matrix, and then
# pass this
a1 = fusedlasso(y,graph=gr)
plot(a1,numbers=TRUE)
D = getDgSparse(gr)
a2 = fusedlasso(y,D=D)

# The 2d fused lasso with a predictor matrix X
set.seed(0)
dim1 = dim2 = 16
p = dim1*dim2
n = 300
X = matrix(rnorm(n*p),nrow=n)
beta0 = matrix(0,dim1,dim2)

beta0[(row(beta0)-dim1/2)^2 + (col(beta0)-dim2/2)^2 <=
        (min(dim1,dim2)/3)^2] = 1
y = X %*% as.numeric(beta0) + rnorm(n)
X[,2] <- NA
#beta0[2,2] <- NA
# Takes about 30 seconds for the full solution path
out = fusedlasso2d(y,X,dim1=dim1,dim2=dim2)

# Grab the solution at 8 values of lambda over the path
a = coef(out,nlam=8)

# Plot these against the true coefficients
par(mar=c(1,1,2,1),mfrow=c(3,3))
cols = terrain.colors(30)
zlim = range(c(range(beta0,na.rm = TRUE),range(a$beta)))
image(beta0,col=cols,zlim=zlim,axes=FALSE)

for (i in 1:8) {
  image(matrix(a$beta[,i],nrow=dim1),col=cols,zlim=zlim,
        axes=FALSE)
  mtext(bquote(lambda==.(sprintf("%.3f",a$lambda[i]))))
}

#
demo(package="igraph") 

# So I need to define graph and give X
a <- make_lattice(c(3,3))
plot(a, layout=layout_on_grid)
b <- delete_vertices(a,8)
plot(b)
plot(b, layout=layout_on_grid)
a
str(a)

c <- make_lattice(c(5,5))
plot(c, layout=layout_on_grid)
d <- delete_vertices(c, c(17:19,22:24))
plot(d)

####
# I would like to create a network with a missing part
# on the top(just like the landdata wont have temperatures)
# create matrix delete elemebt
# in the end put zero again inside for the plot

# or easier
# create graph
# find out how to say which p is which node
# fit fused lasso
# find best lambda
# plot the network and size of nodes accordingly to their coef value
# OR plot and colour along a gg colour gradient
# OR refill into raster and use already existing plotting options (self-written)

# take sst rater
# create graph object from it
m <- matrix(1:9, ncol = 3)
m[2,2] <- NA
a <- make_lattice(c(3,3))
#plot(a, layout = layout_on_grid)
V(a)
V(a)$x <- c(rep(1:3,3))
V(a)$y <- c(rep(1,3), rep(2,3), rep(3,3))
plot(a)
a2 <- delete_vertices(a, 5)
plot(a2)

b <- set_vertex_attr(graph = a, name = "island", index = V(a),
                     value = c(rep(FALSE, 4), TRUE, rep(FALSE, 4)))
vertex_attr(b)
d <- delete_vertices(b, vertex_attr(b)$island)
plot(d)
library(ggplot2)
library(ggraph)
ggraph(d) + geom_edge_link() + geom_node_point()

# remember to load sst first IMPORTANT WORKS!!!! ####
#ext <- extent(-180,0,-50,40)
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst_c <- crop(sst,ext)
co <- xyFromCell(sst_c, 1:ncell(sst_c)) 
dim(sst_c)
e <- make_lattice(c(46,90))

create_coords <- function(vec) {
  x_vec <- rep(c(1:vec[1]), vec[2])
  y_vec <- c()
  for(i in 1:vec[2]) {
    y_vec <- append(y_vec,(rep(i, vec[1])))
  }
  df <- as.data.frame(cbind(x_vec, y_vec))
  return(df)
}
undebug(create_coords)
create_coords(c(3,4))
h <- make_lattice(c(14,10))
hc <- create_coords(c(3,4))
V(h)$x <- hc$x_vec
V(h)$y <- rev(hc$y_vec)
plot(h)
dims <- c(46,90)
e <- make_lattice(dims)
ec <- create_coords(dims)
V(e)$x <- ec$x_vec
V(e)$y <- rev(ec$y_vec)
plot(e, vertex.size = 0.001, edge.width=0.00001,
     vertex.label = NA)
######################

#e <- graph()
V(e)$x <- co[,1] + 180
V(e)$y <- co[,2] + 50
e
plot(e, vertex.size = 0.001, edge.width=0.00001,
     vertex.label = NA, layout = layout_on_grid)


plot(sst)
ggraph(e) +
  geom_edge_link() + geom_node_point()
## test graph from raster
z <- head(co)
z <- make_lattice(dimvector = c(3,4))
?igraph::as_data_frame()


plot(z)
plot(z, vertex.size = 0.001, edge.width = 0.01,
     vertex.label = NA)

##

dim(sst_c)
r <- make_lattice(c(46,20))
dim(sst_c)
m <- as.matrix(sst_c[[1]])
dim(m)
length(m)
any(is.na(m))
del <- which(is.na(m))
V(r)
max(V(r))
r <- delete_vertices(r, del)



##
e2 <- make_lattice(c(10, 10))
V(e2)$x <- c(-)
edges(e2)

plot(e2, vertex.size=0.005,
     vertex.label.cex=0.0004, edge.width = 0.1)
ggraph(e2) +
  geom_edge_link() +
  geom_node_point() +
  #geom_node_label(label.size = 0.25)
  
  
  
  # letter from 1-9, then give 1-9 values, middle is NA
  # then drop those that have NA as attribute
  # this graph should be ready for fitting fusedlasso
  b <- graph()



#make_lattice_from_grid <- function(grid) {
# TODO very important
#}


# fit fused lasso
# remap on raster with NA's and plot


set.seed(0)
dim1 = dim2 = 3
p = dim1*dim2
n = 100
X = matrix(rnorm(n*p),nrow=n)
X <- X[,-2]
beta0 = matrix(0,dim1,dim2)
beta0[2,2] = 1
beta0 <- as.numeric(beta0)[-2]
y = X %*% beta0 + rnorm(n)
# X[,2] <- NA
#beta0[2,2] <- NA
# Takes about 30 seconds for the full solution path
b <- delete_vertices(a, 2)
plot(b)
out = fusedlasso(y,X,graph=b)
out$beta
# Grab the solution at 8 values of lambda over the path
q = coef(out,nlam=8)
q
# Plot these against the true coefficients
par(mar=c(1,1,2,1),mfrow=c(3,3))
cols = terrain.colors(30)
zlim = range(c(range(beta0,na.rm = TRUE),range(q$beta)))
beta[]
image(beta0,col=cols,zlim=zlim,axes=FALSE)

for (i in 1:8) {
  image(matrix(a$beta[,i],nrow=dim1),col=cols,zlim=zlim,
        axes=FALSE)
  mtext(bquote(lambda==.(sprintf("%.3f",a$lambda[i]))))
}

###
library(igraph)
library(raster)
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
dim(sst)
co <- coordinates(sst)
df <- data.frame(long = co[,1], lat = co[,2])
locs <- data.frame(exactloc = paste(co[,1], co[,2]))
g <- graph_from_data_frame(, directed = FALSE)
V(g)$x <- co[,1]
V(g)$y <- co[,2]

nodes <- data.frame(name = as.character(c(1:ncell(sst))))
