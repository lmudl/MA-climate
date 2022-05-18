# check vignettes of genlasso
library(genlasso)
vignette("genlasso")
browseVignettes()
source("code/R/helper-functions.R")
#### igraph plot of raster info ####
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

# remember to load sst first IMPORTANT WORKS!!!! ####
#ext <- extent(-180,0,-50,40)
library(raster)
library(igraph)
library(ggplot2)
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst_c <- crop(sst,ext)

create_coords <- function(vec) {
  x_vec <- rep(c(1:vec[1]), vec[2])
  y_vec <- c()
  for(i in 1:vec[2]) {
    y_vec <- append(y_vec,(rep(i, vec[1])))
  }
  df <- as.data.frame(cbind(x_vec, y_vec))
  return(df)
}


# plot(e, vertex.size = 0.001, edge.width=0.00001,
#      vertex.label = labels)
# begins top left

igraph_from_raster <- function(raster_object) {
  dims <- dim(raster_object)[1:2]
  dims <- rev(dims)
  g <- make_lattice(dims)
  ec <- create_coords(dims)
  V(g)$x <- ec$x_vec
  V(g)$y <- rev(ec$y_vec)
  vals <- values(raster_object[[1]])
  land <- which(is.na(vals))
  g <- delete_vertices(g, land)
  return(g)
}

#debug(igraph_from_raster)
g <- igraph_from_raster(sst)
plot(g, vertex.size = 0.001, edge.width=0.00001,
     vertex.label = NA)

# maybe try now to fit fused lasso with this
# first remove sst NAs and then fit?
# exciting!!!

# genlasso tries ####
s <- readRDS("data/processed/deseasonalised_sst.rds")
#s <- raster(s)
precip <- readRDS("data/processed/deseasonalised_precip.rds")
dim(s)
any(is.na(s))
precip <- apply(precip, 2, mean)
d <- apply(s, 1, function(x) all(is.na(x)))
s <- s[-d,]
dim(s)
s <- t(s)

testmod <- fusedlasso(y=precip,X=s, graph=g)
length(precip)
nrow(s)

# genlasso try with unseasonalised data ###
# prepare sst data with colnames
pr <- readRDS("data/interim/drought/chirps_setreftime_aggregated.rds")
pr <- values(pr)
pr <- apply(pr, 2, mean)
pr <- pr[1:24]

features_path <- "data/interim/sst/ersst_setreftime.nc"
sst <- brick("data/interim/sst/ersst_setreftime.nc", var = "sst")
ext <- extent(-180,0,-50,40)
sst <- crop(sst,ext)
coord <- coordinates(sst)
cnames <- paste(coord[,1], coord[,2])
s <- as.matrix(sst)
s <- t(s)
colnames(s) <- cnames
s <- prepare_sst(s)
s <- s[1:24,]

g <- igraph_from_raster(sst)

length(V(g))
dim(s)

# ex_mod <- fusedlasso(y=pr, X=s, graph=g)
# saveRDS(ex_mod, "results/first_fused_lasso.rds")
plot(ex_mod)
fits <- ex_mod$fit
dim(fits)
z <- apply(fits, 2, function(x) mean((x-pr)^2))
min(z)
which(z==min(z), arr.ind = TRUE)
which(z==min(z))
z[2000]
b <- ex_mod$beta[,2000]
length(b)


ex_mod2 <- fusedlasso(y=pr, X=s, graph=g)
saveRDS(ex_mod2, "results/second_fused_lasso.rds")
date()

co2 <- coef.genlasso(ex_mod2)
length(co2$beta)
dim(co2$beta)
a <- co2$beta[,1000]
plot(density(a))
names(a)
length(cnames)
num_coef_names <- coef_names_to_numeric(colnames(s))
dim(num_coef_names)
coef_mat <- cbind(num_coef_names, a)
plot <- plot_nonzero_coefficients(coef_mat)



all_coef <- coef(mod)[-1,1]
nonzero_coef <- all_coef != 0
nonzero_coef_names <- names(all_coef[nonzero_coef])
num_coef_names <- coef_names_to_numeric(nonzero_coef_names)
coef_mat <- cbind(num_coef_names, all_coef[nonzero_coef])
plt <- plot_nonzero_coefficients(coef_mat)


plot(ts(pr)) + 
plot(ts(fits[,2000]))

plot(density(pr))
mean(pr)
mean(((pr-mean(pr))^2))

# lets create toyexample #####
# create rain
rain <- c(1:50)
preds <- make_lattice(c(5,5))
X <- matrix(rnorm(25*50), ncol=25, nrow=50)
X[,c(1)] <- 1:50

mod <- fusedlasso(y=rain,X=X,graph=preds)
plot(mod)
co <- coef(mod, nlam=10)
m <- matrix(co$beta[,10], nrow = 5)
image(m)
