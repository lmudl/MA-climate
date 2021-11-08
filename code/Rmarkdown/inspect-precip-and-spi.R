# inspect spi
dim(target)
plot(target)
min(target)
max(target)
min(c(target))
any(is.na(target))
target[1:10,1:10]

target <- trim_data(target, 3)
min(target)
max(target)
any(is.infinite(target))
inf_ind <- which(is.infinite(target), arr.ind = TRUE)
# first col gives row, second col gives col of inf
# values in target
cols <- unique(inf_ind[,2])
# in target rows are points and cols are months
dim(target)
# so now we check respective months in precip data
precip <- load_target("data/processed/deseasonalised_precip.rds")
dim(precip)
cols[1]
plot(density(precip[,3]))
# what should we plot?
# maybe inspect the SPI's for the respective rows
# the gamma parameters are fitted for each month seperately
# so SPI says for example, this January was extremely
# wet or dry compared to all the other Januarys in our
# data

# CHIRPS data are already anomalies to climatology?
# units total mm for given time step
# mm/pentad, mm/months
# TODO find out why negative values in precipitation
# DONE negative values on deseasonalised data not on 
# precip.
# TODO if negative values because there is already
#   use of anomalies, maybe SPI is not the correct
#   procedure because it will also use anomaly comparison
# TODO read reference paper, find out which data set
#   they used for chirps
# TODO find out what that dataset used

# TODO should I first deseasonalise and then compute SPI
# or first SPI and then deseasonalise
# or is deseasonalise and SPI not compatible

