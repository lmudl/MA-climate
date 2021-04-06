# cdo code for changing the files
# 1st merge the single sst files into one with mergetime *.nc
# already done maybe write code her again
# 2nd set reftime for both files
# 3nd constrain time to f.e. 1960 - today
# 4th constrain window of lon and lat
rm(list = ls())
getwd()
setwd("Repos/MA-climate")
Sys.getenv("PATH")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\cygwin64\\bin\\",sep = ";"))
# Sys.setenv(PATH = paste(Sys.getenv("PATH"), 
#                          "C:\\Users\\Mufasa\\AppData\\Local\\Packages\\CanonicalGroupLimited.Ubuntu20.04onWindows_79rhkp1fndgsc\\LocalState\\rootfs\\usr\\bin"),
#                          sep = ";")
# Sys.setenv(PATH = paste(Sys.getenv("PATH"),
#                         "\\wsl$\\Ubuntu-20.04\\usr\\bin\\", sep =";"))
library(ClimateOperators)
cdo("--version")
################################################################################
# 1st merge single sst
# merge all single sst files into on with mergetimce *.nc
# single file is called ersst.v5.yearmonth.nc
# precip is called precip.mon(thly).total.1x1.v2018.nc
# how to call the data files after preprocessing?

# interim is only needed when there are steps in between
# ersst.interim
# cdo mergetime *.nc outfile
# takes 6.52 sec test if faster on shell directly

merge_time <- function(infile, outfile) {
  cdo(ssl("mergetime", infile, outfile))
}

merge_time(infile = "data/raw/sst/monthly/*.nc", outfile = "data/raw/sst/sst-merged.nc")

################################################################################
# 2nd set reftime for both files
# we need setreftime for both files actually
# cdo("--version")
# infile <- "data/raw/sst/ersst.v5.202008.nc"
# cdo(ssl("-info", infile))
# cdo("setreftime,1900-1-1,00:00:00,months data/raw/precip/precip.mon.total.1x1.v2018.nc data/raw/testing/test.nc")
# 
# test <- nc_open("data/raw/testing/test.nc")
# {
#   sink('test2.txt')
#   print(test)
#   sink()
# }
# test_t <- ncvar_get(test, "time")
# max(test_t)
set_ref_time <- function(infile, outfile, reftime, unit) {
  cdo(ssl(csl("-z zip setreftime",reftime, unit), infile,
                              outfile))
}

reftime <- "1900-1-1,00:00:00"
unit <- "months"

set_ref_time(infile = "data/raw/precip/precip.mon.total.1x1.v2018.nc" ,
             outfile = "data/interim/precip-interim.nc",
             reftime = reftime, unit = unit)

set_ref_time(infile =  "data/raw/sst/sst-merged.nc", 
             outfile = "data/interim/sst-interim.nc",
             reftime = reftime, unit = unit)

set_ref_time(infile = "data/raw/cru/scPDSI-1901-2019.nc",
             outfile = "data/interim/cru-interim.nc",
             reftime = reftime, unit = unit)

set_ref_time(infile = "data/raw/hadcrut/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc",
             outfile = "data/interim/hadcrut-interim.nc",
             reftime = reftime, unit = unit)

################################################################################
# 3rd constrain time span
# we can not directly save outfile to same file as infile
# need to save to new file and maybe delete afterwards
# try with to delete file from first step 
# ://stackoverflow.com/questions/14219887/how-to-delete-a-file-with-r/14220099
# and maybe rename afterwards
# https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
time_span <- "1902/2019"

set_out <- function(infile) {
  new <- strsplit(x = infile, ".nc", fixed = TRUE)[[1]][1]
  out <- paste0(new, "2.nc")
  return(out)
}

clean_names <- function(infile, outfile) {
  file.remove(infile)
  file.rename(outfile, infile)
}

select_years <- function(infile, time_span) {
  outfile <- set_out(infile)
  cdo(ssl(csl("-selyear", time_span), infile, outfile))
  clean_names(infile, outfile)
}

select_years(infile = "data/interim/precip-interim.nc", time_span = time_span)
select_years(infile = "data/interim/sst-interim.nc", time_span = time_span)

select_years(infile = "data/interim/hadcrut-interim.nc", time_span = time_span)
select_years(infile = "data/interim/cru-interim.nc", time_span = time_span)

################################################################################
# 4th rearrange lat/lon

rearrange_latlon <- function(infile, degrees){
  outfile <- set_out(infile)
  cdo(ssl(csl("-sellonlatbox", degrees), infile, outfile))
  clean_names(infile, outfile)
}
## sst
# our data has lon 0 to 360 paper used -180 to 180
# our data has lat -90 to 90 is nice
# only change lat

rearrange_latlon(infile = "data/interim/precip-interim.nc",
                 degrees = c("-180,180,-90,90"))
rearrange_latlon(infile = "data/interim/precip-interim.nc",
                 degrees = c("-180,-2,-40,40"))
## precip
# our data has lon 0 to 360 paper used -180 to 180
# our has lat -90 to 90 is nice
# only change lat

rearrange_latlon(infile = "data/interim/sst-interim.nc",
                 degrees = c("-180,180,-90,90"))
rearrange_latlon(infile = "data/interim/sst-interim.nc",
                 degrees = c("-180,-2,-40,40"))

rearrange_latlon(infile = "data/interim/cru-interim.nc",
                 degrees = c("-72,-55,-9,0"))

################################################################################
#5th choose window of interest


