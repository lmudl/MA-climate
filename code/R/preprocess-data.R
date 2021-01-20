# cdo code for changing the files
# 1st merge the single sst files into one with mergetime *.nc
# already done maybe write code her again
# 2nd set reftime for both files
# 3nd constrain time to f.e. 1960 - today
# 4th constrain window of lon and lat
rm(list = ls())
getwd()
setwd("MA-climate")
Sys.getenv("PATH")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\cygwin64\\bin\\",sep = ";"))
library(ClimateOperators)
################################################################################
# merge all single sst files into on with mergetimce *.nc
# single file is called ersst.v5.yearmonth.nc
# precip is called precip.mon(thly).total.1x1.v2018.nc
# how to call the data files after preprocessing?

# interim is only needed when there are steps in between
# ersst.interim
# cdo mergetime *.nc outfile
# takes 6.52 sec test if faster on shell directly
cdo("mergetime data/raw/sst/*.nc data/interim/sst-interim.nc")
################################################################################

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
set_ref_time <- function(input_file_path, output_file_path,
                         reftime, unit) {
  cdo(paste(csl("setreftime",reftime, unit), input_file_path,
                              output_file_path))
}

###############################################################################
reftime <- "1900-1-1,00:00:00"
unit <- "months"

infile <- "data/raw/precip/precip.mon.total.1x1.v2018.nc" 
outfile <- "data/interim/precip-interim.nc"

set_ref_time(input_file_path = infile, output_file_path = outfile,
              reftime = reftime, unit = unit)

infile <- "data/interim/sst-interim.nc"
outfile <- "data/interim/sst-interim.nc"

set_ref_time(input_file_path = infile, output_file_path = outfile,
             reftime = reftime, unit = unit)
################################################################################

# we can not directly save outfile to same file as infile
# need to save to new file and maybe delete afterwards
# try with to delete file from first step 
# ://stackoverflow.com/questions/14219887/how-to-delete-a-file-with-r/14220099
# and maybe rename afterwards
# https://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r
time_span <- "1900/200"

select_years <- function(time_span, infile) {
  outfile <- paste0(infile,"2")
  cdo(ssl(csl("-selyear", time_span), infile, outfile))
  #file.remove(infile)
  #file.rename(outfile, infile)
}

select_years(time_span = "1900/2000", "data/interim/precip-interim.nc")
select_years(time_span = "1900/2000", "data/interim/sst-interim.nc")

