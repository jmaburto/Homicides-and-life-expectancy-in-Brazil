
# ----------------------------------------------------------------- #
# GBD                                                               #
# ----------------------------------------------------------------- #

# in results tool, I selected:
# Base: Single
# Location: "seklect only countries and territories"
# Year "select all"
# Context "Cause"
# Age "select all"
# Metric - check each Number, percent, rate, probability of death
# Measure "Deaths"
# Sex check both "Male" and "Female"
# Cause check 4: "Total" "C.3.2" "C.3.3" "C.3.4"

# select "download csv"
# You'll be asked Include IDS or Names, select Names
# enter email addy-- will be multiple files

# you'll receive confirmation email immediately,
# but files will take time to generate

# The data will be made available in 9 different zip files
# get a url by right clicking the link and select "copy url" or similar,
# then paste as character strings, like below. YOUR link will be different
# than this one, which will likely expire.
links <- c(
  "http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-1.zip",
  "http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-2.zip"
  # etc etc etc for 9 urls
)
# EASIER:
# or notice that the links are the same except the last digit, find the pattern
# YOU NEED TO CHANGE THIS PATH 
links <- paste0("http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-",
                1:9,".zip")

# now do bulk download like so
for (i in 1:9){
  this.name <- file.path(gbd.folder,paste0("GBD",i,".zip"))
  download.file(url = links[i],
                # simplify names of zip files...
                destfile = this.name)
  # and unpack them
  unzip(file.path(gbd.folder,this.name),exdir=gbd.folder)
}

# get csv names
gbdcsvs <- list.files(gbd.folder,pattern=".csv")

# read and rbind in one go
GBD <- do.call("rbind", lapply(file.path(gbd.folder,gbdcsvs), fread))

# save as Rdata:
save(GBD, file = file.path(gbd.folder,"GBD.Rdata"))

# remove redundant files:
fls  <- list.files(gbd.folder)
fls  <- fls[!fls%in% c("GBD.Rdata","citation.txt")]
file.remove(file.path(gbd.folder,fls))

# remove from memory
rm(GBD)
gc()
