library(XML)
library(xml2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(purrr)


## Read in solr dump------------------------------------

t <- unzip('http://dlib.york.ac.uk/irisdump.zip', 
           exdir = 'data-raw/solr_dumps/5-23-2016')

files <- list.files('data-raw/solr_dumps/5-23-2016', pattern = '.xml') 
Files <- files[1:6] # for testing the for loops

# Each file as an individual list

for(i in files){
  filepath <- file.path("data-raw/solr_dumps/5-23-2016",paste(i, sep=''))
  i <- str_replace_all(i, '.xml', '_xml')
  assign(i, xmlParse(filepath))
}

# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/5-23-2016/")

read_in <- function(x){
  # as_list(read_xml(x))
  # read_xml(x)
  xmlParse(x)
}

xml_data <- lapply(paste(filepath, files, sep = '/'), read_in)

## Extract  Metadata------------------------------------

test <- xpathSApply(york_29203_xml, c("//*/arr", "//*/file"))

xml_data_ext <- llply(xml_data, xpathSApply, path = c("//*/arr", "//*/file"))

val <- llply(flatten(xml_data_ext), xmlValue)
att <- llply(flatten(xml_data_ext), xmlAttrs)




