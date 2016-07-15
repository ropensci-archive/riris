library(XML); library(xml2); library(plyr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(purrr)
# library(rjson); library(RJSONIO); library(jsonlite)

xmlfile <- xmlParse("data-raw/solr_dumps/5-23-2016/york_29203.xml")

xmlSize(xmlfile)

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
  # assign(i, xpathSApply(xmlParse(filepath), c("//*/arr", "//*/file")))
}




# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/5-23-2016/")

# working best way to read this in and access it

read_in <- function(x){
  # as_list(read_xml(x))
  # read_xml(x)
  # xmlToList(xmlParse(x))
  xmlParse(x)
}

xml_data <- lapply(paste(filepath, files, sep = '/'), read_in)


attrs <- as.data.frame(t(xmlSApply(xml_data[[1]]["/record/solr/response/result/doc/arr"],xmlAttrs)),
                       stringsAsFactors=FALSE)


values <- as.data.frame(t(xmlSApply(xml_data[[1]]["/record/solr/response/result/doc/arr"],xmlValue)),
                        stringsAsFactors=FALSE)

## Create dataframes of query possibilities--------------------------
xml_data_ext <- llply(xml_data, xpathSApply, path = c("//*/arr", "//*/file"))
val <- llply(flatten(xml_data_ext), xmlValue)
att <- llply(flatten(xml_data_ext), xmlAttrs)

xml_d <- lst(att, val)


unique_attrs <- unique(as.character(unlist(xml_d$att)))
unique_vals <- unique(as.character(unlist(xml_d$val)))

iris_instrument_authors <- unique(as.character(unlist(xml_d$val[which(xml_d$att == 'iris.instrument.author')])))
iris_author_id <- 1:length(instrument_authors)

authors <- as.data.frame(cbind(iris_author_id, iris_instrument_authors))

instrument_type <- as.character(unlist(xml_d$val[which(xml_d$att == 'iris.instrument.instrumentType')]))



## Extract  Metadata------------------------------------

test <- xpathSApply(york_29203_xml, c("//*/arr", "//*/file"))
t <- llply(test, xmlAttrs)
u <- llply(test, xmlValue)


d <- xmlToDataFrame(york_29229_xml["/record/solr/response/result/doc"], stringsAsFactors = FALSE)


  



