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
xml_data_ext <- llply(xml_data, xpathSApply, path = c("//*/arr", "//*/file", "//*/str"))
val <- llply(flatten(xml_data_ext), xmlValue)
att <- llply(flatten(xml_data_ext), xmlAttrs)

xml_d <- lst(att, val)


unique_attrs <- unique(as.character(unlist(xml_d$att)))
unique_vals <- unique(as.character(unlist(xml_d$val)))

uid <- as.data.frame(unlist(xml_d$val[which(xml_d$att == 'PID')])) %>%
  setNames(., 'uid')

iris_instrument_authors <- as.data.frame(unlist(xml_d$val[which(xml_d$att == 'iris.instrument.author')])) %>%
  setNames(., 'instrument_authors') 

instrument_type <- as.data.frame(unlist(xml_d$val[which(xml_d$att == 'iris.instrument.instrumentType')])) %>%
  setNames(., 'instrument_type')

instrument_research_area <- as.data.frame(unlist(xml_d$val[which(xml_d$att == 'iris.instrument.researchArea')]))  %>%
  setNames(., 'instrument_research_area')

participant_type <- as.data.frame(unlist(xml_d$val[which(xml_d$att == 'iris.participants.participantType')])) %>%
  setNames(., 'participant_type')

iris_meta <- bind_cols(uid, iris_instrument_authors, instrument_type,
                       instrument_research_area, participant_type)

# Not the same length; fields missing in xml files-------------------------------------

# instrument_subject <- unlist(xml_d$val[which(xml_d$att == 'dc.subject')])
  
# instrument_source_language <- unlist(xml_d$val[which(xml_d$att == 'iris.instrument.sourceLanguage')])

# instrument_data_type <- unlist(xml_d$val[which(xml_d$att == 'iris.instrument.dataType')])

# instrument_linguistic_target <- unlist(xml_d$val[which(xml_d$att == 'iris.instrument.linguisticTarget')])

# participant_target_language <- unlist(xml_d$val[which(xml_d$att == 'iris.participants.targetLanguage')])

# participant_first_language <- unlist(xml_d$val[which(xml_d$att == 'iris.participants.firstLanguage')])

# participant_proficiency <- unlist(xml_d$val[which(xml_d$att == 'iris.participants.proficiencyLearner')])

#-------------------------------------------------------------------
iris_author_id <- 1:length(iris_instrument_authors)

authors <- as.data.frame(cbind(iris_author_id, iris_instrument_authors))
  



