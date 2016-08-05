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


xml_data <- lapply(paste(filepath, Files, sep = '/'), read_in)

# Returns unique entries---------------------------------------------------
# Propose use of these dataframes: exploring the IRIS repository 
# by parameters of interest

# Extract based on a list of attributes only (PID not extracted; need str for that)

xml_data_ext <- xml_data %>%
  llply(., xpathApply, path = c("//*/arr"))

val <- xml_data_ext %>%
  flatten(.) %>%
  llply(., xmlValue)

att <- xml_data_ext %>%
  flatten(.) %>%
  llply(., xmlAttrs) %>%
  unlist(.) %>%
  paste(.) %>%
  unique(.)


# Need to find a way to preserve the attribute's name! ------------------------------------------

all_attrs <- map(xml_data, xpathApply, paste("//arr[@name= '",att,"']", sep = ""), 
                 getChildrenStrings) %>%
  unlist(.)%>%
  paste(.) %>%
  unique(.)

# Extracting individually
authors <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.author']", getChildrenStrings) %>%
  unlist(.) %>%
  paste(.) %>%
  unique(.)

instrument_types <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.instrumentType']", getChildrenStrings) %>%
  unlist(.) %>%
  str_split(., " / ") %>% 
  unlist(.)%>%
  paste(.) %>%
  unique(.)

research_areas <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.researchArea']", getChildrenStrings) %>%
  unlist(.) %>%
  str_split(., " / ") %>% 
  unlist(.)%>%
  paste(.) %>%
  unique(.)


# Returns values ------------------------------------------------
PID <- xml_data %>%
  map(., xpathApply, "//str[@name= 'PID']",xmlValue) %>%
  unlist

value <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'dc.creator']",xmlValue) %>%
  unlist



# Work with stuff above here for now.


## Create dataframes of query possibilities--------------------------
xml_data_ext <- xml_data %>%
  llply(., xpathApply, path = c("//*/arr", "//*/file", "//*/str"))

val <- xml_data_ext %>%
  flatten(.) %>%
  llply(., xmlValue)

att <- xml_data_ext %>%
  flatten(.) %>%
  llply(., xmlAttrs)

xml_d <- lst(att, val)

unique_attrs <- xml_d %>%
  unlist(att) %>%
  unique(.)
  
unique_vals  <- xml_d %>%
  unlist(val) %>%
  unique(.)

uid <- xml_d$val[which(xml_d$att == 'PID')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'uid')

iris_instrument_authors <- xml_d$val[which(xml_d$att == 'iris.instrument.author')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_authors') 

instrument_type <- xml_d$val[which(xml_d$att == 'iris.instrument.instrumentType')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_type')

instrument_research_area <- xml_d$val[which(xml_d$att == 'iris.instrument.researchArea')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_research_area')

participant_type <- xml_d$val[which(xml_d$att == 'iris.participants.participantType')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'participant_type')

iris_meta <- bind_cols(uid, iris_instrument_authors, instrument_type,
                       instrument_research_area, participant_type)

# Not the same length; fields missing in xml files-------------------------------------

instrument_subject <- xml_d$val[which(xml_d$att == 'dc.subject')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_subject')

instrument_source_language <- xml_d$val[which(xml_d$att == 'iris.instrument.sourceLanguage')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_source_lang')

instrument_data_type <- xml_d$val[which(xml_d$att == 'iris.instrument.dataType')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_data_type')

instrument_linguistic_target <- xml_d$val[which(xml_d$att == 'iris.instrument.linguisticTarget')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'instrument_ling_target')

participant_target_language <- xml_d$val[which(xml_d$att == 'iris.participants.targetLanguage')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'participant_target_lang')

participant_first_language <- xml_d$val[which(xml_d$att == 'iris.participants.firstLanguage')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'participant_first_lang')

participant_proficiency <- xml_d$val[which(xml_d$att == 'iris.participants.proficiencyLearner')] %>%
  unlist(.) %>%
  as.data.frame(.) %>%
  setNames(., 'participant_proficiency')
                                    


# Junk? --------------------------------------------------------------------

attrs <- xml_data[[1]]["/record/solr/response/result/doc/arr"] %>%
  xmlSApply(., xmlAttrs) %>%
  t(.) %>%
  as.data.frame(., stringsAsFactors = FALSE) %>%
  setNames(., 'attrs')

values <- xml_data[[1]]["/record/solr/response/result/doc/arr"] %>%
  xmlSApply(., xmlValue) %>%
  t(.) %>%
  as.data.frame(., stringsAsFactors = FALSE) %>%
  setNames(., 'values')

# Notes --------------------------------------------------
# splice large xml dataset by pid, test for presence of variable, 
# extract variable if TRUE, return NA if FALSE; create dataframe



