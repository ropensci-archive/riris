library(XML); library(xml2); library(plyr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(purrr)


## Read in solr dump------------------------------------

t <- unzip('http://dlib.york.ac.uk/irisdump.zip', 
           exdir = 'data-raw/solr_dumps/5-23-2016')

files <- list.files('data-raw/solr_dumps/5-23-2016', pattern = '.xml') 
Files <- files[1:6] # for testing the for loops

# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/5-23-2016/")

# working best way to read this in and access it

read_in <- function(x){
  xmlParse(x, useInternalNodes = TRUE)
}


xml_data <- lapply(paste(filepath, files, sep = '/'), read_in)


# Extracting individually ------------------------------------------------------------------------

# Extract based on a list of attributes only (PID not extracted; need str for that)


authors <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.author']", getChildrenStrings) %>%
  unlist(.) %>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'authors')

instrument_types <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.instrumentType']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  set_names(., 'instrument_types')

research_areas <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.researchArea']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'research_areas')

linguistic_feature <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.linguisticTarget']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'linguistic_feature')

data_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.dataType']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'data_type')

learner_proficiency <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.proficiencyLearner']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'learner_proficiency')

language_domain <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.domainOfUse']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'language_domain')

participant_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.participantType']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'participant_type')

funder <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.funder']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  paste(.) %>%
  unique(.) %>%
  as.data.frame(.) %>%
  set_names(., 'funder')

participant_target_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.targetLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  # paste(.) %>%
  unique(.) %>%
  as.data.frame(., stringsAsFactors = FALSE) %>%
  set_names(., 'participant_target_language')

participant_first_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.firstLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  # str_split(., " / ") %>% # Splits up terms that are together in IRIS
  # unlist(.)%>%
  # paste(.) %>%
  unique(.) %>%
  # as.data.frame(.) %>%
  set_names(., 'participant_first_language') 


# Returns ID information  ------------------------------------------------
PID <- xml_data %>%
  map(., xpathApply, "//str[@name= 'PID']",xmlValue) %>%
  unlist(.) %>%
  paste(.)


# Returns file information

# Attempt to extract all (not working well) ---------------------------------------------------------------


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

all_values <- map(xml_data, xpathApply, paste("//arr[@name= '",att,"']", sep = ""), 
                  xmlToList) %>%
  unlist(.)%>%
  paste(.) %>%
  unique(.)








