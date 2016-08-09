library(XML); library(xml2); library(plyr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(purrr); library(reshape2)


## Read in solr dump------------------------------------

t <- unzip('http://dlib.york.ac.uk/irisdump.zip', 
           exdir = 'data-raw/solr_dumps/5-23-2016')



# Read in data -----------------------------------------


files <- list.files('data-raw/solr_dumps/5-23-2016', pattern = '.xml') 
Files <- files[1:6] # for testing the for loops

# Separate dataframes

filepath <- file.path("data-raw/solr_dumps/5-23-2016",paste(files, sep=''))



for(i in files){
  filepath <- file.path("data-raw/solr_dumps/5-23-2016",paste(i, sep=''))
  i <- str_replace_all(i, '.xml', '_xml')
  assign(i, xmlParse(filepath))
}

# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/5-23-2016",paste(files, sep=''))

xml_data <- map(filepath, xmlParse, useInternalNodes = TRUE)


# Begin making large searchable list of data frames --------------------------------

data_top <- xml_data %>%
  map(., xmlRoot)

attribs <- data_top %>%
  map(., xpathSApply, c("//*/arr", "//*/str[@name= 'PID']"), xmlGetAttr, "name") 

values <- data_top %>%
  map(., xpathSApply, c("//*/arr", "//*/str[@name= 'PID']"), getChildrenStrings)%>%
  at_depth(., 2, str_c, collapse = "; ") %>%
  map(., as_data_frame, validate = FALSE) 


for (i in 1:length(values)) {
  
  colnames(values[[i]]) <- attribs[[i]]
  
}
  
iris_meta <- values

devtools::use_data(iris_meta, overwrite = TRUE)

# For when I need to acces the file
# "//*/file"


ns <- data_top %>%
  map(., getNodeSet, c("//*/arr", "//*/file", "//*/str[@name= 'PID']"))


# Make exploration dataframes ------------------------------------------------------------------------

# Extract based on a list of attributes only (PID not extracted; need str for that)

authors <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.author']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'authors')

instrument_types <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.instrumentType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'instrument_types')

research_areas <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.researchArea']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'research_areas')

linguistic_features <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.linguisticTarget']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'linguistic_features')

data_types <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.dataType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'data_types')

learner_proficiency <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.proficiencyLearner']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'learner_proficiency')

language_domains <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.domainOfUse']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'language_domains')

participant_types <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.participantType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'participant_types')

funders <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.funder']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'funders')

participant_target_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.targetLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'participant_target_language')

participant_first_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.firstLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'participant_first_language') 

devtools::use_data(authors, overwrite = TRUE)
devtools::use_data(instrument_types, overwrite = TRUE)
devtools::use_data(research_areas, overwrite = TRUE)
devtools::use_data(linguistic_features, overwrite = TRUE)
devtools::use_data(data_types, overwrite = TRUE)
devtools::use_data(learner_proficiency, overwrite = TRUE)
devtools::use_data(language_domains, overwrite = TRUE)
devtools::use_data(participant_types, overwrite = TRUE)
devtools::use_data(participant_target_language, overwrite = TRUE)
devtools::use_data(participant_first_language, overwrite = TRUE)


# Returns ID information  ------------------------------------------------
PID <- xml_data %>%
  map(., xpathApply, "//str[@name= 'PID']",xmlValue) %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'PID') 

has_materials <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.hasmaterial']",xmlValue) %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'has_materials') 



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



# Making iris_meta ----------------------------------------------

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
  data_frame(.) %>%
  set_names(., 'uid')

iris_instrument_authors <- xml_d$val[which(xml_d$att == 'iris.instrument.author')] %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'instrument_authors') 

instrument_type <- xml_d$val[which(xml_d$att == 'iris.instrument.instrumentType')] %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'instrument_type')

instrument_research_area <- xml_d$val[which(xml_d$att == 'iris.instrument.researchArea')] %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'instrument_research_area')

participant_type <- xml_d$val[which(xml_d$att == 'iris.participants.participantType')] %>%
  unlist(.) %>%
  data_frame(.) %>%
  set_names(., 'participant_type')

iris_meta <- bind_cols(uid, iris_instrument_authors, instrument_type,
                       instrument_research_area, participant_type)






