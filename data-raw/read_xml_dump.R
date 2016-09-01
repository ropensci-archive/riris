library(XML); library(xml2); library(plyr); library(purrr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(reshape2); library(listless)


## Read in solr dump------------------------------------
# 
# download.file("http://dlib.york.ac.uk/irisdump.zip", 
#          destfile="data-raw/solr_dumps/5-23-2016/irisdump.zip", mode="wb") 
# unzip ("data-raw/solr_dumps/5-23-2016/irisdump.zip", 
#        exdir = "data-raw/solr_dumps/5-23-2016", overwrite = TRUE)




# Read in data -----------------------------------------


files <- list.files('data-raw/solr_dumps/5-23-2016', pattern = '.xml') 
Files <- files[1:6] # for testing the for loops


# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/5-23-2016",paste(files, sep=''))

xml_data <- map(filepath, xmlParse, useInternalNodes = TRUE)


# Begin making large searchable list of data frames --------------------------------

data_top <- xml_data %>%
  map(., xmlRoot)

attribs <- data_top %>%
  map(., xpathSApply, c("//*/arr", "//*/str[@name= 'PID']", 
                        "//*/str[@name= 'iris.hasmaterials']"), xmlGetAttr, 'name')  %>%
  .[1:length(.)-1]



values <- data_top %>%
  map(., xpathSApply, c("//*/arr", "//*/str[@name= 'PID']", 
                        "//*/str[@name= 'iris.hasmaterials']"), getChildrenStrings)%>%
  .[1:length(.)-1] %>%
  at_depth(., 2, str_c, collapse = "; ") %>%
  map(., as_data_frame, validate = FALSE) 


for (i in 1:length(values)) {
  
  colnames(values[[i]]) <- attribs[[i]]
  colnames(values[[i]]) <- gsub("\\.","_", names(values[[i]]))
  colnames(values[[i]]) <- tolower(gsub("([A-Z])", '_\\1', names(values[[i]])))
  colnames(values[[i]]) <- gsub("_p_i_d", "iris_pid", names(values[[i]]))
  colnames(values[[i]]) <- gsub("dc", "iris", names(values[[i]]))
  
}
  
iris_meta <- values %>%
  map(., select, contains('iris_'))
  


iris_metadata <- reduce(iris_meta, full_join)


devtools::use_data(iris_metadata, overwrite = TRUE, internal = TRUE)


# Make exploration dataframes ---------------------------------------------------------

# Extract based on a list of attributes only (PID not extracted; need str for that)

iris_instrument_author <- xml_data %>%
  map(., xpathApply, c("//arr[@name= 'iris.instrument.author']"), 
      getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_author')

iris_instrument_instrument_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.instrumentType']", 
      getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_instrument_type')

iris_instrument_research_area <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.researchArea']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_research_area')

iris_instrument_linguistic_target <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.linguisticTarget']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_linguistic_target')

iris_instrument_data_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.dataType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_data_type')

iris_participants_proficiency_learner <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.proficiencyLearner']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_participants_proficiency_learner')

iris_participants_domain_of_use <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.domainOfUse']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_participants_domain_of_use')

iris_participants_participant_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.participantType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_participants_participant_type')

iris_instrument_funder <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.funder']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_instrument_funder')

iris_participants_target_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.targetLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_participants_target_language')

iris_participants_first_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.firstLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'iris_participants_first_language') 

devtools::use_data(iris_instrument_author, overwrite = TRUE)
devtools::use_data(iris_instrument_instrument_type, overwrite = TRUE)
devtools::use_data(iris_instrument_research_area, overwrite = TRUE)
devtools::use_data(iris_instrument_linguistic_target, overwrite = TRUE)
devtools::use_data(iris_instrument_data_type, overwrite = TRUE)
devtools::use_data(iris_participants_proficiency_learner, overwrite = TRUE)
devtools::use_data(iris_participants_domain_of_use, overwrite = TRUE)
devtools::use_data(iris_participants_participant_type, overwrite = TRUE)
devtools::use_data(iris_participants_target_language, overwrite = TRUE)
devtools::use_data(iris_participants_first_language, overwrite = TRUE)


# Create list of download urls with PIDs


# d_pids <- data_top %>%
#   map(., xpathSApply, c("//*/str[@name= 'PID']"), getChildrenStrings)  %>%
#   .[1:length(.)-1]
# 
# 
# url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# york: 807442, 808184, 822264, 822275, 822276, 822277, 822278, 822279, 822280, 822312 (among others) are missing the file url and code mistake

# This is not working----------------------------

d_urls <- data_top %>%
  map(., xpathSApply, c("//*/file"), xmlAttrs, append = FALSE)  %>%
  .[1:length(.)-1] %>%
  map(., as_data_frame, validate = FALSE) 

d_record <- data_top %>%
  map(., xpathSApply, c("//record"), xmlGetAttr, 'pid')  %>%
  .[1:length(.)-1]
  
  # lmap_if(., is.character, str_extract_all, url_pattern) 
v <- d_urls

for (i in 1:length(v)) {
  
  colnames(v[[i]]) <- d_record[[i]]

}


