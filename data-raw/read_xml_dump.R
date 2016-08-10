library(XML); library(xml2); library(plyr); library(purrr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(reshape2)


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

# For when I need to acces the file
# "//*/file"


ns <- data_top %>%
  map(., getNodeSet, c("//*/arr", "//*/file", "//*/str[@name= 'PID']"))


# Make exploration dataframes ------------------------------------------------------------------------

# Extract based on a list of attributes only (PID not extracted; need str for that)

author <- xml_data %>%
  map(., xpathApply, c("//*/str[@name= 'PID']",
                       "//arr[@name= 'iris.instrument.author']"), getChildrenStrings)%>%
  .[1:length(.)-1] %>%
  at_depth(., 2, str_c, collapse = "; ") %>%
  map(., as_data_frame, validate = FALSE) %>%
  map(., set_names, c('PID', 'iris_instrument_author')) %>%
  ldply(., data.frame)





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


# Use this idea to pull URL and meta data from sysdata

xml_data_ext <- xml_data %>%
  map(., xpathApply, path = c("//*/arr", "//str[@name= 'PID']"))

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






