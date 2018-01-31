library(XML); library(xml2); library(plyr); library(purrr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(reshape2); library(rvest)


## Read in solr dump------------------------------------
# 
# download.file("http://dlib.york.ac.uk/irisdump.zip",
#          destfile="data-raw/solr_dumps/5-23-2016/irisdump.zip", mode="wb")
# unzip ("data-raw/solr_dumps/5-23-2016/irisdump.zip",
#        exdir = "data-raw/solr_dumps/5-23-2016", overwrite = TRUE)
# 
# download.file("https://dlib.york.ac.uk/irisdump.zip",
#          destfile="data-raw/solr_dumps/11-12-2017/irisdump.zip", mode="wb")
# unzip ("data-raw/solr_dumps/11-12-2017/irisdump.zip",
#        exdir = "data-raw/solr_dumps/11-12-2017", overwrite = TRUE)




# Read in data -----------------------------------------


files <- list.files('data-raw/solr_dumps/11-12-2017', pattern = '.xml') 
# Files <- files[1:6] # for testing the for loops


# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps/11-12-2017",paste(files, sep=''))

xml_data <- map(filepath, read_xml, encoding = "ISO-8859-1") 


# Begin making large searchable list of data frames --------------------------------

record <- xml_data %>%
  map(., xml_find_all, '/record')

arr <- xml_data %>%
  map(., xml_find_all, '//*/arr')

str <- xml_data %>%
  map(., xml_find_all, '//*/*/str')

fil <- xml_data %>%
  map(., xml_find_all, '//*/file')

xml_data_listcols <- arr %>%{
  tibble(
    values = map(., xml_text, trim = TRUE),
    attribs = map(., xml_attrs, "name"),
    files = map(fil, xml_attrs, "url"),
    record = map(record, xml_attrs, "pid")
  )
} %>%
  slice(., 1:6)
%>%
  unnest(record)# for drafting

# Takes a long time

# download <- function(x, y){
#   for (i in seq_along(x)){
#     download.file(x[i], destfile = paste('data-raw/solr_dumps/11-12-2017/instruments/', y[i], i, sep = "_"))
#   }
# }
# 
# download(xml_data_listcols$files, xml_data_listcols$record)

data_framing <- xml_data_listcols %>%
  unnest(record) 
%>%
  group_by(record) 
%>%
  mutate(values = map(values, ~gsub("([a-z])([A-Z])", "\\1 \\2", .x)))
  mutate(attribs = map(attribs, str_replace_all, pattern = "\\.", replacement = "_")) 
%>%
  mutate(attribs = map(attribs, str_replace_all, pattern = "dc", replacement = "iris")) %>%
  mutate(attribs = map(attribs, ~tolower(gsub("([a-z1-9])([A-Z])", "\\1_\\2", .x)))) %>%
  mutate(attribs = map(attribs, str))
  mutate(values = map(values, as_data_frame),
         attribs = map(attribs, as_data_frame)) %>%
  mutate(df = map2(values, attribs, ~bind_cols(.x, .y))) %>%
  mutate(df = map(df, spread, key = value1, value = value)) %>%
  mutate(df = map(df, select, contains("iris_"))) %>%
  unnest(df) %>%
  mutate(iris_date = map_chr(iris_date, ~trimws(gsub("(.{4})", "\\1 ", .x)))) %>%
  select(., -`oai_iris_iris_xsi:schema_location`, -values, -attribs)
  nest()



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
  modify_depth(., 2, str_c, collapse = "; ") %>%
  map(., as_data_frame, validate = FALSE) 


for (i in 1:length(values)) {
  
  colnames(values[[i]]) <- attribs[[i]]
  colnames(values[[i]]) <- gsub("\\.","_", names(values[[i]]))
  colnames(values[[i]]) <- tolower(gsub("([A-Z])", '_\\1', names(values[[i]])))
  colnames(values[[i]]) <- gsub("_p_i_d", "riris_pid", names(values[[i]]))
  colnames(values[[i]]) <- gsub("dc", "riris", names(values[[i]]))
  
}
  
riris_meta <- values %>%
  map(., select, contains('riris_'))
  


riris_metadata <- reduce(riris_meta, full_join)


  


devtools::use_data(riris_metadata, overwrite = TRUE, internal = TRUE)


# Make exploration dataframes ---------------------------------------------------------

# Extract based on a list of attributes only (PID not extracted; need str for that)

riris_instrument_author <- xml_data %>%
  map(., xpathApply, c("//arr[@name= 'iris.instrument.author']"), 
      getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_author')

riris_instrument_instrument_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.instrumentType']", 
      getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_instrument_type')

riris_instrument_research_area <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.researchArea']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_research_area')

riris_instrument_linguistic_target <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.linguisticTarget']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_linguistic_target')

riris_instrument_data_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.dataType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_data_type')

riris_participants_proficiency_learner <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.proficiencyLearner']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_participants_proficiency_learner')

riris_participants_domain_of_use <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.domainOfUse']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_participants_domain_of_use')

riris_participants_participant_type <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.participantType']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_participants_participant_type')

riris_instrument_funder <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.instrument.funder']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_instrument_funder')

riris_participants_target_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.targetLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_participants_target_language')

riris_participants_first_language <- xml_data %>%
  map(., xpathApply, "//arr[@name= 'iris.participants.firstLanguage']", getChildrenStrings) %>%
  unlist(.) %>%
  unique(.) %>%
  data_frame(.) %>%
  set_names(., 'riris_participants_first_language') 

devtools::use_data(riris_instrument_author, overwrite = TRUE)
devtools::use_data(riris_instrument_instrument_type, overwrite = TRUE)
devtools::use_data(riris_instrument_research_area, overwrite = TRUE)
devtools::use_data(riris_instrument_linguistic_target, overwrite = TRUE)
devtools::use_data(riris_instrument_data_type, overwrite = TRUE)
devtools::use_data(riris_participants_proficiency_learner, overwrite = TRUE)
devtools::use_data(riris_participants_domain_of_use, overwrite = TRUE)
devtools::use_data(riris_participants_participant_type, overwrite = TRUE)
devtools::use_data(riris_participants_target_language, overwrite = TRUE)
devtools::use_data(riris_participants_first_language, overwrite = TRUE)


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


