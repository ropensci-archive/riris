library(XML); library(xml2); library(plyr); library(purrr); library(dplyr); library(tidyr)
library(stringr); library(magrittr); library(reshape2); library(rvest)
library(tibble)

## Read in solr dump------------------------------------
# download.file("https://dlib.york.ac.uk/irisdump.zip",
#          destfile="data-raw/solr_dumps/irisdump.zip", mode="wb")
# unzip ("data-raw/solr_dumps/irisdump.zip",
#        exdir = "data-raw/solr_dumps/", overwrite = TRUE)




# Read in data -----------------------------------------


files <- list.files('data-raw/solr_dumps', pattern = '.xml') 
# Files <- files[1:6] # for testing the for loops


# One big list of all of the files

filepath <- file.path("data-raw/solr_dumps",paste(files, sep=''))

xml_data <- map(filepath, read_xml, encoding = "ISO-8859-1") 


# Begin making large searchable list of data frames --------------------------------

record <- xml_data %>%
  map(., xml_find_all, '/record')

arr <- xml_data %>%
  map(., xml_find_all, '//*/arr')

titles <- xml_data %>%
  map(., xml_find_all, "//*/*/arr[@name = 'iris.referenceid']/str") %>%
  map(., xml_text, trim = TRUE) %>%
  map(., str_split, "_") %>% 
  flatten()%>%
  map(., ~keep(.x, function(.x) str_length(.x) > 14))

str <- xml_data %>%
  map(., xml_find_all, "//*/*/str[@name= 'iris.hasmaterials']")

fil <- xml_data %>%
  map(., xml_find_all, '//*/file')

strs <- map(str, xml_text)

xml_data_listcols <- arr %>%{
  tibble(
    values = map(., xml_text, trim = TRUE),
    attribs = map(., xml_attrs, "name"),
    files = map(fil, xml_attrs, "url"),
    materials = map(str, xml_text, trim = TRUE),
    record = map(record, xml_attrs, "pid")
  )
} %>%
  # slice(., 1:6) %>%# for drafting
unnest(record)


data_framing <- xml_data_listcols %>%
  unnest(record)%>%
  # unnest()%>%
  mutate(record = str_replace_all(record, 'york:', "")) %>%
  group_by(record) %>%
  mutate(values = map(values, ~gsub("([a-z])([A-Z])", "\\1; \\2", .x))) %>% # adds space and ; between words connected by lowercase and uppercase letters
  mutate(values = map(values, ~gsub("([a-z])(http)", "\\1; \\2", .x))) %>% # adds space and ; between words and a hyperlink
  mutate(values = map(values, ~gsub(")http", "\\1); http\\2", .x))) %>% # adds space and ; between words in () and a hyperlink
  mutate(values = map(values, ~gsub("())([A-Z])", ") \\2", .x))) %>%
  mutate(values = map(values, ~sub("\\s+$", "", gsub("([[:digit:]]{4,20}).{4}", "\\1 ", .x, perl = TRUE)))) %>%
  # mutate(values = map(values, ~gsub("([a-z])([[:digit:]])", "\\1 \\2", .x))) %>%
  mutate(attribs = map(attribs, str_replace_all, pattern = "\\.", replacement = "_"))%>%
  mutate(attribs = map(attribs, str_replace_all, pattern = "\\.", replacement = "_"))%>%
  mutate(attribs = map(attribs, str_replace_all, pattern = "dc", replacement = "iris"))%>%
  mutate(attribs = map(attribs, ~tolower(gsub("([a-z1-9])([A-Z])", "\\1_\\2", .x)))) %>%
  # mutate(attribs = map(attribs, str))
  mutate(values = map(values, as_data_frame),
         attribs = map(attribs, as_data_frame)) %>%
  mutate(df = map2(values, attribs, ~bind_cols(.x, .y))) %>%
  mutate(df = map(df, spread, key = value1, value = value))%>%
  unnest(df, materials, .preserve = 'files')%>%
  mutate(iris_date = map_chr(iris_date, ~trimws(gsub("(.{4})", "\\1; ", .x)))) %>%
  select(., files, record, materials, contains("iris_"), -contains("oai_iris_iris_xsi"))


riris_file_info <- data_framing %>%
  select(record, files) %>%
  unnest(files) %>%
  mutate(files = map(files, enframe)) %>%
  mutate(files = map(files, spread, key = name, value = value))

riris_author <- select(data_framing, record, iris_instrument_author, materials, iris_referenceid) 
riris_instrument <- select(data_framing, record, iris_instrument_instrument_type, iris_instrument_licence,
                           iris_instrument_research_area, iris_instrument_linguistic_target,
                           iris_instrument_source_language, iris_instrument_type_of_file, 
                           iris_instrument_title) %>%
  rename(iris_instrument_type = iris_instrument_instrument_type)

riris_participants <- select(data_framing, record, iris_participants_first_language, iris_participants_gender,
                             iris_participants_participant_type, iris_participants_proficiency_learner,
                             iris_participants_target_language)

iris_description <- select(data_framing, record, iris_description)

devtools::use_data(riris_file_info, overwrite = TRUE)
#--------------------- I'd like to get bibtex style citations here using RefManageR or scitations

# riris_references <- select(data_framing, record, iris_referenceid, iris_references_author, iris_references_author_noack,
#                            iris_references_conference_name, iris_references_journal, iris_references_publication_date,
#                            iris_references_publication_date_str, iris_references_publication_type)
