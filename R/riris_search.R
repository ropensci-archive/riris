

riris_search <- function(..., iris_instrument_author,
                        iris_instrument_instrument_type,
                        iris_instrument_research_area,
                        iris_instrument_linguistic_target,
                        iris_instrument_data_type,
                        iris_participants_proficiency_learner,
                        iris_participants_domain_of_use,
                        iris_participants_participant_type,
                        iris_instrument_funder,
                        iris_participants_target_language,
                        iris_participants_first_language){
  
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) > 0 && any(names(dots) != "")) {
    stop("Use == expressions, not named arguments, as extra arguments to ",
         "iris_search.")
  }
  ret <- filter_(iris_metadata, .dots = dots) %>%
    select(contains('pid'), contains('t_author'), contains('s_email'),
           contains('hasmaterials'), contains('instrument'), 
           contains('participants'), contains('description'))
  
  ret
  }
 






