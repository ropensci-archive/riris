
riris_get <- function(..., riris_author,
                        riris_instrument,
                        riris_participants,
                        riris_description){
  
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) > 0 && any(names(dots) != "")) {
    stop("Use == expressions, not named arguments, as extra arguments to ",
         "iris_search.")
  }
  
  ret <- filter_(iris_metadata, .dots = dots) %>%
    select(contains('record'), contains('t_author'), contains('s_email'),
           contains('hasmaterials'), contains('instrument'), 
           contains('participants'), contains('description'))
  
  ret
  }
 






