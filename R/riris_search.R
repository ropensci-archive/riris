

riris_search <- function(..., riris_author,
                        riris_instrument,
                        riris_participants){
  
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
 






