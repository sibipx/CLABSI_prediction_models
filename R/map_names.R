#' maps terms in a vector based on a dictionary
#' @param: x vector to be mapped
#' @param: dict dictionary used to map. Should be a tibble: first column = map_from, second column = map_to
#' @param: flag_unkown in case no match is found, return UNKNOWN_VALUE (TRUE) or original value (FALSE)
#' @return a vector of same length with mapped terms

map_names <- function(x, dict, flag_unkown = TRUE, case_sensitive = FALSE){
  
  # make vectors from tibble first and second column
  map_from <- dict[,1] %>% unlist() %>% unname()
  map_to <- dict[,2] %>% unlist() %>% unname()
  
  # convert special characters
  x <- iconv(x, to="ASCII//TRANSLIT")
  map_from <- iconv(map_from, to="ASCII//TRANSLIT")
  map_to <- iconv(map_to, to="ASCII//TRANSLIT")
  
  # check case sensitive mapping
  if (!case_sensitive) {
    map_from <- tolower(map_from)
    x <- tolower(x)
  }
  
  # map the matching terms
  out <- map_to[match(x,map_from)] 
  
  if (flag_unkown) { # if original term was present but is not in the dictionary, mark it as UNKNOWN_VALUE
    out[is.na(out) & !is.na(x)] <- "UNKNOWN_VALUE"
  } else { # or leave the original value
    out[is.na(out) & !is.na(x)] <- x[is.na(out) & !is.na(x)]
  }
  
  return(out)
}
