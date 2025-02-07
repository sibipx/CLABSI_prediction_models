# makes friendly percent
make_percent <- function(x) paste0(round(x, 4) * 100, "%")

# silent min and max
min_quiet <- function(x) suppressWarnings(min(x))
max_quiet <- function(x) suppressWarnings(max(x))

# clean strings
#' Cleans strings (variable names) of special characters
#' @param x string or string vector to clean

clean_strings <- function(x){
  x <- trimws(x, which = c("both"))
  x <- gsub("%", "perc", x)
  x <- gsub("[[:punct:]]|[[:space:]]", "_", x)
  x <- gsub('^\\_*|\\_*$', '', x) # remove leading / trailing _ (if left)
  
  return(x)
}

# function to create a list of chunks with a specified size
split_vector <- function(x, chunk_size) split(x, cut(seq_along(x), ceiling(length(x)/chunk_size), labels = FALSE)) 

# gets:    0 1 1 1 0 0 1 1
# returns: 0 1 2 3 0 0 1 2 
cumsum_in_blocks <- function(x){
  
  break_count <- function(x){
    diff <- c(0,base::diff(x))
    cumsum_diff <- cumsum(diff)
    break_count <- cumsum(c(0, base::diff(x) < 0))
    
    return(break_count)
  }
  
  cumsum_in_blocks <- unlist(lapply(split(x, break_count(x)), cumsum))
  
  return(cumsum_in_blocks)
}

# function used to read in the excel files for variable selection
get_df_from_excel <- function(f) {
  all_sheets <- f %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, path = f)
  
  all_sheets$data <- NULL 
  
  all_sheets <- do.call("rbind", all_sheets)
  
  all_sheets$file <- str_replace(f, ".*[?<=\\/]", "")
  
  return(all_sheets)
}
