#' Makes binary or categorical features from list columns
#'
#' @name make_cols
#' @param data data containing columns that contain lists
#' @param cols columns to tranform (columns should contains lists with values of NULL lists)
#' 
#' @return data dataframe with transformed columns

#' @rdname make_cols
#' @export
make_col_binary_all <- function(data, cols) {
  
  message(sprintf("Making binary columns from all values for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_orig <- data[,col]
    
    # create binary variables
    data <- data %>% 
      mutate(id = row_number()) %>%
      unnest(all_of(col), keep_empty = TRUE) %>% 
      distinct() %>% 
      pivot_wider(names_from = all_of(col), names_prefix = paste0(col, "_binary_all_"),
                  values_from = all_of(col), values_fill = 0, values_fn = function(x) 1) %>%
      select(-id)
    
    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_binary_all_", "NA")
    
    if (NA_column %in% colnames(data)) {
      data <- data %>% 
        mutate_at(vars(starts_with(paste0(col, "_binary_all_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>% 
        select(-all_of(NA_column))
    } 
    
    data[,col] <- col_orig
    
  }
  
  return(data)
  
}

#' @rdname make_cols
#' @export
make_col_categorical_last <- function(data, cols) {
  
  message(sprintf("Making categorical columns from last value for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_last <- get_last(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_categorical_last")] <- col_last
  }
  
  return(data)
}

#' @rdname make_cols
#' @export
make_col_binary_last <- function(data, cols) {
  
  message(sprintf("Making binary columns from last value for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    data <- make_col_categorical_last(data, col)
    
    col_categ <- paste0(col, "_categorical_last")
    
    # create binary variables
    data <- data %>% 
      pivot_wider(names_from = all_of(col_categ), names_prefix = paste0(col, "_binary_last_"),
                  values_from = all_of(col_categ), values_fill = 0, values_fn = function(x) 1) 
    
    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_binary_last_", "NA")
    
    if (NA_column %in% colnames(data)) {
      data <- data %>% 
        mutate_at(vars(starts_with(paste0(col, "_binary_last_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>% 
        select(-all_of(NA_column)) 
    } 
    
    return(data)
  }
}

#' @rdname make_cols
#' @export
make_col_categorical_max <- function(data, cols, levels) {
  
  message(sprintf("Making categorical max %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_last <- get_max_ordinal(data[,col][[1]], levels)
    
    # create new column with suffix
    data[,paste0(col, "_categorical_max")] <- col_last
  }
  
  return(data)
}

#' @rdname make_cols
#' @export
make_col_binary_max <- function(data, cols, levels) {
  
  message(sprintf("Making binary max columns from %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    data <- make_col_categorical_max(data, col, levels)
    
    col_categ <- paste0(col, "_categorical_max")
    
    # create binary variables
    data <- data %>% 
      pivot_wider(names_from = all_of(col_categ), names_prefix = paste0(col, "_binary_max_"),
                  values_from = all_of(col_categ), values_fill = 0, values_fn = function(x) 1) 
    
    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_binary_max_", "NA")
    
    if (NA_column %in% colnames(data)) {
      data <- data %>% 
        mutate_at(vars(starts_with(paste0(col, "_binary_max_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>% 
        select(-all_of(NA_column))
    } 
    
    return(data)
  }
}

#' @rdname make_cols
#' @export
make_col_categorical_min <- function(data, cols, levels) {
  
  message(sprintf("Making categorical min %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_last <- get_min_ordinal(data[,col][[1]], levels)
    
    # create new column with suffix
    data[,paste0(col, "_categorical_min")] <- col_last
  }
  
  return(data)
}

#' @rdname make_cols
#' @export
make_col_binary_min <- function(data, cols, levels) {
  
  message(sprintf("Making binary min columns from %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    data <- make_col_categorical_min(data, col, levels)
    
    col_categ <- paste0(col, "_categorical_min")
    
    # create binary variables
    data <- data %>% 
      pivot_wider(names_from = all_of(col_categ), names_prefix = paste0(col, "_binary_min_"),
                  values_from = all_of(col_categ), values_fill = 0, values_fn = function(x) 1) 
    
    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_binary_min_", "NA")
    
    if (NA_column %in% colnames(data)) {
      data <- data %>% 
        mutate_at(vars(starts_with(paste0(col, "_binary_min_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>% 
        select(-all_of(NA_column))
    } 
    
    return(data)
  }
}

#' @rdname make_cols
#' @export
make_col_max <- function(data, cols) {
  
  message(sprintf("Making max columns for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_out <- get_max(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_max")] <- col_out
  }
  
  return(data)
  
}

#' @rdname make_cols
#' @export
make_col_min <- function(data, cols) {
  
  message(sprintf("Making min columns for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_out <- get_min(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_min")] <- col_out
  }
  
  return(data)
  
}

#' @rdname make_cols
#' @export
make_col_mean <- function(data, cols) {
  
  message(sprintf("Making mean columns for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_out <- get_mean(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_mean")] <- col_out
  }
  
  return(data)
  
}

#' @rdname make_cols
#' @export
make_col_median <- function(data, cols) {
  
  message(sprintf("Making median columns for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_out <- get_median(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_median")] <- col_out
  }
  
  return(data)
  
}

#' @rdname make_cols
#' @export
make_col_numeric_last <- function(data, cols) {
  
  message(sprintf("Making columns from last value for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_last <- get_last_num(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_last")] <- col_last
  }
  
  return(data)
}

#' @rdname make_cols
#' @export
make_col_count <- function(data, cols) {
  
  message(sprintf("Making count columns for %s", paste(cols, collapse = ", ")))
  
  for (col in cols){
    
    col_out <- get_count(data[,col][[1]])
    
    # create new column with suffix
    data[,paste0(col, "_count")] <- col_out
  }
  
  return(data)
  
}


get_last <- function(x) {
  out <- sapply(x, tail, 1)
  out[sapply(out, is.null)] <- NA_character_
  out <- unlist(out)
  
  return(out)
}

get_last_num <- function(x) {
  out <- sapply(x, tail, 1)
  out[sapply(out, is.null)] <- NA_real_
  out <- unlist(out)
  
  return(out)
}

get_max_ordinal <- function(x, levels) {
  
  out <- sapply(x, function(x) max_quiet(factor(x,levels=levels, ordered=TRUE)))
  out[sapply(out, is.null)] <- NA_character_
  out <- unlist(out)
  out <- as.character(out)
  
  return(out)
}

get_min_ordinal <- function(x, levels) {
  
  out <- sapply(x, function(x) min_quiet(factor(x,levels=levels, ordered=TRUE)))
  out[sapply(out, is.null)] <- NA_character_
  out <- unlist(out)
  out <- as.character(out)
  
  return(out)
}

get_count <- function(x) {
  lengths(x)
}

get_max <- function(x){
  out <- sapply(x, max_quiet)
  out[is.infinite(out)] <- NA_real_
  
  return(out)
}

get_min <-  function(x){
  out <- sapply(x, min_quiet)
  out[is.infinite(out)] <- NA_real_
  
  return(out)
}

get_mean <-  function(x){
  out <- sapply(x, function(x) ifelse(is.null(x), NA_real_, mean(x)))
  out[is.infinite(out)] <- NA_real_
  
  return(out)
}

get_median <-  function(x){
  out <- sapply(x, function(x) ifelse(is.null(x), NA_real_, median(x)))
  out[is.infinite(out)] <- NA_real_
  
  return(out)
}

get_count <-  function(x){
  out <- sapply(x, function(x) ifelse(is.null(x), 0, length(x)))
  
  return(out)
}


# silent min and max
min_quiet <- function(x) suppressWarnings(min(x, na.rm = TRUE))
max_quiet <- function(x) suppressWarnings(max(x, na.rm = TRUE))

