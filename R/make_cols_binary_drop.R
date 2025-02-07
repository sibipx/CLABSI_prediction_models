make_col_binary_drop <- function(data, cols, dropped_levels = NULL) {
  
  clean_strings <- function(x){
    x <- trimws(x, which = c("both"))
    x <- gsub("%", "perc", x)
    x <- gsub("[[:punct:]]|[[:space:]]", "_", x)
    x <- gsub('^\\_*|\\_*$', '', x) # remove leading / trailing _ (if left)
    
    return(x)
  }
  
  data <- data %>%
    mutate_at(all_of(cols), clean_strings)
  
  if (is.null(dropped_levels)) dropped_levels <- list()
  
  for (col in cols){
    
    col_orig <- data[,col]
    
    if (is.null(dropped_levels[[col]])){
      col_levels <- unique(col_orig)
      col_levels <- col_levels[!is.na(col_levels)]
      dropped_level <- col_levels[[1]]
      
      dropped_levels[[col]] <- dropped_level
    } else {
      dropped_level <- dropped_levels[[col]]
    }
    
    # create binary variables
    data <- data %>%
      mutate(id = row_number()) %>%
      pivot_wider(names_from = all_of(col), names_prefix = paste0(col, "_bin_drop_"),
                  values_from = all_of(col), values_fill = 0, values_fn = function(x) 1) %>%
      select(-id)
    
    # drop 1 level
    col_name_drop <- paste(col, "bin_drop", dropped_level, sep = "_")
    data[col_name_drop] <- NULL
    
    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_bin_drop_", "NA")
    
    if (NA_column %in% colnames(data)) {
      data <- data %>%
        mutate_at(vars(starts_with(paste0(col, "_bin_drop_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>%
        select(-all_of(NA_column))
    }
    
  }
  
  return(list(data = data, dropped_levels = dropped_levels))
  
}

make_col_categ_drop <- function(data, cols, dropped_levels){
  
  data <- as.data.frame(data)
  colnames_data <- colnames(data)
  
  for (col in cols){
    
    # detect all binary columns
    cols_binary <- colnames_data[str_starts(colnames_data, paste0(col, "_bin_drop"))]
    
    # recover dropped level
    dropped_level <- dropped_levels[[col]]
    col_name_dropped <- paste(col, "bin_drop", dropped_level, sep = "_")
    
    data[col_name_dropped] <- 1 - rowSums(data[,cols_binary, drop = FALSE])
    cols_binary <- c(cols_binary, col_name_dropped)
    levels_categ <- str_remove(cols_binary, paste0(col, "_bin_drop_"))
    
    data[,col] <- apply(data[,cols_binary], 1, function(x) levels_categ[which.max(x)])
    data[, col] <- factor(data[, col])
    data[,cols_binary] <- NULL
    
  }
  
  return(data)
}
