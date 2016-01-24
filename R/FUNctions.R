#' Determine if vector is binary
#' 
#' Predicate function for testing only 0, 1 in vector
#' 
#' @param x input vector
#' @author Philip Goddard
#' @export
is.binary <- function(x) {
  x <- as.factor(x)
  if (all(levels(x) %in% c(0, 1))) {
    TRUE
  } else {
    FALSE
  }
}

#' Calculating quadratic interactions
#'
#' This function calculates interactions up to the second order from a data frame
#' containing at least one numeric column
#' 
#' @param df a data frame containing columns of arbitrary class, however at least one must be numeric
#' @param binary do we consider binary columns?
#' @return a data frame containing the interaction terms of the numeric columns of df
#' @author Philip Goddard
#' @export
quadInteraction <- function(df, binary = FALSE){
  
  stopifnot(is.data.frame(df))
  if(any(is.na(df))) stop("Missing values present")
  
  if (binary) {
    numericCols <- vapply(df, function(x) {is.numeric(x) && !is.binary(x)}, logical(1)) 
    binaryCols <- vapply(df, is.binary, logical(1))
  } else {
    numericCols <- vapply(df, is.numeric, logical(1)) 
  }

  stopifnot(length(numericCols) > 1)
  
  dfNum <- df[, numericCols]
  
  if(binary) {
    dfBin <- df[, binaryCols]
    nCols <- ncol(df) + choose(ncol(dfNum), 2) + ncol(dfNum) + (ncol(dfNum) * ncol(dfBin))
  } else {
    nCols <- ncol(df) + choose(ncol(dfNum), 2) + ncol(dfNum) 
  }
  
  out <- data.frame(matrix(NA, nrow = nrow(df), ncol = nCols))
  out[1:ncol(df)] <- df
  names(out)[1:ncol(df)] = names(df)
  
  start <- 1
  dfPosition <- ncol(df) + 1
  for(i in 1:ncol(dfNum)) {
    for(j in start:ncol(dfNum) ) {
      out[dfPosition] <- dfNum[i] * dfNum[j]
      names(out)[dfPosition] <- paste(names(dfNum[i]),
                                      names(dfNum[j]),
                                      sep = '|X|')
      dfPosition <- dfPosition + 1
    }
    start <- start + 1 
  }
  
  # if length(binCols > 0) {
  # then append output by the binary interactions
  if (binary) {
    for (i in 1:ncol(dfBin)) {
      for (j in 1:ncol(dfNum)) {
        out[dfPosition] <- dfBin[i] * dfNum[j]
        names(out)[dfPosition] <- paste(names(dfBin[i]),
                                        names(dfNum[j]),
                                        sep = '|X|')
        dfPosition <- dfPosition + 1
      }
      start <- start + 1  
    }
  }
  
  out
}

#' Rounding a number to a specified base
#'
#' This function rounds a numeric vector input to a user defined base
#' 
#' @param x a vector of arbitrary length to be rounded
#' @param base the base which each element of x will be rounded to
#' @return a numeric vector with elements rounded to the nearest multiple of base
#' @author Philip Goddard
#' @seealso \code{round}
#' @export
flexiRound <- function(x, base){ 
  # check for missing values?
  
  if(!is.numeric(x)) {
    stop("x must be numeric")
  } 
  if(!is.numeric(base)){
    stop("base must be numeric")
  } 
  
  base * round(x / base) 
} 

#' Insert a column into a data frame in  a specified position
#'
#' @author Philip Goddard
#' @param df a data frame
#' @param new_col a vector of same length of df to be added to df
#' @param col_name name of the new column
#' @param numeric_index should the new column be inserted by index (TRUE) or name (FALSE)
#' @param insert_index position for new column to be inserted
#' @param insert_name if numeric_index is FALSE, the column name the new column will be inserted adjacent to
#' @param insert_left if numeric_index is FALSE, will
#'   the column be inserted to left of insert_name (TRUE), or to the right (FALSE)
#' @export
insert_col <- function(df, new_col, col_name = "new_col", numeric_index = TRUE,
                       insert_index, insert_name, insert_left = TRUE) {
  
  if(!numeric_index){
    if(!insert_name %in% names(df)) stop("Invalid column name")
    if(insert_left){
      insert_index = which(names(df) == insert_name) 
    } else {
      insert_index = which(names(df) == insert_name) + 1
    }
  }
  
  if(insert_index > ncol(df) + 1 | insert_index <= 0) stop('enter valid position')
  
  df <- cbind(df, new_col)
  names(df)[ncol(df)] <- col_name
  ncol(df)
  if (insert_index == ncol(df) ){
    df
  } else if (insert_index == 1){
    order <- c(ncol(df), 1:ncol(df) - 1)
    df[, order]
  } else {
    order <- c(1:(insert_index - 1), ncol(df), insert_index:(ncol(df) - 1))
    df[, order]
  }
}


#' Reformat factor levels
#' @param x factor
#' @param search search string of level to rename/combine
#' @param replace string to replace level by
#' @export
facFormat <- function(x, search, replace){
  levels(x)[levels(x) %in% search] <- replace
  x
}

#' Cut data by interval. Alternative to other options out there,
#' with slightly nicer looking output (in my humble opinion)
#' @param x numeric vector
#' @param breaks a numeric vector of break locations
#' @export
cut3 <- function(x, breaks) {
  breaks <- c(breaks, breaks[length(breaks)] + 1e30)
  breaks[1] <- breaks[1] - 1
  labels <- paste0( breaks[-length(breaks)] + 1, "-", breaks[-1L])
  labels[length(labels)] <- paste0(breaks[length(breaks) - 1], "+")
  return(factor(labels[findInterval(x, breaks)], levels = labels))
}

#' Bin into ranges. Simple wrapper around cut3
#' @param input numeric input
#' @param bins vector specifying the breaks
#' @param allowNK logical. If TRUE, a level for NK (not known) will be generated from NA's
#' @export
rangebin <- function(input, bins, allowNK = FALSE) {
  out <- cut3(input, bins)
  if (allowNK) {
    out <- addNA(out)
    levels(out)[levels(out) %in% NA] <- "NK"
  }
  out
}