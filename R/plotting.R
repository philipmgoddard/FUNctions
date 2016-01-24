#' Make a list of density plots using ggplot
#'
#' @author Philip Goddard
#' @param df a data frame containing numerical data
#' @param outcome the groups of the data
#' @import ggplot2
#' @export
ggplotListDens <- function(df, outcome) {
  stopifnot(is.data.frame(df))
  stopifnot(is.factor(outcome))
  stopifnot(nrow(df) == length(outcome))
  
  .e <- environment()
  colNames <- names(df)
  j <- 1
  plotList <- list()
  for(i in colNames){
    plt <- ggplot(df, aes_string(x=i), environment = .e) +
      geom_density(aes(color = outcome)) +
      geom_rug() +
      scale_color_manual(values = philTheme()) +
      theme_bw()   
    
    assign(paste("plot", j, sep = ""), plt)
    j <- j + 1
    plotList[[i]] <- plt
  }
  return(plotList)
}


#' Make a list of stacked histograms plots using ggplot
#'
#' @author Philip Goddard
#' @param df a data frame containing count data
#' @param outcome the groups of the data
#' @import ggplot2
#' @export
ggplotListHist <- function(df, outcome) {
  stopifnot(is.data.frame(df))
  stopifnot(is.factor(outcome))
  stopifnot(nrow(df) == length(outcome))
  
  .e <- environment()
  colNames <- names(df)
  
  j <- 1
  plotList <- list()
  for(i in colNames){
    
    bnwdth <- max(range(df[, i])) / min(max(range(df[, i])), 30)
    plt <- ggplot(df, aes_string(x = i), environment = .e) +
      geom_histogram(aes(color = outcome), alpha = 0.4, binwidth = bnwdth) +
      scale_color_manual(values = philTheme()) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    assign(paste("plot", j, sep = ""), plt)
    j <- j + 1
    plotList[[i]] <- plt
  }
  return(plotList)
}

#' Make a list of stacked bar charts plots using ggplot
#'
#' @author Philip Goddard
#' @param df a data frame containing categorical data
#' @param outcome the groups of the data
#' @import ggplot2
#' @export
ggplotListBar <- function(df, outcome) {
  stopifnot(is.data.frame(df))
  stopifnot(is.factor(outcome))
  stopifnot(nrow(df) == length(outcome))
  
  .e <- environment()
  colNames <- names(df)
  
  j <- 1
  plotList <- list()
  for(i in colNames){
    plt <- ggplot(df, aes_string(x = i), environment = .e) +
      geom_bar(aes(color = outcome), fill = "lightgrey") +
      scale_color_manual(values = philTheme()) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    assign(paste("plot", j, sep = ""), plt)
    j <- j + 1
    plotList[[i]] <- plt
  }
  return(plotList)
}


#' Make a multiplot from a list containing ggplots
#' 
#' @param plots a list containing a plot in each element
#' @param cols number of columns to be produced in the multiplot
#' @author Philip Goddard, modified from an R blog I once found
#' @import grid
#' @export
ggMultiplot <- function(plots = NULL, cols = 1) {
  
  if(!is.list(plots)) {
    stop ("plot objects must be held in a list")
  }
  
  # redo with functional- make a bit more compact
  for(i in seq_along(plots)) {
    if(!any(class(plots[[i]]) %in% c("gg", "ggplot"))){
      stop ("ggplots only")
    }
  }
  
  numPlots = length(plots)
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols))
  
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Phil's plotting colours
#' @export
philTheme <- function() {
  c("#5296d4",
    "#6464a7",
    "#50a273",
    "#e4a051",
    "#dfc54a",
    "#dd5852")
}