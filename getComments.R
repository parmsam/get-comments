#load libraries
library(utils)
library(purrr)

#file path of script in RStudio source editor
getSourceEditorPath <- function() {
  #function to get path to file currently shown in source editor
  path <- paste0(rstudioapi::getSourceEditorContext()$path)
  return(path)
}

#general function to retrieve lines of code based on pattern
getLines <- function(
  filename = getSourceEditorPath(),
  clipboard,
  pattern,
  output,
  line_numbers) {
  #function to get all lines with special pattern (specified) comment
  lines_vector <- readLines(filename, warn = FALSE)
  comments_vector = list()
  i = 0
  for(codeline in lines_vector){
    i = i + 1 
    if(grepl(pattern, codeline, ignore.case = TRUE)){
      line_string = paste0("Line number ", i)
      if (line_numbers == FALSE){
        comments_vector = append(comments_vector, codeline)
      }
      if( line_numbers == TRUE ){
        comments_vector = append(comments_vector, list(list(line_string,codeline)) )
      }
    }
  }
  if(clipboard == TRUE){
    writeClipboard(comments_vector)
    message("Comment(s) copied to clipboard.")
  }
  if (output == 'list'){
    return(comments_vector)
  }
  else if (output == 'dataframe'){
    if (line_numbers == FALSE) {
      df <- data.frame(Comment = map(comments_vector, 1) )
    }
    else if (line_numbers == TRUE){
      Line_Number_list = map(comments_vector,1)
      Comment_list = map(comments_vector,2)
      df <- data.frame("Line_Number" = unlist(Line_Number_list), "Comment" = unlist(Comment_list) )
    }
    return(df)
  }
}

#apply general function to comment pattern
getCommentLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with R comments
  pattern_string = "#"
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#apply general function with outline pattern
getOutlineLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with outline comment
  pattern_string = paste(c("####", "----"), collapse = "|")
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#apply general function to TODO pattern
getTodoLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with TODO comment
  pattern_string = paste(c("TODO","Todo", "todo"), collapse="|") 
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#apply general function to FIXME pattern
getFixmeLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with FIXME comment
  pattern_string = paste(c("FIXME","Fixme", "fixme", "Fix me", "fix me", "FIX ME"), collapse="|") 
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#apply general function to NOTE pattern
getNoteLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with NOTE comment
  pattern_string = paste(c("NOTE","Note", "note"), collapse="|")
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#apply general function to multi pattern
getMultiLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with NOTE comment
  pattern_string = paste(c("FIXME", "TODO", "CHANGED", "IDEA", "HACK", "NOTE",
                           "REVIEW", "BUG", "QUESTION", "COMBAK", "TEMP" ), collapse="|")
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

countComments <- function(
  filename = getSourceEditorPath(),
  pattern_string = "#"){
  coms <- getLines(filename, clipboard=FALSE, pattern=pattern_string, output='list', line_numbers=FALSE)
  coms_count <- length(coms)
  return(coms_count)
}

getCommentLines <- function(
  filename = getSourceEditorPath(),
  clipboard = FALSE,
  output='list',
  line_numbers=FALSE) {
  #function to get all lines with R comments
  pattern_string = "#"
  getLines(filename, clipboard, pattern=pattern_string, output, line_numbers)
}

#TODO: add support for other languages