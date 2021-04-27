#load libraries
library(utils)

#file path of script in RStudio source editor
getSourceEditorPath <- function() {
  #function to get path to file currently shown in source editor
  path <- paste0(rstudioapi::getSourceEditorContext()$path)
  return(path)
}

#general function to retrieve lines of code based on pattern
getLines <- function(
  filename=getSourceEditorPath(),
  clipboard,
  output,
  pattern) {
  #function to get all lines with special pattern (specified) comment
  lines_vector <- readLines(filename, warn = FALSE)
  comments_vector = c()
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  if(clipboard == TRUE){
    writeClipboard(comments_vector)
    message("Comment(s) copied to clipboard.")
  }
  if (output == TRUE){
    return(comments_vector)
  }
}

#apply general function with comment pattern
getCommentLines <- function(
  filename=getSourceEditorPath(),
  clipboard = FALSE) {
  #function to get all lines with R comments
  pattern_string = "#"
  getLines(filename, clipboard, pattern=pattern_string, output=TRUE)
}

#apply general function with outline pattern
getOutlineLines <- function(
  filename=getSourceEditorPath(),
  clipboard = FALSE) {
  #function to get all lines with outline comment
  pattern_string = paste(c("####", "----"), collapse = "|")
  getLines(filename, clipboard, pattern=pattern_string, output=TRUE)
}

#apply general function with TODO pattern
getTodoLines <- function(
  filename=getSourceEditorPath(),
  clipboard = FALSE) {
  #function to get all lines with TODO comment
  pattern_string = paste(c("TODO","Todo", "todo"), collapse="|") 
  getLines(filename, clipboard, pattern=pattern_string, output=TRUE)
}

#apply general function with FIXME pattern
getFixmeLines <- function(
  filename=getSourceEditorPath(),
  clipboard = FALSE) {
  #function to get all lines with FIXME comment
  pattern_string = paste(c("FIXME","Fixme", "fixme", "Fix me", "fix me", "FIX ME"), collapse="|") 
  getLines(filename, clipboard, pattern=pattern_string, output=TRUE)
}

#apply general function with NOTE pattern
getNoteLines <- function(
  filename=getSourceEditorPath(),
  clipboard = FALSE) {
  #function to get all lines with NOTE comment
  pattern_string = paste(c("NOTE","Note", "note"), collapse="|")
  getLines(filename, clipboard, pattern=pattern_string, output=TRUE)
}

#TODO: add line number list or column

#TODO: convert to dataframe output
# data.frame(Comment = unlist(getCommentLines() ))
#TODO: add comments count function

#TODO: add different output options
#TODO: add support for other languages
