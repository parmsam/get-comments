getCommentLines <- function(filename) {
  #function to get all lines with R comments
  lines_vector <- readLines(filename, warn = FALSE) 
  comments_vector = c()
  pattern = "#"
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getOutlineLines <- function(filename) {
  #function to get all lines with outline comment
  lines_vector <- readLines(filename, warn = FALSE)
  comments_vector = c()
  pattern = paste(c("####", "----"), collapse = "|")
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getSpecialLines <- function(filename, pattern) {
  #function to get all lines with special pattern (specified) comment
  lines_vector <- readLines(filename, warn = FALSE)
  comments_vector = c()
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getTodoLines <- function(filename) {
  #function to get all lines with TODO comment
  lines_vector <- readLines(filename, warn = FALSE) 
  comments_vector = c()
  pattern = paste(c("TODO","Todo", "todo"), collapse="|") 
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getFixmeLines <- function(filename) {
  #function to get all lines with FIXME comment
  lines_vector <- readLines(filename, warn = FALSE) 
  comments_vector = c()
  pattern = paste(c("FIXME","Fixme", "fixme", "Fix me", "fix me", "FIX ME"), collapse="|") 
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getNoteLines <- function(filename) {
  #function to get all lines with NOTE comment
  lines_vector <- readLines(filename, warn = FALSE) 
  comments_vector = c()
  pattern = paste(c("NOTE","Note", "note"), collapse="|")
  for(codeline in lines_vector){
    if(grepl(pattern, codeline)){
      comments_vector = append(comments_vector, codeline)
    }
  }
  return(comments_vector)
}

getSourceEditorPath <- function() {
  #function to get path to file currently shown in source editor
  path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),
         gsub("~","",rstudioapi::getSourceEditorContext()$path))
  return(path)
}

#TODO: add copy to clipboard option in functions
#TODO: add different output options
#TODO: add support for other languages