getSourceEditorPath <- function() {
  #function to get path to file currently shown in source editor
  path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                 gsub("~","",rstudioapi::getSourceEditorContext()$path))
  return(path)
}

getSpecialLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE,
  output = TRUE,
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

getCommentLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE, 
  output = TRUE) {
  #function to get all lines with R comments
  pattern = "#"
  getSpecialLines(filename, clipboard, pattern, output)
}

getOutlineLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE,
  output = TRUE) {
  #function to get all lines with outline comment
  pattern = paste(c("####", "----"), collapse = "|")
  getSpecialLines(filename, clipboard, pattern, output)
}

getTodoLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE,
  output = TRUE) {
  #function to get all lines with TODO comment
  pattern = paste(c("TODO","Todo", "todo"), collapse="|") 
  getSpecialLines(filename, clipboard, pattern, output)
}

getFixmeLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE) {
  #function to get all lines with FIXME comment
  pattern = paste(c("FIXME","Fixme", "fixme", "Fix me", "fix me", "FIX ME"), collapse="|") 
  getSpecialLines(filename, clipboard, pattern, output)
}

getNoteLines <- function(
  filename=getSourceEditorPath(),
  clipboard = TRUE,
  output = TRUE) {
  #function to get all lines with NOTE comment
  pattern = paste(c("NOTE","Note", "note"), collapse="|")
  getSpecialLines(filename, clipboard, pattern, output)
}

#TODO: add different output options
#TODO: add support for other languages