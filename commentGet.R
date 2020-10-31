getCommentLines <- function(filename) {
  #read each code line using readLines()
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
  #read each code line using readLines()
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
  #read each code line using readLines()
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
  #read each code line using readLines()
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
  #read each code line using readLines()
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
  #read each code line using readLines()
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
  
#TODO: add copy to clipboard option in functions
#TODO: add different output options
#TODO: add support for other languages