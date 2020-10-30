getCommentLines <- function(filename) {
  #read each code line using readLines()
  lines_vector <- readLines(filename, warn = FALSE) 
  comments_vector = c()
  pattern = '#'
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