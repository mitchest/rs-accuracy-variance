source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}