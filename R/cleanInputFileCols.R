#clean input file columns

cleanInputFileCols <- function(x){
  
  colnames(x) <- gsub(pattern = " ", 
                      replacement = "_",
                      colnames(x))
  
  colnames(x) <- gsub(pattern = "\\.$", 
                      replacement = "",
                      colnames(x))
  
  colnames(x) <- gsub(pattern = "\\.+", 
                      replacement = "_",
                      colnames(x))
  
  colnames(x) <- trimws(colnames(x))
  
  colnames(x) <- gsub(pattern = "_$", 
                      replacement = "",
                      colnames(x))

  colnames(x) <- gsub(pattern = "\\)|\\(", 
                      replacement = "",
                      colnames(x))


  colnames(x) <- gsub(pattern = "_+", 
                      replacement = "_",
                      colnames(x))
x
  
}