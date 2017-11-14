setwd("path")
function_name <- function(x) {
  #assuming x is the file name
  data <- read.xlsx("x") #depending on requirement read specific file or all files in a format (pattern = "*.xlsx")
  if(!missing(data)){
    specific_data <- #select data of interest
    if(!missing(specific_data)){
      proportion <- prop.test(specific_data) # for proportion test. will require parameters like Confidence interval, etc
      fisher <- fisher.test(specific_data) # alternative R function
      print (proportion) # or print (fisher)
    }
    else print("missing data of interest")
  }
  else print("missing data")
}