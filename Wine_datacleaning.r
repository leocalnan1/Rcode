# Extract titles from column, separate out into their own columns and remove old column
# Change filepath as needed

winedata <- read.csv("Wine_Data_Unclean.csv")

# Remove extra ID columns

winedata$X <- NULL
winedata$X.1 <- NULL

# Define Regex for pattern matching

regex <- "(.*?)/"

# Add a '/' to the end of the variable, to make Regex-ing easier

winedata$variety_and_region <- paste(data$variety_and_region, "/")

# Process each column:
#  + Locate matches
#  + Add matches to a new column (defined on function call)
#  + Replace matched substring with blanks

extract_variable <- function(x) {
  
  matches <- regexpr(regex,data$variety_and_region)
  
  matches_list <- regmatches(data$variety_and_region,matches)
  
  winedata[x] <- matches_list
  
  winedata$variety_and_region <- sub(regex, "", data$variety_and_region)
  
  return(data)

}

# Run once for each title type
# eg. 3 Titles = 3 function calls, each with a different String parameter (header)

winedata <- extract_variable('variety')
winedata <- extract_variable('region')
winedata <- extract_variable('sub_region')

# Remove original (empty) column

winedata$variety_and_region <- NULL

# Remove '/' from each column

winedata$variety <- gsub("/","",winedata$variety)
winedata$region <- gsub("/","",winedata$region)
winedata$sub_region <- gsub("/","",winedata$sub_region)

# Add desired output filepath
write.csv("Wine_Data_Clean_2.csv")
