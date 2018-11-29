Wine_Data_Clean <- read.csv("C:/Users/Admin/Documents/Leo - R/Wine_Data_Unclean.csv", stringsAsFactors = F)




#Creates the column "Region" and extracts verything from the first backslash
"/.+" = everything after the first backslash
variable1 <- regmatches(table$old_column_name, regexpr("/.+", table$old_column_name))
variable2 <- sub("/.+", "replacement of thing", variable1)
table$new_column_name <- variable2

#Takes the new created column and puts it in a new column
variable3 <- regmatches(table$old_column_name, regexpr("/.+", table$old_column_name))
variable4 <- sub("/.+", "replacement of thing", variable3)
table$new_column_name <- variable4

#Replacing everything after forward slash
variable5 <- sub("/.+", "replacement of thing", table$column_name)
table$new_column_name <- variable5

colnames(Wine_Data_Clean[number of column]) <- "Variety"