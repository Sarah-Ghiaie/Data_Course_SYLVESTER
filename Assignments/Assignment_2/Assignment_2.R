#view current working directory
getwd()

#set working directory to '/Data'
setwd("/Users/sarah/Desktop/Data_Course_SYLVESTER/Data")

#list all of the .csv files found in wd
list.files(pattern=".csv")

#store the list in an object named "csv_files"
csv_files <- list.files(pattern = ".csv")

#find how many files match the description
length(csv_files)

#open wingspan_vs_mass file and assign data to object called "df"
df <- read.csv("wingspan_vs_mass.csv")

#inspect first 5 lines 
head(df)

#find any files in Data directory beginning with "b"
list.files(pattern = "^b", recursive = TRUE)

#display first line of each "b" file
b_files <- (list.files(pattern = "^b", recursive = TRUE))
for(i in b_files){
  print(readLines(i, n=1))
}

#do the same for all ".csv" files
for(j in csv_files){
  print(readLines(j,n=1))
}

    