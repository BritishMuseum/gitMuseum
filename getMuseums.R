#' ----
#' title: " A script for pulling Museum users from GitHub"
#' author: "Daniel Pett"
#' date: "08/04/2016"
#' output: csv_document
#' ----
#'
setwd("~/Documents/research/gitMuseum") #MacOSX

# Create CSV directory if does not exist
if (!file.exists('csv')){
  dir.create('csv')
}

# Add libraries needed
library(jsonlite)
library(httr)
library(plyr)

# Create archives and filenames
archives <- paste('json/',sep='')

# Set your oauth endpoint
oauth_endpoints("github")

# To get your keys to work for this application, register at https://github.com/settings/applications. 
# Make homepage URL = https://github.com
# Make callback url = http://localhost:1410
#
# Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "2c7cb80507d8d16b94a3",
                   secret = "4c5b70cbfc81b4a2b5fb39cc219321d9f84f93c8")

# Get the credentials from Github oauth endpoint
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Get the token for querying GitHub
gtoken <- config(token = github_token)

# Check rate limit (not really needed)
#req <- GET("https://api.github.com/rate_limit", gtoken)
#stop_for_status(req)
#content(req)

# Set the base url for github
baseurl <- "https://api.github.com/search/users?q=museum&per_page=10"
raw <- GET(paste(baseurl), gtoken)
rawJson <- fromJSON(content(raw, as = "text") , flatten=TRUE)
count <- rawJson$total_count
iterator <- ceiling(count/10)
# Create list for pages
pages <- list()

#Iterate through list 
for(i in 0:iterator){
  message("Retrieving page ", i)
  urlMuseum <- paste(baseurl, "&page=",i, sep='')
  message("retrieving url: ",  urlMuseum)
  museumData <- GET(urlMuseum, gtoken)
  jsonMuseum <- content(museumData, as = "text") 
  museums <- fromJSON(jsonMuseum, flatten=TRUE)
  pages[[i+1]] <- museums$items
}

# Combine all pages into one
museumGit <- rbind.pages(pages)

# Save csv file of museum data from Git
fileName <- paste('csv/', 'museumsList.csv', sep='')
write.csv(museumGit, file=fileName, row.names=FALSE, na="",  fileEncoding="utf8", quote=TRUE)

# Manipulate list for just the api url
subsetItems <- c("url")
# Get the url list for iterating through
newdata <- museumGit[subsetItems]
message("Listed museums from Github search api")
# Save csv file of museum data from Git
fileNameList <- paste('csv/', 'museumsListUrls.csv', sep='')
write.csv(newdata, file=fileNameList, row.names=FALSE, na="",  fileEncoding="utf8", quote=TRUE)

# Set up function for getting data needed
get.data <- function(musData){
  callApi <- GET(musData, gtoken)
  userJson <- content(callApi, as = "text") 
  dataGit <- fromJSON(userJson)
  message("Retrieving page ", musData)
  subsetItemsProfile <- c("html_url", "type", "name", "company", 
                          "blog", "location", "bio", "public_repos", "followers",
                          "created_at", "updated_at")
  dataGit <- dataGit[subsetItemsProfile]
  #git <- as.list(dataGit)
}
count <- nrow(newdata)
# Use lapply to get the data from the list
frame2 <- lapply(newdata[1:count,], get.data)

# Replace values for NULL from JSON
frame2.withNA <- sapply(frame2, function(x) ifelse(x == "NULL", NA, x))

#rotate the data 
test <- t(frame2.withNA)

# Make data frame
df <- as.data.frame(test)
head(df)

# Transform data frame
my.df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
is.na(my.df) <- my.df == "NA"
# Write CSV file output
csvname <- paste('csv/', 'museumsFinal.csv', sep='')
write.csv(my.df, file=csvname, row.names=FALSE, na="",  fileEncoding="utf8", quote=TRUE)