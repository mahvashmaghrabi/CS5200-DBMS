
---
title: "Practicum 2 Part 1"
course: "CS5200 Fall 2022"
author: "Mahvash Maghrabi and Suteja Patil"
date : 12/08/2022
---
  
  
library(RSQLite)
library(XML)
library(stringr)

# Creating a normalized relational schema with entities as Authors, Journals and Articles and Language_Details

## Creating a new database
dbcon <- dbConnect(RSQLite::SQLite(), "pubmed.db")

# Dropping tables if it exists
dbExecute(dbcon, "DROP TABLE IF EXISTS Author")
dbExecute(dbcon, "DROP TABLE IF EXISTS Journal")
dbExecute(dbcon, "DROP TABLE IF EXISTS Article")
dbExecute(dbcon, "DROP TABLE IF EXISTS Language_Details")


# Creating tables in the database
# Creating Author table

dbExecute(dbcon,"CREATE TABLE Author(authID INTEGER PRIMARY KEY AUTOINCREMENT, 
          LastName VARCHAR(25),
          ForeName VARCHAR(25), 
          Initials VARCHAR(25))")

dbGetQuery(dbcon, "SELECT * FROM Author")


#Creating Journal table

dbExecute(dbcon, "CREATE TABLE Journal(ISSN VARCHAR(225) PRIMARY KEY,
          IssnType VARCHAR(225),
          CitedMedium VARCHAR(225),
          Volume VARCHAR(225),
          PubDate Date,
          Title VARCHAR(255),
          ISOAbbreviation VARCHAR(255)
          )")

dbGetQuery(dbcon, "SELECT * FROM Journal")

dbExecute(dbcon, "CREATE TABLE Language_Details(
          lang_id INTEGER PRIMARY KEY AUTOINCREMENT,
          Language VARCHAR(255))")

dbGetQuery(dbcon, "SELECT * FROM Language_Details")

# Creating Article table

dbExecute(dbcon, "CREATE TABLE Article(
          id INTEGER,
          ISSN VARCHAR(225),
          ArticleTitle VARCHAR(225),
          lang_id INTEGER,
          authID INTEGER,
          PRIMARY KEY (id, authID),
          FOREIGN KEY (ISSN) REFERENCES Journal(ISSN),
          FOREIGN KEY (lang_id) REFERENCES Language_Details(lang_id),
          FOREIGN KEY (authID) REFERENCES Author(authID)
          )")

dbGetQuery(dbcon, "SELECT * FROM Article")


## Loading the XML file and validating it

#xmlfile <- "/Users/sutejapatil/Desktop/CS5200 - DBMS/CS5200.PracticumII.PatilS-MaghrabiM/pubmed-tfm-xml.xml"

#xmlDoc <- xmlParse(file = paste(xmlfile, sep="/"), validate=T)

xmlDoc <- xmlParse(file = "pubmed-tfm-xml.xml", validate=T)

root <- xmlRoot(xmlDoc)

nPO <- xmlSize(root)

nPO



# Fuction to remove the quotes

removeQuotes <- function(str){
  x <- str_replace_all(str, "'", "''")
  return(x)
}


# Considering the numeric values of the month

MonthNo <- function(month){
  num <- "0"
  switch(month, 
         Jan={num <- "01"},
         Feb={num <- "02"},
         Mar={num <- "03"},
         Apr={num <- "04"},
         May={num <- "05"},
         Jun={num <- "06"},
         Jul={num <- "07"},
         Aug={num <- "08"},
         Sep={num <- "09"},
         Oct={num <- "10"},
         Nov={num <- "11"},
         Dec={num <- "12"})
  return (num)
}


# Function to generate the date and if/else blocks to handle the empty dates and months
# Empty dates are replaced by 0
# Empty months are replaced by 0
# Empty years are replaced by 9999

generateDate <- function(pubDate) {
  day <- 0
  month <- 0
  year <- 0
  if(is.null(pubDate[['Day']])){}
  else{
    day <- xmlValue(pubDate[['Day']])
  }
  
  if(is.null(pubDate[['Month']])){}
  else{
    month <- MonthNo(xmlValue(pubDate[['Month']]))
  }
  if(is.null(pubDate[['Year']])){
    year <- 9999
  }
  else{
    year <- xmlValue(pubDate[['Year']])
  }
  full_date <- paste0(year,"/",month,"/",day)
  return(full_date)
}


## Parsing the characters in attributes
char_from_attr <- function(att){
  for(char in att){
    return(char)
  }
}


# Function to get the Language Details
# sprintf() - a function to store the character and value in the format provided 

find_lang_id <- function(lang){
  query <- sprintf("Select lang_id from Language_Details where Language='%s'", lang)
  id <- dbGetQuery(dbcon, query)[1,'lang_id']
  return(id)
}


# Function to get the Author
# If/else blocks to solve the null attributes in the Author table

find_author_id <- function(author){
  
  if(is.na(author[['ForeName']]) || is.null(author[['ForeName']])){
    foreName <- ""}
  else{
    foreName<-removeQuotes(xmlValue(author[['ForeName']]))}
  
  if(is.na(author[['LastName']]) || is.null(author[['LastName']])) {
    lastName <- ""}
  else{
    lastName<-removeQuotes(xmlValue(author[['LastName']]))}
  
  if(is.na(author[['Initials']]) || is.null(author[['Initials']])) {
    initials <- "" }
  else{
    initials<-removeQuotes(xmlValue(author[['Initials']]))
  }
  query <- sprintf("Select authID from Author where ForeName LIKE '%s' AND LastName LIKE '%s' AND Initials LIKE '%s'", foreName, lastName, initials)
  id <- dbGetQuery(dbcon, query)[1,'authID']
  return(id)
}




# Article id has been initialized as zero
article_id <- 0

# Parsing all the attributes of the XML file
# strtoi - converts strings to integers
# Checking if Article exists

for(i in 1:nPO){
  pubmed_article <- root[[i]]
  article_id <- strtoi(char_from_attr(xmlAttrs(pubmed_article)))
  if(is.na(pubmed_article)){
    next
  }
  article <- pubmed_article[["Article"]]
  if(is.na(article)){
    next
  }
  #parse the journal from here
  journal <- article[["Journal"]]
  
  if(!is.na(journal)){
    
    if(is.null(journal[['ISSN']])){
      issn <- paste("ISSN",article_id,sep = "-")
    }
    else{
      issn <- xmlValue(journal[['ISSN']])
    }
    
    if(is.null(xmlAttrs(journal[[2]]))){
      x <- xmlAttrs(journal[[1]])
      cited_medium <- char_from_attr(x)
    }
    else{
      x <- xmlAttrs(journal[[1]])
      issn_type <- char_from_attr(x)
      x <- xmlAttrs(journal[[2]])
      cited_medium <- char_from_attr(x)
    }
    
    journal_issue <- journal[['JournalIssue']]
    
    volume <- ""
    if(is.null(journal_issue[['Volume']]) || is.na(journal_issue[['Volume']])){
      volume <- ""
    }
    else{
      volume <- removeQuotes(xmlValue(journal_issue[['Volume']]))
    }
    
    if(is.null(journal_issue[['Issue']]) || is.na(journal_issue[['Issue']])){
      issue <- "" 
    }
    else{
      issue <- xmlValue(journal_issue[['Issue']])  
    }
    
    pubDate <- generateDate(journal_issue[['PubDate']])
    if(is.null(journal[["Title"]]) || is.na(journal[["Title"]])){
      journal_title <- "" 
    }
    else{
      journal_title <- removeQuotes(xmlValue(journal[["Title"]]))
    }
    
    if(is.na(journal[["ISOAbbreviation"]])){
      iso_abbr <- "" 
    }
    else{
      iso_abbr <- removeQuotes(xmlValue(journal[["ISOAbbreviation"]]))
    }
    # Inserting the values into the Journal Table
    query <- sprintf("INSERT INTO Journal(ISSN, IssnType,CitedMedium,Volume,PubDate, Title, ISOAbbreviation) values('%s','%s','%s', '%s', '%s', '%s', '%s') On CONFLICT(ISSN) DO UPDATE SET PubDate='%s'"
                     ,issn,issn_type,cited_medium,volume,pubDate,journal_title,iso_abbr,pubDate)
    dbExecute(dbcon, query)
  }
  
  # Getting info for the language_details and author table 
  language <- xmlValue(article[['Language']])
  
  if(!is.na(language)){
    lang_id <- find_lang_id(language)
    
    # Inserting values into the Language_Details Table
    if(is.na(lang_id)){
      query <- sprintf("INSERT INTO Language_Details (Language) values('%s')", language)
      dbExecute(dbcon, query)
      lang_id <- find_lang_id(language)
    }
  }
  
  if(is.null(article[['ArticleTitle']]) || is.na(article[['ArticleTitle']])){
    article_title <- ""
  }
  else{
    article_title <- removeQuotes(xmlValue(article[['ArticleTitle']]))
  }
  
  #Inserting data into author table
  
  author_list <- article[['AuthorList']]
  if(!is.null(author_list) && !is.na(author_list)){
    author_size <- xmlSize(author_list)
    
    for(j in 1:author_size){
      author <- author_list[[j]]
      if(is.na(author)){
        next
      }
      author_id <- find_author_id(author)
      
      
      # If/else block to check the null attributes of the Author table
      if(is.na(author_id) || is.null(author_id)){
        
        if(is.na(author[['ForeName']]) || is.null(author[['ForeName']])){
          foreName <- ""
        }
        else{
          foreName<-removeQuotes(xmlValue(author[['ForeName']]))
        }
        
        if(is.na(author[['LastName']]) ||is.null(author[['LastName']]) ){
          lastName <- ""
        }
        else{
          lastName<-removeQuotes(xmlValue(author[['LastName']]))
        }
        if(is.na(author[['Initials']]) || is.null(author[['Initials']])){
          initials <- ""
        }
        else{
          initials<-removeQuotes(xmlValue(author[['Initials']]))
        }
        
        # Inserting the values in the Author Table then to the Article Table
        query <- sprintf("INSERT INTO AUTHOR(LastName,ForeName,Initials) values('%s','%s','%s')",lastName,foreName,initials)
        dbExecute(dbcon, query)
        author_id <- find_author_id(author)
        
      }
      
      # Checking for duplicate author ids prior to inserting into article table
      query <- sprintf("INSERT INTO ARTICLE(ID,ISSN, ArticleTitle,authID,lang_id) values(%d,'%s','%s', %d, %d) ON CONFLICT(ID,authID) DO NOTHING", 
                       article_id, issn,article_title,author_id,lang_id)
      
      dbExecute(dbcon, query)
    }
  }
  
  # article_id was initialized to zero so parsing to next article_id will increment the value -- the loop will execute for nPO
  article_id <- article_id + 1
}




# Printing all the tables
print(dbGetQuery(dbcon, "SELECT * FROM Language_Details"))

print(dbGetQuery(dbcon, "SELECT * FROM Journal"))

print(dbGetQuery(dbcon, "SELECT * FROM Article"))

print(dbGetQuery(dbcon, "SELECT * FROM Author"))

# Disconnecting the dbConnection
dbDisconnect(dbcon)

