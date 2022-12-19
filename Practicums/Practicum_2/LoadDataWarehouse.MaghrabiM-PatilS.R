
---
title: "Practicum 2 Part 2"
course: "CS5200 Fall 2022"
author: "Mahvash Maghrabi and Suteja Patil"
date : 12/08/2022
---

#loading the libraries
  
library(RMySQL)
library(RSQLite)
library(sqldf)

# establishing a MySQL connection
mysql_con = dbConnect(RMySQL::MySQL(),
                            host='localhost',
                            port=3306,
                            user='root',
                            password='Sp24041993@')

# dropping database if exists
dbExecute(mysql_con,"DROP DATABASE IF EXISTS PracticumDB")
dbGetQuery(mysql_con,"SHOW DATABASES")

# creating a database in MySQL
dbExecute(mysql_con,"CREATE DATABASE PracticumDB")
dbGetQuery(mysql_con,"SHOW DATABASES")

# Initializing the created DB for further use
dbExecute(mysql_con,"USE PracticumDB")

# dropping tables
dbExecute(mysql_con,"DROP TABLE IF EXISTS AuthorFacts")
dbExecute(mysql_con,"DROP TABLE IF EXISTS JournalFacts")
dbExecute(mysql_con,"DROP TABLE IF EXISTS JournalDimension")
dbExecute(mysql_con,"DROP TABLE IF EXISTS ArticleDimension")


# creating JournalDimension
dbExecute(mysql_con, "CREATE TABLE JournalDimension(
          ISSN VARCHAR(225) PRIMARY KEY,
          IssnType VARCHAR(225),
          CitedMedium VARCHAR(225),
          Volume VARCHAR(225),
          PubDate Date,
          Title VARCHAR(255),
          ISOAbbreviation VARCHAR(255))")

# creating ArticleDimension
dbExecute(mysql_con, "CREATE TABLE ArticleDimension(
          ID INTEGER,
          ISSN VARCHAR(225),
          ArticleTitle VARCHAR(225),
          PRIMARY KEY (ID)
          )")




# Connecting to SQLite database from part 1 to fetch the data

sqlite_con <- dbConnect(RSQLite::SQLite(), "pubmed.db")


#dbListTables(sqlite_con)




### Populating Dimension tables in MySQL using data from SQLite DB

dbSendQuery(mysql_con, "SET GLOBAL local_infile = true;")

## Creating Dataframes

article_df <- dbGetQuery(sqlite_con,"SELECT ID,ISSN,ArticleTitle FROM Article")
article_df

journal_df <- dbGetQuery(sqlite_con,"SELECT ISSN,IssnType,CitedMedium,Volume,PubDate,Title, ISOAbbreviation FROM Journal")
journal_df

## Copying from dataframes into MySQL tables


dbWriteTable(mysql_con,"ArticleDimension",article_df,overwrite=T, row.names=F)
dbWriteTable(mysql_con,"JournalDimension",journal_df,overwrite=T, row.names=F)

## checking if the dimension tables were populated with data

dbGetQuery(mysql_con,"SELECT * FROM ArticleDimension")
dbGetQuery(mysql_con,"SELECT * FROM JournalDimension")


## Creating Author Facts table

dbExecute(mysql_con,"CREATE TABLE AuthorFacts(authID INTEGER PRIMARY KEY,
          LastName VARCHAR(25),
          ForeName VARCHAR(25),
          Total_Articles INTEGER,
          Total_co_authors INTEGER
          )")


## copying the data from AuthorFact to a R dataframe
temp1_df<-dbGetQuery(sqlite_con,"SELECT a.authID,a.LastName,a.ForeName,
                           count(ar.id) as Total_Articles,COALESCE((select COUNT(art.id) from Article art
                           WHERE art.id <> a.authID AND ar.ID = art.ID and ar.ISSN = art.ISSN), 0) as Total_co_authors
                           FROM Author a, Article ar WHERE a.authID=ar.authID GROUP BY a.authID,ar.ID")
temp1_df

## writing the dataframe to a temporary SQLite DB
dbWriteTable(sqlite_con,"tmp",temp1_df,overwrite=T,row.names=F)

temp1_df <- dbGetQuery(sqlite_con,"SELECT authID,LastName,ForeName,SUM(Total_Articles) AS Total_Articles,
                  SUM(Total_co_authors) AS Total_co_authors FROM tmp GROUP BY authID")

temp1_df

## writing the dataframe to  AuthorFacts table in MySQL DB
dbWriteTable(mysql_con,"AuthorFacts",temp1_df,overwrite=T,row.names=F)

dbGetQuery(mysql_con,"SELECT * FROM AuthorFacts")


## Creating Journal Facts table
dbExecute(mysql_con, "CREATE TABLE JournalFacts(ISSN VARCHAR(225) PRIMARY KEY,
          Title VARCHAR(255),
          jmonth INTEGER,
          jquarter INTEGER,
          jyear INTEGER
          )")

journal_df <-dbGetQuery(mysql_con,"SELECT ISSN,Title,Month(PubDate) AS jmonth ,
           Quarter(PubDate) AS jquarter ,Year(PubDate) AS jyear  FROM JournalDimension")

## writing the dataframe to  JournalFacts table in MySQL DB
dbWriteTable(mysql_con,"JournalFacts",journal_df,overwrite=T,row.names=F)

dbGetQuery(mysql_con,"SELECT * FROM JournalFacts")


dbGetQuery(mysql_con,"SELECT DISTINCT YEAR(PubDate) FROM JournalDimension")

## Query 1
## What the are number of articles published in every journal in 2012 and 2013?

dbGetQuery(mysql_con,"SELECT COUNT(Title) FROM JournalFacts WHERE jyear=2012")

dbGetQuery(mysql_con,"SELECT COUNT(Title) FROM JournalFacts WHERE jyear=1975")

## Query 2
## How many articles were published each quarter (across all years)?

dbGetQuery(mysql_con,"SELECT jquarter,COUNT(Title) FROM JournalFacts GROUP BY jquarter ORDER BY jquarter ASC")

## Disconnecting MySQL and SQLite connection
dbDisconnect(sqlite_con)
dbDisconnect(mysql_con)
