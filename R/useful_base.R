# ENVIRONMENT

# in ~/.bash_profile
alias R='R --no-save'


#####

# check R version number
R.Version()

# set locale to deal with unicode characters
Sys.setlocale('LC_ALL','C')

# session info and package versions
sessionInfo()
packageVersion('foo')

#Disable scientific notation (1.05e10)
options(scipen=999)

# load library
library(foo)
# silent loading
suppressMessages(library(foo))

# pacman loading (auto install if not found)
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(XML, R.utils)

# how to cite R / package 'x'
citation()
citation('x')

# load packages automatically
# update ~/.Rprofile

# argvars
analyseRaspScores_allOrders_v1.R 
args <- commandArgs(trailingOnly=T)
foo <- args[1]
etc <- args[2]


# set working directory
setwd('/Users/apcaines/')  # etc


# get username
library(R.utils)
uname <- as.character(System$getUsername())


# try to open file, catch error messages and ignore; useful in combo with list.files
logs <- list.files(path = "logs", pattern = "wordLearn_.*.csv", all.files=T, full.names=T) #list file names in folder
for (log in logs){
    tryCatch(df <- read.csv(log, header = T), error=function(e) NULL)	
    # do something with df
}


# check if file exists
if (file.exists(filein)) { foo }


# list files in directory
files <- list.files(path=dirpath, pattern='.xml', full.names=T)


# read list
readLines("eg.txt")
filein <- scan(plaintxt, character(0), sep = ".")


# generate sequence of numbers
seq(0, 10, by = 2)


# order a data frame
totals <- totals[with(totals, order(-score)), ]

# transpose a data frame
df2 <- t(df1)

# unique rows from a data frame (dplyr) by column x (n.b. keep all false by default)
distinct(df, x, .keep_all=T)


# define a function
cefrUpper <- function(x) limits$upper[match(x, limits$cefr)]


# return multiple objects
foo <- 12
bar <- c("a", "b", "e")
newList <- list('integer'=foo, 'names'=bar)
newList$integer
newList$names


# apply function to list (column from data frame in this case; see also http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/)
lapply(clc$level, cefrUpper)

# apply function to data frame
apply(df, 1, fun)  # rows
apply(df, 2, fun)  # cols


# omit NAs
totals <- na.omit(totals)
# or
good <- complete.cases(data$Ozone)
data$Ozone[complete.cases]
# or
bad <- is.na(data$Ozone)
data$Ozone[!bad]
# inspect NAs
head(airquality[!complete.cases(airquality), ])


# identify a row number
s000 <- which(totals$subject == 's000')


# download from web
fileUrl <- "https://www.foo.com"
download.file(fileUrl, destfile = "/filepath/...", method = "curl")  # curl method for https
dateDownloaded <- date()


# load xml
library(XML)
fileUrl <- "http://www.foo.xml"
doc <- xmlTreeParse(fileUrl, useInternal=T)
rootNode <- xmlRoot(doc)

names(rootNode)  # list of elements
rootNode[[1]]  # directly access first element
rootNode[[1]][[1]]  # directly access first element of first element
xmlValue(rootNode[[1]][[1]])  # actually access the data
xmlGetAttr(rootNode[[1]], 's')  # get value of attribute, e.g. <eval s='2'>

xmlSApply(rootNode, xmlValue)  # loops thru every element of root node and extracts value
# and learn XPath for more capabilities

# write xml
testxml <- xmlNode('recording', attrs = c('job' = 'foo', 'scenario' = 1))
append.xmlNode(testxml, xmlNode('text', 'blah'))


# write table, including number of digits to print, sep and quotes
write.table(format(dfOut, digits = 5), file = filepath, sep="\t", quote = FALSE, row.names = FALSE)
# write csv
write.table(df, filepath, sep = ",", quote = T, row.names = F)

# add an empty line to file
cat('\n', file=foo.txt, append=T)


# if else
if (machine == "desktop") print("ack") else print("yack")
# or if on separate lines, ensure 'else (if)' statement is on same line as closing (else) if bracket
if (foo == foo) {
    print("fo")
} else if {
    print("foo")
} else {
    print("fooo")
}

# if with 'or' operator
if (either == 'one' | or == 'other')

# if with 'and' operator
if (both == 'one' & and == 'other')


# system call
system(paste("open -g", fileout))
# capture output
ls <- system("ls", intern = TRUE)


# get date and time
timestamp <- Sys.time()
date <- format(Sys.Date(), format="%Y-%m-%d")

# get current year
as.integer(format(Sys.Date(), "%Y"))


# colour palettes
library(grDevices)
rainbow(n, start = 1, end = 5)
heat.colors(n, alpha = 1)
terrain.colors(n, alpha = 0.5)
topo.colors(n)
cm.colors(n)


# count TRUE values of a logical test (not FALSE or NA)
sum(unique(countries$mapcolor13) > 0, na.rm = TRUE)


# reverse a vector
vec2 <- rev(vec1)


# find values in vector
which(foo1 %in% foo2)
which(!(foo1 %in% foo2))  # not in vector


# subset a factor by multiple conditions: AND, OR
subs <- subset(data, var1 >= 0 & var2 <= 2)  # AND
subs <- subset(data, var1 == 0 | var2 == 2)  # OR

# subset with grep
endangered <- subset(langs, grepl("endangered", langs$status))

# subset a factor [subset() doesn't work!]
subs <- data[data$Code %in% selected,]


# substitute pattern (replacement, x, ignore.case=F)
sub("\xea", "'", chi$cpm_sentences[4], perl=T)  # non-greedy
gsub("\xea", "'", chi$cpm_sentences[4], perl=T)  # greedy


# get string matching pattern
regmatches(ppx, regexpr("[0-9\\.]+", ppx, perl=T))

# greedy and inverse matching
regmatches(ppx, gregexpr("[0-9\\.]+", ppx, perl=T), invert=T)


# read.csv: if numbers are quoted with commas ("30,000"), read 'as.is' and convert to numeric with gsub()
socioecon <- read.csv("census2011_release2Cstd/QS611SC.csv", as.is = TRUE)
socioecon[, col] <- as.numeric(gsub(",", "", socioecon[, col]))


# stopwords: http://www.inside-r.org/packages/cran/tm/docs/stopwords
library(tm)
stopwords("en")


# lower casing
tolower()

# title case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
simpleCap("united kingdom")


# grep for matching patterns using regex
newdf <- df[grep("word1", data$Column2, perl=TRUE), ]
newdf <- df[grep("word1|word2", data$Column2, perl=TRUE), ]  # disjunctive
newdf <- df[grepl("(?=.*word1)(?=.*word2)", data$Column2, perl=TRUE), ]  # conjunctive using grepl
# grep for values (default FALSE), and inverse match (i.e. non-matching)
grep(patt, x, value = TRUE, invert = TRUE)

## reuse regex matched pattern with \\1 etc
test <- "I am... very??? Hungry!!! And this is。。。"
gsub("([.?!。])+", "\\1", test)

## all of a class but not some (e.g. punctuation: retain full stops, question marks, exclamation marks)
punctpatt <- "[[:punct:]，～【】……]&&[^.?!]"


# concatenate list into character string
paste(foo, collapse = ",")  # comma separated


# count number of characters in string
nchar(foo)


# wait for user input in interactive session (i.e. console)
cat("Press [enter] to continue")  # n.b. print() doesn't work for this
line <- readline()

# wait for user input in interactive() == FALSE session (i.e. running script from cmd line)
response <- readLines(file("stdin"), n=1L)
print(response)

# a function to check whether session is interactive
if (interactive()) {
   userInput <- function() {
     readline(prompt="")
   }
} else {
  userInput <- function() {
    readLines(file("stdin"), n=1L)
  }
}
response <- userInput()


# split string
unlist(strsplit(foo, ",|:"))
unlist(strsplit(foo, "\\."))[2]  # split on fullstops, specifying which item in list


# prompt user input
print("Press any key to continue...")
response <- readLines(file("stdin"), n=1L)


# substring
foo <- "foo"
substr(foo, 1, 1)


# sleep
Sys.sleep(1)


# deal with timestamps
# strptime() deals with timestamps if you specify format
# as.POSIXct() converts to date/time object class
# as.integer() converts to epoch seconds
as.integer(as.POSIXct(strptime("3/15/2015 18:21:31", "%m/%d/%Y %H:%M:%S")))


# work with XML
library(XML)
data <- xmlParse('test.xml')
xml_data <- xmlToList(data)
unlist(xml_data[["nbest-parses"]][["parse-set"]][[".attrs"]][["score"]])
xml_data[["lemma-list"]][length(as.list(xml_data[["lemma-list"]]))][["lemma"]][["wnum"]]


# break a loop
if (x != "foo") {
  break
}


# quit with Error message
warning("message")


# function to trim leading / trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim(foo)


# substitute characters in string
group <- c("12357e", "12575e", "197e18", "e18947")
group
# [1] "12357e" "12575e" "197e18" "e18947"
gsub("e", "", group)
# [1] "12357" "12575" "19718" "18947"


# quantiles, percentiles
quantile(numvec)  # default: 0, 25, 50, 75, 100%
quantile(numvec, c(0.1, 0.3, 0.85, 0.999))  # percentiles
ecdf(numvec)(value)  # empirical cumulative distribution: percentile of value you are interested in


## order factor levels
sizes <- factor(sizes, levels = c("small", "medium", "large"))

## rename factor labels
library(plyr)
revalue(x, c("beta"="two", "gamma"="three"))


## clear console memory
rm(list = ls())


## merge data frames: note speech marks
merge(df1, df2, by = "x")


## sum table values by group
aggregate(. ~ scfs, data = df, FUN = sum)
## or by two variables
aggregate(data=df, type ~ color, function(x) length(unique(x)))


## percentages
library(scales)
percent(x)


## reshape dataframes: see http://seananderson.ca/2013/10/19/reshape.html and http://had.co.nz/reshape
library(reshape2)
melt(df)  # wide to long
dcast(df)  # long to wide

## add an empty row to dataframe
df[nrow(df)+1,] <- NA


## data preprocessing: see https://tgmstat.wordpress.com/2013/11/07/unsupervised-data-pre-processing-for-predictive-modeling/
set.seed(1)
predictors = data.frame(x1 = rnorm(1000, mean = 5, sd = 2), x2 = rexp(1000, rate=10))
require(caret)
trans = preProcess(predictors, c("BoxCox", "center", "scale"))
predictorsTrans = data.frame(trans = predict(trans, predictors))


## convert numbers from scientific
format(n, scientific=F)

## pretty numbers
r <- c("76491283764.97430", "29.12345678901", "-7.1234", "-100.1","1123")
formatC(r, big.mar = ",", format = "f", digits = 0)


## add new column to df conditional on existing column
ifelse(w1$age < 16, 0, 1)  # test, yes, no


## pearson's correlation test, ignoring N/As
cor.test(diversity$tokens, diversity$verb, na.action = "na.exclude", method = "pearson")


## remove infinite values (e.g. after log transform)
m[!is.finite(m)] <- NA
colMeans(m, na.rm=TRUE)


## reverse xlim type plot axis
xlim = rev(range(xyz$acceptability))


## print list without list of levels
print(foolist, max.levels = 0)


## check if object exists (note speech marks)
exists("foo")
!exists("foo")
## or like this
#mapzoom <- "hello"
if(!is.null(r <- get0("mapzoom"))) {print(r)} else {print("foo")}


## print without line number (or quotes)
cat(foo)  # use cat() rather than print()
cat(foo, fill = T)  # fill space (i.e. newlines)

## print label from factor only (default is all levels)
print(w$accepted[w$worker==test][[1]], max.levels=0)


## create new factor with levels specified (but not used at first)
r$accepted <- factor("y", levels = c("y", "n"))


## string functions using stringr package
library(stringr)
## convert first character of string to upper case
su <- paste0(toupper(substr(su, 1, 1)), substr(su, 2, nchar(su)))
# or 
segpatt <- '^(\\w)|\\.\\s*(\\w)'  # initial/post-full-stop characters
uppercase <- function(str) {
  locations <- as.data.frame(str_locate_all(str, segpatt))  # table of string character start/end positions
  numseg <- nrow(locations)
  newstring <- ""
  for (seg in 1:numseg) {
      initial <- toupper(substring(str, locations$start[seg], locations$end[seg]))
      if (seg < numseg) {
        remainder <- substring(str, locations$end[seg]+1, locations$start[seg+1]-1)
  	} else {
  	  remainder <- substring(str, locations$end[seg]+1, str_length(str))
  	  }
  	  newstring <- paste0(newstring, initial, remainder)
  }
  return(newstring)
}

## count occurrences in string
foo <- 'foo bar foo bar'
str_count(foo, 'oo')


## add hours minutes seconds
library(chron)
t <- times(c("0:19:36", "0:29:35", "0:19:47"))
sum(t)


## Shiny Apps
library(shiny)
runApp(launch.browser = T)  # opens ui.R server.R app

## bin numeric vector: https://stat.ethz.ch/R-manual/R-devel/library/base/html/bincode.html
bins <- seq(0, max(suMeans$suCount)+20, 20)
suMeans$suCountBins <- as.factor(.bincode(suMeans$suCount, bins))


## random sample from data frame
random <- sample(1:nrow(df$id), 100)
randtemp <- temporal[random,]


## remove items from list
IDs <- IDs[!grepl("{{%PROLIFIC_PID%}}|NULL|emma", IDs, perl=T)]


## pairwise maxima / minima of two vectors
pmax(x, y)
pmin(x, y)


## check for missing values in df
sapply(relevant,function(x) sum(is.na(x)))

## number of unique values
sapply(relevant, function(x) length(unique(x)))


## check for factors and values
is.factor(df$foo)
contrasts(df$foo)


## summary of df and variable
summary(df)
summary(df$var)


## tabulate including zero values in range
table(factor(subs2$tagfreq, levels=1:175))

## values of table only
as.vector(table(x))

## table as data frame columns (i.e. single row)
data.frame(rbind(table(factor(subs2$tagfreq, levels=1:175))))


## play a sound
# https://www.r-project.org/nosvn/pandoc/beepr.html
library(beepr)
beep(1)


## hash
## n.b. also 'hash' and 'hashmap' libraries
hash <- new.env(hash=T)  # base R solution
hash[[key]] <- 'foo'
hash[[k1]][[k2]] <- 'bar'
for (key in ls(hash)) {  # loop thru keys
  print(hash[[key]])
}


## make your own library
# see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch
library(devtools)
library(roxygen2)
setwd('root/of/local/library')
document()
setwd('..')
install('nameOfLibrary')


## time a job
starttime <- Sys.time()
Sys.sleep(60)
endtime <- Sys.time()
endtime - starttime
# or
library(tictoc)
tic('Sleeping')
Sys.sleep(60)
toc()
# see also: https://www.r-bloggers.com/5-ways-to-measure-running-time-of-r-code


## parallelization
library(parallel)
library(doSNOW)
library(foreach)
# see https://garthtarr.com/parallel-computation-in-r


# repeat vector elements
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, each = 2, len = 4)    # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications
