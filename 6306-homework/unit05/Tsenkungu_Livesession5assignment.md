---
title: "Assignment 5"
author: "TQ Senkungu"
date: "6/14/2018"
output:
    html_document:
        keep_md: true
---



## 1. Data Munging

###a. First I import the data and add column names.

```r
setwd("~/Dropbox/Senkungu Fam/Education/SMU/Courses/MSDS 6306 Doing Data Science/GitHub Repo/smu-msds-homework/6306-homework/unit05")
df <- read.delim("yob2016.txt", header = FALSE, sep = ";")
names(df) <- c("Name", "Sex", "Frequency")
```
###b. This is the summary and structure

```r
str(df)
```

```
## 'data.frame':	32869 obs. of  3 variables:
##  $ Name     : Factor w/ 30295 levels "Aaban","Aabha",..: 9317 22546 3770 26409 12019 20596 6185 339 9298 11222 ...
##  $ Sex      : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Frequency: int  19414 19246 16237 16070 14722 14366 13030 11699 10926 10733 ...
```

```r
summary(df)
```

```
##       Name       Sex         Frequency      
##  Aalijah:    2   F:18758   Min.   :    5.0  
##  Aaliyan:    2   M:14111   1st Qu.:    7.0  
##  Aamari :    2             Median :   12.0  
##  Aarian :    2             Mean   :  110.7  
##  Aarin  :    2             3rd Qu.:   30.0  
##  Aaris  :    2             Max.   :19414.0  
##  (Other):32857
```
###c. I list the misspelled name

```r
misspelled_name <- df[grep("yyy",df$Name), ]
misspelled_name
```

```
##         Name Sex Frequency
## 212 Fionayyy   F      1547
```
###d. Here is the list with the missplled name removed

```r
y2016 <- df[-c(216), ]
```

##2. Data Merging for the 2016 Data

###a. Import the file into R

```r
setwd("~/Dropbox/Senkungu Fam/Education/SMU/Courses/MSDS 6306 Doing Data Science/GitHub Repo/smu-msds-homework/6306-homework/unit05")
y2015 <- read.delim("yob2015.txt", header = FALSE, sep = ",")
names(y2015) <- c("Name", "Sex", "Frequency")
```

###b. Here are the last 10 rows.

```r
tail(y2015, 10)
```

```
##         Name Sex Frequency
## 33054   Ziyu   M         5
## 33055   Zoel   M         5
## 33056  Zohar   M         5
## 33057 Zolton   M         5
## 33058   Zyah   M         5
## 33059 Zykell   M         5
## 33060 Zyking   M         5
## 33061  Zykir   M         5
## 33062  Zyrus   M         5
## 33063   Zyus   M         5
```
All of the last 10 items which are all names starting with Z were also used just 5 times. There are no NA values.

###c. I merged the two tables by name and sex and checked to ensure there are no NA values.

```r
final <- merge(y2015,y2016,by=c("Name","Sex"))
sapply(final, function(x) any(is.na(x)))
```

```
##        Name         Sex Frequency.x Frequency.y 
##       FALSE       FALSE       FALSE       FALSE
```


##3. Data Summary

###a. Total the frequencies of both years

```r
final$Total <- rowSums(final[,c("Frequency.x", "Frequency.y")])
head(final)
```

```
##        Name Sex Frequency.x Frequency.y Total
## 1     Aaban   M          15           9    24
## 2     Aabha   F           7           7    14
## 3 Aabriella   F           5          11    16
## 4     Aadam   M          22          18    40
## 5   Aadarsh   M          15          11    26
## 6     Aaden   M         297         194   491
```

I don't understand question 3a. But there were 26549 total people on the list. If we say these are popular names, that's the number.

###b. I sorted the data by the Total column.

```r
final_sorted <- final[order(-final$Total), ]
head(final_sorted, 10)
```

```
##           Name Sex Frequency.x Frequency.y Total
## 8290      Emma   F       20415       19414 39829
## 19885   Olivia   F       19638       19246 38884
## 19593     Noah   M       19594       19015 38609
## 16114     Liam   M       18330       18138 36468
## 23272   Sophia   F       17381       16070 33451
## 3252       Ava   F       16340       16237 32577
## 17714    Mason   M       16591       15192 31783
## 25240  William   M       15863       15668 31531
## 10993    Jacob   M       15914       14416 30330
## 10682 Isabella   F       15574       14722 30296
```
Here are the names top 10 most popular names:

Emma
Olivia
Noah
Liam
Sophia
Ava
Mason
William
Jacob
Isabella

###c. Here is the list without boys

```r
final_sorted_female <- final_sorted[-which(final_sorted$Sex  == "M"),]
head(final_sorted_female,10)
```

```
##            Name Sex Frequency.x Frequency.y Total
## 8290       Emma   F       20415       19414 39829
## 19885    Olivia   F       19638       19246 38884
## 23272    Sophia   F       17381       16070 33451
## 3252        Ava   F       16340       16237 32577
## 10682  Isabella   F       15574       14722 30296
## 18246       Mia   F       14871       14366 29237
## 5493  Charlotte   F       11381       13030 24411
## 277     Abigail   F       12371       11699 24070
## 8273      Emily   F       11766       10926 22692
## 9980     Harper   F       10283       10733 21016
```

###d. I created a csv with the names and frequencies.


```r
final_girls_list <- head(final_sorted_female[,c("Name", "Total")], 10)
final_girls_list
```

```
##            Name Total
## 8290       Emma 39829
## 19885    Olivia 38884
## 23272    Sophia 33451
## 3252        Ava 32577
## 10682  Isabella 30296
## 18246       Mia 29237
## 5493  Charlotte 24411
## 277     Abigail 24070
## 8273      Emily 22692
## 9980     Harper 21016
```

```r
write.csv(final_girls_list, file = "girl_names.csv", row.names=FALSE)
```
