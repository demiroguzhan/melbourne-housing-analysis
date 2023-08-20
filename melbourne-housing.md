Melbourne Housing
================

- [1 Importing Dataset /
  Pre-analysis](#1-importing-dataset--pre-analysis)
- [2 Preparing Columns for Analysis](#2-preparing-columns-for-analysis)
  - [2.1 X Column](#21-x-column)
  - [2.2 Date Column](#22-date-column)
  - [2.3 Type Column](#23-type-column)
  - [2.4 Price Column](#24-price-column)
  - [2.5 Landsize and BuildingArea
    Columns](#25-landsize-and-buildingarea-columns)
  - [2.6 Rooms, Bathroom, Car Columns](#26-rooms-bathroom-car-columns)
  - [2.7 YearBuilt Column](#27-yearbuilt-column)
  - [2.8 Distance Column](#28-distance-column)
  - [2.9 Regionname Column](#29-regionname-column)
  - [2.10 PropertyCount Column](#210-propertycount-column)
- [3 Dealing With Missing Values](#3-dealing-with-missing-values)

# 1 Importing Dataset / Pre-analysis

We are importing our data set to studio and we save a backup version to
save original data on studio. I decided to use strings as non-factor
objects because i will set all the variables by myself after doing some
analysis on data. Since we have some cells with “\#N/A” text in it, R
doesn’t see them as NA values. To avoid them, we are using na.strings
parameter to import them as NA’s.

``` r
housing_dataset <- read.csv("melbourne_data.csv",
                            stringsAsFactors = FALSE, na.strings = "#N/A")
```

Before doing any cleaning and altering, we are checking structure and
summary of the data to have some understanding about our data that we
will analyze.

``` r
str(housing_dataset)
```

    ## 'data.frame':    34857 obs. of  13 variables:
    ##  $ X            : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Date         : chr  "3/09/2016" "3/12/2016" "4/02/2016" "4/02/2016" ...
    ##  $ Type         : chr  "h" "h" "h" "u" ...
    ##  $ Price        : chr  "NA" "1480000" "1035000" "NA" ...
    ##  $ Landsize     : chr  "126" "202" "156" "0" ...
    ##  $ BuildingArea : chr  "NA" "NA" "79" "NA" ...
    ##  $ Rooms        : int  2 2 2 3 3 3 4 4 2 2 ...
    ##  $ Bathroom     : chr  "1" "1" "1" "2" ...
    ##  $ Car          : chr  "1" "1" "0" "1" ...
    ##  $ YearBuilt    : chr  "NA" "NA" "1900" "NA" ...
    ##  $ Distance     : num  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 ...
    ##  $ Regionname   : chr  "Northern Metropolitan" "Northern Metropolitan" "Northern Metropolitan" "Northern Metropolitan" ...
    ##  $ Propertycount: int  4019 4019 4019 4019 4019 4019 4019 4019 4019 4019 ...

When we check the structure of our data set, we can see there is a
column named X which is not necessary since we already have built in
numbering system in R. We have lots of columns which has “chr” type
which looks incorrect. Since they are character, it won’t be healthy to
check summary of the dataset now. So we will work with our column types
now.

# 2 Preparing Columns for Analysis

## 2.1 X Column

As we mentioned above we won’t use column X. According to that, we are
removing that column from our dataset for further analysis.

``` r
housing_dataset$X <- NULL
```

## 2.2 Date Column

We are formatting the “Date” column to Date object, looking into the new
structure and summary to check everything is working as what we wanted.
Fortunately, we had no NA values in “Date” column.

``` r
housing_dataset$Date <- as.Date(housing_dataset$Date, format = "%d/%m/%Y")
```

``` r
str(housing_dataset$Date)
```

    ##  Date[1:34857], format: "2016-09-03" "2016-12-03" "2016-02-04" "2016-02-04" "2017-03-04" ...

``` r
summary(housing_dataset$Date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2016-01-28" "2016-11-19" "2017-07-08" "2017-05-23" "2017-10-28" "2018-03-17"

## 2.3 Type Column

We are renaming strings for better explanation and factorizing those
values. Then, checking levels of created factor to be sure everything is
alright. Looking into the new structure and summary to check everything
looks like as what we wanted. We had no NA values in “Type” column too.

``` r
housing_dataset$Type[housing_dataset$Type == "h"] <- "House"
housing_dataset$Type[housing_dataset$Type == "u"] <- "Unit/Duplex"
housing_dataset$Type[housing_dataset$Type == "t"] <- "Townhouse"
housing_dataset$Type <- factor(housing_dataset$Type)
```

``` r
levels(factor(housing_dataset$Type))
```

    ## [1] "House"       "Townhouse"   "Unit/Duplex"

``` r
str(housing_dataset$Type)
```

    ##  Factor w/ 3 levels "House","Townhouse",..: 1 1 1 3 1 1 1 1 1 1 ...

``` r
summary(housing_dataset$Type)
```

    ##       House   Townhouse Unit/Duplex 
    ##       23980        3580        7297

## 2.4 Price Column

We are formatting “Price” column to integer since we don’t have any
double values in Price column. Then, we are checking the structure,
summary and NA values in the “Price” column. We have 7610 missing values
on “Price” column.

``` r
housing_dataset$Price <- as.integer(housing_dataset$Price)
```

``` r
str(housing_dataset$Price)
```

    ##  int [1:34857] NA 1480000 1035000 NA 1465000 850000 1600000 NA NA NA ...

``` r
summary(housing_dataset$Price)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    ##    85000   635000   870000  1050173  1295000 11200000     7610

## 2.5 Landsize and BuildingArea Columns

We are formatting “Landsize” and “BuildingArea” columns to integer since
we don’t have any double values in them and they are sharing the same
structures. Then, we are checking the structure, summary and NA values
in those columns. We have 11810 missing values on “Landsize” and 21115
missing values on “BuildingArea” column according to summary of the
column.

``` r
housing_dataset$Landsize <- as.integer(housing_dataset$Landsize)
housing_dataset$BuildingArea <- as.integer(housing_dataset$BuildingArea)
```

``` r
str(housing_dataset$Landsize)
```

    ##  int [1:34857] 126 202 156 0 134 94 120 400 201 202 ...

``` r
summary(housing_dataset$Landsize)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    ##      0.0    224.0    521.0    593.6    670.0 433014.0    11810

``` r
str(housing_dataset$BuildingArea)
```

    ##  int [1:34857] NA NA 79 NA 150 NA 142 220 NA NA ...

``` r
summary(housing_dataset$BuildingArea)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0   102.0   136.0   160.2   188.0 44515.0   21115

## 2.6 Rooms, Bathroom, Car Columns

We will apply the same approach to “Rooms”, “Bathroom” and “Car” columns
since they are identical in terms of data. First of all, we will format
them to integer and then we will check their structures and summaries
one by one. Fortunately, we have no NA values in “Rooms” column but we
have 8226 missing values in “Bathroom” and 8728 missing values in “Car”
column.

``` r
housing_dataset$Rooms <- as.integer(housing_dataset$Rooms)
housing_dataset$Bathroom <- as.integer(housing_dataset$Bathroom)
housing_dataset$Car <- as.integer(housing_dataset$Car)
```

``` r
str(housing_dataset$Rooms)
```

    ##  int [1:34857] 2 2 2 3 3 3 4 4 2 2 ...

``` r
summary(housing_dataset$Rooms)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   3.031   4.000  16.000

``` r
str(housing_dataset$Bathroom)
```

    ##  int [1:34857] 1 1 1 2 2 2 1 2 1 2 ...

``` r
summary(housing_dataset$Bathroom)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   1.000   2.000   1.625   2.000  12.000    8226

``` r
str(housing_dataset$Car)
```

    ##  int [1:34857] 1 1 0 1 0 1 2 2 2 1 ...

``` r
summary(housing_dataset$Car)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   1.000   2.000   1.729   2.000  26.000    8728

## 2.7 YearBuilt Column

We will format “YearBuilt” column to integer. It will help us while
dealing with outliers in the column. Since we don’t have any month and
day information in this column, date type won’t fit to it. After
formatting, we are checking the structure and summary of this column. We
have 19306 missing values in this column.

``` r
housing_dataset$YearBuilt <- as.integer(housing_dataset$YearBuilt)
```

``` r
str(housing_dataset$YearBuilt)
```

    ##  int [1:34857] NA NA 1900 NA 1900 NA 2014 2006 1900 1900 ...

``` r
summary(housing_dataset$YearBuilt)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1196    1940    1970    1965    2000    2106   19306

## 2.8 Distance Column

We are formatting “Distance” column to numeric since we have digits in
those columns. Then we are checking structure and summary as always. We
have just 1 cell missing in “Distance” column.

``` r
housing_dataset$Distance <- as.numeric(housing_dataset$Distance)
```

``` r
str(housing_dataset$Distance)
```

    ##  num [1:34857] 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 ...

``` r
summary(housing_dataset$Distance)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    6.40   10.30   11.18   14.00   48.10       1

## 2.9 Regionname Column

We are factorizing the “Regionname” column and checking its levels.
Then, we look into structure and summary of it to be sure everything is
correct. We have 3 cells missing in “Regionname” column.

``` r
housing_dataset$Regionname <- factor(housing_dataset$Regionname)
```

``` r
levels(factor(housing_dataset$Regionname))
```

    ## [1] "Eastern Metropolitan"       "Eastern Victoria"          
    ## [3] "Northern Metropolitan"      "Northern Victoria"         
    ## [5] "South-Eastern Metropolitan" "Southern Metropolitan"     
    ## [7] "Western Metropolitan"       "Western Victoria"

``` r
str(housing_dataset$Regionname)
```

    ##  Factor w/ 8 levels "Eastern Metropolitan",..: 3 3 3 3 3 3 3 3 3 3 ...

``` r
summary(housing_dataset$Regionname)
```

    ##       Eastern Metropolitan           Eastern Victoria 
    ##                       4377                        228 
    ##      Northern Metropolitan          Northern Victoria 
    ##                       9557                        203 
    ## South-Eastern Metropolitan      Southern Metropolitan 
    ##                       1739                      11836 
    ##       Western Metropolitan           Western Victoria 
    ##                       6799                        115 
    ##                       NA's 
    ##                          3

## 2.10 PropertyCount Column

We will factorize the “PropertyCount” column because those values
actually are not different from each other. There are several property
areas in different regions which has exactly the same amount of
properties. So, they show us “which” property area that property belongs
to. According to that, we will use that variable as factor, instead of
integer. Then we are checking its structure and summary. We have 3
missing rows in “PropertyCount” column, same as “Regionname”

``` r
housing_dataset$Propertycount <- factor(housing_dataset$Propertycount)
```

``` r
levels(factor(housing_dataset$Propertycount))
```

*Output of levels are not included since there are so many different
levels. You can remove ‘results’ parameter in code block above to see
output.*

``` r
str(housing_dataset$Propertycount)
```

    ##  Factor w/ 342 levels "83","121","129",..: 185 185 185 185 185 185 185 185 185 185 ...

``` r
summary(housing_dataset$Propertycount)
```

    ##   21650    8870   10969   14949   10412   14577   10331   10579   11918   14887 
    ##     844     722     583     552     491     485     467     456     444     435 
    ##   11308   11364    8920    7809    9264   11204    6938    7485   13240    8648 
    ##     428     424     422     420     409     405     393     378     374     371 
    ##    8801    7717    5682    6795    6543    7082    5457    8989    6232    6482 
    ##     369     336     319     319     304     304     293     288     285     284 
    ##    5454    7217    7822    7570    9028   15510    5498    6567   13366    4836 
    ##     281     278     277     262     257     255     251     249     241     237 
    ##    5678   15321    5549    5629    4918    4675    5420    5070    6380   17496 
    ##     237     235     234     232     228     222     212     211     210     204 
    ##    5058    5263    3755    6763    5943    5533   13830    6244   16166   10529 
    ##     201     199     198     197     194     186     186     184     178     176 
    ##    3873    3284    4442    3540   10175   10926    5132    4502    3464    9758 
    ##     175     174     173     172     172     171     167     163     162     162 
    ##    4480    6821    3052   14092    3578    3445    4217    2947    5825    2674 
    ##     160     159     157     157     153     151     151     150     150     149 
    ##   14042   10999    2291    8524    2954    3265    4019    2894    2651    2671 
    ##     146     145     144     142     140     137     137     136     135     134 
    ##    3692   11925    4898    4380    3280   15542    2555    4553 (Other)    NA's 
    ##     134     134     133     132     131     129     126     124    9871       3

# 3 Dealing With Missing Values

First, let’s have a look the data we have obtained during the
pre-process. Date Column: 0 missing rows Type Column: 0 missing rows
Price Column: 7610 missing rows Landsize Column: 11810 missing rows
BuildingArea Column: 21115 missing rows Rooms Column: 0 missing rows
Bathroom Column: 8226 missing rows Car Column: 8728 missing rows
earBuilt Column: 19306 missing rows Distance Column: 1 missing rows
Regionname Column: 3 missing rows Propertycount Column: 3 missing rows

When we look at the table above, we can see that the Distance,
Regionname and Propertycount columns have very little missing data.
Starting from this data will be the easiest step at this point.

``` r
housing_dataset[is.na(housing_dataset$Distance), ]
```

    ##             Date  Type  Price Landsize BuildingArea Rooms Bathroom Car
    ## 29484 2018-01-06 House 616000       NA           NA     3       NA  NA
    ##       YearBuilt Distance Regionname Propertycount
    ## 29484        NA       NA       <NA>          <NA>

``` r
housing_dataset[is.na(housing_dataset$Regionname), ]
```

    ##             Date        Type  Price Landsize BuildingArea Rooms Bathroom Car
    ## 18524 2017-07-15 Unit/Duplex 710000       NA           NA     2       NA  NA
    ## 26889 2017-11-11       House 825000       NA           NA     2       NA  NA
    ## 29484 2018-01-06       House 616000       NA           NA     3       NA  NA
    ##       YearBuilt Distance Regionname Propertycount
    ## 18524        NA      5.1       <NA>          <NA>
    ## 26889        NA      7.7       <NA>          <NA>
    ## 29484        NA       NA       <NA>          <NA>

``` r
housing_dataset[is.na(housing_dataset$Propertycount), ]
```

    ##             Date        Type  Price Landsize BuildingArea Rooms Bathroom Car
    ## 18524 2017-07-15 Unit/Duplex 710000       NA           NA     2       NA  NA
    ## 26889 2017-11-11       House 825000       NA           NA     2       NA  NA
    ## 29484 2018-01-06       House 616000       NA           NA     3       NA  NA
    ##       YearBuilt Distance Regionname Propertycount
    ## 18524        NA      5.1       <NA>          <NA>
    ## 26889        NA      7.7       <NA>          <NA>
    ## 29484        NA       NA       <NA>          <NA>

It seems that these 3 columns are missing 3 rows in common. We have the
“Distance” data in the 2 missing rows, and as we know, the “Distance”,
“Regionname”, and “Propertycount” columns are there to provide the same
information: the area where the property is located. With that in mind,
we can use this “Distance” data to predict other columns.

First, let’s take a look at the regions where the “Distance” data is
5.1.

``` r
summary(housing_dataset$Regionname[housing_dataset$Distance == 5.1])
```

    ##       Eastern Metropolitan           Eastern Victoria 
    ##                          0                          0 
    ##      Northern Metropolitan          Northern Victoria 
    ##                          0                          0 
    ## South-Eastern Metropolitan      Southern Metropolitan 
    ##                          0                         76 
    ##       Western Metropolitan           Western Victoria 
    ##                        170                          0 
    ##                       NA's 
    ##                          2

When we perform some mathematical operations on the above data, we
obtain the Regionname value of the first row can be Western Metropolitan
with a probability of 69.1%, or Southern Metropolitan with a probability
of 30.9%. Accordingly, instead of deleting this data, we can add it to
the analysis by restoring it.

``` r
housing_dataset[18524, "Regionname"] <- "Western Metropolitan"
```

Now let’s do the same steps for second line.

``` r
summary(housing_dataset$Regionname[housing_dataset$Distance == 7.7])
```

    ##       Eastern Metropolitan           Eastern Victoria 
    ##                          0                          0 
    ##      Northern Metropolitan          Northern Victoria 
    ##                          0                          0 
    ## South-Eastern Metropolitan      Southern Metropolitan 
    ##                          0                        352 
    ##       Western Metropolitan           Western Victoria 
    ##                         31                          0 
    ##                       NA's 
    ##                          2

When we do the same operations for the second line, we get the Southern
Metropolitan value with a probability of 91.9% and a Western
Metropolitan value with a probability of 8.1%. Based on this
information, we can assume the Regionname of second line to be Southern
Metropolitan and include it in the analysis.

``` r
housing_dataset[26889, "Regionname"] <- "Southern Metropolitan"
```

We can do a similar approach for the “Propertycount” column. If we
examine the property area where the properties are located in that
region and include the house prices in this examination, we can find
where these properties belong.

``` r
summary(housing_dataset$Propertycount[housing_dataset$Distance == 5.1 &
                                      housing_dataset$Regionname == "Western Metropolitan"])
```

    ##    7570    2417      83     121     129     242     249     271     290     335 
    ##     119      51       0       0       0       0       0       0       0       0 
    ##     342     389     394     438     457     534     538     570     588     604 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     608     642     709     768     777     790     794     802     810     821 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     845     849     851     852     869     892     902     915     938     962 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     973     984    1008    1048    1052    1058    1071    1119    1123    1124 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1130    1158    1160    1184    1202    1223    1240    1281    1308    1328 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1345    1369    1390    1414    1424    1442    1475    1490    1543    1554 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1588    1595    1607    1624    1651    1686    1690    1705    1721    1793 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1808    1863    1889    1899    1989    1999    2003    2004    2019    2076 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    2079    2126    2170    2185    2206    2211    2230    2243 (Other)    NA's 
    ##       0       0       0       0       0       0       0       0       0       2

When we look at the table above, we can say that this property is more
likely to be located in the area with 7570 properties. However, it is
also useful to compare the price.

``` r
housing_dataset[18524, "Price"]
```

    ## [1] 710000

``` r
summary(housing_dataset$Price[housing_dataset$Distance == 5.1 &
                              housing_dataset$Regionname == "Western Metropolitan" &
                              housing_dataset$Propertycount == 7570])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  170000  453750  800000  736400  919000 1560000      21

``` r
summary(housing_dataset$Price[housing_dataset$Distance == 5.1 &
                              housing_dataset$Regionname == "Western Metropolitan" &
                              housing_dataset$Propertycount == 2417])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  322500  853000  985000 1003046 1129250 1870000       9

The value of this property is defined as 710.000 units. When we look at
the mean and median price values in two different fields, the arrows
again shows the field with 7570 properties. With this information, we
edit the Propertycount of this property as 7570 and include it in our
analysis.

``` r
housing_dataset[18524, "Propertycount"] <- 7570
```

Now let’s do the same approach for the second row.

``` r
summary(housing_dataset$Propertycount[housing_dataset$Distance == 7.7 &
                                      housing_dataset$Regionname == "Southern Metropolitan"])
```

    ##    8920    8989      83     121     129     242     249     271     290     335 
    ##     196     156       0       0       0       0       0       0       0       0 
    ##     342     389     394     438     457     534     538     570     588     604 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     608     642     709     768     777     790     794     802     810     821 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     845     849     851     852     869     892     902     915     938     962 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##     973     984    1008    1048    1052    1058    1071    1119    1123    1124 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1130    1158    1160    1184    1202    1223    1240    1281    1308    1328 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1345    1369    1390    1414    1424    1442    1475    1490    1543    1554 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1588    1595    1607    1624    1651    1686    1690    1705    1721    1793 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    1808    1863    1889    1899    1989    1999    2003    2004    2019    2076 
    ##       0       0       0       0       0       0       0       0       0       0 
    ##    2079    2126    2170    2185    2206    2211    2230    2243 (Other)    NA's 
    ##       0       0       0       0       0       0       0       0       0       2

It is very difficult to make a prediction based on the above table.
There are almost equal number of properties in 2 different regions.
Therefore, we will make a prediction based on the price values.

``` r
housing_dataset[26889, "Price"]
```

    ## [1] 825000

``` r
summary(housing_dataset$Price[housing_dataset$Distance == 7.7 &
                              housing_dataset$Regionname == "Southern Metropolitan" &
                              housing_dataset$Propertycount == 8920])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  410000 1205000 1845000 1874412 2396250 6000000      60

``` r
summary(housing_dataset$Price[housing_dataset$Distance == 7.7 &
                              housing_dataset$Regionname == "Southern Metropolitan" &
                              housing_dataset$Propertycount == 8989])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  300000  559000  685000  931183 1180000 3572000      49

The value of our second property is defined as 825.000 units. When we
look at the mean and median values in two different area, we can easily
say that this property belongs to the second area. Therefore, we change
the “Propertycount” data of this property to 8989.

``` r
housing_dataset[26889, "Propertycount"] <- 8989
```

We recovered two of the 3 missing rows. Let’s take a look at the third
one.

``` r
housing_dataset[29484, ]
```

    ##             Date  Type  Price Landsize BuildingArea Rooms Bathroom Car
    ## 29484 2018-01-06 House 616000       NA           NA     3       NA  NA
    ##       YearBuilt Distance Regionname Propertycount
    ## 29484        NA       NA       <NA>          <NA>

Unfortunately there is nothing we can do for this row. While we only
have the number of rooms and the price, there is no way we can recover
such missing data. Therefore, we will not include this row in our
analysis.

``` r
housing_dataset <- housing_dataset[-c(29484), ]
```

Before concluding this section, let’s take a final look at a summary of
our data.

``` r
summary(housing_dataset)
```

    ##       Date                     Type           Price             Landsize       
    ##  Min.   :2016-01-28   House      :23979   Min.   :   85000   Min.   :     0.0  
    ##  1st Qu.:2016-11-19   Townhouse  : 3580   1st Qu.:  635000   1st Qu.:   224.0  
    ##  Median :2017-07-08   Unit/Duplex: 7297   Median :  870000   Median :   521.0  
    ##  Mean   :2017-05-23                       Mean   : 1050189   Mean   :   593.6  
    ##  3rd Qu.:2017-10-28                       3rd Qu.: 1295000   3rd Qu.:   670.0  
    ##  Max.   :2018-03-17                       Max.   :11200000   Max.   :433014.0  
    ##                                           NA's   :7610       NA's   :11809     
    ##   BuildingArea         Rooms           Bathroom           Car        
    ##  Min.   :    0.0   Min.   : 1.000   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.:  102.0   1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 1.000  
    ##  Median :  136.0   Median : 3.000   Median : 2.000   Median : 2.000  
    ##  Mean   :  160.2   Mean   : 3.031   Mean   : 1.625   Mean   : 1.729  
    ##  3rd Qu.:  188.0   3rd Qu.: 4.000   3rd Qu.: 2.000   3rd Qu.: 2.000  
    ##  Max.   :44515.0   Max.   :16.000   Max.   :12.000   Max.   :26.000  
    ##  NA's   :21114                      NA's   :8225     NA's   :8727    
    ##    YearBuilt        Distance                          Regionname   
    ##  Min.   :1196    Min.   : 0.00   Southern Metropolitan     :11837  
    ##  1st Qu.:1940    1st Qu.: 6.40   Northern Metropolitan     : 9557  
    ##  Median :1970    Median :10.30   Western Metropolitan      : 6800  
    ##  Mean   :1965    Mean   :11.18   Eastern Metropolitan      : 4377  
    ##  3rd Qu.:2000    3rd Qu.:14.00   South-Eastern Metropolitan: 1739  
    ##  Max.   :2106    Max.   :48.10   Eastern Victoria          :  228  
    ##  NA's   :19305                   (Other)                   :  318  
    ##  Propertycount  
    ##  21650  :  844  
    ##  8870   :  722  
    ##  10969  :  583  
    ##  14949  :  552  
    ##  10412  :  491  
    ##  14577  :  485  
    ##  (Other):31179
