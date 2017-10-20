# Homework 5: Factor and figure management
Fariha Khan  
2017-10-17  




```r
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(singer))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(reshape2))
```
## Factor management
> Using the singer dataset


```r
data("singer_locations")
glimpse(singer_locations) 
```

```
## Observations: 10,100
## Variables: 14
## $ track_id           <chr> "TRWICRA128F42368DB", "TRXJANY128F42246FC",...
## $ title              <chr> "The Conversation (Cd)", "Lonely Island", "...
## $ song_id            <chr> "SOSURTI12A81C22FB8", "SODESQP12A6D4F98EF",...
## $ release            <chr> "Even If It Kills Me", "The Duke Of Earl", ...
## $ artist_id          <chr> "ARACDPV1187FB58DF4", "ARYBUAO1187FB3F4EB",...
## $ artist_name        <chr> "Motion City Soundtrack", "Gene Chandler", ...
## $ year               <int> 2007, 2004, 1998, 1995, 1968, 2006, 2003, 2...
## $ duration           <dbl> 170.4485, 106.5530, 527.5947, 695.1179, 237...
## $ artist_hotttnesss  <dbl> 0.6410183, 0.3937627, 0.4306226, 0.3622792,...
## $ artist_familiarity <dbl> 0.8230522, 0.5700167, 0.5039940, 0.4773099,...
## $ latitude           <dbl> NA, 41.88415, 40.71455, NA, 42.33168, 40.99...
## $ longitude          <dbl> NA, -87.63241, -74.00712, NA, -83.04792, -7...
## $ name               <chr> NA, "Gene Chandler", "Paul Horn", NA, "Doro...
## $ city               <chr> NA, "Chicago, IL", "New York, NY", NA, "Det...
```

#
### Define factor variables
> The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors)


*Taking a quick look at the dataset*

```r
sapply(singer_locations, function(x) length(unique(x)))
```

```
##           track_id              title            song_id 
##              10100               9799              10100 
##            release          artist_id        artist_name 
##               9049               7346               7498 
##               year           duration  artist_hotttnesss 
##                 70               6742               7348 
## artist_familiarity           latitude          longitude 
##               7456                985                985 
##               name               city 
##               2913               1317
```

By looking at the dataframe, I decided that the columns for year, name and city would be good variables to cast as factors because they are discrete. Using the base as.factor() function coerces missing values to NA automatically. For forcats::as_factor(), there are limitations to the values that can be used in the function.

Note: forcats::as_factor() can't be applied to numeric objects, or NA values - producing the following errors:

 - *Error in UseMethod("as_factor") : no applicable method for 'as_factor' applied to an object of class "c('integer', 'numeric')"*

 - *Error: `idx` must contain one integer for each level of `f`*

To solve these errors I casted the year variable to characters, and explicitly marked NA values. I initially changed NA values in the catagorical variables to the string "NA" before converting to factor by using the following ifelse statement: `mutate(city_forcat_fct = ifelse(is.na(city), "NA", city), city = as_factor(city_forcat_fct))`. 
I  later found found the fct_explicit_na() function which essentially combines the two mutate statements into one.


```r
singer_factors <- singer_locations %>% 
      mutate(year_base_fct = as.factor(year),
             name_base_fct = as.factor(name),
             city_base_fct = as.factor(city),
             year_forcat_fct = as_factor(as.character(year)),
             name_forcat_fct = fct_explicit_na(name, na_level = "(NA)"),
             city_forcat_fct = fct_explicit_na(city, na_level = "(NA)"))

## For sanity check
glimpse(singer_factors)
```

```
## Observations: 10,100
## Variables: 20
## $ track_id           <chr> "TRWICRA128F42368DB", "TRXJANY128F42246FC",...
## $ title              <chr> "The Conversation (Cd)", "Lonely Island", "...
## $ song_id            <chr> "SOSURTI12A81C22FB8", "SODESQP12A6D4F98EF",...
## $ release            <chr> "Even If It Kills Me", "The Duke Of Earl", ...
## $ artist_id          <chr> "ARACDPV1187FB58DF4", "ARYBUAO1187FB3F4EB",...
## $ artist_name        <chr> "Motion City Soundtrack", "Gene Chandler", ...
## $ year               <int> 2007, 2004, 1998, 1995, 1968, 2006, 2003, 2...
## $ duration           <dbl> 170.4485, 106.5530, 527.5947, 695.1179, 237...
## $ artist_hotttnesss  <dbl> 0.6410183, 0.3937627, 0.4306226, 0.3622792,...
## $ artist_familiarity <dbl> 0.8230522, 0.5700167, 0.5039940, 0.4773099,...
## $ latitude           <dbl> NA, 41.88415, 40.71455, NA, 42.33168, 40.99...
## $ longitude          <dbl> NA, -87.63241, -74.00712, NA, -83.04792, -7...
## $ name               <chr> NA, "Gene Chandler", "Paul Horn", NA, "Doro...
## $ city               <chr> NA, "Chicago, IL", "New York, NY", NA, "Det...
## $ year_base_fct      <fctr> 2007, 2004, 1998, 1995, 1968, 2006, 2003, ...
## $ name_base_fct      <fctr> NA, Gene Chandler, Paul Horn, NA, Dorothy ...
## $ city_base_fct      <fctr> NA, Chicago, IL, New York, NY, NA, Detroit...
## $ year_forcat_fct    <fctr> 2007, 2004, 1998, 1995, 1968, 2006, 2003, ...
## $ name_forcat_fct    <fctr> (NA), Gene Chandler, Paul Horn, (NA), Doro...
## $ city_forcat_fct    <fctr> (NA), Chicago, IL, New York, NY, (NA), Det...
```

```r
singer_factors %>% 
      select(year, year_base_fct, year_forcat_fct, 
             name, name_base_fct, name_forcat_fct,
             city, city_base_fct, city_forcat_fct) %>% 
      head() %>% 
      kable(align = "c")
```



 year    year_base_fct    year_forcat_fct        name         name_base_fct    name_forcat_fct        city        city_base_fct    city_forcat_fct 
------  ---------------  -----------------  ---------------  ---------------  -----------------  --------------  ---------------  -----------------
 2007        2007              2007               NA               NA               (NA)               NA              NA               (NA)       
 2004        2004              2004          Gene Chandler    Gene Chandler     Gene Chandler     Chicago, IL      Chicago, IL       Chicago, IL   
 1998        1998              1998            Paul Horn        Paul Horn         Paul Horn       New York, NY    New York, NY      New York, NY   
 1995        1995              1995               NA               NA               (NA)               NA              NA               (NA)       
 1968        1968              1968          Dorothy Ashby    Dorothy Ashby     Dorothy Ashby     Detroit, MI      Detroit, MI       Detroit, MI   
 2006        2006              2006           Barleyjuice      Barleyjuice       Barleyjuice      Pennsylvania    Pennsylvania      Pennsylvania   



### Drop 0
Filter the singer_locations data to remove observations associated with the uncorrectly inputed  year 0. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and levels; address the number of rows and the levels of the affected factors.

Looking at the singer_factor dataframe, I'm going to look at the same 3 variable is chose to manipulate in the previous step.


```r
with_0 <- singer_factors %>% 
      select(year_forcat_fct, name_forcat_fct, city_forcat_fct) %>% 
      summarise_all(funs(length, nlevels)) %>% 
      mutate(datatype = "Before dropping 0")

dropped_0 <- singer_factors %>% 
      select(year_forcat_fct, name_forcat_fct, city_forcat_fct) %>% 
      filter(year_forcat_fct != 0) %>% 
      droplevels() %>% 
      summarise_all(funs(length, nlevels)) %>% 
      mutate(datatype = "After dropping 0")

bind_rows(with_0, dropped_0) %>% 
      rename_all(funs(gsub("_forcat_fct", "", make.names(names(with_0))))) %>% 
      kable()
```



 year_length   name_length   city_length   year_nlevels   name_nlevels   city_nlevels  datatype          
------------  ------------  ------------  -------------  -------------  -------------  ------------------
       10100         10100         10100             70           2913           1317  Before dropping 0 
       10000         10000         10000             69           2879           1309  After dropping 0  


#### Reorder levels based on knowledge from data

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:



## Including Plots

You can also embed plots, for example:

![](hw05_factors_figureManagement_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
