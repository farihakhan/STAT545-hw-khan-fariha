Homework 5: Factor and figure management
================
Fariha Khan
2017-10-17

``` r
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(singer))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(reshape2))
```

Factor management
-----------------

> Using the singer dataset

``` r
data("singer_locations")
glimpse(singer_locations) 
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

### Define factor variables

> The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors)

*Taking a quick look at the dataset*

``` r
sapply(singer_locations, function(x) length(unique(x)))
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

By looking at the dataframe, I decided that the columns for year, name and city would be good variables to cast as factors because they are discrete. Using the base as.factor() function coerces missing values to NA automatically. For forcats::as\_factor(), there are limitations to the values that can be used in the function.

Note: forcats::as\_factor() can't be applied to numeric objects, or NA values - producing the following errors:

-   *Error in UseMethod("as\_factor") : no applicable method for 'as\_factor' applied to an object of class "c('integer', 'numeric')"*

-   *Error: `idx` must contain one integer for each level of `f`*

To solve these errors I casted the year variable to characters, and explicitly marked NA values. I initially changed NA values in the catagorical variables to the string "NA" before converting to factor by using the following ifelse statement: `mutate(city_forcat_fct = ifelse(is.na(city), "NA", city), city = as_factor(city_forcat_fct))`. I later found found the fct\_explicit\_na() function which essentially combines the two mutate statements into one.

``` r
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

``` r
singer_factors %>% 
      select(year, year_base_fct, year_forcat_fct, 
             name, name_base_fct, name_forcat_fct,
             city, city_base_fct, city_forcat_fct) %>% 
      head() %>% 
      kable(align = "c")
```

| year | year\_base\_fct | year\_forcat\_fct |      name     | name\_base\_fct | name\_forcat\_fct |     city     | city\_base\_fct | city\_forcat\_fct |
|:----:|:---------------:|:-----------------:|:-------------:|:---------------:|:-----------------:|:------------:|:---------------:|:-----------------:|
| 2007 |       2007      |        2007       |       NA      |        NA       |        (NA)       |      NA      |        NA       |        (NA)       |
| 2004 |       2004      |        2004       | Gene Chandler |  Gene Chandler  |   Gene Chandler   |  Chicago, IL |   Chicago, IL   |    Chicago, IL    |
| 1998 |       1998      |        1998       |   Paul Horn   |    Paul Horn    |     Paul Horn     | New York, NY |   New York, NY  |    New York, NY   |
| 1995 |       1995      |        1995       |       NA      |        NA       |        (NA)       |      NA      |        NA       |        (NA)       |
| 1968 |       1968      |        1968       | Dorothy Ashby |  Dorothy Ashby  |   Dorothy Ashby   |  Detroit, MI |   Detroit, MI   |    Detroit, MI    |
| 2006 |       2006      |        2006       |  Barleyjuice  |   Barleyjuice   |    Barleyjuice    | Pennsylvania |   Pennsylvania  |    Pennsylvania   |

### Drop 0

Filter the singer\_locations data to remove observations associated with the uncorrectly inputed year 0. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and levels; address the number of rows and the levels of the affected factors.

Looking at the singer\_factor dataframe, I'm going to look at the same 3 variable is chose to manipulate in the previous step.

``` r
with_0 <- singer_factors %>% 
      select(year_forcat_fct, name_forcat_fct, city_forcat_fct) %>% 
      summarise_all(funs(length, nlevels)) %>% 
      mutate(levels = "ALL")

dropped_0 <- singer_factors %>% 
      select(year_forcat_fct, name_forcat_fct, city_forcat_fct) %>% 
      filter(year_forcat_fct != 0) %>% 
      droplevels() %>% 
      summarise_all(funs(length, nlevels)) %>% 
      mutate(levels = "DROPPED 0")

bind_rows(with_0, dropped_0) %>% 
      rename_all(funs(gsub("_forcat_fct", "", make.names(names(with_0))))) %>% 
      column_to_rownames(var = "levels") %>% 
      kable(align = "c")
```

    ## Warning: Setting row names on a tibble is deprecated.

|           | year\_length | name\_length | city\_length | year\_nlevels | name\_nlevels | city\_nlevels |
|-----------|:------------:|:------------:|:------------:|:-------------:|:-------------:|:-------------:|
| ALL       |     10100    |     10100    |     10100    |       70      |      2913     |      1317     |
| DROPPED 0 |     10000    |     10000    |     10000    |       69      |      2879     |      1309     |

### Reorder levels based on knowledge from data

Reorder year in different ways

``` r
singer_yrdrop <- singer_locations %>% 
      mutate(year = as_factor(as.character(year)),
             artist_name = fct_explicit_na(artist_name, na_level = "(NA)"),
             title = fct_explicit_na(title, na_level = "(NA)")) %>% 
      filter(year != 0) %>% 
      droplevels()

## Unordered levels
singer_yrdrop$artist_name %>%
      levels() %>% head()
```

    ## [1] "'t Hof Van Commerce" "'Til Tuesday"        "[re:jazz]"          
    ## [4] "*Shels"              "+44"                 "10 Years"

``` r
## order by frequency
singer_yrdrop$artist_name %>% 
      fct_infreq() %>%
      levels() %>% head()
```

    ## [1] "The Boo Radleys" "U2"              "Floyd Cramer"    "Jerry Goldsmith"
    ## [5] "Jimi Hendrix"    "Pearl Jam"

``` r
## order by reverse frequency
singer_yrdrop$artist_name %>% 
      fct_infreq() %>% fct_rev() %>% 
      levels() %>% head()
```

    ## [1] "µ-ziq"                      "ZZ Top"                    
    ## [3] "Zykos"                      "Zuco 103 feat. Dani Macaco"
    ## [5] "Zounds"                     "Zombina And The Skeletones"

Reorder artist\_name based on other variables using forcats::fct\_reorder()

``` r
## order artist_name by max artist_hotttnesss
fct_reorder(singer_yrdrop$artist_name, singer_yrdrop$artist_hotttnesss,
            fun = max) %>% 
      levels() %>% head()
```

    ## [1] "A La Carte Brass & Percussion" "Abe Duque feat. Blake Baxter" 
    ## [3] "Abi Wallenstein"               "Abstürzende Brieftauben"      
    ## [5] "After Dark"                    "ÄI-TIEM"

``` r
## reverse
fct_reorder(singer_yrdrop$artist_name, singer_yrdrop$artist_hotttnesss,
            fun = max, .desc = TRUE) %>% 
      levels() %>% head()
```

    ## [1] "Daft Punk"       "Black Eyed Peas" "Coldplay"        "Rihanna"        
    ## [5] "Rihanna / Slash" "Michael Jackson"

Create plots to compare the difference between arrange() and fct\_reorder(). I'm using the 0.75 of artist\_hotttnesss as the minimum threshold for filtering artist\_name

##### plotting arrange()

Plotting the values by only using arrange does not reorder the points on the graph

``` r
## arrange artist_name by artist_hotttnesss
top25 <- singer_yrdrop %>% 
      filter(artist_hotttnesss >= 0.75) %>% 
      arrange(desc(artist_hotttnesss)) %>% 
      ggplot(aes(x = artist_hotttnesss, y = artist_name)) +
      geom_point() +
      theme(axis.text.y = element_text(angle = 15, hjust = 1))

top25
```

![](hw05_factors_figureManagement_files/figure-markdown_github-ascii_identifiers/plot%20arrange-1.png)

##### plotting fct\_reorder()

Plotting the values by only using fct\_reorder() gives an graph with ordered points

``` r
top25_reorder <- singer_yrdrop %>% 
      filter(artist_hotttnesss >= 0.75) %>% 
      ggplot(aes(x = artist_hotttnesss, y = fct_reorder(artist_name, artist_hotttnesss))) +
      geom_point() +
      theme(axis.text.y = element_text(angle = 15, hjust = 1))
top25_reorder
```

![](hw05_factors_figureManagement_files/figure-markdown_github-ascii_identifiers/plot%20fct_reorder-1.png)

##### plotting arrange() + fct\_reorder()

Plotting the values by using arrange () AND fct\_reorder() also gives an graph with ordered points

``` r
top25_arrange_reorder <- singer_yrdrop %>% 
      filter(artist_hotttnesss >= 0.75) %>% 
      arrange(desc(artist_hotttnesss)) %>%
      ggplot(aes(x = artist_hotttnesss, y = fct_reorder(artist_name, artist_hotttnesss))) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
top25_arrange_reorder
```

![](hw05_factors_figureManagement_files/figure-markdown_github-ascii_identifiers/plot%20fct_reorde%20+%20arrange-1.png)

File I/O
--------

I'm going to save a subsetted dataframe that contains data filtered artist\_hotttnesss &gt;= 0.75, and contained the variables for year, artist name and title.

``` r
top25_hotness <- singer_yrdrop %>% 
      filter(artist_hotttnesss >= 0.75) %>% 
      select(year, artist_name, title, artist_hotttnesss)
glimpse(top25_hotness)
```

    ## Observations: 81
    ## Variables: 4
    ## $ year              <fctr> 2008, 2003, 2002, 2009, 2005, 2007, 1987, 2...
    ## $ artist_name       <fctr> Josh Groban, Alicia Keys, Maroon 5, Jason A...
    ## $ title             <fctr> Awake [Live], You Don't Know My Name, Sunda...
    ## $ artist_hotttnesss <dbl> 0.7551499, 0.7786736, 0.8433803, 0.8492910, ...

``` r
## Save file as csv
write_csv(top25_hotness, "top25_hotness.csv")
```
