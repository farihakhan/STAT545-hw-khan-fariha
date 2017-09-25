# hw02_gapminder.Rmd
Fariha Khan  
2017-09-24  






#### Work with the gapminder data we explored in class


Load Gapminder and dplyr (via tidyverse)


```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

##### Explore the gapminder object:

1. Is it a data.frame, a matrix, a vector, a list?

```r
str(gapminder)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

2. What’s its class?

```r
class(gapminder)
```

```
## [1] "tbl_df"     "tbl"        "data.frame"
```

```r
typeof(gapminder)
```

```
## [1] "list"
```

3. How many variables/columns?

```r
ncol(gapminder)
```

```
## [1] 6
```

4. How many rows/observations?

```r
nrow(gapminder)
```

```
## [1] 1704
```

5. Can you get these facts about “extent” or “size” in more than one way? Can you imagine different functions being useful in different contexts?

```r
summary(gapminder)
```

```
##         country        continent        year         lifeExp     
##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
##  Australia  :  12                  Max.   :2007   Max.   :82.60  
##  (Other)    :1632                                                
##       pop              gdpPercap       
##  Min.   :6.001e+04   Min.   :   241.2  
##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
##  Median :7.024e+06   Median :  3531.8  
##  Mean   :2.960e+07   Mean   :  7215.3  
##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
##  Max.   :1.319e+09   Max.   :113523.1  
## 
```

```r
dim(gapminder)
```

```
## [1] 1704    6
```

```r
names(gapminder)
```

```
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
```

```r
length(gapminder)
```

```
## [1] 6
```

6. What data type is each variable?

```r
str(gapminder)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1704 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

#### Explore individual variables
#####Pick at least one categorical variable and at least one quantitative variable to explore.
Using continent and population

```r
gapminder %>% 
      distinct(continent)
```

```
## # A tibble: 5 x 1
##   continent
##      <fctr>
## 1      Asia
## 2    Europe
## 3    Africa
## 4  Americas
## 5   Oceania
```

```r
gapminder %>% 
      group_by(continent) %>% 
      tally()
```

```
## # A tibble: 5 x 2
##   continent     n
##      <fctr> <int>
## 1    Africa   624
## 2  Americas   300
## 3      Asia   396
## 4    Europe   360
## 5   Oceania    24
```

```r
gapminder %>% 
      group_by(continent) %>% 
      summarise(min_population = min(pop),
                avg_population = mean(pop),
                max_population = max(pop))
```

```
## # A tibble: 5 x 4
##   continent min_population avg_population max_population
##      <fctr>          <dbl>          <dbl>          <dbl>
## 1    Africa          60011        9916003      135031164
## 2  Americas         662850       24504795      301139947
## 3      Asia         120447       77038722     1318683096
## 4    Europe         147962       17169765       82400996
## 5   Oceania        1994794        8874672       20434176
```


```r
var_quant <- gapminder$year
```





You can also embed plots, for example:

![](hw02_gapminder_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
