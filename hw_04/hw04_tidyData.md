Assignment 4
================
Fariha Khan
2017-10-09

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(reshape2))
```

### Activity 1

#### Data reshaping cheatsheet

| reshape2 Function | tidyr Function |
|-------------------|----------------|
| `melt`            | `gather`       |
| `dcast`           | `spread`       |

### Activity 2

> Make a tibble with one row per year and columns for life expectancy for two or more countries. Use knitr::kable() to make this table look pretty in your rendered homework.

I chose six countries at random for this activity, mostly because I don't know too much about them. I first created a subset of the data without applying the spread() function, so I could later plot it more easily. I used paste() to rename the column names to add the string "lifeExp" to each country column before calling kable().

``` r
actv2_lifeExp <- as.tibble(gapminder %>% 
      filter(country %in% c("Benin","Iceland","Mozambique",
                            "Sierra Leone","Venezuela", "Zambia")) %>% 
      group_by(country) %>% 
      select(country, year, lifeExp))

actv2_lifeExp_spr <- actv2_lifeExp %>% 
      spread(country, lifeExp)
names(actv2_lifeExp_spr)[2:7] <- paste(names(actv2_lifeExp_spr)[2:7], "LifeExp", sep = " ")
kable(actv2_lifeExp_spr,
      align = "c", digits = 2, padding = 1,
      caption = "Life expectancy of a given country by year")
```

| year | Benin LifeExp | Iceland LifeExp | Mozambique LifeExp | Sierra Leone LifeExp | Venezuela LifeExp | Zambia LifeExp |
|:----:|:-------------:|:---------------:|:------------------:|:--------------------:|:-----------------:|:--------------:|
| 1952 |     38.22     |      72.49      |        31.29       |         30.33        |       55.09       |      42.04     |
| 1957 |     40.36     |      73.47      |        33.78       |         31.57        |       57.91       |      44.08     |
| 1962 |     42.62     |      73.68      |        36.16       |         32.77        |       60.77       |      46.02     |
| 1967 |     44.88     |      73.73      |        38.11       |         34.11        |       63.48       |      47.77     |
| 1972 |     47.01     |      74.46      |        40.33       |         35.40        |       65.71       |      50.11     |
| 1977 |     49.19     |      76.11      |        42.49       |         36.79        |       67.46       |      51.39     |
| 1982 |     50.90     |      76.99      |        42.80       |         38.45        |       68.56       |      51.82     |
| 1987 |     52.34     |      77.23      |        42.86       |         40.01        |       70.19       |      50.82     |
| 1992 |     53.92     |      78.77      |        44.28       |         38.33        |       71.15       |      46.10     |
| 1997 |     54.78     |      78.95      |        46.34       |         39.90        |       72.15       |      40.24     |
| 2002 |     54.41     |      80.50      |        44.03       |         41.01        |       72.77       |      39.19     |
| 2007 |     56.73     |      81.76      |        42.08       |         42.57        |       73.75       |      42.38     |

To plot this data:

``` r
ggplot(data = actv2_lifeExp, aes(x = year, y = lifeExp, color = country)) +
      geom_point(aes(shape = country)) +
      geom_smooth(size = 1, method = 'lm', se = FALSE) +
      scale_color_brewer(palette = "Set2") +
      ggtitle("Linear Model of Life Expectancy from 1952 - 2007")
```

![](hw04_tidyData_files/figure-markdown_github-ascii_identifiers/plot%20lifeExp%202+%20countries-1.png)

### Activity 3

> Compute some measure of life expectancy (mean? median? min? max?) for all possible combinations of continent and year. Reshape that to have one row per year and one variable for each continent. Or the other way around: one row per continent and one variable per year.

I took the variables continent, year and life expectancy here, and took measures of the minumim, mean, and average life expectancy. The first table summarizes the first 15 rows of this data.

``` r
actv3_measures <-  gapminder %>% 
      select(continent, year, lifeExp) %>% 
      group_by(year, continent) %>% 
      summarise(Min = min(lifeExp), Avg = mean(lifeExp), Max = max(lifeExp))

kable(actv3_measures[1:15,],
              align = "c", digits = 2, padding = 1,
              caption = "Measures of life expectancy per year")
```

| year | continent |  Min  |  Avg  |  Max  |
|:----:|:---------:|:-----:|:-----:|:-----:|
| 1952 |   Africa  | 30.00 | 39.14 | 52.72 |
| 1952 |  Americas | 37.58 | 53.28 | 68.75 |
| 1952 |    Asia   | 28.80 | 46.31 | 65.39 |
| 1952 |   Europe  | 43.59 | 64.41 | 72.67 |
| 1952 |  Oceania  | 69.12 | 69.25 | 69.39 |
| 1957 |   Africa  | 31.57 | 41.27 | 58.09 |
| 1957 |  Americas | 40.70 | 55.96 | 69.96 |
| 1957 |    Asia   | 30.33 | 49.32 | 67.84 |
| 1957 |   Europe  | 48.08 | 66.70 | 73.47 |
| 1957 |  Oceania  | 70.26 | 70.30 | 70.33 |
| 1962 |   Africa  | 32.77 | 43.32 | 60.25 |
| 1962 |  Americas | 43.43 | 58.40 | 71.30 |
| 1962 |    Asia   | 32.00 | 51.56 | 69.39 |
| 1962 |   Europe  | 52.10 | 68.54 | 73.68 |
| 1962 |  Oceania  | 70.93 | 71.09 | 71.24 |

To look at the data by one row per year and one variable for each continen, I took a spread of the minimum life expectancy.

``` r
actv3_measures %>% 
      select(year, continent, Min) %>% 
      spread(continent, Min) %>% 
      kable(align = "c", digits = 2, padding = 1,
      caption = "Minimum life expectancy per year")
```

| year | Africa | Americas |  Asia | Europe | Oceania |
|:----:|:------:|:--------:|:-----:|:------:|:-------:|
| 1952 |  30.00 |   37.58  | 28.80 |  43.59 |  69.12  |
| 1957 |  31.57 |   40.70  | 30.33 |  48.08 |  70.26  |
| 1962 |  32.77 |   43.43  | 32.00 |  52.10 |  70.93  |
| 1967 |  34.11 |   45.03  | 34.02 |  54.34 |  71.10  |
| 1972 |  35.40 |   46.71  | 36.09 |  57.01 |  71.89  |
| 1977 |  36.79 |   49.92  | 31.22 |  59.51 |  72.22  |
| 1982 |  38.45 |   51.46  | 39.85 |  61.04 |  73.84  |
| 1987 |  39.91 |   53.64  | 40.82 |  63.11 |  74.32  |
| 1992 |  23.60 |   55.09  | 41.67 |  66.15 |  76.33  |
| 1997 |  36.09 |   56.67  | 41.76 |  68.83 |  77.55  |
| 2002 |  39.19 |   58.14  | 42.13 |  70.84 |  79.11  |
| 2007 |  39.61 |   60.92  | 43.83 |  71.78 |  80.20  |

### Activity 4

> In Window functions, we formed a tibble with 24 rows: 2 per year, giving the country with both the lowest and highest life expectancy (in Asia). Take that table (or a similar one for all continents) and reshape it so you have one row per year or per year \* continent combination.

First input the tibble formed in class.

``` r
actv4_tbl <- gapminder %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year) %>%
  print(n = Inf)
```

    ## # A tibble: 24 x 3
    ## # Groups:   year [12]
    ##     year     country lifeExp
    ##    <int>      <fctr>   <dbl>
    ##  1  1952 Afghanistan  28.801
    ##  2  1952      Israel  65.390
    ##  3  1957 Afghanistan  30.332
    ##  4  1957      Israel  67.840
    ##  5  1962 Afghanistan  31.997
    ##  6  1962      Israel  69.390
    ##  7  1967 Afghanistan  34.020
    ##  8  1967       Japan  71.430
    ##  9  1972 Afghanistan  36.088
    ## 10  1972       Japan  73.420
    ## 11  1977    Cambodia  31.220
    ## 12  1977       Japan  75.380
    ## 13  1982 Afghanistan  39.854
    ## 14  1982       Japan  77.110
    ## 15  1987 Afghanistan  40.822
    ## 16  1987       Japan  78.670
    ## 17  1992 Afghanistan  41.674
    ## 18  1992       Japan  79.360
    ## 19  1997 Afghanistan  41.763
    ## 20  1997       Japan  80.690
    ## 21  2002 Afghanistan  42.129
    ## 22  2002       Japan  82.000
    ## 23  2007 Afghanistan  43.828
    ## 24  2007       Japan  82.603
