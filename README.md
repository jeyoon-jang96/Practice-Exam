Practice-Exam
================
Jane Jang
2/28/2020

# Practice Exam

This practice exam asks you to do several code wrangling tasks that we
have done in class so far.

Clone this repo into Rstudio and fill in the necessary code. Then,
commit and push to github. Finally, turn in a link to canvas.

    ## -- Attaching packages -- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.4
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ----- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Make a plot with three facets, one for each airport in the weather data.
The x-axis should be the day of the year (1:365) and the y-axis should
be the mean temperature recorded on that day, at that airport.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
weather %>%
  mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin,day_of_year )%>%
  summarize(mean_temp = mean(temp)) %>%
  ggplot+
  geom_point(mapping= aes(x=day_of_year, y=mean_temp, color=origin))+
  facet_wrap(~origin)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Practice_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Make a non-tidy matrix of that data where each row is an airport and
each column is a day of the year.

``` r
weather %>%
  mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin,day_of_year )%>%
  summarize(mean_temp = mean(temp))%>%
  pivot_wider(names_from = day_of_year, values_from = mean_temp)
```

    ## # A tibble: 3 x 365
    ## # Groups:   origin [3]
    ##   origin   `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `10`  `11`  `12`
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 EWR     36.8  28.7  29.6  34.3  36.6  39.9  40.3  38.6  42.1  43.6  42.0  46.0
    ## 2 JFK     36.9  28.6  30.1  34.7  36.8  39.3  40.1  39.4  42.7  43.6  41.3  45.0
    ## 3 LGA     37.2  28.8  30.3  35.8  38.3  41.0  41.4  42.3  44.9  44.3  40.3  43.9
    ## # ... with 352 more variables: `13` <dbl>, `14` <dbl>, `15` <dbl>, `16` <dbl>,
    ## #   `17` <dbl>, `18` <dbl>, `19` <dbl>, `20` <dbl>, `21` <dbl>, `22` <dbl>,
    ## #   `23` <dbl>, `24` <dbl>, `25` <dbl>, `26` <dbl>, `27` <dbl>, `28` <dbl>,
    ## #   `29` <dbl>, `30` <dbl>, `31` <dbl>, `32` <dbl>, `33` <dbl>, `34` <dbl>,
    ## #   `35` <dbl>, `36` <dbl>, `37` <dbl>, `38` <dbl>, `39` <dbl>, `40` <dbl>,
    ## #   `41` <dbl>, `42` <dbl>, `43` <dbl>, `44` <dbl>, `45` <dbl>, `46` <dbl>,
    ## #   `47` <dbl>, `48` <dbl>, `49` <dbl>, `50` <dbl>, `51` <dbl>, `52` <dbl>,
    ## #   `53` <dbl>, `54` <dbl>, `55` <dbl>, `56` <dbl>, `57` <dbl>, `58` <dbl>,
    ## #   `59` <dbl>, `60` <dbl>, `61` <dbl>, `62` <dbl>, `63` <dbl>, `64` <dbl>,
    ## #   `65` <dbl>, `66` <dbl>, `67` <dbl>, `68` <dbl>, `69` <dbl>, `70` <dbl>,
    ## #   `71` <dbl>, `72` <dbl>, `73` <dbl>, `74` <dbl>, `75` <dbl>, `76` <dbl>,
    ## #   `77` <dbl>, `78` <dbl>, `79` <dbl>, `80` <dbl>, `81` <dbl>, `82` <dbl>,
    ## #   `83` <dbl>, `84` <dbl>, `85` <dbl>, `86` <dbl>, `87` <dbl>, `88` <dbl>,
    ## #   `89` <dbl>, `90` <dbl>, `91` <dbl>, `92` <dbl>, `93` <dbl>, `94` <dbl>,
    ## #   `95` <dbl>, `96` <dbl>, `97` <dbl>, `98` <dbl>, `99` <dbl>, `100` <dbl>,
    ## #   `101` <dbl>, `102` <dbl>, `103` <dbl>, `104` <dbl>, `105` <dbl>,
    ## #   `106` <dbl>, `107` <dbl>, `108` <dbl>, `109` <dbl>, `110` <dbl>,
    ## #   `111` <dbl>, `112` <dbl>, ...

For each (airport, day) contruct a tidy data set of the airport’s
“performance” as the proportion of flights that departed less than an
hour late.

``` r
flights %>%
  mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year)%>%
  mutate(performance = dep_time - sched_dep_time)%>%
  filter(performance< 60 & performance >0)%>%
  select(origin, day_of_year, performance)
```

    ## # A tibble: 81,110 x 3
    ## # Groups:   origin, day_of_year [1,095]
    ##    origin day_of_year performance
    ##    <chr>        <dbl>       <int>
    ##  1 EWR              1           2
    ##  2 LGA              1           4
    ##  3 JFK              1           2
    ##  4 EWR              1           1
    ##  5 EWR              1           8
    ##  6 JFK              1          11
    ##  7 JFK              1           3
    ##  8 LGA              1          13
    ##  9 EWR              1          24
    ## 10 EWR              1           8
    ## # ... with 81,100 more rows

Construct a tidy data set to that give weather summaries for each
(airport, day). Use the total precipitation, minimum visibility, maximum
wind\_gust, and average wind\_speed.

``` r
weather %>%
  mutate(day_of_year = yday(time_hour))%>%
  group_by(origin, day_of_year)%>%
  summarize(minimum_visibility = min(visib), avg_wind_speed = mean(wind_speed))
```

    ## # A tibble: 1,092 x 4
    ## # Groups:   origin [3]
    ##    origin day_of_year minimum_visibility avg_wind_speed
    ##    <chr>        <dbl>              <dbl>          <dbl>
    ##  1 EWR              1                 10          13.2 
    ##  2 EWR              2                 10          10.9 
    ##  3 EWR              3                 10           8.58
    ##  4 EWR              4                 10          14.0 
    ##  5 EWR              5                 10           9.40
    ##  6 EWR              6                  6           9.11
    ##  7 EWR              7                 10           7.34
    ##  8 EWR              8                  8           7.19
    ##  9 EWR              9                  6           5.99
    ## 10 EWR             10                 10           8.92
    ## # ... with 1,082 more rows

Construct a linear model to predict the performance of each
(airport,day) using the weather summaries and a “fixed effect” for each
airport. Display the summaries.

``` r
flights1 <- flights %>%
  mutate(day_of_year = yday(time_hour)) %>%
  group_by(origin, day_of_year)%>%
  mutate(performance = dep_time - sched_dep_time)
 

mod1 <- lm(performance~ origin + day_of_year, data=flights1)
summary(mod1)
```

    ## 
    ## Call:
    ## lm(formula = performance ~ origin + day_of_year, data = flights1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2361.83   -21.25    -7.35     5.55  1504.06 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.125768   0.613267  31.187   <2e-16 ***
    ## originJFK   -19.180131   0.609933 -31.446   <2e-16 ***
    ## originLGA   -11.995984   0.622247 -19.278   <2e-16 ***
    ## day_of_year  -0.002481   0.002431  -1.021    0.307    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 145.2 on 328517 degrees of freedom
    ##   (8255 observations deleted due to missingness)
    ## Multiple R-squared:  0.003081,   Adjusted R-squared:  0.003072 
    ## F-statistic: 338.5 on 3 and 328517 DF,  p-value: < 2.2e-16

Repeat the above, but only for EWR. Obviously, exclude the fixed effect
for each airport.

``` r
flights2 <- flights %>%
  filter(origin=="EWR") %>%
  mutate(day_of_year = yday(time_hour)) %>%
  group_by(day_of_year)%>%
  mutate(performance = dep_time - sched_dep_time)
  
 
mod2 <- lm(performance ~ day_of_year, data = flights2 )
summary(mod2)
```

    ## 
    ## Call:
    ## lm(formula = performance ~ day_of_year, data = flights2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2362.44   -23.79   -19.17     0.47  1285.78 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 20.889593   0.648253  32.224  < 2e-16 ***
    ## day_of_year -0.012147   0.003085  -3.937 8.26e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 110.2 on 117594 degrees of freedom
    ##   (3239 observations deleted due to missingness)
    ## Multiple R-squared:  0.0001318,  Adjusted R-squared:  0.0001233 
    ## F-statistic:  15.5 on 1 and 117594 DF,  p-value: 8.261e-05
