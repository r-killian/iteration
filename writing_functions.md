Writing Functions
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

set.seed(1)
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1] -0.2022306 -0.3204049 -1.8795425 -0.7026191  0.3598721  1.4753003
    ##  [7] -0.2575539  0.3239812 -0.1994754 -1.7684514 -0.6277359 -0.6031866
    ## [13] -0.2060066  1.1686154  0.7692145 -0.3307538 -0.4360887  0.6907069
    ## [19]  0.5243537 -0.9523331 -0.9745524  0.2966039  0.7755665 -0.2688873
    ## [25]  0.9090459  0.3363531 -0.8613556  0.2687848 -1.4747590  1.5634498
    ## [31]  2.2124710 -0.5710916 -1.3737041  0.5398346 -0.2958126  2.7119068
    ## [37] -0.1822057  0.6821414 -0.1024770 -1.0169742

What else can we do?

``` r
z_scores(3)
```

    ## [1] NA

NA?

``` r
z_scores(c("my", "name", "is", "rose"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

Error!

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

MANY errors!!!

Let’s try again

``` r
z_scores = function(x) {
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 values")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

## Mutiple outputs

``` r
mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 values")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.67  3.80

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.253

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.51  2.82

Write a function that simulates data, computes mean and sd

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  #do checks on inputs
  
sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
#shift + ctrl to select multiple lines!!!
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.96  2.43

``` r
sim_mean_sd(30)
```

    ## Error in rnorm(n, mean = mu, sd = sigma): argument "mu" is missing, with no default

``` r
#wont work

sim_mean_sd = function(n, mu = 4, sigma = 3) {
  
  #do checks on inputs
  
sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.75  2.73

``` r
#now it will

sim_mean_sd(30, 40, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.8  3.21

``` r
#can still specify a value. Using positional matching
```

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay, but there are more pages of reviews

Write a functions that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
    )
  
  return(reviews)
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  3 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  4 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  5 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ##  6 Painful                                               1.0 ou~ "\n  I think I~
    ##  7 GRAND                                                 5.0 ou~ "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou~ "\n  So nostal~
    ##  9 Cult Classic                                          5.0 ou~ "\n  Watched i~
    ## 10 Format was inaccurate                                 4.0 ou~ "\n  There was~

``` r
# OR

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

get_page_reviews(urls[1])
```

    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  3 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  4 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  5 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ##  6 Painful                                               1.0 ou~ "\n  I think I~
    ##  7 GRAND                                                 5.0 ou~ "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou~ "\n  So nostal~
    ##  9 Cult Classic                                          5.0 ou~ "\n  Watched i~
    ## 10 Format was inaccurate                                 4.0 ou~ "\n  There was~

``` r
get_page_reviews(urls[2])
```

    ## # A tibble: 10 x 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Good funny                                  3.0 out of 5 stars "\n  Would re~
    ##  2 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn~
    ##  3 Your mom went to college.                   5.0 out of 5 stars "\n  Classic ~
    ##  4 Very funny movie                            5.0 out of 5 stars "\n  I watch ~
    ##  5 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing ~
    ##  6 A classic                                   5.0 out of 5 stars "\n  If you d~
    ##  7 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g~
    ##  8 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t~
    ##  9 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf~
    ## 10 Okay                                        3.0 out of 5 stars "\n  Okay\n"

``` r
get_page_reviews(urls[3])
```

    ## # A tibble: 10 x 3
    ##    title                                           stars              text      
    ##    <chr>                                           <chr>              <chr>     
    ##  1 "A WHOLESOME comedic journey"                   5.0 out of 5 stars "\n  Not ~
    ##  2 "Hilarious"                                     5.0 out of 5 stars "\n  Funn~
    ##  3 "Love it"                                       5.0 out of 5 stars "\n  What~
    ##  4 "WORTH IT!"                                     5.0 out of 5 stars "\n  It's~
    ##  5 "Funny movie."                                  5.0 out of 5 stars "\n  Grea~
    ##  6 "Best movie ever!"                              5.0 out of 5 stars "\n  Got ~
    ##  7 "I was stuck in the oil patch back in the day." 5.0 out of 5 stars "\n  I wa~
    ##  8 "Funny Dork humor"                              5.0 out of 5 stars "\n  Humo~
    ##  9 "Still funny!"                                  5.0 out of 5 stars "\n  Stil~
    ## 10 "Love it!! \U0001f49c"                          5.0 out of 5 stars "\n  Love~

``` r
get_page_reviews(urls[4])
```

    ## # A tibble: 10 x 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 "LOVE it"                         5.0 out of 5 stars "\n  cult classic. So u~
    ##  2 "Perfect"                         5.0 out of 5 stars "\n  Exactly what I ask~
    ##  3 "Love this movie!"                5.0 out of 5 stars "\n  Great movie and se~
    ##  4 "Love it"                         5.0 out of 5 stars "\n  Love this movie. H~
    ##  5 "As described"                    3.0 out of 5 stars "\n  Book is as describ~
    ##  6 "GOSH!!!"                         5.0 out of 5 stars "\n  Just watch the mov~
    ##  7 "Watch it right now"              5.0 out of 5 stars "\n  You need to watch ~
    ##  8 "At this point it’s an addiction" 5.0 out of 5 stars "\n  I watch this movie~
    ##  9 "\U0001f495"                      5.0 out of 5 stars "\n  Hands down, one of~
    ## 10 "Good dumb movie"                 5.0 out of 5 stars "\n  I really wanted to~

``` r
get_page_reviews(urls[5])
```

    ## # A tibble: 10 x 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 funny                             5.0 out of 5 stars "\n  so funny and inven~
    ##  2 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  3 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik~
    ##  4 So Funny                          5.0 out of 5 stars "\n  This is such a goo~
    ##  5 Best movie ever                   5.0 out of 5 stars "\n  It's napoleon dyna~
    ##  6 Funny                             5.0 out of 5 stars "\n  Classic\n"         
    ##  7 It’s broke!                       1.0 out of 5 stars "\n  I don’t know if yo~
    ##  8 Stupid                            1.0 out of 5 stars "\n  What can I say? St~
    ##  9 Not funny                         1.0 out of 5 stars "\n  Not funny\n"       
    ## 10 Great family movie                5.0 out of 5 stars "\n  Everyone should se~

``` r
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  3 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  4 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  5 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ##  6 Painful                                               1.0 ou~ "\n  I think I~
    ##  7 GRAND                                                 5.0 ou~ "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou~ "\n  So nostal~
    ##  9 Cult Classic                                          5.0 ou~ "\n  Watched i~
    ## 10 Format was inaccurate                                 4.0 ou~ "\n  There was~
    ## # ... with 40 more rows
