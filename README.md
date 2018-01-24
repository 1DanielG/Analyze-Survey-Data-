Brand Survey Analysis
================
Ali Gharaee
January 23, 2018

I made the following project report to analyze a survey result. My client has allowed me to put a piece of data on my public GitHub account.

This survey covers a wide range of public opinions on specific brands and begins with a summary of the key findings drawn from the survey analysis and public score for each brand. The recommendations are followed by detailed findings which will not be published here. I have already saved part of original data as an excel file which you can find in the data section.

This data contains three different surveys such as brand awareness, advertisement awareness, and brand recommendation for 3 specific brands (the brand names have been replaced with dummy names at client request )

Packages instalation
--------------------

``` r
library(tidyverse)
library(readxl)
library(chron)
library(lubridate)
```

Data Import
-----------

The data is in an `.xlsx` file. I read it into R using `readxl::read_excel`.

``` r
data <- read_excel("survey_data.xlsx", sheet = 1 )
head(data)
```

    ## # A tibble: 6 x 11
    ##   InterviewStart      InterviewEnd        Brnd1_Awa Brnd2_Awa Brnd3_Awa
    ##   <dttm>              <dttm>                  <dbl>     <dbl>     <dbl>
    ## 1 2017-12-02 00:21:23 2017-12-02 00:29:36      1.00      0         0   
    ## 2 2017-12-02 03:52:07 2017-12-02 04:00:37      0         0         0   
    ## 3 2017-12-01 08:23:34 2017-12-01 08:30:37      0         1.00      1.00
    ## 4 2017-12-01 10:34:36 2017-12-01 10:40:48      1.00      1.00      1.00
    ## 5 2017-12-01 23:25:35 2017-12-01 23:30:28      1.00      1.00      0   
    ## 6 2017-12-01 20:02:49 2017-12-01 20:12:02      0         1.00      0   
    ## # ... with 6 more variables: Brnd1_AdAw <chr>, Brnd2_AdAw <chr>,
    ## #   Brnd3_AdAw <chr>, Brnd1_Recmnd <chr>, Brnd2_Recmnd <chr>, Brnd3_Recmnd
    ## #   <chr>

At first glance, data need to be tidy up. Tidy data is the data obtained as a result of a process called data tidying. It is one of the important cleaning processes during big data processing and is a recognized step in the practice of data science.

Use `gather::tidyr` to convert a wide-format table to a tidy dataframe:

``` r
data_tidy <- data %>% 
  gather(survey, response , -c(1:2))

head(data_tidy)
```

    ## # A tibble: 6 x 4
    ##   InterviewStart      InterviewEnd        survey    response
    ##   <dttm>              <dttm>              <chr>     <chr>   
    ## 1 2017-12-02 00:21:23 2017-12-02 00:29:36 Brnd1_Awa 1       
    ## 2 2017-12-02 03:52:07 2017-12-02 04:00:37 Brnd1_Awa 0       
    ## 3 2017-12-01 08:23:34 2017-12-01 08:30:37 Brnd1_Awa 0       
    ## 4 2017-12-01 10:34:36 2017-12-01 10:40:48 Brnd1_Awa 1       
    ## 5 2017-12-01 23:25:35 2017-12-01 23:30:28 Brnd1_Awa 1       
    ## 6 2017-12-01 20:02:49 2017-12-01 20:12:02 Brnd1_Awa 0

First I will work on brand awareness and advertising awareness surveys. The client would like to see:

-   How many % of the respondents know brand1, brand2, and brand3?
-   How many % of the respondents can remember seeing an advertisement for brand1, brand2, and brand3?

According to the survey metadata, brand awareness survey was a force survey which respondents must answer something (aware or not aware) but for advertisement survey respondents have considerable freedom to not answer.

``` r
data_tidy %>%
  select(survey, response) %>%
  # filter only row revelent to brand awareness and adv awareness surveys
  # brand awareness data end with "Awa" and adv awareness end with "Adaw" 
  filter(grepl("Awa$|AdAw$", survey)) %>% 
  group_by(survey) %>%
  summarise(
    Aware = sum(response == 1),
    Not_Aware = sum(response == 0),
    Not_Answered = sum(response == "N/A"),
    Total = n(),
    Prop_Aware = Aware / Total  )
```

    ## # A tibble: 6 x 6
    ##   survey     Aware Not_Aware Not_Answered Total Prop_Aware
    ##   <chr>      <int>     <int>        <int> <int>      <dbl>
    ## 1 Brnd1_AdAw   138       115          247   500      0.276
    ## 2 Brnd1_Awa    253       247            0   500      0.506
    ## 3 Brnd2_AdAw   137       118          245   500      0.274
    ## 4 Brnd2_Awa    255       245            0   500      0.510
    ## 5 Brnd3_AdAw   124       128          248   500      0.248
    ## 6 Brnd3_Awa    252       248            0   500      0.504

     % of the respondents know about brands: 

    + Brand 1     50.6 % 
    + Brand 2     51 %
    + Brand 3     50.4 % 

    % of the respondents can remember seeing an advertisement:

    - Brand 1    27.6 %
    - Brand 2    27.4 %
    - Brand 3    24.8 %

I worked on recommendation survey separately. I could absolutely do these in one step, but I wanted to split things up and check the results along the way, to ensure each step does what I intend it to.

For this survey, respondent was asked to give a score out of 5 to the brand. *`N/A` in this survey means the question was not asked by the surveyor.*

``` r
data_tidy %>%
  select(survey, response) %>%
  #filter only row revelent to Recmnd surveys
  filter(grepl("Recmnd$", survey)) %>%
  # Replaced "N/A" (question was not asked) with NA
  mutate(response = na_if(response , "N/A"),
         response = as.integer(response)) %>% 
  group_by(survey) %>%
  # Calculate summary statistical summary
  summarise(
    Total_Score = sum(response , na.rm = TRUE),
    Not_Asked = sum(is.na(response)),
    Total = n() - Not_Asked,
    Prop_Score = Total_Score / Total)
```

    ## # A tibble: 3 x 5
    ##   survey       Total_Score Not_Asked Total Prop_Score
    ##   <chr>              <int>     <int> <int>      <dbl>
    ## 1 Brnd1_Recmnd         794       247   253       3.14
    ## 2 Brnd2_Recmnd         744       245   255       2.92
    ## 3 Brnd3_Recmnd         768       248   252       3.05

% Average recommendation for;

    + Brand 1     3.14
    + Brand 2     2.92
    + Brand 3     3.05

The survey data also contains date and interview duration. The client was also interested to know the average length of an interview as well. I used `transmute::dplyr` and `lubridate` packages to prepare interview data.

``` r
time <-  data %>% 
  transmute(start_date = as.Date(InterviewStart), 
         start_time = times(format(InterviewStart, "%H:%M:%S")),
         end_date = as.Date(InterviewEnd), 
         end_time = times(format(InterviewEnd, "%H:%M:%S")),
         time_dif = end_time - start_time )

head(time)
```

    ## # A tibble: 6 x 5
    ##   start_date start_time  end_date   end_time    time_dif   
    ##   <date>     <S3: times> <date>     <S3: times> <S3: times>
    ## 1 2017-12-02 00:21:23    2017-12-02 00:29:36    00:08:13   
    ## 2 2017-12-02 03:52:07    2017-12-02 04:00:37    00:08:30   
    ## 3 2017-12-01 08:23:34    2017-12-01 08:30:37    00:07:03   
    ## 4 2017-12-01 10:34:36    2017-12-01 10:40:48    00:06:12   
    ## 5 2017-12-01 23:25:35    2017-12-01 23:30:28    00:04:53   
    ## 6 2017-12-01 20:02:49    2017-12-01 20:12:02    00:09:13

Finally calculate the average length of an interview with `summarise::dplyr`

``` r
time %>% 
  summarise(avg_interview_length = mean(time_dif))
```

    ## # A tibble: 1 x 1
    ##   avg_interview_length
    ##   <S3: times>         
    ## 1 00:05:15

Average length of an interview is 05:15 (present in the format "tt:mm:ss")
