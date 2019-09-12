#### SDS exercise 1

## Date: Aug-05-2019

#### Load packages

    library(tidyverse)
    library(data.table)
    library(lubridate)


#### Load dataset

    trips <- data.table::fread("https://github.com/SDS-AAU/M1-2019/raw/master/data/trips.csv")
    people <- data.table::fread("https://github.com/SDS-AAU/M1-2019/raw/master/data/people.csv")
    country <- data.table::fread("https://github.com/SDS-AAU/M1-2019/raw/master/data/countrylist.csv")

    glimpse(trips)
    glimpse(people)
    glimpse(country)



## Exercise 1 -----

#### 1.a ----

    ## Dates to timestamps using as_date from lubridate
    trips1 <- trips %>%
        mutate(date_start = lubridate::as_date(date_start),
               date_end = as_date(date_end))


#### 1.b ----

    ## Calculate duration
    trips1 <- trips1 %>%
        mutate(duration = date_end - date_start)


#### 1.c ----

    ## Filter extreme duration observations
    # Check what might be extreme date_start and duration observations graphically
    trips1 %>%
        ggplot(., aes(x = duration)) +
        geom_histogram()

    trips1 %>%
        ggplot(., aes(x = date_start)) +
        geom_histogram()

    # Create percentiles for date_start and date_end
    trips1 <- trips1 %>%
        mutate(pct_date_start = percent_rank(as.numeric(date_start)),
               pct_date_end = percent_rank(as.numeric(date_end)))

    # Filter observations to be in interval
    trips1 <- trips1 %>%
        filter(0 < duration,
               0.01 <= pct_date_start, pct_date_start <= 0.97,
               0.01 <= pct_date_end, pct_date_end <= 0.97) %>%
        mutate(dur_num = as.numeric(duration))
    glimpse(trips1)



#### 1.d ----

    ## Alternative 1 with renaming
    # Rename alpha_2 to country_code in country-dataset
    country1 <- country %>%
        rename(country_code = alpha_2)

    # Join with countrylist
    trips_c <- left_join(trips1, country1)

    ## Alternative 2 with different keys in datasets
    trips_c <- left_join(trips1, country, by = c("country_code" = "alpha_2"))


## Exercise 2 -----

#### 2.a ----

    # Create new HS variable  using a str_detect function (assuming a standardized HS reference "High School")
    people1 <- people %>%
        mutate(HS = ifelse(str_detect(education_raw, "High School"), 1, 0))

    # Count people
    sum(people1$HS)



#### 2.b ----

    # We'll reuse the Master's degree variable, so create it first, then filter and count
    people1 <- people1 %>%
        mutate(Master = ifelse(str_detect(education_raw, "Master's"), 1, 0))

    people1 %>%
        filter(str_detect(work_raw, "Software Dev")) %>%
        count(Master)



#### 2.c ----

    #  Same procedure, creating Master's variable  people1 %>%
    top_fol <- people1 %>%
        filter(Master == 1) %>%
        arrange(desc(followers)) %>%
        head()

    # Show the dataset in a manageable format
    glimpse(top_fol)




## Exercise 3 -----

#### 3.a  ----

    # Country with the most trips using n() from dplyr
    trips_c %>%
        group_by(country) %>%
        summarise(trips = n()) %>%
        arrange(desc(trips))



#### 3.b ----

    # Create start year variable
    trips_c <- trips_c %>%
        mutate(start_y = year(date_start))


    # Find trips in 2017
    trips_c %>%
        filter(start_y == 2017) %>%
        group_by(country) %>%
        summarise(trips = n()) %>%
        arrange(desc(trips)) %>%
        head()


#### 3.c ----

    # Redo the analysus for region == "Eastern Asia"
    trips_c %>%
        filter(sub_region == "Eastern Asia") %>%
        group_by(country) %>%
        summarise(trips = n()) %>%
        arrange(trips) %>%
        head()



#### 3.d ----

    # Merge people and trips data
    trips_pc <- left_join(trips_c, people, by = "username")

    # Create a Software Dev variable to group by
    trips_pc <- trips_pc %>%
        mutate(soft_dev = ifelse(str_detect(work_raw, "Software Dev"), 1, 0))

    # Then get the average duration by group
    trips_pc %>%
        group_by(soft_dev) %>%
        summarise(ave_dur = mean(duration, na.rm = T))



#### 3.e ----

    # Create week-month-year with lubridate's floor_date function
    # and summarise to get number of observations by region
    trips_t <- trips_pc %>%
        mutate(date_start_week = floor_date(date_start, unit = "week"),
               date_start_month = floor_date(date_start, unit = "month"))



    # Create aggregated datasets
    trips_w <- trips_t %>%
        group_by(date_start_week, region) %>%
        summarise(trips = median(duration))
    trips_m <- trips_t %>%
        group_by(date_start_month, region) %>%
        summarise(trips = median(duration))

    # Some countries have no trips in a given period and so no rows
    # complete the dataset
    trips_w <- trips_w %>%
        complete(., date_start_week, region)
    trips_m <- trips_m %>%
        complete(., date_start_month, region)


    # Plot with ggplot2
    trips_w %>%
        ggplot(., aes(x = date_start_week, y = trips, color = region)) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(x = "Time",
             y = "Trips")
    trips_m %>%
        ggplot(., aes(x = date_start_month, y = trips, color = region)) +
        geom_point() +
        geom_line() +
        theme_classic() +
        labs(x = "Time",
             y = "Trips")


    ## Overall
    trips_t %>%
        mutate(week = floor_date(date_start, unit = "week")) %>%
        group_by(week) %>%
        summarise(med_dur = median(duration)) %>%
        ggplot(., aes(x = week, y = med_dur)) +
        geom_point() +
        geom_line() +
        theme_classic()





