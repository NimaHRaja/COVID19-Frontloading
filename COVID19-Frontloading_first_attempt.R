#### INIT

library(dplyr)
library(ggplot2)

#### Read demo

demog <- 
    inner_join(read.csv("death_rate_v_age.csv"), read.csv("num_people_v_age.csv"), by = "age") %>%
    mutate(year = 0)

#### sanity checks

demog %>% 
    ggplot(aes(x = age, y = num_people)) + 
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() 

demog %>% 
    ggplot(aes(x = age, y = death_rate)) + 
    geom_point(colour = "blue") + 
    scale_y_log10()

demog %>% summarise(num_death = sum(num_people*death_rate), population = sum(num_people))
demog %>% summarise(annual_rate = sum(num_people*death_rate) / sum(num_people))

#### Generate next years

num_years <- 50
annual_birth <- 750000

for(i in (1:num_years)){
    demog <- 
        demog %>% 
        filter(year == max(year)) %>%
        mutate(num_people = num_people * (1-death_rate),
               num_people = num_people %>% lag(1),
               num_people = replace(num_people, is.na(num_people), annual_birth),
               year = year + 1) %>%
        rbind(demog)
}

#### time-series summaries

demog_summary <- 
    demog %>% 
    group_by(year) %>% 
    summarise(num_death = sum(num_people*death_rate), population = sum(num_people)) %>%
    mutate(annual_death_rate = num_death/population)

demog_summary %>% 
    ggplot(aes(x = year, y =annual_death_rate)) +
    geom_point(colour = "blue")

demog_summary %>% 
    ggplot(aes(x = year, y =population)) +
    geom_point(colour = "blue")

demog_summary %>% 
    ggplot(aes(x = year, y =annual_death_rate)) + 
    geom_point(colour = "blue")

demog %>% 
    filter(year <= 10) %>%
    ggplot(aes(x = age, y = num_people, colour = as.factor(year))) + 
    geom_point() 

