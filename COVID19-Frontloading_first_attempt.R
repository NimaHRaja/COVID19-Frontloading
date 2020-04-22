library(dplyr)
library(ggplot2)

demog <- data.frame(age = 0:100,
                    year = 0)

demog <- 
    demog %>% 
    inner_join(read.csv("death_rate_v_age.csv"), by = "age") %>% 
    inner_join(read.csv("num_people_v_age.csv"), by = "age")




# 
# 
# 
# DR_0 <- 0.005
# DR_10 <- 0.0001
# DR_100 <- 0.2
# 
# demog <-
#     demog %>%
#     mutate(death_rate = if_else(age <11,
#                                 exp(age * (log(DR_10/DR_0))/10 + log(DR_0)),
#                                 exp((age-10) * (log(DR_100/DR_10))/90 + log(DR_10)))) %>%
#     mutate(num_people = num_people - if_else(age < 51, 0, 800000* (age - 51) / 50)) %>%
#     mutate(num_people = if_else(age == 0, 1000000, num_people))

demog %>% ggplot(aes(x = age, y = death_rate)) + geom_point(colour = "blue") + scale_y_log10()
demog %>% ggplot(aes(x = age, y = num_people, colour = as.factor(year))) + geom_point() 


demog %>% summarise(num_death = sum(num_people*death_rate), population = sum(num_people))

for(i in (1:50)){
    demog <- 
        demog %>% 
        filter(year == max(year)) %>%
        mutate(num_people = num_people * (1-death_rate),
               num_people = num_people %>% lag(1),num_people = replace(num_people, is.na(num_people), 750000),
               year = year + 1) %>%
        rbind(demog)
}



demog %>% group_by(year) %>% summarise(num_death = sum(num_people*death_rate), population = sum(num_people)) %>% View()


demog %>% group_by(year) %>% summarise(sum(num_people*death_rate)) %>% View()

demog %>% group_by(year) %>% summarise(population = sum(num_people)) %>%
    ggplot(aes(x = year, y = population)) + geom_point(colour = "blue")


demog %>% group_by(year) %>% summarise(population = sum(num_people)) %>% 
    ggplot(aes(x = year, y = population)) + 
    geom_point(colour = "blue")


demog %>% group_by(year) %>% summarise(num_death = sum(num_people* death_rate)) %>% 
    ggplot(aes(x = year, y = num_death)) + 
    geom_point(colour = "blue")
