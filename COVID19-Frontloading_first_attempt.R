library(dplyr)
library(ggplot2)

demog <- data.frame(age = 0:100,
                    num_people = rep(800000,101),
                    death_rate = rep(1,101),
                    year = 0)


demog <- 
    demog %>%
    mutate(death_rate = if_else(age <11, 10 ** (age * (-0.2) - 2), 10 ** (age * (3.5/90) - 39.5/9))) %>%
    
    mutate(num_people = num_people - if_else(age < 50, 0, 800000* (age - 50) / 50)) 

demog %>% ggplot(aes(x = age, y = death_rate)) + geom_point(colour = "blue") + scale_y_log10()
demog %>% ggplot(aes(x = age, y = num_people)) + geom_point(colour = "blue") 


demog %>% summarise(num_death = sum(num_people*death_rate), population = sum(num_people))

for(i in (1:100)){
    demog <- 
        demog %>% 
        filter(year == max(year)) %>%
        mutate(num_people = num_people * (1-death_rate) %>% lag(1)) %>% 
        mutate(num_people = replace(num_people, is.na(num_people), 1000000)) %>%
        mutate(year = year + 1) %>%
        rbind(demog)
}



demog %>% group_by(year) %>% summarise(num_death = sum(num_people*death_rate), population = sum(num_people)) %>% View()


demog %>% group_by(year) %>% summarise(sum(num_people*death_rate)) %>% View()

demog %>% group_by(year) %>% summarise(population = sum(num_people)) %>%
    ggplot(aes(x = year, y = population)) + geom_point(colour = "blue")
