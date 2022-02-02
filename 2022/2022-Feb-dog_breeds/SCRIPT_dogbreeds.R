#---------------------------
### TidyTuesday 2022: week 5
### Dog breeds
### Jodie Lord
### Feb 2022
#---------------------------


# Installing and loading libraries -----

library(ggbump)
library(ggimage)
library(ggtext)
library(here)
library(tidyverse)


## setting dir -----

setwd(here("2022",
           "2022-Feb-dog_breeds"))


# Obtaining data -----

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
#trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')


# Prepping data -----

breed_rank <- breed_rank_all %>%
  rename(`2013` = `2013 Rank`, 
         `2014` = `2014 Rank`, 
         `2015` = `2015 Rank`, 
         `2016` = `2016 Rank`, 
         `2017` = `2017 Rank`, 
         `2018` = `2018 Rank`, 
         `2019` = `2019 Rank`, 
         `2020` = `2020 Rank`) %>% 
  select(-links) %>%
  #filtering to only dogs which have ranked in the top 20
  filter(`2020`<=20) %>% 
  #pivotting to long format
  pivot_longer(`2013`:`2020`) %>% 
  rename(year = name) %>% 
  #parsing year as number so can be read into axis of graph
  mutate(year = parse_number(year),
         #changing to factors so can easily colour label required dogs
         Breed = factor(Breed,
                 levels = c("Retrievers (Labrador)",
                            "Rottweilers",
                            "Doberman Pinschers",
                            "German Shepherd Dogs",
                            "Retrievers (Golden)",
                            "Beagles",
                            "Bulldogs",
                            "Yorkshire Terriers",
                            "Boxers",
                            "Poodles",
                            "Dachshunds",
                            "French Bulldogs",
                            "Pointers (German Shorthaired)",
                            "Siberian Huskies",
                            "Shih Tzu",
                            "Great Danes",
                            "Miniature Schnauzers",
                            "Cavalier King Charles Spaniels",
                            "Australian Shepherds",
                            "Pembroke Welsh Corgis"))) 


# Plotting data -----

p1 <- ggplot() +
  #pump plot for the main visual
  geom_bump(breed_rank,
            mapping=aes(x = year, 
                        y = value, 
                        color = Breed),
            size = 3, 
            smooth = 5) +
  #adding images as axis labels
  geom_image(data = breed_rank %>%
               filter(year == min(year) | year == max(year)),
             mapping = aes(x = year,
                           y = value,
                           image = Image),
             size = 0.03) +
  #including breed names on both sides of axis
  geom_text(data = breed_rank %>%
              filter(year == min(year)),
            aes(x = year - .2,
                y = value,
                label = Breed),
            size = 3,
            hjust = 1) +
  geom_text(data = breed_rank %>%
              filter(year == max(year)),
            aes(x = year + .2,
                y = value,
                label = Breed),
            size = 3, hjust = 0) +
  labs(y = "",
       x="",
       subtitle = "<span style='color:steelblue'>**Blue hues:**</span> Dogs that have been owned by Jodie Lord in her lifetime!",
       caption="
        Graphic by: Jodie Lord | @jodielord5 | #TidyTuesday #R4DS | Data source: American Kennel Club" ) +
  scale_x_continuous(limits = c(2012, 2021),
                     breaks = seq(2013, 2020, 1)) +
  theme_minimal() +
  ggtitle("**Labrador reigns supreme in the 20 most popular dog breeds in America between 2013 - 2020**") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.text.y = element_blank(),
        plot.title=element_markdown(size=14),
        plot.subtitle=element_markdown(size=12),
        plot.caption = element_text(size=10),
        plot.margin = unit(c(1, .5, 1, .5), "cm"),) +
  coord_cartesian(clip = "off") +
  #colouring dog breeds which I have owned in blue
  scale_color_manual(values = c("steelblue", "steelblue", "steelblue",
                                "#DCDCDC", "#DCDCDC", "#DCDCDC",
                                "#DCDCDC", "#DCDCDC", "#DCDCDC",
                                "#DCDCDC", "#DCDCDC", "lightgrey", 
                                "#DCDCDC", "#DCDCDC", "#DCDCDC", "lightgrey",
                                "#DCDCDC", "#DCDCDC", "#DCDCDC", "lightgrey")) +
  #reversing scale so that 1 descending
  scale_y_reverse(breaks = seq(from = 1, to = 20)) 


## saving visualisation -----
ggsave("OUTPUT_dog_breeds.pdf",
       plot = p1,
       width = 15,
       height = 10,
       dpi = 300)


