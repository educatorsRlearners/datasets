#Load libraries
library(tidyverse)
library(rvest)
library(ggthemes)
library(spData)


#Hat tip to https://categitau.com/using-rvest-to-scrape-data-from-wikipedia/ 

site <- read_html("https://en.wikipedia.org/wiki/List_of_official_languages_by_country_and_territory")

world_languages <- site %>% 
  html_node("table.wikitable") %>%
  html_table()

#Remove extra columns
world_languages <- world_languages %>% 
  filter(Country != "Country")

#Create only one official language per country
world_languages$`Official language` <- str_extract(world_languages$`Official language`, '[A-Za-z]+')

world_languages$`Official language` <- if_else(
  condition = is.na(world_languages$`Official language`), 
  true = world_languages$`National language`,
  false = world_languages$`Official language`)

#Remove brackets from the first two columns
world_languages$Country <- str_remove(world_languages$Country, "\\[.*\\]")
world_languages$`Official language` <- str_remove(world_languages$`Official language`, "\\[.*\\]" )
world_languages$`Official language` <- str_extract(world_languages$`Official language`, '[A-Za-z]+')

#Update Myannmar
world_languages[127, 1] <- 'Myanmar'

#Get continents
continents <- spData::world %>% 
  select(name_long, continent)

#Update North and South Korea 
continents[96, 1] <- 'North Korea'
continents[97, 1] <- 'South Korea'

#Update Laos
continents[93, 1] <-'Laos'

#Update Ivory Coast
continents[61, 1] <- 'Ivory Coast'

#Update Russia
continents[19, 1] <- 'Russia'


#Add continents
world_languages <- left_join(world_languages, continents, by =c("Country"="name_long"))

#Update Kuwait 
world_languages[97, 2] <- 'Arabic'

#Update Bolivia
world_languages[23, 2] <- 'Spanish'

#Update Pakistan
world_languages[140, 2] <- "Urdu"

#Update Myanmar
world_languages[127, 1] <- "Myanmar"

#Update Iran
world_languages[82, 2] <- "Farsi"

#Update Afghanistan
world_languages[2,2] <- "Dari"

#Update Philipines
world_languages[147, 2] <- 'Tagalog'


# Scrape FSI website
fsi_site <- read_html("https://www.state.gov/foreign-language-training/")


#Scrape category Category 1 languages
lang_hours <- c("language", "hours")

fsi_site %>% 
  html_node(xpath = '/html/body/div[1]/div[1]/main/article/div[2]/div/table[1]') %>% 
  html_table() %>% 
  stack() -> category1

category1$ind <- NULL

category1 <- category1 %>% 
  separate(values, c('language', 'hours'))

category1$hours <- as.numeric(category1$hours)
category1$hours[category1$hours==24] <- 600
category1$hours[category1$hours==30] <- 750
category1$category <- "Category 1"

#Scrape category 2 languages
fsi_site %>% 
  html_node(xpath = '//*[@id="post-7487"]/div[2]/div/table[2]') %>% 
  html_table() %>% 
  stack() -> category2

colnames(category2) <- lang_hours
category2$hours <- 900

category2$category <- "Category 2"

category2 <- category2[-c(6), ]


#Scrape Category 3 Languages
fsi_site %>% 
  html_node(xpath = '//*[@id="post-7487"]/div[2]/div/table[3]') %>% 
  html_table() %>% 
  stack() -> category3

colnames(category3) <- lang_hours 

#Add Croation
category3 <- category3 %>% 
  add_row(language = "Croatian")

#Add Bosnian
category3 <- category3 %>% 
  add_row(language = "Bosnian")

#Add Serbian
category3 <- category3 %>% 
  add_row(language = "Serbian")

#Change Tajiki to Tajik
category3[13, 1] <- "Tajik"

#Change Slovenian to Slovene	
category3[12, 1] <- "Slovene"

category3$hours <- 1100

category3$category <- "Category 3"

category3 <- category3[-c(48,11),]

fsi_site %>% 
  html_node(xpath = '/html/body/div[1]/div[1]/main/article/div[2]/div/div/table') %>% 
  html_table() %>% 
  stack()-> category4

colnames(category4) <- lang_hours 
category4$hours <- 2200

#Remove blank row
category4 <- category4[-c(6),]

#Add category column
category4$category <- "Category 4"

#Remove make Mandarin and Cantonese separate languages
category4$language <- str_remove(category4$language, "Chinese – ")


#Combine categories
fsi_rankings <- bind_rows(category1, category2, category3, category4)

fsi_rankings <- fsi_rankings %>% 
  add_row(language = "English", hours = 0, category = "Category 0")

fsi_rankings$category <- as_factor(fsi_rankings$category)

fsi_rankings$category <- fct_relevel(fsi_rankings$category, "Category 0")

#Join the dataframes
df <- left_join(x = world_languages, y = fsi_rankings, by = c("Official language" = "language"))

df <- df %>% 
  select(Country, `Official language`, hours, category, everything())




world_map <- map_data("world")

world_map <- world_map %>% 
  filter(region != "Antarctica")

world_map$region <- world_map$region %>% 
  str_replace('USA', 'United States')

world_map$region <- world_map$region %>% 
  str_replace('UK', 'United Kingdom')


world_map_difficulty <- left_join(x = world_map, y = df, by = c("region" = "Country"))

ggplot(world_map_difficulty, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = category), color = "white") +
  theme_map()+
  scale_fill_brewer(type = "seq", 
                    palette = "Reds", 
                    na.value = "gray",
                    labels = c("Category 0 - English as (an) Official Language",
                               "Category 1 ~600 hours",
                               "Category 2 ~900 hours", 
                               "Category 3 ~1100 hours",
                               "Category 4 ~2200 hours", 
                               "No Data")) +
  theme(legend.text=element_text(size=12)) +
  labs(fill = '') -> world_difficulty

world_difficulty

asian_difficulty <-  world_map_difficulty %>% 
  filter(continent == "Asia")

ggplot(asian_difficulty, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = category), color = "white") +
  theme_map()+
  scale_fill_brewer(type = "seq", 
                    palette = "Reds", 
                    na.value = "gray",
                    labels = c("Category 2 ~900 hours", 
                               "Category 3 ~1100 hours",
                               "Category 4 ~2200 hours", 
                               "No Data"))+
  theme(legend.text=element_text(size=12)) +
  labs(fill = '') -> asian_map_difficulty

asian_map_difficulty
