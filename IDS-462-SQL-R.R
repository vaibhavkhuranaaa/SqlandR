#Clear the environment
rm(list=ls())

#libraries
library(dplyr)
library(sqldf)
library(haven)
library(tidyr)

#Load the data files
# Load the Province and Country tables
Province <- data.frame(read_sas("province(1).sas7bdat"))
Country <- data.frame(read_sas("country(1).sas7bdat"))
city <- data.frame(read_sas("city(1)(1).sas7bdat"))
orders <- data.frame(read.csv("OrderList.csv"))

# Filter out rows with blank province names
Province <- Province %>% filter(!is.na(ProvinceName)) %>% rename("Code" = "Country") %>% drop_na()
city <- city %>% filter(!is.na(CityName)) %>% rename("Code" = "Country")

#ANSWER 1; a table showing the Province name, Province population and Country Name that the Province is located in.
#IN R
# Join the Province and Country tables by the CountryCode field
Answer_1_R <- inner_join(Province, Country, by = "Code") %>%
  select(CountryName, province_name = ProvinceName, province_population = Population.x) %>%
  group_by(CountryName) %>%
  arrange(CountryName)

head(Answer_1_R)

#In SQLDF
Answer_1_SQL <- sqldf("
  SELECT Province.ProvinceName AS province_name,
         Province.Population AS province_population,
         Country.CountryName AS country_name
  FROM Province
  INNER JOIN Country
  ON Province.Code = Country.Code
  ORDER BY Country.CountryName
")
head(Answer_1_SQL)

#ANSWER 2; a table showing the maximum and minimum population size of the Provinces in each country.  
#IN R
Answer_2_R <- Province %>%
  left_join(Country, by = "Code", multiple = "all") %>%
  group_by(CountryName, Code) %>%
  summarise(max_population = max(as.numeric(Population.x)),
            min_population = min(as.numeric(Population.x))) %>%
  arrange(CountryName)
  
head(Answer_2_R)

#In SQLDF
Answer_2_SQL <- sqldf("
SELECT Country.CountryName, Province.Code, MAX(CAST(Province.Population AS NUMERIC)) AS max_population, MIN(CAST(Province.Population AS NUMERIC)) AS min_population
FROM Province
LEFT JOIN Country ON Province.Code = Country.Code
GROUP BY Country.CountryName, Province.Code
ORDER BY Country.CountryName
")

head(Answer_2_SQL)


#ANSWER 3; number of cities in each country
#IN R
joined_table <- inner_join(Country, city, by = "Code", multiple = "all")
Answer_3_R <- joined_table %>%
  group_by(CountryName) %>%
  summarize(num_cities = n())

head(Answer_3_R)

#In SQLDF
Answer_3_SQL <- sqldf("
SELECT Country.CountryName, COUNT(city.CityName) AS num_cities
FROM Country
INNER JOIN city ON Country.Code = city.Code
GROUP BY Country.CountryName
")

head(Answer_3_SQL)

#ANSWER 4; ID codes of the cities which do not match to a country.
#Only R
Answer_4_R <- anti_join(city, Country, by= "Code")
head(Answer_4_R)

#Answer 5; the average area for the Provinces in each country
Province_clean <- Province %>% filter(!grepl("NULL", Area))
Province_clean$Area <- as.numeric(Province_clean$Area)
#In R
A5 <- merge(Province_clean, Country, by = "Code") 
Answer_5_R <- A5 %>%group_by(Code, CountryName) %>%
  summarise(AVG_Area= mean(Area.x))

head(Answer_5_R)

#In SQLDF
Answer_5_SQL <- sqldf("
  SELECT Province_clean.Code, Country.CountryName, AVG(Province_clean.Area) AS AVG_Area
  FROM Province_clean
  INNER JOIN Country ON Province_clean.Code = Country.Code
  GROUP BY Province_clean.Code, Country.CountryName
")

head(Answer_5_SQL)

