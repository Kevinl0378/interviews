install.packages("tidyverse")
install.packages("magrittr")
install.packages("dotenv")
install.packges("httr")
install.packges("jsonlite")

library(tidyverse)
library(magrittr)
library(dotenv)
library(httr)
library(jsonlite)

load_dot_env()     #This method of hiding the API key was found at https://towardsdatascience.com/using-dotenv-to-hide-sensitive-information-in-r-8b878fa72020. "load_dot_env()" loads the .env file. 
key <- Sys.getenv("Name_of_api_key_env_variable")     #Obtains the value of the environment variable. Stores the API key in a variable named "key".

get_fred_data <- function(id) {     #Defines a function named "get_fred_data" that takes in the series id as its input parameter 
  url <- paste("https://api.stlouisfed.org/fred/series/observations?series_id=", id, "&api_key=", key, "&file_type=json", sep="")     #Concatenates the elements/parameters of the URL to the API. "sep=''" is used to get rid of spaces. 
  response <- GET(url)     #Guidance was provided by https://www.dataquest.io/blog/r-api-tutorial/. The GET request is used to retrieve data from the concatenated API URL. 
  json_text <- content(response, "text")     #Content() tries to determine which output type is the best. The "text" argument tells it to output plain text. The resulting plain text is in JSON format.
  df <- as.data.frame(fromJSON(json_text))     #fromJSON() converts objects from JSON format. as.data.frame() converts objects into a dataframe. 
  return(df)  
}

payems <- get_fred_data("PAYEMS")     #The "get_fred_data" function is called on the series id for the data on Quarterly Total Nonfarm Employment
payems_final <- payems %>% 
  select(observations.date, observations.value) %>%     #The "observations.date" and "observations.value" columns are selected
    mutate(observations.date = strptime(observations.date, format = "%Y-%m-%d")) %>%     #The "observations.date column is converted into a time object
      filter(observations.date >= "2000-01-01" & observations.date <= "2020-12-31") %>%     #Data that is not from 2000 through 2020 is filtered out
        rename(Date = observations.date, Nonfarm_Employment = observations.value)     #The two columns are renamed

gdp <- get_fred_data("GDPC1") #The "get_fred_data" function is called on the series id for the data on Quarterly Real Gross Domestic Product.
gdp_final <- gdp %>% 
  select(observations.date, observations.value) %>% 
   mutate(observations.date = strptime(observations.date, format = "%Y-%m-%d")) %>% 
    filter(observations.date >= "2000-01-01" & observations.date <= "2020-12-31") %>% 
      rename(Date = observations.date, Real_GDP = observations.value)

cpi <- get_fred_data("CPIAUCSL") #The "get_fred_data" function is called on the series id for the data on Quarterly Consumer Price Index.
cpi_final <- cpi %>%
  select(observations.date, observations.value) %>% 
    mutate(observations.date = strptime(observations.date, format = "%Y-%m-%d")) %>% 
    filter(observations.date >= "2000-01-01" & observations.date <= "2020-12-31") %>% 
      rename(Date = observations.date, CPI = observations.value)

FRED_data <- payems_final %>% 
  inner_join(gdp_final, by = "Date") %>%     #The dataframe on Nonfarm Employment is inner joined to the dataframe on Real GDP. Only rows that meet the matching condition are joined.
    inner_join(cpi_final, by = "Date") %>%     #The dataframe on CPI is inner joined to the dataframe that was just created by the inner join on the previous line.
      mutate(Nonfarm_Employment = as.numeric(Nonfarm_Employment), Real_GDP = as.numeric(Real_GDP), CPI = as.numeric(CPI)) #The columns in the combined dataframe are converted into numbers so that they can be graphed



FRED_data_cpi_gdp_long <- FRED_data %>%
  pivot_longer(     
    cols = c(Real_GDP, CPI),     #The pivot_longer function takes the Real_GDP and CPI columns and combines them into one longer column. This format helps with graphing. 
    names_to = "column",
    values_to = "value"
  )
FRED_data_cpi_gdp_long %>%
  ggplot(aes(x = Date, y = value, color = column)) +     #ggplot is used to graph the data. The x and y axes are defined. 
  geom_line() +     #geom_line generates a line plot 
  labs(     #Responsible for adding a title and axes labels
    title = "CPI and Real GDP vs. Time", 
    x = "Time",
    y = ""
  ) +
  scale_color_manual(     #Responsible for creating a key and giving the lines different colors
    name = "Key",
    values = c("red", "blue"),
    labels = c("Consumer Price Index", "Real GDP")
  )


FRED_data %>% 
  ggplot(aes(x = CPI, y = Nonfarm_Employment)) +
  geom_point() +     #geom_point generates a scatter plot
  labs(
    title = "Total Nonfarm Employment vs. CPI ",
    x = "Consumer Price Index",
    y = "Total Nonfarm Employment"
  )


FRED_data %>%
  ggplot(aes(x = Real_GDP)) + 
  geom_histogram(bins = 7, col=("blue")) +     #geom_histogram generates a histogram. Sturges's rule was used to determine the number of intervals (bins)
  labs(
    title = "Histogram of Real GDP",
    x = "Real GDP",
    y = "Frequency"
  ) 

FRED_data %>%
  ggplot(aes(x = Real_GDP, y = Nonfarm_Employment)) + 
  geom_point() +
  labs(
    title = "Total Nonfarm Employment vs Real GDP", 
    x = "Real GDP", 
    y = "Total Nonfarm Employment"
  )

  