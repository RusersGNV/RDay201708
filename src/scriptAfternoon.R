# Examples of vectors
1:3
length(1:3)
2:7

# Mathematical operation using vectors
2:7 * 3

x <- 2:7 * 3
y <- c(4, 2, 6, 7, 8, 10)
y

x+y
x * y

gapminder$pop[1:6]
(gapminder$pop + 100)[1:6]

gapminder$pop_million <- gapminder$pop / 1000000
head(gapminder)
head(gapminder[4:7])

# Logical operationa and applying function on vectors
y
y > 6
log(1)
log(y)

z <- y > 6
table(z)

# Sum of series 1/(k^2), k = 1, 2, ..., n
n <- 100
y <- 1:n
y
sum(1/y^2)

n <- 10000
y <- 1:n
sum(1/y^2)

# Functions
sum
mean

# Converting Fahrenheit to Celsius, the "calculator" way
(80 - 32) * 5 / 9
(70 - 32) * 5 / 9

# Converting Fahrenheit to Celsius, the "functions" way
fahr_to_celsius <- function (fahr) {
  celsius <- (fahr -32) * 5 / 9
  celsius
}

# You can call the "argument" variable of a function whatever
# name you want, e.g. abc
fahr_to_celsius <- function (abc) {
  celsius <- (abc -32) * 5 / 9
  celsius
}

fahr_to_celsius(fahr = 80)
fahr_to_celsius(fahr = 32)

fahr_to_celsius(32)

# Write a function to convert celsius to kelvin
# Kelvin = Celsius + 273.15
# cels_to_kelvin(0) = 273.15
# cels_to_kelvin(100) = 373.15
cels_to_kelvin <- function (cels) {
  kelvin = cels + 273.15
  return(kelvin)
}

cels_to_kelvin(0)

# Write a function to convert fahrenheit to kelvin
# USING the two functions we just created:
# fahr_to_celsius and cels_to_kelvin
# fahr_to_kelvin(32) = 273.15
# fahr_to_kelvin(212) = 373.15
fahr_to_kelvin <- function (fahr) {
  celsius <- fahr_to_celsius(fahr)
  kelvin <- cels_to_kelvin(celsius)
  return(kelvin)
}

fahr_to_kelvin(32)

# Using functions in the nested way
cels_to_kelvin(fahr_to_celsius(32))

# Take vector into our custom function, receive output as a vector too
fahr_to_celsius(c(32, 42, 53, 67, 80))

# Taking dataframe as a function argument
calcGDP <- function (dat) {
  GDP <- dat$gdpPercap * dat$pop
  return(GDP)
}

head(calcGDP(gapminder))

calcGDP(dat = gapminder)

# Take dataframe, calculate GDP for the year and country 
# specified by the user
# Note how we set the default year and country argument here
# as NULL
calcGDP <- function (dat, year=NULL, country=NULL) {
  if (!is.null(year)) {
    dat <- dat[dat$year %in% year,]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  
  GDP <- dat$pop * dat$gdpPercap
  newdat <- cbind(dat, GDP)
  return(newdat)
}

calcGDP(dat = gapminder, year = 1957)
calcGDP(dat = gapminder, country = "Malaysia")
calcGDP(dat = gapminder, country = "Malaysia", year = 1957)

##### Packages for data manipulation
# Now what if I want to know the mean country GDP for
# each continent in 2007?

# The tedious way
withGDP2007 <- calcGDP(dat = gapminder, year = 2007)
GDPAfrica <- withGDP2007[withGDP2007$continent == "Africa", 
                         "GDP"]
mean(GDPAfrica)

GDPAmericas <- withGDP2007[withGDP2007$continent == "Americas", 
                         "GDP"]
mean(GDPAmericas)

# Ideally we want to have a table with 5 rows, 2 columns
# column will be Continent and MeanGDP

# This is a Split - Apply - Combine problem
# Using package "plyr", we can do this easily!
install.packages("plyr")
library(plyr)

# xxply(.data, .variables, .fun)
meanGDP <- function (dat) {
  gdp <- mean(dat$GDP)
  names(gdp) <- "GDP"
  return(gdp)
}

# Split according to continent, apply "meanGDP" function, 
# get a dataframe as return
ddply(
  .data = withGDP2007,
  .variables = "continent",
  .fun = meanGDP
)

# Same but note that we can specify the function in such way
ddply(
  .data = withGDP2007,
  .variables = "continent",
  .fun = function (dat) return(mean(dat$GDP))
)

# We can split according to more than 1 variable
withGDP <- calcGDP(gapminder)

ddply(
  .data = withGDP,
  .variables = c("continent", "year"),
  .fun = function (dat) return(mean(dat$GDP))
)

# dataframe in, list out
dlply(
  .data = withGDP2007,
  .variables = "continent",
  .fun = meanGDP
)

# dataframe in, nothing out? An example
meanGDP.print <- function (dat) {
  gdp <- mean(dat$GDP)
  continent <- dat$continent[1]
  shout <- paste("The mean GDP for", continent, "is", gdp)
  print(shout)
}

d_ply(
  .data = withGDP2007,
  .variables = "continent",
  .fun = meanGDP.print
)

# Mean Life Expectancy
meanLifeExp <- function (dat) {
  meanLE <- mean(dat$lifeExp)
  return(meanLE)
}

lifeExp <- ddply(
  .data = gapminder,
  .variables = c("continent", "year"),
  .fun = meanLifeExp
)

# Comparing 2007 and 1957
lifeExp[lifeExp$year == "2007",]$V1 - 
  lifeExp[lifeExp$year == "1957",]$V1
lifeExp[lifeExp$year == "1957",]

##### dplyr!!!!
install.packages("dplyr")
library(dplyr)
# Key functions to introduce
# select()
# filter()
# group_by()
# summarize()
# mutate()

# Select the country, year and gdpPerCap columns
head(select(gapminder, country, year, gdpPercap))

# selecting rows? filter()
filter(gapminder, country == "Malaysia")

sum(is.na(gapminder)) # Find out how many missing values in a dataframe

# let's talk about pipe %>%
1:3 %>% mean() %>% sqrt()
mean(1:3)
sqrt(2)
# sqrt(mean(1:3)) works too but piping make your code more readable

# Both of these are the same, one with pipe, one without
gapminder %>% select(country, year, gdpPercap)
select(gapminder, country, year, gdpPercap)

# Show the country, year, and gdpPercap of India using filter()
# and select()
df <- select(gapminder, country, year, gdpPercap)
df1 <- filter(df, country == "India")

# Use Shift+Ctrl(CMD)+M to automatically get a pipe!

# Notice how we can use pipe to reduce the usage of "transitional"
# variables (e.g. df, df1 vs only df).
df <- gapminder %>% select(country, year, gdpPercap) %>%
  filter(country == "India")

gapminder %>% filter(continent == "Africa") %>%
  select(country, year, lifeExp)

# summarize can aggregate the variables we want
gapminder %>% summarize(meanLifeExp = mean(lifeExp))

# but without group_by, dplyr aggregate the whole table!
# Using group_by, we can aggregate and get the mean value
# per each country
gapminder %>% group_by(country) %>% 
  summarize(meanLifeExp = mean(lifeExp))

# per continent per year
gapminder %>% group_by(continent, year) %>% 
  summarize(meanLifeExp = mean(lifeExp))

# mutate creates new column
gapminder %>% mutate(pop_billion = pop / 1000000000) %>%
  select(country, year, pop, pop_billion)

# An example of mixing the dplyr functions and putting them
# together by pipe, imagine creating df1, df2, df3 just to
# get this result without pipe!
gapminder %>% mutate(GDP = gdpPercap * pop) %>%
  filter(GDP > 5000000000) %>%
  group_by(country) %>%
  summarise(count = n())
