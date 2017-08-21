weight_grams <- 22
weight_in_kg <- weight_grams * 1000 
logical_variable <- FALSE
june <- "june"

logical_var2 <- weight_grams < weight_in_kg
logical_var2 <- weight_grams > weight_in_kg
logical_var2 <- weight_grams == weight_in_kg
sin(weight_grams)

# value is 22
# variable is weight
# function 
ls()
ls







# http://pad.software-carpentry.org/2017-08-18-UFII-R

typeof(weight_grams)
typeof(logical_var2)
integer1 <- 1L

install.packages("gapminder")
library(gapminder)
?as.numeric
is.numeric(weight_grams)

#copy from the etherpad to a new file in the text editor and save as .csv


cats <- read.csv("inputs/cats.csv")
typeof(cats[,2])
typeof(cats$coat)
str(cats$coat)

this_is_a_vector <- levels(cats$coat)
this_is_the_new_order <- c("very good", "good", "ok", "bad")
factor_new_rder <- factor(this_is_the_new_order)
factor_new_rder <- factor(this_is_the_new_order, levels = c("very good", "good", "ok", "bad"))

factor_new_rder <- factor(this_is_the_new_order, levels =this_is_the_new_order )



download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv",               destfile = "inputs/data.csv")

dat <- read.csv("inputs/data.csv")
head(dat)

# coercion rule: logicals > integers > numeric > complex > characters

subset <- dat[(dat$year < 2005) & (dat$year > 2000), ]
subset <- dat[(dat$year < 2005) & (dat$year > 2000), 2]
subset <- dat[(dat$year < 2005) & (dat$year > 2000), colnames(dat) == "year"]


if(logical_variable == TRUE){
  print("it is true")
}

x <- rpois(1, lambda = 11)
if(x < 11){
  print("less than 11")
} else if(x > 11){
  print("more than 11")
} else {
  print("Sergio, don't make jokes!")
}
x <- rpois(10, lambda = 11)
if(any(x < 11)){
  print("less than 11")
} else if(any(x > 11)){
  print("more than 11")
} else {
  print("Sergio, don't make jokes!")
}
if(all(x < 11)){
  print("less than 11")
} else if(all(x > 11)){
  print("more than 11")
} else {
  print("Sergio, don't make jokes!")
}


for(jj in 1:10 ){
  print(paste("round", jj))
  for(ii in c("a", "b", "c")){
    print(ii)
  }
}

install.packages(("tidyverse"))
#install.packages("ggplot2")

library(ggplot2)
ggplot(data = dat, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggplot(data = dat, aes(x = continent, y = lifeExp)) +
  geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.3, color = dat$year )

ggplot(data = dat, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method="lm", size = 1.5)

start_with <- substr(dat$country, start = 1, stop = 1)
az.countries <- dat[start_with %in% c("A", "Z"),]

ggplot(data = az.countries, aes(x = year, y = lifeExp, color = continent)) + 
  geom_line() + 
  facet_wrap( ~ country)

