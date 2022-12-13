library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
data<- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
View(data)
data_1<- data%>%
  filter( state == "AL", county_name == "Barbour County")%>%
  select(
    year,
    state,
    county_name,
    white_jail_pop,
    total_jail_pop,
    black_jail_pop
      ) 
View(data_1)
#I chose to observe White versus black jailed population in Alabama in the Barbour County. I took the total
#population of prisnors against the white population agaisnt teh black population. For example in 1990, the black 
#imprisoned population is 21.61, almost 100% of the imprisoned population, 27.60, while the white imprisoned population
#is only 2.94.This pattern continues on even in 2007, the black imprisoned population per day (58.75) is over 50% 
#the entire prisoner population (93.00), white the white population is only (36.25). As observed through history, black
# people are  put into jail at an disproportionate rate in comparison to white people. This may be due to a plethora of 
# reasons, but the main one is a blatent racism in out society. 


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>


##data wrangling function
get_year_jail_pop<- function(){
  new_data<- data%>%
    select(year, total_jail_pop)
  return(new_data)
}

get_year_jail_pop()


labels<- labs(
  title = "Increase in Jail Population in the U.S (1970-2018)",
  x = "Year",
  y = "Total Jail Population"
)

## chart function
plot_jail_pop_for_us<- function(){
  bar_chart<- ggplot(data = new_data, mapping = aes(
    x = year, 
    y = total_jail_pop)) +
    geom_col() + labels
  
  return(bar_chart)
}

plot_jail_pop_for_us()



  

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas


get_jail_pop_by_states<- function(){
    state_df<- data%>%
      group_by(state)%>%
      select(year, state, total_jail_pop)%>%
      filter(state == "WA" | state == "AL" | state == "AZ" | state == "OR")
    return(state_df)
  
}

get_jail_pop_by_states()

plot_jail_pop_by_states<- function(){
  line_chart<- ggplot(data = state_df, mapping = aes(x = year, y = total_jail_pop, group = state, color = state )) +
    geom_smooth() +
    labs(
      title = "Growth of Prison Population by state",
      x = "Year",
      y = "Total Jail Population"
    )
  return(line_chart)
}

plot_jail_pop_by_states(states)


states<- data%>%
  select(state)%>%
  filter( state == "WA" | state== "AL" | state == "AZ" | state == "OR")


View(states)


##Summary Paragraph
#I wanted to analyze which state out of the 4 I picked, Washington
# California, Arizona, and Alabama had the largest grown of US prison
# Population from 1970 to 2018. Once I created my graph, I noticed that Arizona 
# had the highest jail population, and increasing at a rate much faster than the 
# other 4 states. 






#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas


black_jail<- function(){
  black_df<- data%>%
    group_by(state)%>%
    select(total_pop_15to64, black_prison_pop_rate, year)%>%
    filter(state == "WA" | state == "GA" | state == "AZ" | state == "OH")
  return(black_df)
  
}

black_jail()

plot_black_jail<- function(){
  scatterplot_chart<- ggplot(data = black_df, mapping = aes(
    x = total_pop_15to64 , y = black_prison_pop_rate , color = state))+
    geom_point() +
    labs(
      title = "Rate of Black People In Prison In Comparison To Total Population",
      x = "Total Population",
      y = "Black Imprsioned Population"
    )
  return(scatterplot_chart)
}

plot_black_jail()


##Summary
#The question I wanted to analyze was in different states, how was the  total imrpisoned population
#being compared to the Black imprsioned population. I wanted to see how different states compared to 
#each other in the amount of Black people imprisoned, from their jailed population. I obsereved 4
#states, Washington, Ohio, Georgia, and Arizona. From observing the graph, we can see that Ohio
#has prisons with the most imprisoned black people in comparison to any other state, and it is the
#least populated state. 



#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 

nativejail<- function(){
  nativedf<- data%>%
    filter(year == 1990)%>%
    select(year, state, native_pop_15to64)
  return(nativedf)
}
nativejail()

nativeplot<- function(){
  map<- ggplot(data = nativedf, mapping = aes( x = long, y = lat, group = group),
               color = "white", size = .1) + coord_map() 
  return(nativeplot)
}

nativeplot()

