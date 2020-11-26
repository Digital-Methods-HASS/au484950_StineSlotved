#create new folders
dir.create("data")
dir.create("script")

#download dataset for today 
download.file("https://ndownloader.figshare.com/files/11492171",
              "data/SAFI_clean.csv", mode = "wb")

#to see where my working directory is
getwd()

#start creating some digital objects
area_hectares <- 1.0
area_hectares * 2.7
area_hectares <- 5

area_acres <- area_hectares*2.5 

#Vectors (vektor = en serie af værdier)
hh_members <- c(3,4,1,6,4,5,10)
wall_type <- c("muddaub","burntbricks","sunbricks")
--
length(hh_members)
length(wall_type)


#subsetting vectors
# [] lader os få enkelte elementer ud af et objekt 

str(hh_members)
hh_members[4]
hh_members[5:7]
hh_members[4:7]
wall_type[2]
wall_type
wall_type[c(3,2,1)]
new_wall_type <- wall_type[c(3,3,2,2,1,1,1)]
new_wall_type
new_wall_type[-c(1,2,3)]

# -c fjerner elementer^

#hvis jeg kun vil have elementer, der f.eks. er større end 4 + dem, der er 4:
hh_members[hh_members>4 | hh_members == 4]

#Missing data
rooms <- c(2,1,1,NA,4)
rooms
#mean = gennemsnittet
mean(rooms)
#Vi skal fjerne de manglende værdier, fordi ellers kan R ikke beregne det
mean(rooms, na.rm = TRUE)
#TRUE er ja/bekræftelse
#Maksimum-værdi i rooms
max(rooms, na.rm = TRUE)
#Eliminere missing values while subsetting
#Use functions such as is.na(), na.omit()
# Tilføj et ! for at få alle de værdier, der ikke er NA
rooms[!is.na(rooms)]

#Vi loader tiyverse
library(tidyverse)

?read_csv

#Kommando for at få sættet frem: read_csv(".") - HUSK TAB efter punktum!
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")
view(interviews)

#Exercise 1
rooms <- c(1,2,1,3,1,NA,3,1,3,2,1,NA,1,8,3,1,4,NA,1,3,1,2,1,7,1,NA)
rooms
#Vi skal have fjernet NA'erne, og det gør vi med kommmandoen is.na() og sætter det ind i en ny fil med <- 
rooms <- rooms[!is.na(rooms)]
#hvis jeg kun vil have elementer, der f.eks. er større end 2
rooms[rooms>2.]
str(rooms[rooms>2])

#Exercise 2
mean(rooms)
round(2.318182)

#Exercise 3
class(rooms)

#Torsdag d. 26. november
#Hver gang, du åbner RStudio for at arbejde med materialet, skal du gøre "library" kommandoen
library(tidyverse)
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")
view(interviews)

#na = "NULL" => hvordan vil du have dine missings, her skal de være 0 
#head = se de første 6 kolonner, til inspecering
head(interviews)
dim(interviews)
nrow(interviews)
ncol(interviews)
#De sidste 6 linjer 
tail(interviews)

names(interviews)
str(interviews)
summary(interviews)

#Subset this data frame / tibble
#vi skal give referencer til både kolonne og række 
interviews[5]
interviews[,5] #giver kolonnen
interviews[5,5] #giver kolonne og række

#Hvis vi vil have de første 6 rækker
interviews[1:6,] #samme resultat som head(interviews)

#Hvis jeg vil have alt andet end første række
i130 <- interviews[-1 , ]
interviews[ , ]
#Danne ny vektor med egne valgte rækker
subset <- interviews[c(2,4,6,8,10),1:5]
str(subset)

#2 square brackets gør data til vektor 
interviews[["years_liv"]]
mean(interviews[["years_liv"]])
mean(interviews$years_liv)


#interviews$ viser alle forskellige kolonne-navne 
interviews$village
interviews$village
interviews["village"]

#Subsetting dataframes
i_last <- interviews[nrow(interviews),] #meget mere robust
interviews[131,] #mindre robust 
str(i_last)

#Få fat i den midterste række
n_rows <- nrow(interviews)
interviews[median(1:n_rows),] #den mest robuste
#Andre metoder, ikke nær så gode 
1:n_rows
median(1:n_rows)

#Data wrangling with dplyr 

interviews

#select: vælger kolonnen "village" fra interviews
select(interviews,village)
select(interviews,village,no_membrs,years_liv)
filter(interviews,village =="God")

#Pipes lader dig tage et objekt, arbejde med det
#En pipe ser således ud: %>%, Command Shift M on Macs
#Pipes will take object interviews and filter only the rows where village is God.
interviews %>% 
filter(village=="God") %>% 
select(no_membrs,years_liv)

#exercise
#Using pipes, subset the interviews data to include interviews
#where respondents were members of an irrigation association(memb_assoc)
#and retain only the columns affect_conflicts, liv_count, and no_meals.

interviews %>% 
  filter(memb_assoc=="yes") %>% 
  select(affect_conflicts,liv_count,no_meals)


#Mutate, create a new column
interviews %>% 
  mutate(people_per_room=no_membrs/rooms) %>% 
  select(people_per_room)

#vil fjerne NAs/missing values - vi filtrerer 
interviews %>% 
  filter(!is.na(memb_assoc)) %>%
  mutate(people_per_room=no_membrs/rooms) %>% 
  select(people_per_room, village)

#Summarize
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village,memb_assoc) %>% 
  summarize(mean_no_membrs=mean(no_membrs)) %>% 


#Why use pipes?
#and if you use filter on its own and not in a pipe
  #it requires the following arguments
  #filter(your_dataset, your_dataset$your_column == "your value")
  #using pipes, you skip the need to define your dataset
  #on every single command and you can combine different commands together
  #filter allows you to filter only the rows that fulfil the condition,
  #in our case where the village name == "God"

#Summarize
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village,memb_assoc) %>% 
  summarize(mean_no_membrs=mean(no_membrs),
            min_no_membrs=min(no_membrs),
            max_no_members=max(no_membrs)) %>%
  ungroup()

#du kan undlade ungroup(), men fjern da den sidste pipe:
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village,memb_assoc) %>% 
  summarize(mean_no_membrs=mean(no_membrs),
            min_no_membrs=min(no_membrs),
            max_no_members=max(no_membrs)) 

#Arranger, så det ikke er tilfældig rækkefælge
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village,memb_assoc) %>% 
  summarize(mean_no_membrs=mean(no_membrs),
            min_no_membrs=min(no_membrs),
            max_no_members=max(no_membrs)) %>%
  arrange(desc(min_no_membrs)) #stigende med desc

#count
interviews %>% 
  count(village)

interviews %>% 
  group_by(village) %>% 
  count(no_meals)




#reshaping data, se tutorial => kan være brugbart for mig 





#Exporting data 
interviews_plotting <- interviews %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical, 
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food, 
              values_from = months_lack_food_logical, 
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

which(is.na(interviews$items_owned))

#Gemme
write_csv(interviews_plotting,
          file="output_data/interviews_plot.csv")

#plotting with ggplot



#Exercises for week 5
install.packages("gganimate")

install.packages("gapminder")

library(tidyverse)
library(gganimate)
library(gapminder)
library(gifski)
library(av)

install.packages("gifski")
install.packages("av")

library(tidyverse)
library(gganimate)
library(gapminder)
library(gifski)
library(av)

unique(gapminder$year)

theme_set(theme_bw())  # set theme to white background for better visibility
ggplot(subset(gapminder, year == 1952), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() 

#  Explore the recent global developments with R
Today, you will load a filtered gapminder dataset -  with a subset of data on global development from 1952 - 2007 in increments of 5 years - to capture the period between the Second World War and the Global Financial Crisis. 

**Your task: Explore the data and visualise it in both static and animated ways, providing answers and solutions to 7 questions/tasks below.**
  
  ## Get the necessary packages
  First, start with installing the relevant packages 'tidyverse', 'gganimate', and 'gapminder'.

```{r libraries, echo = FALSE}
library(tidyverse)
library(gganimate)
library(gapminder)
```

## Look at the data
First, see which specific years are actually represented in the dataset and what variables are being recorded for each country. Note that when you run the cell below, Rmarkdown will give you two results - one for each line - that you can flip between.
```{r}
unique(gapminder$year)
head(gapminder)
```
The dataset contains information on each country in the sampled year, its continent, life expectancy, population, and GDP per capita.

Lets plot all the countries in 1952.
```{r 1957}
theme_set(theme_bw())  # set theme to white background for better visibility
ggplot(subset(gapminder, year == 1952), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() 
```
We see an interesting spread with an outlier to the right. Answer the following questions, please:

#Q1. Why does it make sense to have a log10 scale on x axis?
#A1: The results are very big, so instead of a loong x-line, it is instead useful to use log. The lifeExpectancy is numeric small compared to the GDP, so by using the log, we get a more digestable result.  

#Q2. What country is the richest in 1952 (far right on x axis)? 
#A2: So I want to have the top countries on the variable GDP pr. capita see, which country lies the highest.
  #To do so, I group the data with country and GDP pr. capita 
  #And the result: It turns out, Kuwait is the richest country in 1952. 
  #Note: GDP pr. capita is a flawed variable to use to measure a country's wealth, but none the less it is the most useful in this dataset.

  ubset(gapminder,year==1952)%>% 
  group_by(country,gdpPercap) %>% 
  summarize(min_gdpPercap=min(gdpPercap),
            max_gdpPercap=max(gdpPercap)) %>%
  arrange(desc(max_gdpPercap)) #stigende med desc



#You can generate a similar plot for 2007 and compare the differences

ggplot(subset(gapminder, year == 2007), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() 

ggplot(subset(gapminder, year == 2007), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point(alpha = 0.5, aes(color = continent)) +
  scale_x_log10()

#The black bubbles are a bit hard to read, the comparison would be easier with a bit more visual differentiation.
#Q3. Can you differentiate the continents by color and fix the axis labels?
#A3: By inserting color = continent, it simplý adds color: 

ggplot(subset(gapminder, year == 2007), aes(gdpPercap, lifeExp, size = pop)) +
  geom_point(alpha = 0.5, aes(color = continent)) +
  scale_x_log10()


#Q4. What are the five richest countries in the world in 2007?
#A4: Using the same commands as in exercise 2, I just change the year and get the results to be Norway as the richest, followed by Kuwait, Singapore, USA and Ireland.
subset(gapminder,year==2007)%>% 
  group_by(country,gdpPercap) %>% 
  summarize(min_gdpPercap=min(gdpPercap),
            max_gdpPercap=max(gdpPercap)) %>%
  arrange(desc(max_gdpPercap)) #stigende med desc


##Make it move!

The comparison would be easier if we had the two graphs together, animated. We have a lovely tool in R to do this: the `gganimate` package. And there are two ways of animating the gapminder ggplot.

### Option 1: Animate using transition_states() 
#The first step is to create the object-to-be-animated
```{r anim1 start}
anim <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10()  # convert x to log scale
anim
```

#This plot collates all the points across time. The next step is to split it into years and animate it. This may take some time, depending on the processing power of your computer (and other things you are asking it to do). Beware that the animation might appear in the 'Viewer' pane, not in this rmd preview. You need to knit the document to get the viz inside an html file.

```{r anim1}
anim + transition_states(year, 
                      transition_length = 1,
                      state_length = 1)
```
#Notice how the animation moves jerkily, 'jumping' from one year to the next 12 times in total. This is a bit clunky, which is why it's good we have another option. 


### Option 2 Animate using transition_time()
#This option smoothes the transition between different 'frames', because it interpolates and adds transitional years where there are gaps in the timeseries data.

```{r anim2}
anim2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() + # convert x to log scale
  transition_time(year)
anim2
```

#The much smoother movement in Option 2 will be much more noticeable if you add a title to the chart, that will page through the years corresponding to each frame.


#Q5 Can you add a title to one or both of the animations above that will change 
#in sync with the animation? [hint: search labeling for transition_states() and transition_time() functions respectively]


#Q6 Can you made the axes labels and units more readable? Consider expanding the abreviated lables as well as the scientific notation in the legend and x axis to whole numbers.[hint:search disabling scientific notation]



#Q7 Come up with a question you want to answer using the gapminder data and write it down. Then, create a data visualisation that answers the question and explain how your visualization answers the question. (Example: you wish to see what was mean life expectancy across the continents in the year you were born versus your parents' birth years). [hint: if you wish to have more data than is in the filtered gapminder, you can load either the `gapminder_unfiltered` dataset and download more at https://www.gapminder.org/data/ ]





