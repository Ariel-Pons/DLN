library(tidyverse)

surveys <- read_csv("data/portal_data_joined.csn")

str(surveys)
surveys

View(surveys)
#to subset columns: select(). first argument to select is dataframe, then columns
#to pull out rows: filter() 
#to create new columns based on existing data: mutate()
#summary stats on specific data: group_by %>%summarise()
#to sort columns: arrange()
#number of observations in specific groups: count() [counts discrete values]

select(surveys, plot_id, species_id, weight)
#to not select columns, use -. 
select(surveys,-record_id,-species_id)

filter(surveys,year==1995)

#getting rows and columns (select AND filter): use pipes. 
#but there are some other ways: 
#1. intermediate steps.
surveys2 <- filter(surveys,weight<5)
surveys_sml <- select(surveys2,species_id,sex,weight)
surveys_sml

#2. nested functions. instead of surveys2, do surveys of weight <5.
#exactly like formulae. (R does middlemost brackets first)
surveys_sml <- select(filter(surveys,weight<5),species_id,sex,weight)
surveys_sml

#Pipes. looks like %>% [shortcut shift+ctrl+m]. 
#shoots output from one function to another
surveys %>% filter(weight<5) %>% select(species_id,sex,weight)
#can consider a %>%  as 'then'
#to save the output, assign it to an object
surveys_sml <- surveys %>% filter(weight<5) %>% select(species_id,sex,weight)
surveys_sml


##Challenge: pipes to subset surveys including animals BEFORE 1995.
#only include columns year, sex, weight

surveys_chlg <- select(surveys,year,sex,weight) %>% filter(surveys,year<1995)

#answer:
surveys %>% filter(year<1995) %>% select(year,sex,weight)

#mutate: to create a new column of weight in kg
surveys %>% mutate(weight_kg = weight/1000)

#create multiple columns
surveys %>% mutate(weight_kg = weight/1000, weightkg2 = weight_kg^2)
#to see these: use head
surveys %>% mutate(weight_kg = weight/1000, weightkg2 = weight_kg^2) %>% head
#normally to do head, have to do (). but with pipes, can standalone


#filtering out NAs:
#remember ! = not, and is.na looks for NAs
surveys %>% filter(!is.na(weight)) %>%  mutate(weight_kg = weight/1000) %>% head

#group_by is the column name that you want to calculate summary statistics for.
#summarise is what you want to summarise. have to remove NAs, though

surveys %>% group_by(sex) %>% summarise(mean_weight = mean(weight,na.rm=TRUE))

#Grouping by multiple columns
surveys %>% group_by(sex,species_id) %>% summarise(mean_weight=mean(weight,na.rm=TRUE))

#To see more tibble: print function. replace head command with print.
surveys %>% group_by(sex,species_id) %>% summarise(mean_weight=mean(weight,na.rm=TRUE)) %>% print(n=30)
#there are some NAs visible: these are missing datapoints.
#to get rid of these, one method is to get NAs out at the start
surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight=mean(weight)) %>% print(n=30)

#to arrange, use arrange function.
surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight=mean(weight),min_weight=min(weight)) %>% arrange(min_weight) %>% head

surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight=mean(weight),min_weight=min(weight)) %>% arrange(mean_weight) %>% head

#to sort by descending weight, can't just have range: add function descend

surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight=mean(weight),min_weight=min(weight)) %>% arrange(desc(mean_weight) %>% head

 #counting to work out numbers of rows
 surveys%>%count(sex)
#arrange by descending counts
 surveys %>% count(sex,species) %>% arrange(species,desc(n)) 

 #challenge: how many animals in each plot type, and mean+min+max hindfoot length for each species
 #hint: use group_by and summarise
 
 surveys %>% count(plot_id) %>% filter(!is.na(hindfoot_length))  summarise(mean_l=mean(hindfoot_length),min_l=min(hindfoot_length),max_l=max(hindfoot_length))

 #answer:
 
 surveys %>% count(plot_type)

 surveys %>% filter(!is.na(hindfoot_length)) %>% group_by(species_id) %>% summarise(min_l=min(hindfoot_length),max_l=max(hindfoot_length))

 #filtering out NA values so we can plot data
 surveys_complete <- surveys %>% filter(!is.na(weight),!is.na(hindfoot_length),!is.na(sex))
#object of most common species id
 species_counts <- surveys_complete %>% count(species_id) %>% filter(n>=50)
species_counts 
species_counts %>% head
#only things in surveys complete that are also in most common species

surveys_complete <- surveys_complete %>% filter(species_id%in%species_counts$species_id)
surveys_complete
dim(surveys_complete)















#PLOTTING
#to generate plots, and then modify asthetics.
#learn about universal plot settings
#maybe faceting: display multiple different plots on one big plot
#ggplot is in tidyverse package; already loaded
#ggplot builds graphics step-by-step.
#basic template:

#ggplot(data=<DATA>,mapping=aes(<MAPPINGS>))+<GEOM_FUNCTION>()

ggplot(surveys_complete)
#this gives us gret box
#define axes:
ggplot(surveys_complete,aes(x=weight,y=hindfoot_length))
#decide how to plot data. lines? points? we're going points.
ggplot(surveys_complete,aes(x=weight,y=hindfoot_length))+geom_point()

#options for data display:geom_point() gives scattergraph.
#boxplot = geom_boxplot(). linegraph = geom_line()

#will assign plot to object, and then alter display of that object.

surveys_plot <- ggplot(surveys_complete,aes(x=weight,y=hindfoot_length))
surveys_plot

#using the plus option to determine data display:

surveys_plot+geom_point()
#can go down a line, as long as plus is on end of line
surveys_plot +
  geom_point()
#how to display data with hexagonal bins (colour differs depending on number of observations; gives density)
#we need to install a package for this.
install.packages("hexbin")
library(hexbin)
surveys_plot+geom_hex()
#tweaking further: add transparency
surveys_plot+geom_point(alpha = 0.1)
#add colour
surveys_plot+geom_point(alpha = 0.1, colour="blue")
#colouring by somethign useful, eg species
surveys_plot+geom_point(aes(colour=species_id))

#challenge: create scatterplot of weight/species_Id and colour points by species id

survey_chlg1 <- ggplot(surveys_complete,aes(x=weight,y=species_id))+geom_point(aes(colour=species_id))
#change to boxplot
survey_chlg1 <- ggplot(surveys_complete,aes(x=weight,y=species_id))+geom_boxplot(aes(colour=species_id))
survey_chlg1+geom_jitter(alpha=0.3,colour="tomato")
#want to make boxplots appear over jitter: give boxplots on top
ggplot(surveys_complete,aes(x=weight,y=species_id))+geom_jitter(alpha=0.3,colour="tomato")+geom_boxplot(aes(alpha=0))


#time-series: where line graphs come in handy
#will create new tibble to plot with time data

yearly_counts <- surveys_complete %>% count(year,species_id)
yearly_counts

ggplot(yearly_counts,aes(x=year,y=n))+geom_line()
#not a great plot, bc species not separate: need to modify aes for different line for each species
ggplot(yearly_counts,aes(x=year,y=n,group=species_id))+geom_line()
#better, but now some colour would help
ggplot(yearly_counts,aes(x=year,y=n,colour=species_id))+geom_line()


#plotting sub-graphs: separate plots for each species on same overall plot. command: facet_wrap
#~is used bc means 'do by'
ggplot(yearly_counts,aes(x=year,y=n))+geom_line()+facet_wrap(~species_id)
#add different colours:
ggplot(yearly_counts,aes(x=year,y=n,colour=species_id))+geom_line()+facet_wrap(~species_id)
#splitting data even further, eg by sex: colour by it
ggplot(yearly_counts,aes(x=year,y=n,colour=sex))+geom_line()+facet_wrap(~species_id)
#wait, in yearly counts, 'sex' not captured.
#need to make a new tibble with sex included
yearly_sex_counts <- surveys_complete %>% count(year,species_id,sex)
yearly_sex_counts
ggplot(yearly_sex_counts,aes(x=year,y=n,colour=sex))+geom_line()+facet_wrap(~species_id)
#to alter line width: geom_line(lwd=x)
#tweaking larger-scale aesthetics, eg background colour of plots. use themes
ggplot(yearly_sex_counts,aes(x=year,y=n,colour=sex))+geom_line()+facet_wrap(~species_id)+
  theme_bw()+
  theme(panel.grid=element_blank())

#theme doesn't have to be black and white.
ggplot(yearly_sex_counts,aes(x=year,y=n,colour=sex))+geom_line()+facet_wrap(~species_id)+
  theme_minimal()+
  theme(panel.grid=element_blank())

ggplot(yearly_sex_counts,aes(x=year,y=n,colour=sex))+geom_line()+facet_wrap(~species_id)+
  theme_light()+
  theme(panel.grid=element_blank())
