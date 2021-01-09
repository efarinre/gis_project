#librarypackages 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)
library(stringr)
library(grid)
library(hrbrthemes)
library(viridis)
library(reshape2)
library("shinyjs")
library(spdep)
library(StepReg)
library(lmtest)
library(spatialreg)
library(kableExtra)
library(magick)
library(webshot)



UK_LA_dists <- st_read("Local_Authority_Districts__May_2020__Boundaries_UK_BFE.shp")

#check the data
#qtm(UK_LA_dists)


#checking what epsg the data is in #27700
LDN_LAs <- UK_LA_dists %>%
  dplyr::filter(str_detect(lad20cd, "^E09"))

#qtm(LDN_LAs)
#summary(LDN_LAs) 



#read in some attribute data
LdnEarnings19 <- read_csv(("2019_ukhourlywages_mean_median_all.csv"),
                          col_names = TRUE,
                          locale = locale(encoding = 'Latin1'))%>%
  dplyr::filter(str_detect(Code, "^E09"))

#check all of the columns have been read in correctly
Datatypelist <- LdnEarnings19 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

#Datatypelist


#Cleaning the data as I read it in
#We can use readr to deal with the issues in this dataset - which are to do with text values being stored in columns containing numeric values
#read in some data - couple of things here. Read in specifying a load of likely 'n/a' values.
LdnEarnings19 <- read_csv(("2019_ukhourlywages_mean_median_all.csv"), 
                          na = c("", "NA", "n/a", "#", "-", "!", "x", ".."), 
                          locale = locale(encoding = 'Latin1'), 
                          col_names = TRUE)%>%
  dplyr::filter(str_detect(Code, "^E09"))


#check all of the columns have been read in correctly
Datatypelist <- LdnEarnings19 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Datatypelist



#merge boundaries and data
LDN_LA_Earnings19 <- left_join(LDN_LAs,
                               LdnEarnings19, 
                               by = c("lad20cd" = "Code"))


#Additional data
#country of birth of population
country_of_birth19 <- read_csv(("2019populationbycountryofbirthandnationality16_64.csv"),
                               na = c("", "NA", "n/a", "#", ".", "c", ":", "0~"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

# I already deleted all the non eu countries of birth from the data on excel so rename the name here
EU_born19 <- country_of_birth19 %>%
  clean_names()


#focus on London to begin with
EU_born_LDN19 <- EU_born19 %>%
  dplyr::filter(str_detect(area_code, "^E09"))


#creating a new column for the proportion of EU born population
EU_born_LDN_proportions19 <- EU_born_LDN19 %>% 
  
  #new column with proportion of all EU migrants
  mutate(eu_prop = (european_union / 
                      total)*100) %>%
  
  #new column with proportion of EU14 migrants
  mutate(eu14_prop = (european_union_eu14 / 
                        total)*100) %>%
  
  #new column with proportion of EU8 migrants
  mutate(eu8_prop = (european_union_eu8 / 
                       total)*100) %>%
  
  #new column with proportion of EU2 migrants
  mutate(eu2_prop = (european_union_eu2 / 
                       total)*100) %>%
  
  #select only the proportion columns
  dplyr::select(area_code, area_name, 
                eu_prop,
                eu14_prop,
                eu8_prop, 
                eu2_prop)


#merge earnings/boundary data with the EU data
LDN_LA_Earnings_EUProp19 <- left_join(LDN_LA_Earnings19,
                                      EU_born_LDN_proportions19, 
                                      by = c("lad20cd" = "area_code"))

#view(LDN_LA_Earnings_EUProp19) 









####PART 1
####UNDERSTANDING THE DATA

# mapping the dependent variable(s) to see if the join has worked:
tmap_mode("view")
qtm(LDN_LA_Earnings_EUProp19, 
    fill = "eu_prop", 
    borders = NULL,  
    fill.palette = "Blues")


qtm(LDN_LA_Earnings_EUProp19, 
    fill = "eu14_prop", 
    borders = NULL,  
    fill.palette = "Blues")


qtm(LDN_LA_Earnings_EUProp19, 
    fill = "eu8_prop", 
    borders = NULL,  
    fill.palette = "Blues")

qtm(LDN_LA_Earnings_EUProp19, 
    fill = "eu2_prop", 
    borders = NULL,  
    fill.palette = "Blues")





########################### MAKING BETTER MAPS 22/12/2020

tmap_mode("plot")
# set the breaks
# for our mapped data
breaks = c(0, 5, 10, 15, 20, 25) 


EU <- tm_shape(LDN_LA_Earnings_EUProp19) + 
  tm_polygons("eu_prop",
              title = "EU Proportion",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU

#Export the map
tmap_save(EU, 'euproportions.png')



EU14 <- tm_shape(LDN_LA_Earnings_EUProp19) + 
  tm_polygons("eu14_prop",
              title = "EU14 Proportion",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU14

#Export the map
tmap_save(EU14, 'eu14proportions.png')



EU8 <- tm_shape(LDN_LA_Earnings_EUProp19) + 
  tm_polygons("eu8_prop",
              title = "EU8 Proportion",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU8

#Export the map
tmap_save(EU8, 'eu8proportions.png')



EU2 <- tm_shape(LDN_LA_Earnings_EUProp19) + 
  tm_polygons("eu2_prop",
              title = "EU2 Proportion",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU2

#Export the map
tmap_save(EU2, 'eu2proportions.png')



LDN_LA_Earnings_EUProp19 <- LDN_LA_Earnings_EUProp19 %>%
  clean_names()
#view(LDN_LA_Earnings_EUProp19) 


#LETS CHECK THE DISTRIBUTION OF THE DEPENDENT VARIABLE
#eu_prop
ggplot(LDN_LA_Earnings_EUProp19, aes(x=eu_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu_14
ggplot(LDN_LA_Earnings_EUProp19, aes(x=eu14_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu8
ggplot(LDN_LA_Earnings_EUProp19, aes(x=eu8_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu2
ggplot(LDN_LA_Earnings_EUProp19, aes(x=eu2_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)




#using code from stackoverflow to get my data into the correct format for plotting nicer boxplots/violin plots
#url for page that helped with the data formatting: https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
boxplotdf <- EU_born_LDN_proportions19 %>%
  dplyr::select(area_name, 
                eu_prop,
                eu14_prop,
                eu8_prop, 
                eu2_prop)

boxplotdf.m <- melt(boxplotdf, id.var = "area_name")
#show the new format of the data 
boxplotdf.m

#standard
ggplot(data = boxplotdf.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))



# 'Fancier' Plots
ggplot(data = boxplotdf.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplots of the different EU country groupings, 
  showing the proportions across each London Borough") +
  xlab("")


# Boxplot basic
ggplot(data = boxplotdf.m, aes(x=variable, y=value, fill=variable)) +  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")


# Violin basic
ggplot(data = boxplotdf.m, aes(x=variable, y=value, fill=variable)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")



#tmaptools::palette_explorer()





#PART 2
####SPATIAL DEPENDENCIES####


#have a look to check that it's 
#in the right projection
st_crs(LDN_LA_Earnings_EUProp19)
#all good to go



#Before being able to calculate Moran's I and any similar statistics, we need to first define a spatial weights matrix


#THE MORAN'S I TEST will not work because City of London has NA values for each EU classification 
#Time to remove City of London from the data
LDN_32LA_Earnings_EUProp19 <- LDN_LA_Earnings_EUProp19[-c(1),] # '1'is th row number for City of London


#First calculate the centroids of all Boroughs in London
coordsB <- LDN_32LA_Earnings_EUProp19 %>%
  st_centroid()%>%
  st_geometry()

plot(coordsB,axes=TRUE, ylim= c(155000, 205000))




#Now we need to generate a spatial weights matrix. 
#Starting with a simple binary matrix of queen's case neighbours (aka Contiguity edges corners). 
#This method means that polygons with a shared edge or a corner will be included in computations for the target polygon.

#create a neighbours list
LBorough_nb <- LDN_32LA_Earnings_EUProp19 %>%
  poly2nb(., queen=T)


#plot them
plot(LBorough_nb, st_geometry(coordsB), col="red", axes=TRUE, ylim= c(155000, 205000))
#add a map underneath
plot(LDN_32LA_Earnings_EUProp19$geometry, axes = TRUE, add=T)


#create a spatial weights object from these weights
LBorough.bw <- LBorough_nb %>%
  nb2listw(., style="C")

head(LBorough.bw$neighbours)


#for eu_prop
I_LBorough_Global_EUprop <- LDN_32LA_Earnings_EUProp19 %>%
  pull(eu_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EUprop  # 0.35322686


#for eu14_prop
I_LBorough_Global_EU14prop <- LDN_32LA_Earnings_EUProp19 %>%
  pull(eu14_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EU14prop  # 0.42897042


#for eu8_prop
I_LBorough_Global_EU8prop <- LDN_32LA_Earnings_EUProp19 %>%
  pull(eu8_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EU8prop  # 0.12585839

### BE SURE TO TRY OUT OTHER SPATIAL WEIGHTS MATRICES




### PART 3 ###
# LOCALISED STATISTICS


#We can now also calculate local versions of the Moran's I statistic (for each Borough)
#use the localmoran function to generate I for each borough in the city

#I can't use a local moran's I statistic because my data for each borough is an average, and doesn't provide points across the borough







#### PART 4 ####
###### OLS REGRESSIONS 


#################################### EU14 ##################

# clean names again
LDN_LA_Earnings_EUProp19 <- LDN_LA_Earnings_EUProp19 %>%
  clean_names()
#view(LDN_LA_Earnings_EUProp19) 


#REGRESSION BASICS
q <- qplot(x = hourly_median, 
           y = eu14_prop, 
           data = LDN_LA_Earnings_EUProp19)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method = "lm", se= FALSE, size=1) + 
  geom_jitter()


#Now run a regression model
#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LDN_LA_Earnings_EUProp19%>%
  dplyr::select(eu14_prop, hourly_median)

#now model
model1 <- lm(eu14_prop ~
               hourly_median, 
             data = Regressiondata)

#show the summary of those outputs
tidy(model1)
glance(model1) # r.sqr = 0.316, p-value = 8.10e-10



######add some unemployment data so that you can run a multiple regression

#stick to London again to start with
#add UK Unemployment data across local authorities

#read in some attribute data
LdnUnemployment19 <- read_csv(("2018-2019_ukunemployment.csv"),
                              col_names = TRUE,
                              locale = locale(encoding = 'Latin1')) %>%
  dplyr::filter(str_detect(Code, "^E09"))

#check all of the columns have been read in correctly
Data2typelist <- LdnUnemployment19 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Data2typelist


#Cleaning the data as I read it in
LdnUnemployment19 <- read_csv(("2018-2019_ukunemployment.csv"),
                              na = c("", "NA", "n/a", "#", "-", "!", "x", ".."), 
                              locale = locale(encoding = 'Latin1'), 
                              col_names = TRUE) %>%
  dplyr::filter(str_detect(Code, "^E09"))


#check all of the columns have been read in correctly
Data2typelist <- LdnUnemployment19 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Data2typelist


#select only the unemployment column from the data
LdnUnemployment19 <- LdnUnemployment19 %>%
  dplyr::select('Code', 'Description', 'Unemployment rate 16-64', 'Claimant Count proportion')

#view(LdnUnemployment19)



#merge with previous data containing all the other variables and boundaries data
LDN_LA_Earnings_EUProp19 <- left_join(LDN_LA_Earnings_EUProp19,
                                      LdnUnemployment19, 
                                      by = c("lad20cd" = "Code"))

# clean names again
LDN_LA_Earnings_EUProp19 <- LDN_LA_Earnings_EUProp19 %>%
  clean_names()




#start reading in the data that will be the industry specialisation location quotients

LdnBusinessIndex19 <- read_csv(("2019_englandbusiness_industryindex.csv"),
                               col_names = TRUE,
                               locale = locale(encoding = 'Latin1')) %>%
  dplyr::filter(str_detect(Code, "^E09"))

#check all of the columns have been read in correctly
Datatypelist <- LdnBusinessIndex19 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Datatypelist
#all good. numeric is numeric. character is character



# create the functions for the industry location quotients
#Location quotients for industry concentration/specialisation
#A location quotient is simply the ratio of a local distribution to the ratio of a global distribution
#In this case, our global distribution will be London

#Location Quotient function 1
LQ1<-function(pctVariable){
  pctVariable /mean(pctVariable)
}

#Location Quotient function 2
LQ2<-function(variable,rowtotal){
  localprop<-variable/rowtotal
  globalprop<-sum(variable)/sum(rowtotal)
  return(localprop/globalprop)
}

#summary(LdnBusinessIndex19)

#simplify and the clean column names first
LdnBusinessIndex19 <- LdnBusinessIndex19 %>%
  dplyr::rename(Agriculture ='Agriculture, forestry & fishing') %>%
  dplyr::rename(Production = 'Production') %>%
  dplyr::rename(Motor = 'Motor trades') %>%
  dplyr::rename(Transport = 'Transport & Storage (inc postal)')%>%
  dplyr::rename(Accomodation = 'Accommodation & food services') %>%
  dplyr::rename(Information = 'Information & communication') %>%
  dplyr::rename(Finance = 'Finance & insurance') %>%
  dplyr::rename(Professional = 'Professional, scientific & technical') %>%
  dplyr::rename(Business = 'Business administration & support services') %>%
  dplyr::rename(Public = 'Public administration & defence') %>%
  dplyr::rename(Arts = 'Arts, entertainment, recreation & other services') %>%
  
  clean_names()



#calculate Location Quotients for the 5 Housing tenure variable
attach(LdnBusinessIndex19)
#summary(LdnBusinessIndex19)

LdnBusinessIndex19$Lqagriculture <- LQ2(agriculture, total)
LdnBusinessIndex19$Lqproduction <- LQ2(production, total)
LdnBusinessIndex19$Lqconstruction <- LQ2(construction, total)
LdnBusinessIndex19$Lqmotor <- LQ2(motor, total)
LdnBusinessIndex19$Lqwholesale <- LQ2(wholesale, total)
LdnBusinessIndex19$Lqretail <- LQ2(retail, total)
LdnBusinessIndex19$Lqtransport <- LQ2(transport, total)
LdnBusinessIndex19$Lqaccomodation <- LQ2(accomodation, total)
LdnBusinessIndex19$Lqinformation <- LQ2(information, total)
LdnBusinessIndex19$Lqfinance <- LQ2(finance, total)
LdnBusinessIndex19$Lqproperty <- LQ2(property, total)
LdnBusinessIndex19$Lqprofessional <- LQ2(professional, total)
LdnBusinessIndex19$Lqbusiness <- LQ2(business, total)
LdnBusinessIndex19$Lqpublic <- LQ2(public, total)
LdnBusinessIndex19$Lqeducation <- LQ2(education, total)
LdnBusinessIndex19$Lqhealth <- LQ2(health, total)
LdnBusinessIndex19$Lqarts <- LQ2(arts, total)

#view(LdnBusinessIndex19)


#select the location quotients
LdnIndustryConcentration19 <- LdnBusinessIndex19 %>%
  
  dplyr::select(code,
                Lqagriculture,
                Lqproduction,
                Lqconstruction, 
                Lqmotor,
                Lqwholesale,
                Lqretail,
                Lqtransport,
                Lqaccomodation,
                Lqinformation,
                Lqfinance,
                Lqproperty,
                Lqprofessional,
                Lqbusiness,
                Lqpublic,
                Lqeducation,
                Lqhealth,
                Lqarts
  )

LdnIndustryConcentration19 <- LdnIndustryConcentration19 %>%
  clean_names()

#view(LdnIndustryConcentration19)


#merge this industry mix data data with the EU data
LDN_LA_Earnings_EUProp19 <- left_join(LDN_LA_Earnings_EUProp19,
                                      LdnIndustryConcentration19, 
                                      by = c("lad20cd" = "code"))

#view(LDN_LA_Earnings_EUProp19)




####multiple regression attempt######

#WITH WAGES, UNEMPLOYMENT AND ALL LQS
Muilti_Regressiondata <- LDN_LA_Earnings_EUProp19%>%
  dplyr::select(eu14_prop, hourly_median, unemployment_rate_16_64, lqagriculture, lqproduction, lqconstruction, lqmotor, lqwholesale, lqretail, lqtransport, lqaccomodation, lqinformation, lqfinance, lqproperty, lqprofessional, lqbusiness, lqpublic, lqeducation, lqhealth, lqarts)

multi_model1 <- lm(eu14_prop ~ hourly_median  + unemployment_rate_16_64 + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                   data = Muilti_Regressiondata)

#show the summary of those outputs
tidy(multi_model1)
glance(multi_model1) # ADJ.R-SQR = 0.873, P-VALUE = 1.57e-5, and only the hourly median is significant to 0.05



#JUST WAGES AND UNEMPLOYMENT
multi_model2 <- lm(eu14_prop ~ hourly_median + unemployment_rate_16_64,
                   data = Muilti_Regressiondata)

#show the summary of those outputs
tidy(multi_model2)
glance(multi_model2) # ADJ.R-SQR = 0.304, P-VALUE = 0.00198, only hourly median is significant to 0.05 and 0.01



#JUST WAGES AND ALL LQS
multi_model3 <- lm(eu14_prop ~ hourly_median + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                   data = Muilti_Regressiondata)

#show the summary of those outputs
tidy(multi_model3)
glance(multi_model3) # ADJ.R-SQR = 0.881, P-VALUE = 4.23e-6, only hourly median, accommodation, property, and public are significant to 0.05 



# JUST UNEMPLOYMENT AND ALL LQS
multi_model4 <- lm(eu14_prop ~ unemployment_rate_16_64 + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                   data = Muilti_Regressiondata)

#show the summary of those outputs
tidy(multi_model4)
glance(multi_model4) #ADJ.R-SQR = 0.826, P-VALUE = 5.09e-5, no variables are significant to 0.05



# JUST ALL LQS
multi_model5 <- lm(eu14_prop ~ lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                   data = Muilti_Regressiondata)

#show the summary of those outputs
tidy(multi_model5)
glance(multi_model5) # ADJ.R-SQR = 0.838, P-VALUE = 1.47e-5, no variables are significant to 0.05




### stepwise regression
full_model <- lm(eu14_prop ~ hourly_median  + unemployment_rate_16_64 + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                 data = Muilti_Regressiondata)
#coef(full_model)


eu14_model_start = lm(eu14_prop ~ 1, data = Muilti_Regressiondata)

TheoryDrivenmodel_both_aic = step(
  eu14_model_start,
  scope = eu14_prop ~ hourly_median  + unemployment_rate_16_64 +  lqaccomodation + lqfinance  + lqpublic + lqeducation + lqhealth,
  direction = "both")

#AIC(TheoryDrivenmodel_both_aic) # 165
# eu14_prop ~ lqfinance + lqaccommodation + lqhealth

#compare the adj.r.squared for the original model and this 'best' model
summary(TheoryDrivenmodel_both_aic)$adj.r.squared # 0.5089
summary(TheoryDrivenmodel_both_aic) # p-value = 3.789e-5


#checking for multicolinearity 
vif(TheoryDrivenmodel_both_aic) # all have VIF values below 1.50. Meant to start worry about colinearity when it's above 10


#checking for homoscedasticity 
#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(TheoryDrivenmodel_both_aic)


##### Breusch-Pagan test for homoscedasticity  
#perform Breusch-Pagan Test
bptest(TheoryDrivenmodel_both_aic) # BP = 2.1521, p-value = 0.5414, which is greater than the 0.05 threshold, 
#therefore we cannot reject the null hypothesis that there is no heteroscedasticity, and thus the is HOMOSCEDASTICITY


#Time to remove City of London from the data (AGAIN)
#view(LDN_LA_Earnings_EUProp19)
LDN_32LA_Earnings_EUProp19 <- LDN_LA_Earnings_EUProp19[-c(1),] # '1'is the row number for City of London


#### CHECKING FOR INDEPENDENCE OF ERRORS
#spatial autocorrelation

# add the RESIDUALS  to the shapelayer
LDN_32LA_Earnings_EUProp19 <- LDN_32LA_Earnings_EUProp19 %>%
  mutate(eu14_theorydrivenmodel_resids = residuals(TheoryDrivenmodel_both_aic))


#CHECKING IF RESIDUALS ARE NORMALLY DISTRIBUTED
ggplot(LDN_32LA_Earnings_EUProp19, aes(x=eu14_theorydrivenmodel_resids)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) # NORMAL-ish distributed residuals


###### Proper mapping of the residuals
EU14_Theory_Residuals <- tm_shape(LDN_32LA_Earnings_EUProp19) + 
  tm_polygons("eu14_theorydrivenmodel_resids",
              title = "EU14 (Theory) \nResiduals", 
              palette="RdYlBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU14_Theory_Residuals

#Export the map
#tmap_save(EU14_Theory_Residuals, 'eu14_theory_residuals.png')




#### global Moran's I of the residuals

#Now we need to generate a spatial weights matrix. 
#We'll start with a simple binary matrix of queen's case neighbours

LBorough_nb <- LDN_32LA_Earnings_EUProp19 %>%
  poly2nb(., queen=T)

summary(LBorough_nb)


#or nearest neighbours
knn_borough <-coordsB %>%
  knearneigh(., k=5)


LBorough_knn <- knn_borough %>%
  knn2nb()

par(mfrow=c(1,1))
#plot them
plot(LBorough_nb, st_geometry(coordsB), col="red")

plot(LBorough_knn, st_geometry(coordsB), col="blue")



#create a spatial weights matrix object from these weights
LBorough.queens_weight <- LBorough_nb %>%
  nb2listw(., style="C")

LBorough.knn_5_weight <- LBorough_knn %>%
  nb2listw(., style="C")


#Now run a moran's I test on the residuals, first using queens neighbours
Queen <- LDN_32LA_Earnings_EUProp19 %>%
  st_drop_geometry()%>%
  dplyr::select(eu14_theorydrivenmodel_resids)%>%
  pull()%>%
  moran.test(., LBorough.queens_weight)%>%
  tidy()


#Then nearest k-nearest neighbours
Nearest_neighbour <- LDN_32LA_Earnings_EUProp19 %>%
  st_drop_geometry()%>%
  dplyr::select(eu14_theorydrivenmodel_resids)%>%
  pull()%>%
  moran.test(., LBorough.knn_5_weight) %>%
  tidy()

#view Moran's I results using the different spatial weights matrixes
Queen # 0.0980, p-value = 0.106

Nearest_neighbour 
#when k = 2, Moran's I = -0.0274, p-value = 0.487
#when k = 3, Moran's I = 0.0756, p-value = 0.1968  
#when k = 4, Moran's I = 0.0735, p-value = 0.161
#when k = 5, Moran's I = 0.0410, p-value = 0.216

#all of the different spatial weights matrices above have p-values greater than 0.05,
#which means that we cannot reject the null hyothesis of Complete Spatial Randomness (CSR)
#therefore it's pretty conclusive that there is no spatial autocorrelation with the residuals
#and that there IS Independence of Errors!



######################
##################################################
######################




































##2015##

#read in some attribute data
LdnEarnings15 <- read_csv(("2015_ukhourlywages_mean_median_all.csv"),
                          col_names = TRUE,
                          locale = locale(encoding = 'Latin1'))%>%
  dplyr::filter(str_detect(Code, "^E09"))


#check all of the columns have been read in correctly
Datatypelist <- LdnEarnings15 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Datatypelist


#Cleaning the data as I read it in
LdnEarnings15 <- read_csv(("2015_ukhourlywages_mean_median_all.csv"), 
                          na = c("", "NA", "n/a", "#", "-", "!", "x", ".."), 
                          locale = locale(encoding = 'Latin1'), 
                          col_names = TRUE)%>%
  dplyr::filter(str_detect(Code, "^E09"))


#check all of the columns have been read in correctly
Datatypelist <- LdnEarnings15 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Datatypelist


#merge boundaries and data
LDN_LA_Earnings15 <- left_join(LDN_LAs,
                               LdnEarnings15, 
                               by = c("lad20cd" = "Code"))


#Additional data
#country of birth of population
country_of_birth15 <- read_csv(("2015populationbycountryofbirthandnationality16_64.csv"),
                               na = c("", "NA", "n/a", "#", ".", "c", ":", "0~"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

# I already deleted all the non eu countries of birth from the data on excel so rename the name here
EU_born15 <- country_of_birth15 %>%
  clean_names()


#focus on London to begin with
EU_born_LDN15 <- EU_born15 %>%
  dplyr::filter(str_detect(area_code, "^E09"))


#creating a new column for the proportion of EU born population
EU_born_LDN_proportions15 <- EU_born_LDN15 %>% 
  
  #new column with proportion of all EU migrants
  mutate(eu_prop = (european_union / 
                      total)*100) %>%
  
  #new column with proportion of EU14 migrants
  mutate(eu14_prop = (european_union_eu14 / 
                        total)*100) %>%
  
  #new column with proportion of EU8 migrants
  mutate(eu8_prop = (european_union_eu8 / 
                       total)*100) %>%
  
  #new column with proportion of EU2 migrants
  mutate(eu2_prop = (european_union_eu2 / 
                       total)*100) %>%
  
  
  #select only the proportion columns
  dplyr::select(area_code, area_name, 
                eu_prop,
                eu14_prop,
                eu8_prop, 
                eu2_prop)


#merge earnings/boundary data with the EU data
LDN_LA_Earnings_EUProp15 <- left_join(LDN_LA_Earnings15,
                                      EU_born_LDN_proportions15, 
                                      by = c("lad20cd" = "area_code"))

#view(LDN_LA_Earnings_EUProp15) 







####PART 1
####UNDERSTANDING THE DATA

# mapping the dependent variable(s) to see if the join has worked:
tmap_mode("view")
qtm(LDN_LA_Earnings_EUProp15, 
    fill = "eu_prop", 
    borders = NULL,  
    fill.palette = "Blues")


qtm(LDN_LA_Earnings_EUProp15, 
    fill = "eu14_prop", 
    borders = NULL,  
    fill.palette = "Blues")


qtm(LDN_LA_Earnings_EUProp15, 
    fill = "eu8_prop", 
    borders = NULL,  
    fill.palette = "Blues")


qtm(LDN_LA_Earnings_EUProp15, 
    fill = "eu2_prop", 
    borders = NULL,  
    fill.palette = "Blues")


# clean names again
LDN_LA_Earnings_EUProp15 <- LDN_LA_Earnings_EUProp15 %>%
  clean_names()
#view(LDN_LA_Earnings_EUProp15) 
#######################################################



########################### MAKING BETTER MAPS 23/12/2020

tmap_mode("plot")
# set the breaks
# for our mapped data
breaks = c(0, 5, 10, 15, 20, 25) 


EU_2015 <- tm_shape(LDN_LA_Earnings_EUProp15) + 
  tm_polygons("eu_prop",
              title = "EU Proportion \n(2015)",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU_2015

#Export the map
tmap_save(EU_2015, 'eu2015proportions.png')



EU14_2015 <- tm_shape(LDN_LA_Earnings_EUProp15) + 
  tm_polygons("eu14_prop",
              title = "EU14 Proportion \n(2015)",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU14_2015

#Export the map
tmap_save(EU14_2015, 'eu142015proportions.png')



EU8_2015 <- tm_shape(LDN_LA_Earnings_EUProp15) + 
  tm_polygons("eu8_prop",
              title = "EU8 Proportion \n(2015)",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU8_2015

#Export the map
tmap_save(EU8_2015, 'eu82015proportions.png')



EU2_2015 <- tm_shape(LDN_LA_Earnings_EUProp15) + 
  tm_polygons("eu2_prop",
              title = "EU2 Proportion \n(2015)",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU2_2015

#Export the map
tmap_save(EU2_2015, 'eu22015proportions.png')




#LETS CHECK THE DISTRIBUTION OF THE DEPENDENT VARIABLE
#eu_prop
ggplot(LDN_LA_Earnings_EUProp15, aes(x=eu_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu_14
ggplot(LDN_LA_Earnings_EUProp15, aes(x=eu14_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1.5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu8
ggplot(LDN_LA_Earnings_EUProp15, aes(x=eu8_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#eu2
ggplot(LDN_LA_Earnings_EUProp15, aes(x=eu2_prop)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)




#using code from stackoverflow to get my data into the correct format for plotting nicer boxplots/violin plots
#url for page that helped with the data formatting: https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
boxplot2015df <- EU_born_LDN_proportions15 %>%
  dplyr::select(area_name, 
                eu_prop,
                eu14_prop,
                eu8_prop, 
                eu2_prop)

boxplot2015df.m <- melt(boxplot2015df, id.var = "area_name")
#show the new format of the data 
boxplot2015df.m

#standard
ggplot(data = boxplot2015df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))



# 'Fancier' Plots
ggplot(data = boxplot2015df.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplots of the different EU country groupings, 
  showing the proportions across each London Borough (2015)") +
  xlab("")


# Boxplot basic
ggplot(data = boxplot2015df.m, aes(x=variable, y=value, fill=variable)) +  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")


# Violin basic
ggplot(data = boxplot2015df.m, aes(x=variable, y=value, fill=variable)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")




#######PART 2
####SPATIAL DEPENDENCIES####


#have a look to check that it's 
#in the right projection
st_crs(LDN_LA_Earnings_EUProp15)
#all good to go #27700



#Before being able to calculate Moran's I and any similar statistics, we need to first define a spatial weights matrix


#THE MORAN'S I TEST will not work because City of London has NA values for each EU classification 
#Time to remove City of London from the data
LDN_32LA_Earnings_EUProp15 <- LDN_LA_Earnings_EUProp15[-c(1),] # '1'is the row number for City of London


#for eu_prop
I_LBorough_Global_EUprop15 <- LDN_32LA_Earnings_EUProp15 %>%
  pull(eu_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EUprop15  # Moran's I = 0.2665, p-value = 0.002501


#for eu14_prop
I_LBorough_Global_EU14prop15 <- LDN_32LA_Earnings_EUProp15 %>%
  pull(eu14_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EU14prop15  # Moran's I = 0.4564, p-value = 1.165e-06


#for eu8_prop
I_LBorough_Global_EU8prop15 <- LDN_32LA_Earnings_EUProp15 %>%
  pull(eu8_prop) %>%
  as.vector()%>%
  moran.test(., LBorough.bw)

I_LBorough_Global_EU8prop15  # Moran's I = 0.1216, p-value = 0.07109

### BE SURE TO TRY OUT OTHER SPATIAL WEIGHTS MATRICES




#### PART 4 ####
###### OLS REGRESSIONS 


#################################### EU14 ##################

# clean names again
LDN_LA_Earnings_EUProp15 <- LDN_LA_Earnings_EUProp15 %>%
  clean_names()
#view(LDN_32LA_Earnings_EUProp15) 


#REGRESSION BASICS
a <- qplot(x = hourly_median, 
           y = eu14_prop, 
           data = LDN_LA_Earnings_EUProp15)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
a + stat_smooth(method = "lm", se= FALSE, size=1) + 
  geom_jitter()


#Now run a regression model
#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LDN_LA_Earnings_EUProp15%>%
  dplyr::select(eu14_prop, hourly_median)

#now model
model1 <- lm(eu14_prop ~
               hourly_median, 
             data = Regressiondata)

#show the summary of those outputs
tidy(model1)
glance(model1) # r.sqr = 0.254, p-value = 0.00325



######add some unemployment data so that you can run a multiple regression

#stick to London again to start with
#add UK Unemployment data across local authorities

#read in some attribute data
LdnUnemployment15 <- read_csv(("2014-2015_ukunemployment.csv"),
                              col_names = TRUE,
                              locale = locale(encoding = 'Latin1')) %>%
  dplyr::filter(str_detect(Code, "^E09"))

#check all of the columns have been read in correctly
Data2typelist <- LdnUnemployment15 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Data2typelist


#Cleaning the data as I read it in
LdnUnemployment15 <- read_csv(("2014-2015_ukunemployment.csv"),
                              na = c("", "NA", "n/a", "#", "-", "!", "x", ".."), 
                              locale = locale(encoding = 'Latin1'), 
                              col_names = TRUE) %>%
  dplyr::filter(str_detect(Code, "^E09"))


#check all of the columns have been read in correctly
Data2typelist <- LdnUnemployment15 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Data2typelist


#select only the unemployment column from the data
LdnUnemployment15 <- LdnUnemployment15 %>%
  dplyr::select('Code', 'Description', 'Unemployment rate 16-64', 'JSA rate')

#view(LdnUnemployment15)



#merge with previous data containing all the other variables and boundaries data
LDN_LA_Earnings_EUProp15 <- left_join(LDN_LA_Earnings_EUProp15,
                                      LdnUnemployment15, 
                                      by = c("lad20cd" = "Code"))

# clean names again
LDN_LA_Earnings_EUProp15 <- LDN_LA_Earnings_EUProp15 %>%
  clean_names()



####### let's start reading in the data that will be the industry location quotients

#start with London again
#read in some attribute data
LdnBusinessIndex15 <- read_csv(("2015_englandbusiness_industryindex.csv"),
                               col_names = TRUE,
                               locale = locale(encoding = 'Latin1')) %>%
  dplyr::filter(str_detect(Code, "^E09"))

#check all of the columns have been read in correctly
Datatypelist <- LdnBusinessIndex15 %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

Datatypelist
#all good. numeric is numeric. character is character

#summary(LdnBusinessIndex15)


#simplify and the clean column names first
LdnBusinessIndex15 <- LdnBusinessIndex15 %>%
  dplyr::rename(Agriculture ='Agriculture, forestry & fishing') %>%
  dplyr::rename(Production = 'Production') %>%
  dplyr::rename(Motor = 'Motor trades') %>%
  dplyr::rename(Transport = 'Transport & Storage (inc postal)')%>%
  dplyr::rename(Accomodation = 'Accommodation & food services') %>%
  dplyr::rename(Information = 'Information & communication') %>%
  dplyr::rename(Finance = 'Finance & insurance') %>%
  dplyr::rename(Professional = 'Professional, scientific & technical') %>%
  dplyr::rename(Business = 'Business administration & support services') %>%
  dplyr::rename(Public = 'Public administration & defence') %>%
  dplyr::rename(Arts = 'Arts, entertainment, recreation & other services') %>%
  
  clean_names()


#calculate Location Quotients
attach(LdnBusinessIndex15)
#summary(LdnBusinessIndex15)

LdnBusinessIndex15$Lqagriculture <- LQ2(agriculture, total)
LdnBusinessIndex15$Lqproduction <- LQ2(production, total)
LdnBusinessIndex15$Lqconstruction <- LQ2(construction, total)
LdnBusinessIndex15$Lqmotor <- LQ2(motor, total)
LdnBusinessIndex15$Lqwholesale <- LQ2(wholesale, total)
LdnBusinessIndex15$Lqretail <- LQ2(retail, total)
LdnBusinessIndex15$Lqtransport <- LQ2(transport, total)
LdnBusinessIndex15$Lqaccomodation <- LQ2(accomodation, total)
LdnBusinessIndex15$Lqinformation <- LQ2(information, total)
LdnBusinessIndex15$Lqfinance <- LQ2(finance, total)
LdnBusinessIndex15$Lqproperty <- LQ2(property, total)
LdnBusinessIndex15$Lqprofessional <- LQ2(professional, total)
LdnBusinessIndex15$Lqbusiness <- LQ2(business, total)
LdnBusinessIndex15$Lqpublic <- LQ2(public, total)
LdnBusinessIndex15$Lqeducation <- LQ2(education, total)
LdnBusinessIndex15$Lqhealth <- LQ2(health, total)
LdnBusinessIndex15$Lqarts <- LQ2(arts, total)

view(LdnBusinessIndex15)


#select the location quotients
LdnIndustryConcentration15 <- LdnBusinessIndex15 %>%
  
  dplyr::select(code,
                Lqagriculture,
                Lqproduction,
                Lqconstruction, 
                Lqmotor,
                Lqwholesale,
                Lqretail,
                Lqtransport,
                Lqaccomodation,
                Lqinformation,
                Lqfinance,
                Lqproperty,
                Lqprofessional,
                Lqbusiness,
                Lqpublic,
                Lqeducation,
                Lqhealth,
                Lqarts)

LdnIndustryConcentration15 <- LdnIndustryConcentration15 %>%
  clean_names()


#merge this industry mix data data with the EU data
LDN_LA_Earnings_EUProp15 <- left_join(LDN_LA_Earnings_EUProp15,
                                      LdnIndustryConcentration15, 
                                      by = c("lad20cd" = "code"))

#view(LDN_LA_Earnings_EUProp15)






####multiple regression attempt######

#WITH WAGES, UNEMPLOYMENT AND ALL LQS
Muilti_Regressiondata_2015 <- LDN_LA_Earnings_EUProp15%>%
  dplyr::select(eu14_prop, hourly_median, unemployment_rate_16_64, lqagriculture, lqproduction, lqconstruction, lqmotor, lqwholesale, lqretail, lqtransport, lqaccomodation, lqinformation, lqfinance, lqproperty, lqprofessional, lqbusiness, lqpublic, lqeducation, lqhealth, lqarts)

multi_model1 <- lm(eu14_prop ~ hourly_median  + unemployment_rate_16_64 + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                   data = Muilti_Regressiondata_2015)

#show the summary of those outputs
tidy(multi_model1)
glance(multi_model1) # ADJ.R-SQR = 0.732, P-VALUE = 0.00132, and only lqeducation is significant to 0.05



###### stepwise regression
full_model_2015 <- lm(eu14_prop ~ hourly_median  + unemployment_rate_16_64 + lqagriculture + lqproduction + lqconstruction + lqmotor + lqwholesale + lqretail + lqtransport + lqaccomodation + lqinformation + lqfinance + lqproperty + lqprofessional + lqbusiness + lqpublic + lqeducation + lqhealth + lqarts,
                      data = Muilti_Regressiondata_2015)
#coef(full_model_2015)


eu14_2015model_start = lm(eu14_prop ~ 1, data = Muilti_Regressiondata_2015)

TheoryDrivenmodel_both_aic_2015 = step(
  eu14_2015model_start,
  scope = eu14_prop ~ hourly_median  + unemployment_rate_16_64 +  lqaccomodation + lqfinance  + lqpublic + lqeducation + lqhealth,
  direction = "both")

AIC(TheoryDrivenmodel_both_aic_2015) # 163.6 in comparison with 165 in 2019
# eu14_prop ~  lqaccommodation + lqhealth , in comparison with the 2019 model that included lqfinance

#compare the adj.r.squared for the original model and this 'best' model
summary(TheoryDrivenmodel_both_aic_2015)$adj.r.squared # 0.4679 in comparison to 0.5089 in 2019
summary(TheoryDrivenmodel_both_aic_2015) # p-value = 4.047e-05 in comparison to 3.789e-5 in 2019


#checking for multicolinearity 
vif(TheoryDrivenmodel_both_aic_2015) # all have VIF values below 1.10. Meant to start worry about colinearity when it's above 10


#checking for homoscedasticity 
#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(TheoryDrivenmodel_both_aic_2015)


##### Breusch-Pagan test for homoscedasticity  
#perform Breusch-Pagan Test
bptest(TheoryDrivenmodel_both_aic_2015) # BP = 1.6955, p-value = 0.4284, which is greater than the 0.05 threshold, 
#therefore we cannot reject the null hypothesis that there is no heteroscedasticity, and thus the is HOMOSCEDASTICITY


#Time to remove City of London from the data (AGAIN)
#view(LDN_LA_Earnings_EUProp15)
LDN_32LA_Earnings_EUProp15 <- LDN_LA_Earnings_EUProp15[-c(1),] # '1'is th row number for City of London


#### CHECKING FOR INDEPENDENCE OF ERRORS
#spatial autocorrelation

# add the RESIDUALS  to the shapelayer
LDN_32LA_Earnings_EUProp15 <- LDN_32LA_Earnings_EUProp15 %>%
  mutate(eu14_theorydriven2015model_resids = residuals(TheoryDrivenmodel_both_aic_2015))


#CHECKING IF RESIDUALS ARE NORMALLY DISTRIBUTED
ggplot(LDN_32LA_Earnings_EUProp15, aes(x=eu14_theorydriven2015model_resids)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) # NORMAL-ish distributed residuals


###### Proper mapping of the residuals
EU14_2015_Theory_Residuals <- tm_shape(LDN_32LA_Earnings_EUProp15) + 
  tm_polygons("eu14_theorydriven2015model_resids",
              title = "EU14 (2015) \nResiduals", 
              palette="RdYlBu")+
  tm_scale_bar(position = c(0.01 , 0.01), text.size = .70)+
  tm_layout(legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  #tm_credits("(c) OpenStreetMap contrbutors and ONS", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.04,0.04,0.02,0.2))

EU14_2015_Theory_Residuals

#Export the map
tmap_save(EU14_2015_Theory_Residuals, 'eu14_2015_theory_residuals.png')




#### global Moran's I of the residuals

#Now we need to generate a spatial weights matrix. 
#We'll start with a simple binary matrix of queen's case neighbours

LBorough_nb <- LDN_32LA_Earnings_EUProp15 %>%
  poly2nb(., queen=T)

summary(LBorough_nb)


#or nearest neighbours
knn_borough <-coordsB %>%
  knearneigh(., k=5)


LBorough_knn <- knn_borough %>%
  knn2nb()


par(mfrow=c(1,1))
#plot them
plot(LBorough_nb, st_geometry(coordsB), col="red")

plot(LBorough_knn, st_geometry(coordsB), col="blue")



#create a spatial weights matrix object from these weights
LBorough.queens_weight <- LBorough_nb %>%
  nb2listw(., style="C")

LBorough.knn_5_weight <- LBorough_knn %>%
  nb2listw(., style="C")


#Now run a moran's I test on the residuals, first using queens neighbours
Queen15 <- LDN_32LA_Earnings_EUProp15 %>%
  st_drop_geometry()%>%
  dplyr::select(eu14_theorydriven2015model_resids)%>%
  pull()%>%
  moran.test(., LBorough.queens_weight)%>%
  tidy()


#Then nearest k-nearest neighbours
Nearest_neighbour15 <- LDN_32LA_Earnings_EUProp15 %>%
  st_drop_geometry()%>%
  dplyr::select(eu14_theorydriven2015model_resids)%>%
  pull()%>%
  moran.test(., LBorough.knn_5_weight) %>%
  tidy()

#view Moran's I results using the different spatial weights matrices
Queen15 # 0.135, p-value = 0.0519

Nearest_neighbour15 
#when k = 5, Moran's I = 0.114, p-value = 0.0555
#when k = 4, Moran's I = 0.138, p-value = 0.0529
#when k = 3, Moran's I = 0.222, p-value = 0.0210 ### only one that suggest there is spatial autocorrelation
#when k = 2, Moran's I = 0.180, p-value = 0.0827

#all, except 1, of the different spatial weights matrices above have p-values greater than 0.05,
#also, the other p-values are very close to the 0.05 threshold which is not as conclusive as the p-values in 2019
#this model doesn't include the lqfinance like 2019 so might that explain this difference?




# ALTHOUGH THE EVIDENCE FROM THE 2015 EU14 MODEL MOSTLY SUGGEST THAT THERE IS NO SPATIAL AUTOCORRELATION,
# THE EVIDENCE IS STILL ON THE EDGE
# I WILL NOW USE TWO SPATIAL ECONOMETRIC MODELS:
#1. THE SPATIAL LAG MODEL
#2. THE SPATIAL ERROR MODEL




#DO THE TEST BELOW FOR BOTH MODELS 
#*The Lagrange Multiplier Test Diagnostics for Spatial Dependence and 
#*Spatial Heterogeneity assess model misspecification due to spatial 
#*patterning and WILL BE used to test the robustness for both models (Anselin, 1998). 


#######################################################################################################################################################################
# 1.
######The Spatial Lag (lagged dependent variable) model


#Now run a spatially-lagged regression model with a queen’s case weights matrix

#view(Muilti_Regressiondata_2015)
Muilti_Regressiondata_2015 <- Muilti_Regressiondata_2015[-c(1),] # '1'is the row number for City of London

slag_dv_eu14_2015theorymodel_queen <- lagsarlm(eu14_prop  ~ lqhealth + lqaccomodation,
                                               data = Muilti_Regressiondata_2015, 
                                               nb2listw(LBorough_nb, style="C"), 
                                               method = "eigen")

#what do the outputs show?
tidy(slag_dv_eu14_2015theorymodel_queen) # rho (the spatial lag): estimate = 0.392, p-value = 0.00526
glance(slag_dv_eu14_2015theorymodel_queen) # r.sqr = 0.618, AIC = 152, loglik = -74.2

#Running the spatially-lagged model with a Queen’s case spatial weights matrix revealed 
# A STATISTICALLY SIGNIFICANT and moderate positive effect associated with the spatially lagged dependent variable. 
#However, a different conception of neighbours we might get a different outcome



#Now run a spatially-lagged regression model with a knn 5 weights matrix
slag_dv_eu14_2015theorymodel_knn5 <- lagsarlm(eu14_prop  ~ lqhealth + lqaccomodation ,
                                              data = Muilti_Regressiondata_2015, 
                                              nb2listw(LBorough_knn, 
                                                       style="C"), 
                                              method = "eigen")

#what do the outputs show?
tidy(slag_dv_eu14_2015theorymodel_knn5) # rho (the spatial lag): estimate = 0.708, p-value = 0.000000101
glance(slag_dv_eu14_2015theorymodel_knn5) # r.sqr = 0.697, AIC = 154, loglik = -71.8 (the greater the loglik value, the better the fit of the data)

#Running the spatially-lagged model with knn = 5 spatial weights matrix revealed 
#again, A MORE SIGNIFICANT and small (BUT SLIGHTLY BIGGER) positive effect associated with the spatially lagged dependent variable. 



#check that the residuals from the spatially lagged model are now no-longer exhibiting spatial autocorrelation:
#write out the residuals
# for queen's weight first 
LDN_32LA_Earnings_EUProp15 <- LDN_32LA_Earnings_EUProp15 %>%
  mutate(slag_dv_eu14_2015theorymodel_queen_resids = residuals(slag_dv_eu14_2015theorymodel_queen))


QueenMoran <- LDN_32LA_Earnings_EUProp15 %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_eu14_2015theorymodel_queen_resids)%>%
  pull()%>%
  moran.test(., LBorough.queens_weight)%>%
  tidy()

QueenMoran # Moran's I = -0.0951, but p-value = 0.729 which is greater than 0.05 so we cannot reject the null hypothesis that there is complete spatial randomness!
# no spatial autocorrelation



# now for the KNN weight
LDN_32LA_Earnings_EUProp15 <- LDN_32LA_Earnings_EUProp15 %>%
  mutate(slag_dv_eu14_2015theorymodel_knn5_resids = residuals(slag_dv_eu14_2015theorymodel_knn5))


Knn5Moran <- LDN_32LA_Earnings_EUProp15 %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_eu14_2015theorymodel_knn5_resids)%>%
  pull()%>%
  moran.test(., LBorough.knn_5_weight)%>%
  tidy()

Knn5Moran # Moran's I = -0.124, but p-value = 0.839 which is greater than 0.05 so we cannot reject the null hypothesis that there is complete spatial randomness!
# no spatial autocorrelation








# 2. 
######The Spatial Error Model

sem_eu14_2015theorymodel <- errorsarlm(eu14_prop  ~ lqhealth + lqaccomodation,
                                       data = Muilti_Regressiondata_2015,
                                       nb2listw(LBorough_knn, style="C"), 
                                       method = "eigen")

tidy(sem_eu14_2015theorymodel)
glance(sem_eu14_2015theorymodel) # r.sqr = 0.669, AIC = 161, loglik = -75.3



#check that the residuals from the spatial error model (SEM) are now no-longer exhibiting spatial autocorrelation:
#write out the residuals
LDN_32LA_Earnings_EUProp15 <- LDN_32LA_Earnings_EUProp15 %>%
  mutate(sem_eu14_2015theorymodel_knn5_resids = residuals(sem_eu14_2015theorymodel))

SEM_KNN5Moran <- LDN_32LA_Earnings_EUProp15 %>%
  st_drop_geometry()%>%
  dplyr::select(sem_eu14_2015theorymodel_knn5_resids)%>%
  pull()%>%
  moran.test(., LBorough.knn_5_weight)%>%
  tidy()

SEM_KNN5Moran #Moran's I = -0.00306. p-value = 0.349. Can conclude that there is no longer any spatial autocorrelation










