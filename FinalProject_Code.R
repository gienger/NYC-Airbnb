rm(list=ls())

#read in NYC Airbnb Kaggle
Airbnb<-read.csv("NYC_Airbnb_Data.csv",
                 header = TRUE,na.strings = c(""))
View(Airbnb)

#scrape and crawl data
library(xml2)

url<-paste('https://guides.newman.baruch.cuny.edu/nyc_data/nbhoods')
page<-read_html(url)

#get hyper links
links<-xml_attr(xml_find_all(page, ".//div[@id='s-lg-content-7151850']/ul/li/a"),"href")
links

#filter economics in hyperlinks
neighborhoods_econ<-links[c(2,6,10,14,18,22)]
neighborhoods_econ

#for loop to read in NYC, Bronx, Brooklyn, Manhattan, Queens, & Staten Island scraped data
totalHouseholds<-character(0)
highIncomeHouseholds<-character(0)
medianIncome<-character(0)
meanIncome<-character(0)
for (i in neighborhoods_econ){
  Sys.sleep(5)
  page<-read_html(i)
  households<-xml_text(xml_find_all(page,".//tr/td[2]")[1])
  totalHouseholds<-c(totalHouseholds,households)
  highIncome<-xml_text(xml_find_all(page,".//tr/td[2]")[11])
  highIncomeHouseholds<-c(highIncomeHouseholds,highIncome)
  income<-xml_text(xml_find_all(page,".//tr/td[2]")[18])
  medianIncome<-c(medianIncome,income)
  meanInc<-xml_text(xml_find_all(page,".//tr/td[2]")[19])
  meanIncome<-c(meanIncome,meanInc)
}
totalHouseholds
highIncomeHouseholds
medianIncome
meanIncome

#creating vector with each neighborhood data scraped for
Neighborhood<-c("New York City","Bronx","Brooklyn","Manhattan","Queens","Staten Island")

#creating data frame with neighborhood and related information
scraped_nyc<-data.frame(Neighborhood,totalHouseholds,highIncomeHouseholds,medianIncome,meanIncome)
colnames(scraped_nyc)<-c("Neighborhood","Total_Households","High_Income_Households","Median_Income","Mean_Income")
View(scraped_nyc)

#Horizontally integrate both data sets
all_nyc<-merge(Airbnb,scraped_nyc,by.x = "neighbourhood_group",by.y = "Neighborhood",all.x = TRUE)
View(all_nyc)


### Analysis #1

#Running summary to gain understanding of price & number of reviews for all NYC Airbnb listings
summary(all_nyc$price)
summary(all_nyc$number_of_reviews)


#How greatly does the price impact the number of reviews a listing received?
cor_price_reviews<-cor.test(all_nyc$price, all_nyc$number_of_reviews)
class(cor_price_reviews)
cor_price_reviews$p.value
#Being that the p < .05, we reject the null hypothesis that the correlation is 0

#Is there a substantial difference in the number of reviews a rental unit receives across the different New York City neighborhoods by room type?  
#How does the monthly rate of reviews vary among the different neighborhoods by room type? 
library(dplyr)
NYC_reviews<-group_by(all_nyc,neighbourhood_group,room_type)
reviews_summary<-summarize(NYC_reviews,
                           Total_Listings=n(),
                           Average_Reviews=mean(number_of_reviews),
                           Average_Monthly_Reviews=mean(reviews_per_month,na.rm=TRUE))
reviews_summary

#Transforming reviews_summary into a pivot table that shows average reviews by neighborhood and room type
library(reshape2)
pivot_reviews<-dcast(reviews_summary, #dplyr summary table
                     neighbourhood_group~room_type,
                     value.var="Average_Reviews")
pivot_reviews

#Created bar chart to show the average number of reviews & average monthly reviews by neighborhood
barplot(tapply(all_nyc$number_of_reviews,all_nyc$neighbourhood_group,FUN = mean),
        col="blue",
        xlab="Neighborhood",
        ylab="Average Reviews",
        ylim=c(0,40),
        main="Average Reviews by NYC Neighborhood")


### Analysis #2

#Which neighborhood group has the highest average prices of rental unit listings? 
avg_price_by_neighborhood_group<-aggregate(Airbnb[, 10], list(Airbnb$neighbourhood_group), mean)
max(avg_price_by_neighborhood_group$x)
avg_price_by_neighborhood_group[order(avg_price_by_neighborhood_group$x,decreasing = T),"Group.1"][1]

#Is there substantial difference in price among the different NYC neighborhoods?
library(scales)
library(ggplot2)
qplot(Group.1, x,data=avg_price_by_neighborhood_group,
      geom="point",
      xlab="Neighborhood Group",
      ylab="Avg Price",
      main="Neighborhood Group Avg Price",
      color="red")+geom_point(size=4)


### Analysis #3

#How does the room type and availability impact the minimum number of nights that a customer must book for an Airbnb listing?(Allison)

ct<-cor.test(all_nyc$availability_365, all_nyc$minimum_nights)
ct
#There is no linear correlation between the availability of Airbnbs in NYC and the minimum nights stayed in Airbnbs proven by the near zero correlation value of 0.1515449, and a very low p-value which rejects the null hypothesis of correlation.

#Analysis on how availability is related to the room type of the listing
boxplot(all_nyc$availability_365~all_nyc$room_type)
anovamodel<-aov(all_nyc$availability_365~all_nyc$room_type)
summary(anovamodel)
#reject null hypothesis, don't all have the same mean


#Cleaning the data to do further analysis with the scraped data

all_nyc$Total_Households <- gsub(",", "", all_nyc$Total_Households)
all_nyc$High_Income_Households <- gsub(",", "", all_nyc$High_Income_Households)

all_nyc$Mean_Income<- gsub("[\\$]", "", all_nyc$Mean_Income)
all_nyc$Mean_Income<- gsub(",", "", all_nyc$Mean_Income)

all_nyc$Median_Income<- gsub("[\\$]", "", all_nyc$Median_Income)
all_nyc$Median_Income<- gsub(",", "", all_nyc$Median_Income)

all_nyc$Total_Households <- as.numeric(all_nyc$Total_Households)
all_nyc$High_Income_Households <- as.numeric(all_nyc$High_Income_Households)
all_nyc$Mean_Income<- as.numeric(all_nyc$Mean_Income)
all_nyc$Median_Income<- as.numeric(all_nyc$Median_Income)

str(all_nyc)

### Analysis #4

#Barplot for the average availability of households in each neighborhood 
group_means<-tapply(all_nyc$availability_365,all_nyc$neighbourhood_group,FUN = mean)
barplot(sort(group_means,decreasing = T),
        names.arg =c("Staten Island","Bronx","Queens","Manhattan","Brooklyn"),
        ylab = "Average Days Available",
        xlab = "Neighborhood",
        main = "Average Availability of Listings by Neighborhood",
        ylim = c(0,200),
        col = "forestgreen")

#Table to compare with the average availability showing high income household percentage
households_table<-group_by(all_nyc,neighbourhood_group)
hh_table<-summarize(households_table,
                    Total_Households=(Total_Households),
                    High_Income_Households=(High_Income_Households),
                    Percent_HighIncome_Houses=((High_Income_Households/Total_Households)*100))
grouped_hh_table<-unique(hh_table)
grouped_hh_table

# Load scales and ggplot2 packages:
library(scales)
library(ggplot2)

## Visualization with 3 Variables: 2 Continuous, 1 Categorical
#Creates the table with total households and average availability by neighborhood
library(dplyr)
grouped_neighborhood<-group_by(all_nyc,neighbourhood_group)
a_table<-summarize(grouped_neighborhood,
                        Total_Households=(Total_Households),
                        Avg_Availability=mean(availability_365))
                        
#Fixes the table so that only includes unique values - the 5 neighborhood values
availability_table<-unique(a_table)
availability_table

#Creates the plot displaying the table
qplot(Total_Households, # x axis
      Avg_Availability, # y axis
      data=availability_table, 
      geom="point",
      ylab="Average Availability (days)",
      xlab="Total Households",
      main="                                 Listing Availability and Households by NYC Neighborhood",
      ylim=c(100,200),
      col=neighbourhood_group)+geom_point(size=6)

#How greatly does the price impact the number of reviews a listing received
cor_households_availability<-cor.test(all_nyc$Total_Households, all_nyc$availability_365)
cor_households_availability
class(cor_households_availability)
#Being that the p < .05, we reject the null hypothesis that the correlation is 0

### Analysis #5

#Is there a relationship between the median or mean income in an area and the number of customer reviews on the listing describing the customer experience? 
#Specifically, do areas with higher median or mean income cause more customer reviews for listings or for average price to be higher also?
ct1 <- cor.test(all_nyc$Median_Income, all_nyc$number_of_reviews)
ct1

ct2 <- cor.test(all_nyc$Mean_Income, all_nyc$number_of_reviews)
ct2


library(ggplot2)

colors <- c("$41,432 (Bronx)" = "blue", "$66,937 (Brooklyn)" = "blue", "$73,696 (Queens)" = "blue", "$89,821 (Staten Island)" = "blue", "$93,651 (Manhattan)" = "blue")

# Median Income and Price
ggplot(all_nyc, aes(x = Median_Income)) +
  geom_point(aes(y = price, color = "$41,432 (Bronx)"), size = 1.5) +
  labs(x = "Median Income",
       y = "Price",
       color = "Legend") +ggtitle('Median Income and Price')+
  scale_color_manual(values = colors)

# Median Income and Number of Reviews
ggplot(all_nyc, aes(x = Median_Income)) +
  geom_point(aes(y = number_of_reviews, color = "$41,432 (Bronx)"), size = 1.5) +
  labs(x = "Median Income",
       y = "Number of Reviews",
       color = "Legend") +ggtitle('Median Income and Number of Reviews')+
  scale_color_manual(values = colors)






colors1 <- c("$59,664 (Bronx)" = "red", "$96,631 (Queens)" = "red", "$99,472 (Brooklyn)" = "red", "$113,245 (Staten Island)" = "red", "$170,453 (Manhattan)" = "red")

# Mean Income and Price
ggplot(all_nyc, aes(x =Mean_Income)) +
  geom_point(aes(y = price, color = "$59,664 (Bronx)"), size = 1.5) +
  labs(x = "Mean Income",
       y = "Price",
       color = "Legend") +ggtitle('Mean Income and Price')+
  scale_color_manual(values = colors1)

# Mean Income and Number of Reviews
ggplot(all_nyc, aes(x = Mean_Income)) +
  geom_point(aes(y = number_of_reviews, color = "$59,664 (Bronx)"), size = 1.5) +
  labs(x = "Mean Income",
       y = "Number of Reviews",
       color = "Legend") +ggtitle('Mean Income and Number of Reviews')+
  scale_color_manual(values = colors1)





