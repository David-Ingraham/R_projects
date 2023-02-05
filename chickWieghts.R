install.packages("tidyverse")
library(tidyverse)
library(datasets)
data()
glimpse(ChickWeight)
?ChickWeight
colnames(ChickWeight)
chickWeight <- as_tibble(ChickWeight)
rename(chickWeight, Weight = weight)
chickWeight <- rename(chickWeight, Weight = weight)
ggplot(chickWeight, aes(x = Time, y = Weight)) + geom_line() #this plot shows that the chicks have 
#a tight distribution for weight at a young age, but as they get older their weights vary more
tail(chickWeight)
plot(chickWeight$Time, chickWeight$Weight, main = "Chick Weight Over Time",
     xlab ="Time after hatching", ylab = "Chick Weight") #this plot gives the same information
#as the line plot, but here we can see each chick as thier own data point 
ggplot(chickWeight, aes(x = Weight)) + geom_bar() #this chart shows the count of each weight, 
#or in other words, how frequent each weight appears in the data. We see that the most common 
#weight is around 30 grams
ggplot(chickWeight, aes(x = Weight)) + geom_boxplot() #This plot shows the interquartile range for
#the data. We see that the median weight is just over 100 grams, while the heaviest chick
#is just shy of 400, and the lightest chick is under 30 grams
ggplot(chickWeight, aes(x = Weight)) + geom_histogram() #Since weight is a continous data type,
#the histogram more accuratley depicts the nature of the chicks weight distribution
plot(density(chickWeight$Weight)) #this chart is the same as the histogram but with a smoothed out
#relationship bwtween susequent data points
ggplot(chickWeight, aes(Weight)) + stat_ecdf(geom = "point")
ggplot(chickWeight, aes(Time, Weight) ) +
  geom_bin2d(bins = 200) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
