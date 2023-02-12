install.packages("scales")
library(scales)
install.packages("fivethirtyeight")
library(fivethirtyeight)
library(tidyverse)
library(dplyr)
?college_recent_grads
glimpse(college_recent_grads)
college_recent_grads %>% arrange(unemployment_rate)
college_recent_grads %>% arrange(unemployment_rate) %>% select(major, unemployment_rate, rank)
college_recent_grads %>% arrange(unemployment_rate) %>% select(major, unemployment_rate, rank) %>% mutate(unemployment_rate = percent(unemployment_rate))
#1.
college_recent_grads %>% arrange(desc(sharewomen)) %>% select(major, sharewomen, total) %>% top_n(3)

college_recent_grads %>% group_by(major) %>% arrange(desc(median)) %>% select(major, median, p25th, p75th)
#How do the distributions of median income compare across major categories?
#2. We use the median instead of the mean to describe income because the median is more resistant to outliers, and income can have wild outliers

ggplot(data = college_recent_grads, mapping = aes(x = median)) + geom_histogram() + stat_bin(bins = 5000) + labs(title = "5000 Bin Width")
ggplot(data = college_recent_grads, mapping = aes(x = median)) + geom_histogram() + stat_bin(bins = 1000) + labs(title = "1000 Bin Width")       
#3. Both bin levels seems to give the same result

college_recent_grads %>%
  summarise(min = min(median), max = max(median),
            mean = mean(median), med = median(median), sd = sd(median),
            q1 = quantile(median, probs = 0.25),
            q3 = quantile(median, probs = 0.75))
#{
#4. The first quantile describes most of the data, so it would be the most useful stat in describing it  
#Most of the data falls within 1 standard deviation 
#Based on the shape of the histogram you created in the previous exercise, 
#determine which of these summary statistics is useful for describing the distribution.
#Write up your description (remember shape, center, spread, any unusual observations) 
#and include the summary statistic output as well. 
#}

#5. 
ggplot(data = college_recent_grads, mapping = aes(x = median)) + geom_histogram() + stat_bin(bins = 1000) + labs(title = "1000 Bin Width") + facet_wrap(~major_category)       
#??? Question 4,, Now that we’ve seen the shapes of the distributions of median incomes for each major category, 
#we should have a better idea for which summary statistic to use to quantify the typical median income. ???


#6
#Which major category has the highest avg median income? 
college_recent_grads %>% group_by(major_category) %>% summarise(mean = mean(median)) %>% arrange(desc(mean))
#Engineering has the highest mean median income for the major category 

#7 
college_recent_grads %>% count(major_category)
glimpse(college_recent_grads)

stem_categories <- c("Biology & Life Science", "Computers & Mathematics",
                     "Engineering", "Physical Sciences")
college_recent_grads <- college_recent_grads %>%
  mutate(major_type = ifelse(major_category %in% stem_categories, "stem", "not stem"))
glimpse(college_recent_grads)

#8.
college_recent_grads %>% filter(
  major_type == "stem", median <= 36000) %>% select(major, median, p25th, p75th) %>% arrange(desc(median))

college_recent_grads %>% filter(sharewomen > 0.5) %>% select(sharewomen, major) %>%  arrange(desc(sharewomen))

#9 

ggplot(data = college_recent_grads, mapping = aes(x = sharewomen, y = median,
      color = major_type)) + geom_point() + 
      labs(title = "Median Incomes of stem/nonStem majors and its share of women ",x = "Proportion of Women in Major",
           y = "Median Income", color = "STEM Status") 


#10 Is there a bias in the sample size with respect to median income?

ggplot(data = college_recent_grads, mapping = aes(x = sample_size, y = median, color = major_type)) + geom_point() +
  labs(x = "Sample Size", y = "Median Income")

college_recent_grads %>% count(major_category %in% stem_categories)
#We have 109 non stem majors and 64 stem majors being sampled, so by the law of big numbers,
#we can expect the median income of non stem majors to be closer to the median of the rest of the
#dataset
nrow(college_recent_grads) == 109 + 64
college_recent_grads %>% count(major)

tally(college_recent_grads)
count(college_recent_grads)
group_by(college_recent_grads) == college_recent_grads
ungroup(college_recent_grads) == college_recent_grads
