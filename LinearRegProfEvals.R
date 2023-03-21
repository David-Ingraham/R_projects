install.packages("tidymodels")
install.packages("openintro")
library(tidymodels)
library(openintro)
library(tidyverse)
glimpse(evals)

#This project analyses the performance of regression models  applied
#to a data set of professor evaluation scores and varies traits of the professor.


#1
evals %>% ggplot(aes(x = score)) + geom_bar()
summary(evals$score)
#The bar chart for scores is skewed left, and this is backed up 
#by the summary function which tells us that 75% of the score 
#are above 3.8. Students are likely to give positive scores for 
#professors in general
?evals

#2

evals %>% ggplot(aes(x = score, y = bty_avg)) + geom_point() +
  labs(x = "Score", y = "BTY AVG", title = "Score vs BTY AVG Scaatter")
#3
evals %>% ggplot(aes(x = score, y = bty_avg)) + geom_jitter() +
  labs(x = "Score", y = "BTY AVG", title = "Score vs BTY AVG Jitter")
#the jitter function adds slight randomness to the position of the points
#on the graph. Without this, the discrete nature or the scores makes the
#correlation less apparent 
?geom_jitter

#4 
score_bty_fit <- linear_reg() %>% set_engine("lm") %>% 
  fit(score ~ bty_avg, data = evals)
aug_score_bty_fit <- augment(score_bty_fit$fit)
glimpse(aug_score_bty_fit)
score_bty_fit

#5
evals %>% ggplot(aes(x = score, y = bty_avg)) + geom_jitter() +
  geom_smooth(se = FALSE, color = "orange")
length(evals$score)

#6The linear model tells us that changes in the attractiveness of the 
#professor have a slightly positive impact on that professors rating
#in general. Futhermre, professors 

#7
glimpse(aug_score_bty_fit)
score_bty_fit
sum(evals$score == 0)
#The y intercept of the model is 3.88, meaning that 3.88 is the mean 
#attractiveness of a professor when their rating is zero. This doesn't
#make much sense in this context because zero is ot a possible rating
tidy(score_bty_fit)
#8
glance(score_bty_fit)$r.squared
#R squared for the data is 0.035. This means that 3.5% of the bty_avg
#scores are determined by the score of the professor evaluation

#9
score_gender_fir <- linear_reg() %>% set_engine("lm") %>% 
  fit(score ~ gender, data = evals)

glance(score_gender_fir)
aug_score_gender_fit <- augment(score_gender_fir$fit)
glimpse(aug_score_gender_fit)
tidy(score_gender_fir)
score_gender_fir
#intercept is 4.0928, coefficient is 0.145. So the slope of the best fit
#line is y = 0.145x + 4.0928
#line equation for male teachers is y = 2.78x + 4.23
#line 



#12, #13


#10

#11

evals <- evals %>% 
  mutate(rank_relevel = fct_relevel(rank, "tenure track", "tenured", "teaching"))

score_rank_fit <- linear_reg() %>% set_engine("lm") %>% 
  fit(score ~ rank_relevel, data = evals)

glance(score_rank_fit)
tidy(score_rank_fit)

#The slope intercept of the line for teachers on the tenure track is y = 79.7x + 4.15
#The slope intercept of the line for teachers already tenured is y = 79.5x + 4.09
#The slope intercept of the line for teachers not on the tenure track is y = 79.83x + 4.28





#14 


#14
evals <- evals %>% 
  mutate(tenure_elibile = case_when(
    rank %in% c("teaching") ~ "no",
    rank %in% c("tenure track", "tenured") ~ "yes"))


#15



score_tenure_eligible <- linear_reg() %>% set_engine("lm") %>% 
  fit(score ~ tenure_elibile, data = evals)


glance(score_tenure_eligible)
#The r squared for this model is 0.0115. This model tells us that tenure status
#is a very poor predictor of a professor's rank and the variability of the rank

#is only accounted for by 1.5% of the tenure track status 

 
