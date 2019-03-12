# Objective
# The goal of this lab is to get familiar plotting continuous data,
# working with more complicated data structures, and running different types of T-tests.

# Example: T-tests

#In the following example, researchers wanted to know how smoking affected sprint times.
# They measured 50 meter sprint times for randomly sampled women. For the "before smoking" trials,
#each woman ran the 50-m dash and her time was recorded. 
#Then, the researchers had each woman smoke tobacco. They measured the womeS
#Youll see that there are 10 women, coded as A through J. Each woman is timed before smoking and  after smoking.
  

sprints <- data.frame(
  "Person" = rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),2),
  "Trial" = c(rep("Before", 10), rep("After", 10)),
  "Time" = c(10.4,  8.8,  9.9,  9.5,  9.2,  9.6,  9.2,  10.9,  9.5,  8.5,
             11.9, 8.9, 8.9, 10.7, 11.2,  9.9,  9.5, 10.8, 10.5, 9.5))
head(sprints)
sprints

#Let's run a T-test to compare whether running times are different before and after smoking. 
#In the line of code below, I am using a different format than you've seen before to run the test. Instead of giving `t.test` two separate vectors to compare, I use `Time ~ Trial` to denote that my response (dependent) variable is "Time" and my explanatory (indepedent) variable is "Trial".
#To make sure that the paired test is carried out correctly, the subjects need to be in the same order in the "After" group and in the "Before" group, so that R knows which data points are linked. Remember, for the paired t-test, the difference between Before and After is computed, and that is what is compared against the null distribution. 
#for an unpaired test, the order of data does not matter.
#Another way of formatting data would be to have one column for the Before group, and another column for the After group; then you can run the T test using two separate vectors, as we did in last week's lab: `t.test(dataframe$Group1, dataframe$Group2, paired = TRUE)`. 


sprint.t.test.paired <- t.test(Time~Trial, data = sprints, paired=TRUE, alternative = "two.sided")
sprint.t.test.paired
sprint.t.test.unpaired <- t.test(Time~Trial, data = sprints, alternative = "two.sided")
sprint.t.test.unpaired


#How do the results of the paired t-test compare to the unpaired t-test? Which one is appropriate?}
#The paired t-test yields a p-value of 0.049, while the unpaired t-test yields a p-value of 0.125. In this case, we should use a paired t-test, since we are comparing the difference within the same subjects before and after an event.*


#Notice that the t-test output doesn't tell us what trial (before vs. after) was faster. To figure this out, let's get the average of the before and after groups:


Mean.Per.Trial <- aggregate(Time~Trial, data = sprints, FUN = mean)
Mean.Per.Trial


#We can see that the mean time for the "after" trial was larger, therefore subjects ran slower, on average, after using tobacco.


#What are the assumptions of the t-test?}
#*1. Both samples are independent, random samples.*
#*2. Both populations have normal distributions.*
#*3. The variance of both popualations are equal.*

#Let's make a plot of the data. First, let's get the mean and standard deviation for each group

# get the means of each group (Before and After)
Mean.Per.Trial <- aggregate(Time~Trial, data = sprints, FUN = mean)
Mean.Per.Trial

# get the standard deviation of each group (Before and After)
SD.Per.Trial <- aggregate(Time~Trial, data = sprints, FUN = sd)
SD.Per.Trial
```


#By now these plotting commands should look familiar:

{stripchart(Time ~ Trial, data = sprints, method = "jitter", 
vertical = TRUE, pch = 16, col = "forestgreen", 
ylab = "50 m sprint time (s)", xlab = "Before or After Smoking", 
main = "Effect of Smoking on Sprint Times", ylim = c(0, 15))

segments(x0 = c(1,2) , x1 = c(1,2) ,
y0 = Mean.Per.Trial$Time-SD.Per.Trial$Time,
y1 = Mean.Per.Trial$Time+SD.Per.Trial$Time)

points(Mean.Per.Trial$Time ~ c(1,2), pch = 16, cex = 1.2)}


# Lab Dataset: Pigeon Picassos
#This example dataset comes from one of the more unusual studies of the past few years. In this study, S. Watanabe, an expert on animal cognition, measured whether pigeons could discriminate between good and bad artwork. The theory is that the ability to appreciate good art is an evolutionary advantageous trait that comes from cultural conditioning. In other words: art appreciation is so built into animals' DNA, even a pigeon can do it.

#To measure whether pigeons can be taught to appreciate good artwork, Watanabe first showed pigeons a variety of artwork, and gave them rewards when they pecked on a picture classified as "good". Then, he gave them new pictures that they had never seen before: 5 drawings that were determined to be "good" and 5 drawings that were determined to be "bad". The drawings were taken from school children, and were classified as either "good" or "bad" by adults. He measured their relative rates of pecking on those drawings to see if the pigeons had been trained to discriminate good artwork from bad. This experiment was repeated for 4 pigeons. 

#For more details on this experiment, including what the authors concluded, and how their experiments were run, see Watanabe et al. posted on D2L. We are focusing on Experiment 1 and you should read the methods and results for Experiment 1 to evaluate their experimental design.

#\textcolor{red}{\textbf{Question 3 (0.5 pts):} Read in the dataset "pigeons.csv" from D2L. Take a look at the structure of the data and think back to the experimental background you were given. Are the data points independent? Why or why not?}

Pigeons <- read.csv("Pigeons.csv")
Pigeons

#*These data points are not independent (e.g. the response depends on the specific pigeon, for which we have multiple data points per pigeon). This is an example of pseudoreplication, which we must amend for.*
  
  
#\textcolor{red}{\textbf{Question 4 (1 pt):} What is the explanatory variable in this study? What is the response variable? What kinds of variables are they?}

#Explanatory variable: Artwork quality (good vs. bad). This is a categorical variable.*
 # *Response variable: Pecking rate. This is a continuous quantitative variable.*
  
  
  #The `aggregate` function, demonstrated previously, can be used to get the average pecking rate of each pigeon or could be used to get the average rate of pecking on good vs. bad artwork. 

#However, to make our datapoints independent, we want to make sure that we get the *average pecking rate for each pigeon*, but we want to do this separately for good artwork vs. bad artwork. In other words, we want the mean of pigeon 1 on good artwork and the mean of pigeon 1 on bad artwork. 

#To do this, we can put both arguments, `Pigeon` and `Painting` after the `~` in the `aggregate` function. 

#For instance: `aggregate(SprintTime ~ Person + Treatment, data = runtimes, FUN = mean)` would take the average sprint time for each runner if they were measured more than once, but it would do so separately for each treatment. This is an important step when you have paired data, since each subject is present in both treatment groups.

#\textcolor{red}{\textbf{Question 5  (1 pt):} Calculate the mean pecking rate for each pigeon on good artwork and bad artwork, using the `aggregate` function. Save this as a new dataframe. Hint: your data should have 8 rows if you have done this correctly.}

Aggregated.Pigeons <- aggregate(Response~Pigeon + Painting, data=Pigeons, FUN=mean)
Aggregated.Pigeons
aggregate(Response~Painting, data=Aggregated.Pigeons, FUN=mean)


#*Mean pecking rate for good artwork here: 0.3805*
 # *Mean pecking rate for bad artwork here: 0.1805*
  
  
 # \textcolor{red}{\textbf{Question 6  (0.5 pts):} The researcher wanted to know whether pigeons demonstrate (through pecking) an ability to detect good artwork. State the null and alternative hypotheses.}

#*Null: There is no difference in pecking rates between "good" and "bad" artwork (difference in means equals 0)*
 # *Alternative: There is a difference in pecking rates between "good" and "bad" artwork (difference in means does not equal 0)*
  
  
#  \textcolor{red}{\textbf{Question 7 (1 pt):} Run the appropriate T-test to evaluate the hypothesis. Report your P-value and test-statistic. Justify whether you used a two-tailed vs. one-tailed, and a paired vs. unpaired T-test.}


t.test(Response~Painting, data = Aggregated.Pigeons, paired=TRUE, alternative = "two.sided")


#*P-value: 0.007416, t-statistic: -6.4934.*
 # *I ran a paired t-test because we're comparing the difference in pecking rates between each individual pigeon (we're using the same pigeons in both trials.)*
  
  
 # \textcolor{red}{\textbf{Question 8 (1 pt):} Can pigeons discriminate between good and bad art? Revisit the null and alternative hypotheses, interpret your P-value, and explain and what the data indicate about pigeon's art critic skills. In your answer, make sure you explain what exactly the P-value is (e.g. what is it the probability of?)}
    
    #*Our paired t-test yielded a p-value of 0.007416, and a t-statistic of -6.4934 (df=3). This means that we reject the null hypothesis, and that the mean pecking rate is signficantly higher for "good" artwork than "bad" artwork.*
    
  
  #  \textcolor{red}{\textbf{Question 9 (1 pt):} Create a stripchart with standard deviation bars to show the difference in pecking rates on good vs. bad artwork.}
    

    Mean.Per.Trial.Birds <- aggregate(Response~Painting, data=Aggregated.Pigeons, FUN=mean)
    Mean.Per.Trial.Birds
    
    # get the standard deviation of each group (Before and After)
    SD.Per.Trial.Birds <- aggregate(Response~Painting, data=Aggregated.Pigeons, FUN=sd)
    SD.Per.Trial.Birds
    
    
    {stripchart(Response ~ Painting, data = Aggregated.Pigeons, method = "jitter", 
    vertical = TRUE, pch = 16, col = "forestgreen", 
    ylab = "Pecking rate", xlab = "Art classification", 
    main = "Picasso Pigeons?", ylim = c(0, 0.6))
    
    segments(x0 = c(1,2) , x1 = c(1,2) ,
    y0 = Mean.Per.Trial.Birds$Response-SD.Per.Trial.Birds$Response,
    y1 = Mean.Per.Trial.Birds$Response+SD.Per.Trial.Birds$Response)
    
    points(Mean.Per.Trial.Birds$Response ~ c(1,2), pch = 16, cex = 1.2, col="darkorange1")}

    
    
    # Review questions
    #A particular strain of bacteria is used for nitrogen fixation on a certain variety of alfalfa. A scientist claims that the mean amount of nitrogen fixed in the plant is at least 26.7 mg. (USE A TWO-TAILED TEST.)
    
    #He samples 12 plants and measures the rate of fixation. His data are: 23.8, 26.2, 27.9, 22.2, 24.4, 25.8, 25.6, 29.1, 26.6, 26, 24.9, 23.3
    
    #\textcolor{red}{\textbf{Question 10 (1 pt):} What test should he use to analyze his data? Give the null and alternative hypotheses.}
    
    #*He should a run a one-sample t-test.*
    #*Null hypothesis: The mean amount of nitrogen fixed is 26.7 mg or greater.*
    #*Alternative hypothesis: The mean amount of nitrogen fixed is less than 26.7 mg.*
    
    
    #\textcolor{red}{\textbf{Question 11 (1 pts):} Run the appropriate statistical test and interpret the outcome.}
    
  
    nitrogen.data <- c(23.8, 26.2, 27.9, 22.2, 24.4, 25.8, 25.6, 29.1, 26.6, 26, 24.9, 23.3)
    t.test(nitrogen.data, mu=26.7, alternative = "two.sided")
    sd(nitrogen.data)
    
    #*P-value: 0.05148, t-statistic: -2.1842, df=11, mean=25.48 ± standard deviation of 1.9296. Since our p-value >0.05, we retain the null. The mean amount of nitrogen fixed is 26.7 mg or greater.*
    
    
   # A political research group wants to know whether education level influences the political party affiliation of voters in Connecticut. They randomly sample 1000 voters from across the state, and classify them as having either (1) less than high school degree (2) high school degree or GED  (3) college degree (4) post-graduate degree. Then, they ask whether they affiliate as a Republican, Democrat, or neither. 
    
    #\textcolor{red}{\textbf{Question 12 (0.5 pts):} What statistical test would be used to analyze this data? Give the null and alternative hypotheses.}
    
    #*Chi-squared contigency test.*
    #*Null hypothesis: Education level and political party affiliation are independent of each other.*
    #*Alternative hypothesis: Education level and political party affiliation are dependent on each other.*
    
    