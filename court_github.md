```{r set-options, echo=FALSE, cache=FALSE}
options(width = 800)
```

---
title: "Supreme Court Analysis"
author: "Alan Ward"
output: 
  html_document:
    keep_md: true

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Intro and TL;DR

This is some analysis of data on U.S. Supreme Court Justices, spanning 1937-2015. Source: Kaggle, via Abigail Larion. Original data from Supreme Court Database, with scores calculated by Andrew Martin (U Michigan) and Kevin Quinn (UC Berkeley)

Caveat: everything that follows is exploratory. It was lots of fun to do, but don't take it too seriously. 

This analysis is divided into four parts:

1. Do Supreme Court trends change depending on which party is in office? **Answer**: *no, they do not*

2. Where do judges fall on the liberal-conservative spectrum? **Answer**: *pretty much all over the place!*

3. My partner is really into Ruth Bader Ginsburg. How does her historical trend look relative to other current justices? **Answer**: *The Notorious RBG exhibited the greated change in ideological position of any current court member, going from moderate to very liberal*

4. Can you divide all the justices into groups? **Answer**: *Yes, I divide them into seven groups that span the ideological spectrum*

## Data Preprocessing
What does the dataset look like? *I'll also load packages that will be useful later*

```{r warning = FALSE, error = FALSE, message = FALSE}
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(lsr)
library(car)

library(ggthemes)
library(grid)

ideology <- read.csv("C:/Users/alan/Documents/R/Scripts and projects/Kaggle/Datasets/Supreme Court Ideology/scores.csv")

dim(ideology)
str(ideology)
head(ideology, n = 2)
tail(ideology, n = 2)

```

The data are clean but incomplete for my purposes. First, I want to make a column indicating whether a given year corresponds to a democratic or republican presidency. My assumption is that a party is in power when a member of that party takes office and ends on the year when the other party wins office. 

In other words: 2016 would be Democratic, 2017 would be Republican.

```{r}
dem_years <- c(1937:1952,1961:1968, 1977:1980, 1993:2000, 2009:2015) #via Wikipedia

ideology$president <- ifelse(ideology$court_term %in% dem_years, ideology$president <- 0, ideology$president <- 1)

```

Next I want to make a column indicating whether they were appointed by a Democratic or Republican president. For the sake of this analysis, I'm assuming that a judges' first term corresponds with the rule of the party that nominated him/her.

Observers of American's current hyper-polarized political system might be surprised that there have been presidents who nominated judges affiliated with the opposing party, Yes, you're allowed to do that. 

```{r}
#number of years on the bench - might be useful later
ideology <- ideology %>%
  arrange(justice_name) %>%
  group_by(justice_name) %>%
  mutate(n_years = n())

#sets up column for year of appointment
ideology <- ideology %>%
  group_by(justice_name) %>%
  mutate(appoint_year = min(court_term)) 

#sets up a column for party of appointment. 0 means democrat, 1 means republican
ideology$appoint_party <- ifelse(ideology$appoint_year %in% dem_years, "D", "R")

#need to manually input part of appointment of justices in 1937. Not going to change the
#appointment year for the sake of this analysis
manual_change <- ideology %>%
  select(justice_code, justice_name, appoint_year, appoint_party) %>%
  group_by(justice_name) %>%
  filter(appoint_year == 1937) 

manual_change_names <- unique(as.character(manual_change$justice_name))
manual_change_names #these are the justices whose appoint_year is listed as 1937

#manually shifting justices 1,2,3,4,8,9 to "R". Note that some Democrat judges
#were nominated by Republicans. Column only reflects president who nominated
#them. Info gathered by Wikipedia

ideology <- mutate(ideology, appoint_party = replace(appoint_party, justice_code %in% c(71,72,74,75,76,77), "R"))
```

## Analysis
### Do Supreme Court trends change depending on which party is in office?

Time for some analysis. First let's try to compress a lot of historical data into one chart. The x-axis corresponds to the year and the y-axis corresponds to the court's mean ideology for that year. The lower the score, the more liberal the justice. For more on the scoring, click [here](http://mqscores.berkeley.edu/).

The size corresponds to the mean justice *deviation* within that year. Finally, the color represents the party in power. 

```{r}
ideology %>%
  group_by(court_term) %>%
  summarize(court_mean = mean(posterior_mean), president = mean(president), deviation = mean(standard_deviation)) %>%
  ggplot(aes(x = court_term, y = court_mean, size = deviation)) + 
  geom_point(aes(col = factor(president)), alpha = 0.5) +
  scale_color_manual(values = c("royalblue", "red"), guide = "none") + 
  labs(title = "Ideology and Variance Over Time", x = "Year", y = "Ideology", size = "Deviation", col = NULL) + 
  theme_hc() + 
  theme(plot.title = element_text(hjust = 0.5))
```

To make things more interesting I'm adding lines corresponding to how Democratic-appointed justices change over time vs how Republican-appointed justices change. 

```{r}
republican_hist <- ideology %>%
  group_by(court_term) %>%
  filter(appoint_party == "R") %>%
  summarize(court_mean = mean(posterior_mean), president = mean(president), deviation = mean(standard_deviation))
  
democrat_hist <- ideology %>%
  group_by(court_term) %>%
  filter(appoint_party == "D") %>%
  summarize(court_mean = mean(posterior_mean), president = mean(president), deviation = mean(standard_deviation))

#same chart, but with additional lines for Republican and Democratic sentiments

ideology %>%
  group_by(court_term) %>%
  summarize(court_mean = mean(posterior_mean), president = mean(president), deviation = mean(standard_deviation)) %>%
  ggplot(aes(x = court_term, y = court_mean, size = deviation)) + 
  geom_point(aes(col = factor(president)), alpha = 0.5) +
  scale_color_manual(values = c("royalblue", "red"), guide = "none") +
  geom_line(data = republican_hist, size = 1, col = "dark red", alpha = 0.5) +
  geom_line(data = democrat_hist, size = 1, col = "dark blue", alpha = 0.5) + 
  theme_hc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ideology and Variance Over Time", x = "Year", y = "Ideology", size = "Deviation", col = NULL)
```

The craziness of all this hints that there won't be a clear relationship between the party that controls the White House and Supreme Court ideology. In principle, we could run a **t-test** comparing Democratic years vs Republican years...

```{r}
dem_years <- filter(ideology, president == 0) %>%
  group_by(court_term) %>%
  summarize(dem_year_mean = mean(posterior_mean), dem_year_sd = sd(standard_deviation))
  
rep_years <- filter(ideology, president == 1) %>%
  group_by(court_term) %>%
  summarize(rep_year_mean = mean(posterior_mean), rep_year_sd = sd(standard_deviation))
```

But since we're running an independent t-test - in other words, we're comparing two independent group means (mean score in democratic years vs. mean score in republican years), we need to run Levene's Test in order to be satisfied that there is a homogeneity of variance between the two groups. 

In other words, the t-test won't work if the groups have significantly different levels of variance.

Is the t-test the most appropriate test to run here? Probably not. It's good enough for now, though. What this dataset represents are two samples (Democrat years, Republican years) taken from a broader population (total Supreme Court years) whose mean and variance are unknown. Given this, running a t-test isn't insane. 

```{r}
y <- c(dem_years$dem_year_mean, rep_years$rep_year_mean)
group <- as.factor(c(rep(1, length(dem_years$dem_year_mean)), rep(2, length(rep_years$rep_year_mean))))
leveneTest(y, group)
```

Crud. The fact that the test is significant means that we reject the null hypothesis that any differences in variation between the groups is due to chance. This means that the variance occurring in Democratic years is quite different from that in Republican years. 

This is a potentially interesting point that I won't address here. Sorry. 

If this were a matter of life or death, we'd use Welsh's Procedure or use a non-parametric test. Since today is Valentine's Day and I want to spend some time with my partner (more on her later), let's just pretend we forgot to run Levene's Test and run the t-test anyway.

```{r}
comparison_by_party <- t.test(dem_years$dem_year_mean, rep_years$rep_year_mean)
comparison_by_party

```

Nope! The p-value is over 0.05 and the confidence interval spans across 0. This implies that whatever difference in means does occur could plausibly be due to chance. The Cohen's D statistic wasn't that impressive, anyway. 

So let's move on!


### Where do judges fall on the liberal-conservative spectrum?
First let's see what the spectrum looks like. Then let's focus on the current (early 2017) justices. 

```{r}
ideology_ranked <- ideology %>%
  group_by(justice_name) %>%
  summarize(ind = mean(posterior_mean), first_year = first(court_term), last_year = last(court_term), party = first(appoint_party), spread = max(posterior_mean) - min(posterior_mean)) %>%
  arrange(ind) %>%
  mutate(hist_rank = rank(ind))
```

The purple line corresponds to the median justice rank, i.e., rank 22.5 / 45. Democrat-nominated justices are blue, Republican-nominated ones are red. Down is liberal, up is conservative.

```{r}
ggplot(ideology_ranked, aes(x = hist_rank, y = ind, fill = party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("royalblue", "red")) + 
  geom_vline(xintercept = 22.5, color = "purple") +
  theme_hc() + 
  labs(title = "Supreme Court Justice Ideological Spectrum", x = "Rank", y = "Ideology") +
  scale_y_continuous(limits = c(-5,5)) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
```  

Interestingly, there are quite a few judges appointed by Democrats who are conservative and judges appointed by Republicans who are liberal. Here's a list of each:

```{r}
democratic_conservatives <- filter(ideology_ranked, ind > 0, party == "D")
republican_liberals <- filter(ideology_ranked, ind < 0, party == "R")
democratic_conservatives
republican_liberals
```

Now let's focus on current justices and their rank relative to their predecessors.

```{r warning = FALSE}
#Which correspond to current justices?
filter(ideology_ranked, last_year == 2015)

#dropping Scalia, deceased in 2016
ideology_ranked_current <- filter(ideology_ranked, last_year == 2015 & !(hist_rank == 42))

#current justices - dropped Scalia
ggplot(ideology_ranked, aes(x = hist_rank, y = ind, fill = ifelse(last_year == "2015" & !(hist_rank == 42), "purple", "grey"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey", "purple")) + 
  geom_vline(xintercept = 22.5, color = "purple") +
  theme_hc() + 
  labs(fill = "Rank", title = "Supreme Court Justice Ideological Spectrum", x = "Rank", y = "Ideology") + 
  scale_y_continuous(limits = c(-5,5)) + 
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
```  

And finally, a graph of current justices, from most liberal to most conservative. The dashed line represents the mean ideology score of the court in 2015.

```{r warning = FALSE}
#bar graph of current justices
ideology_ranked_current$current_rank <- 1:8

ggplot(ideology_ranked_current, aes(x = reorder(justice_name, hist_rank), y = ind, fill = ifelse(party == "D", "royalblue", "red"))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "royalblue")) +
  theme(legend.position="none") +
  geom_hline(yintercept = mean(ideology_ranked_current$ind), linetype = "dashed") + 
  theme_hc() + 
  labs(title = "Current Individual Ideological Spectrum", x = NULL, y = "Ideology") +
  theme(axis.text.x = element_text(size = 6)) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
```

Three notes before moving on. First, remember that this bar graph represents the **average** ideology of each justice over the course of their tenure, not a reflection of where they stand today. I'll get to that in the next section. 

Second, and on the same note, you may wonder why the average here is above zero whereas the average in the historical graph (above) is below zero. That's because the first graph gives a snapshot of the average ideology of each year, uncontaminated by previous years. The bar chart shows the the average ideology of the current justices. In other words, 2015 was a "liberal" year (because it was below 0) but the current set of justices, taken as a whole over their tenures, have been very slightly conservative. 

Third, Justice Thomas is clearly very conservative. My partner passed the bar exam in California and is an avid court watcher. when I reported the results to her, she suggested that Thomas' mega-conservatism may not be as decisive as it appears. 

According to her on contentious issues, Thomas is never the decisive vote - in other words, he, Alito, and (until recently) Scalia could just be counted on to be interchangeably conservative. It's on *other* opinions where Thomas takes incredibly conservative positions. 

My partner may be right or may be wrong. In fairness, we were having some wine while discussing it. My partner is very much not a conservative, and in fact is a huge Ruth Bader Ginsburg (or [RBG](http://notoriousrbg.tumblr.com/), for the uninitiated) fan. So in the next section I'll check out some trends and see where everyone's favorite tiny justice stands in relation to her peers. 


### How does RBG's historical trend look relative to other current justices?
As you can see from this line graph, RBG has become significantly more liberal over the course of her tenure. I bolded her line to make it stand out. 

```{r}
ideology_current <- filter(ideology, justice_name %in% ideology_ranked_current$justice_name)

ggplot(ideology_current, aes(x = court_term, y = posterior_mean)) +
  geom_line(data = subset(ideology_current, justice_code == 106), col = "red4") + #Kennedy
  geom_line(data = subset(ideology_current, justice_code == 108), col = "red") + #Thomas
  geom_line(data = subset(ideology_current, justice_code == 109), col = "dodgerblue2", size = 1.25) + #Ginsburg
  geom_line(data = subset(ideology_current, justice_code == 110), col = "darkslateblue") + #Breyer
  geom_line(data = subset(ideology_current, justice_code == 111), col = "red3") + #Roberts
  geom_line(data = subset(ideology_current, justice_code == 112), col = "red2") + #Alito
  geom_line(data = subset(ideology_current, justice_code == 113), col = "dodgerblue") + #Sotomayor
  geom_line(data = subset(ideology_current, justice_code == 114), col = "dodgerblue4") + #Kagan
  theme_hc() +
  labs(title = "Current Individual Trends - RBG Bolded", x = "Year", y = "Ideology") +
  scale_y_continuous(limits = c(-4,4)) +
  theme(plot.title = element_text(hjust = 0.5))
```  

In fact, it looks like Ginsburg had the most dramatic shift in any direction of any of the current justices:

```{r}
ideology_ranked_current %>%
  select(justice_name, spread) %>%
  arrange(desc(spread))
```
Silliness aside, this suggests that the recent infatuation with RBG is not merely the result of an increased popularity of social media or random chance. "Early" RBG was not the liberal that she is today. 

This presents interesting classification problems. Should we consider RBG "radical"? If we look at her current (2015) ideological ranking, the claim isn't ridiculous. But if we look at her body of work as a whole since becoming a Supreme Court Justice, the label doesn't seem as appropriate as it would be for, say, Justice Thomas. 

And speaking of clustering, let's move on to the last piece of analysis...


### Can you divide all the justices into groups?

Let's do this thing. And let's do it with with the kmeans() function. 

First, here is a graph of all the justices, with the x-axis representing ideology (from more liberal to more conservative) and the y-axis representing their average deviation from the norm throughout their tenure. Blue is for Democrat-nominated justices, red is for Republic-nominated ones. The size of the dot represents the number of years the justice served.

```{r}
ideology %>%
  group_by(justice_name) %>%
  summarize(avg = mean(posterior_mean), dev = mean(standard_deviation), num = n(), party = first(appoint_party)) %>%
  ggplot(aes(x = avg, y = dev, size = num, col = party)) +
  geom_point(alpha = 0.3) + 
  scale_color_manual(values = c("royalblue", "red")) + 
  theme_hc() + 
  scale_x_continuous(name = "Ideology", limits = c(-5,5)) + 
  scale_y_continuous(name = "Deviation from Peers") +
  labs(title = "Individual Justices by Party", col = "Party", size = "Years Served") + 
  theme(plot.title = element_text(hjust = 0.5))
```

One very important point here: the deviation represens their deviation from the courts when *they* served. Why does this matter? Because someone who might be a weirdo in one time period might be totally normal in another. 

In other words, the y-axis represents the "maverick"ness of a judge during the time in which s/he served.

Another point I have to cop to: the size of the bubbles will only be accurate for judges that started their tenure from 1938 onwards. I don't have the data to accurately represent the number of years served by the judges that served in 1937. As far as this dataset is concernerd, they each started their tenure in that year. 

Now let's get clustering. Following a common rough rule of thumb, the number of clusters should be (1) an odd number, and (2) approximately the square root of the number of observations. Since there are 45 judges, let's go with seven. I think this ends up working out nicely. 

```{r}
#clustering without president or appoint_party
ideology_cluster <- ideology %>%
  group_by(justice_name) %>%
  summarize(avg = mean(posterior_mean), dev = mean(standard_deviation), num = n(), party = first(appoint_party))

vec.posterior_mean <- scale(as.vector(ideology_cluster$avg))
vec.standard_deviation <- scale(as.vector(ideology_cluster$dev))
vec.n_years <- scale(as.vector(ideology_cluster$num))

cluster_ideology <- as.data.frame(cbind(vec.posterior_mean, vec.standard_deviation))


set.seed(1)
ideology_7_clusters <- kmeans(cluster_ideology, centers = 7, nstart = 15)
ideology_cluster$seven_clusters <- as.factor(ideology_7_clusters$cluster)

#The initial plot needs work, so I'm not printing it
#ggplot(ideology_cluster, aes(x = avg, y = dev, size = num, col = #seven_clusters)) +
#  geom_point(alpha = 0.6) + 
#  scale_color_manual(values = c("deepskyblue4", "orchid4", "red3", #"deepskyblue1", "red", "royalblue1","firebrick4")) +
#  theme_hc() 

#renaming clusters
levels(ideology_cluster$seven_clusters) <- c("Liberal Compromisor", "Placid Moderate", "Conservative Agitator", "Liberal Renegade", "Conservative Renegade", "Liberal Agitator", "Conservative Compromisor")
                                           
ggplot(ideology_cluster, aes(x = avg, y = dev, size = num, col = seven_clusters)) +
  geom_point(alpha = 0.6) + 
  scale_color_manual(values = c("deepskyblue4", "orchid4", "red3", "deepskyblue1", "red", "royalblue1","firebrick4")) +
  theme_hc() + 
  scale_x_continuous(name = "Ideology", limits = c(-5,5)) + 
  scale_y_continuous(name = "Deviation from Peers") +
  scale_size_continuous(guide = FALSE) +
  labs(title = "Individual Justices by Group", col = NULL, size = "Years Served") +
  theme(plot.title = element_text(hjust = 0.5))
  
```

Based on the algorithm, I think it makes sense to think of the justices as falling along the following spectrum:

1. Liberal renegade
2. Liberal agitator
3. Liberal compromisor
4. Placid moderate
5. Conservative compromisor
6. Conservative agitator
7. Conservative renegade

As expected, there are far more compromisors and moderates than there are renegades and agitators. Because the scores represent the averages for each justice, the questions regarding classification that I mentioned earlier once again become relevant. 

For instance, how is Ginsburg classified here?

```{r}
#Where is Ginsburg?
filter(ideology_cluster, justice_name == "Ruth Bader Ginsburg")
```

Here she's a **liberal compromisor**. But that doesn't seem to do justice (ha) to her recent move toward more extreme liberalism. 

Incidentally, if you're a very liberal-leaning person (like my partner), you may want to figure out who the heck is that weirdo in the upper-left corner. 

```{r}
filter(ideology_cluster, avg < -3)
```

That's William O. Douglas, who is apparently more to the left than Karl Marx, and just as much of a rabble-rouser. Crucially, this isn't some random guy who sat on the court for two months and died. He has apparently sat on the court for *thirty-eight years*. 

There is of course a better than epsilon chance that there's more to the story than that, but for now I'll leave you with the full table of justices, where they stand, and the category to which they were assigned.

Thanks for reading! This was a fun mini-project.

```{r}
print(ideology_cluster[,1:5], n = 45)
print(ideology_cluster[,c(1,6)], n = 45)
```
