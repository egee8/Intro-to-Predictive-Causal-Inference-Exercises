### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

for(i in date.columns) # loops through the "date.columns"
{
  # Find missing values
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  # Replace them by NAs
  foo[which_values_are_missing, i] <- NA
  # Turn values into dates
  foo [, i] <- as.Date(as.character(foo[, i]))
}


# Remove the rows with NA or dates before 2009-01-01

# Only consider projects with non-missing “Circulation.Date” >= 2009-01-01.
with_NAs <- which(is.na(foo$CirculationDate)) 
new_foo <- foo[-with_NAs, ] 

with_date <- which(new_foo$CirculationDate >= "2009-01-01")
new_foo <- new_foo[with_date,] 


#QUESTION 1
#1a - Is the claim true?

diff_dates <- (new_foo$OriginalCompletionDate) - (new_foo$ApprovalDate)
diff_dates

diff_dates_omit <- na.omit(diff_dates)

#1b -Has the length of the project delay changed over time?

#Remove blanks and NAs in OriginalCompletionDates.
which_have_blanks_completion <- which(is.na(new_foo$OriginalCompletionDate)) 
which_have_blanks_completion 
df_revised_completion <- new_foo[-which_have_blanks_completion, ] 

#Calculate the length of project delay.
delay_diff = (df_revised_completion$RevisedCompletionDate) - (df_revised_completion$OriginalCompletionDate)
delay_diff

#Circulations Dates
circulation_dates <- new_foo$CirculationDate
circulation_dates <- circulation_dates[-which_have_blanks_completion]

#Scatterplot of circulation dates. 
plot(df_revised_completion$CirculationDate, delay_diff, main = "Figure 2 - Project Delay Over Time", xlab = "Year", ylab = "Amount of Delay (in days)")

# Circulation dates of projects from 2009 and after
dates_2009 <- which(circulation_dates >= "2009-01-01" & circulation_dates <= "2009-12-31")
dates_2010 <- which(circulation_dates >= "2010-01-01" & circulation_dates <= "2010-12-31")
dates_2011 <- which(circulation_dates >= "2011-01-01" & circulation_dates <= "2011-12-31")
dates_2012 <- which(circulation_dates >= "2012-01-01" & circulation_dates <= "2012-12-31")
dates_2013 <- which(circulation_dates >= "2013-01-01" & circulation_dates <= "2013-12-31")
dates_2014 <- which(circulation_dates >= "2014-01-01" & circulation_dates <= "2014-12-31")
dates_2015 <- which(circulation_dates >= "2015-01-01" & circulation_dates <= "2015-12-31")
dates_2016 <- which(circulation_dates >= "2016-01-01" & circulation_dates <= "2016-12-31")
dates_2017 <- which(circulation_dates >= "2017-01-01" & circulation_dates <= "2017-12-31")
dates_2018 <- which(circulation_dates >= "2018-01-01" & circulation_dates <= "2018-12-31")

# Projects delays from 2009 and after
df_revised_completion.2009 <- delay_diff[dates_2009]
df_revised_completion.2009 <- delay_diff[dates_2009]
df_revised_completion.2010 <- delay_diff[dates_2010]
df_revised_completion.2011 <- delay_diff[dates_2011]
df_revised_completion.2012 <- delay_diff[dates_2012]
df_revised_completion.2013 <- delay_diff[dates_2013]
df_revised_completion.2014 <- delay_diff[dates_2014]
df_revised_completion.2015 <- delay_diff[dates_2015]
df_revised_completion.2016 <- delay_diff[dates_2016]
df_revised_completion.2017 <- delay_diff[dates_2017]
df_revised_completion.2018 <- delay_diff[dates_2018]
df_revised_circulation

# Means of projects from 2009 and after
mean_2009 <- mean(df_revised_completion.2009)
mean_2010 <- mean(df_revised_completion.2010)
mean_2011 <- mean(df_revised_completion.2011)
mean_2012 <- mean(df_revised_completion.2012)
mean_2013 <- mean(df_revised_completion.2013)
mean_2014 <- mean(df_revised_completion.2014)
mean_2015 <- mean(df_revised_completion.2015)
mean_2016 <- mean(df_revised_completion.2016)
mean_2017 <- mean(df_revised_completion.2017)
mean_2018 <- mean(df_revised_completion.2018)

means_of_projects <- c(mean_2009, mean_2010, mean_2011, mean_2012, mean_2013, mean_2014, mean_2015, mean_2016, mean_2017, mean_2018)
means_of_projects

# Medians of projects from 2009 and after
median_2009 <- median(df_revised_completion.2009)
median_2010 <- median(df_revised_completion.2010)
median_2011 <- median(df_revised_completion.2011)
median_2012 <- median(df_revised_completion.2012)
median_2013 <- median(df_revised_completion.2013)
median_2014 <- median(df_revised_completion.2014)
median_2015 <- median(df_revised_completion.2015)
median_2016 <- median(df_revised_completion.2016)
median_2017 <- median(df_revised_completion.2017)
median_2018 <- median(df_revised_completion.2018)

medians_of_projects <- c(median_2009, median_2010, median_2011, median_2012, median_2013, 
             median_2014, median_2015, median_2016, median_2017, median_2018)

dates <- c(as.Date("2009-01-01"), as.Date("2010-01-01"), as.Date("2011-01-01"), as.Date("2012-01-01"), 
           as.Date("2013-01-01"), as.Date("2014-01-01"), as.Date("2015-01-01"), as.Date("2016-01-01"), 
           as.Date("2017-01-01"), as.Date("2018-01-01"))
means_of_projects

plot(dates, means_of_projects, col="hotpink",
     ylim=c(0, 1000), pch=19,
     main="Figure 1 - Change Over Time of Project Delays (2010-2018)",
     ylab="Project Delays (in days)", 
     xlab="Year")
points(dates, medians_of_projects, col="purple", pch=19)

legend(x="topright", c("mean","median"),cex=.8,col=c("hotpink","purple"), pch=c(19, 19))

abline(lm(means_of_projects ~ dates), col="hotpink")
abline(lm(medians_of_projects ~ dates), col="purple")

#25th quantile of project delays from 2009 and onwards
quantile_25_2009 <- quantile(df_revised_completion.2009, 0.25)
quantile_25_2010 <- quantile(df_revised_completion.2010, 0.25)
quantile_25_2011 <- quantile(df_revised_completion.2011, 0.25)
quantile_25_2012 <- quantile(df_revised_completion.2012, 0.25)
quantile_25_2013 <- quantile(df_revised_completion.2013, 0.25)
quantile_25_2014 <- quantile(df_revised_completion.2014, 0.25)
quantile_25_2015 <- quantile(df_revised_completion.2015, 0.25)
quantile_25_2016 <- quantile(df_revised_completion.2016, 0.25)
quantile_25_2017 <- quantile(df_revised_completion.2017, 0.25)
quantile_25_2018 <- quantile(df_revised_completion.2018, 0.25)

quantile_25_2009
quantile_25_2010
quantile_25_2011
quantile_25_2012
quantile_25_2013
quantile_25_2014
quantile_25_2015
quantile_25_2016
quantile_25_2017
quantile_25_2018

# 75th quantile of project delays from 2009 and onwards
quantile_75_2009 <- quantile(df_revised_completion.2009, 0.75)
quantile_75_2010 <- quantile(df_revised_completion.2010, 0.75)
quantile_75_2011 <- quantile(df_revised_completion.2011, 0.75)
quantile_75_2012 <- quantile(df_revised_completion.2012, 0.75)
quantile_75_2013 <- quantile(df_revised_completion.2013, 0.75)
quantile_75_2014 <- quantile(df_revised_completion.2014, 0.75)
quantile_75_2015 <- quantile(df_revised_completion.2015, 0.75)
quantile_75_2016 <- quantile(df_revised_completion.2016, 0.75)
quantile_75_2017 <- quantile(df_revised_completion.2017, 0.75)
quantile_75_2018 <- quantile(df_revised_completion.2018, 0.75)

quantile_75_2009
quantile_25_2010
quantile_75_2011
quantile_75_2012
quantile_75_2013
quantile_75_2014
quantile_75_2015
quantile_75_2016
quantile_75_2017
quantile_75_2018

#1c - How does the original planned project duration differ from actual duration (if the actual duration is measured as the duration between "Approval Date" and "RevisedCompletionDate")


original_duration <- (omit_or_foo$OriginalCompletionDate) - (omit_or_foo$ApprovalDate)
original_duration
avg_or_duration <- mean(original_duration)
avg_or_duration

actual_duration <- (omit_or_foo$RevisedCompletionDate) - (omit_or_foo$ApprovalDate)
actual_duration
avg_act_duration <- mean(actual_duration)
avg_act_duration

diff_duration <- actual_duration - original_duration
diff_duration


mean((diff_duration)/30.4)
median((diff_duration)/30.4)
quantile((diff_duration)/30.4)

hist((as.numeric(diff_duration)/30.4), main = "Figure 2 - Difference of Actual and Planned Project Duration", xlab = "Difference of Duration (in months)", xlim = c(0,110), ylab = "Frequency of Projects", col = "purple")


#QUESTION 2
#What % of projects completed between 2010 and now were rated 0?

#First just isolate the data so it considers only projects with revised completion dates after 2010.

projects_after_2010 <- which(new_foo$RevisedCompletionDate >= "2010-01-01")
new_foo_2010 <- new_foo[projects_after_2010,]

projects_0 <- which(new_foo_2010$Rating == 0)
projects_0

projects_1 <- which(new_foo_2010$Rating == 1)
projects_1

projects_2 <- which(new_foo_2010$Rating == 2)
projects_2

projects_3 <- which(new_foo_2010$Rating == 3)
projects_3

projects_NA <- which(is.na(new_foo_2010$Rating))
projects_NA

total_with_NA = length(projects_0) + length(projects_1) + length(projects_2) + length(projects_3) + length(projects_NA)
total_without_NA = length(projects_0) + length(projects_1) + length(projects_2) + length(projects_3)

#Table summary of percentages

table2 <- data.frame(
  Ratings = c("0", "1", "2", "3", "NA"),
  Number_of_Projects = c(length(projects_0), 
                         length(projects_1), 
                         length(projects_2), 
                         length(projects_3), 
                         length(projects_NA)),
  Percentage_with_NA_included = c(length(projects_0) / total_with_NA * 100,
                                  length(projects_1) / total_with_NA * 100,
                                  length(projects_2) / total_with_NA * 100,
                                  length(projects_3) / total_with_NA * 100,
                                  length(projects_NA) / total_with_NA * 100),
  Percentage_with_NA_excluded = c(length(projects_0) / total_without_NA * 100,
                                  length(projects_1) / total_without_NA * 100,
                                  length(projects_2) / total_without_NA * 100,
                                  length(projects_3) / total_without_NA * 100,
                                  "-"),
  stringsAsFactors = FALSE
)

table2

#Question 3

# Ratings of completed PATA projects from 2010 and onwards
PATA_projects_2010 <- which(new_foo$RevisedCompletionDate >= "2010-01-01" & new_foo$Type == "PATA")
foo_PATA_2010 <- new_foo[PATA_projects_2010,]

PATA_project_0 <- which(foo_PATA_2010$Rating == 0)
PATA_project_1 <- which(foo_PATA_2010$Rating == 1)
PATA_project_2 <- which(foo_PATA_2010$Rating == 2)
PATA_project_3 <- which(foo_PATA_2010$Rating == 3)
PATA_project_NA <- which(is.na(foo_PATA_2010$Rating))

PATA_total_NA = length(PATA_project_0) + length(PATA_project_1) + length(PATA_project_2) + length(PATA_project_3) + length(PATA_project_NA)
PATA_total_without_NA = length(PATA_project_0) + length(PATA_project_1) + length(PATA_project_2) + length(PATA_project_3)

#Table of Percentages for PATA
table3 <- data.frame(
  Ratings = c("0", "1", "2", "3", "NA"), 
  Number_of_Projects = c(length(PATA_project_0), 
                         length(PATA_project_1), 
                         length(PATA_project_2), 
                         length(PATA_project_3), 
                         length(PATA_project_NA)),
  
  Percentage_NA = c(length(PATA_project_0) / PATA_total_NA * 100,
                                  length(PATA_project_1) / PATA_total_NA * 100,
                                  length(PATA_project_2) / PATA_total_NA * 100,
                                  length(PATA_project_3) / PATA_total_NA * 100,
                                  length(PATA_project_NA) / PATA_total_NA * 100),
  
  Percentage_without_NA = c(length(PATA_project_0) / total_wo_NA * 100,
                                  length(which.projects.rated_1) / total_wo_NA * 100,
                                  length(which.projects.rated_2) / total_wo_NA * 100,
                                  length(which.projects.rated_3) / total_wo_NA * 100,
                                  "-"),
  
  stringsAsFactors = FALSE
)
table3


#QUESTION 4

#First sort everything to identify the Top 10% and Bottom 10%

sorted_new_foo <- new_foo[order(-new_foo$RevisedAmount),] #Sorts in descending order
with_NAs <- which(is.na(sorted_new_foo$Rating)) #Identify which projects are rated NAs to remove
sorted_new_foo <- sorted_new_foo[-with_NAs, ] #Remove the projects with NAs

top_10 <- head(sorted_new_foo, nrow(new_foo) * 0.1)
bottom_10 <- tail(sorted_new_foo, nrow(new_foo) * 0.1)

top_10
bottom_10

mean(top_10$Rating)
mean(bottom_10$Rating)

median(top_10$Rating)
median(bottom_10$Rating)

#When just comparing the means of the Top 10% and Bottom 10%, there is not much a difference. So you cannot draw a causal conclusion.
#But what happens when you break it down by characteristic?

summary(top_10$Dept)
summary(bottom_10$Dept)

summary(top_10$Division)
summary(bottom_10$Division)

summary(top_10$Country)
summary(bottom_10$Country)

summary(top_10$Cluster)
summary(bottom_10$Cluster)

