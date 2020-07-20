# Factors affecting campus placemnt
# Part 1: Focus on placement status, work experience, and gender

library(ggplot2)
library(readr)
library(patchwork)
placement <- read.csv("~\\R\\win-library\\4.0\\My R Projects and Files\\Kaggle - Campus Placement\\Placement_Data_Full_Class.csv")
View(placement)
attach(placement)

# The first variable in the placement dataset is sl_no, ranging from 1..215, 
# and does not contribute to the information in the data. 
# So let's delete that column - remains 14 variables we can work 
# 6 - numerical & 8 - categorical

# placement <- placement[,-1]

# To check variable names and data types
names(placement)
class(gender)
str(placement)

# Check which columns have missing values, if any
colSums(is.na(placement))

# Remark: Only the salary column has missing values (67) for those students who were not placed.
# Verify with the following.
summary(status)

# Or
table(status)



# Set up as factors
gender <- as.factor(gender)
ssc_b <- as.factor(ssc_b)
hsc_b <- as.factor(hsc_b)
degree_t <- as.factor(degree_t)
workex <- as.factor(workex)
specialisation <- as.factor(specialisation)
status <- as.factor(status)


# Data Visualizations
require(scales)

# First question - what is the placement rate?
# 
# Placement is recorded by the categorical variable status. A bar char is a great visualization for that.
p1 <- ggplot(placement, aes(x = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Number of Students", 
       title = "Student Campus Placement Rates")

# Testing with position = "fill" (not to be included with results)
p2 <- ggplot(placement, aes(x = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Number of Students", 
       title = "Student Campus Placement Rates")


p1 + p2

# We can get the percentages from the following:
prop.table(table(placement$status)) * 100


# Remark: We see that more students were placed than not placed.
# Not Placed: ~31%, Placed: ~69%.


# Second question - what is the numeric range of the salaries offered?
ggplot(placement, aes(x = salary)) +
  theme_bw() + 
  geom_histogram() + 
  labs(x = "Salary", 
       y = "Number of Students", 
       title = "Histogram Plot of the Salaries Offered") +
  scale_x_continuous(labels = comma)

# Density on y-axis
ggplot(placement, aes(x = salary, y = ..density..)) +
  theme_bw() + 
  geom_histogram() + 
  labs(x = "Salary", 
       y = "Proportion of Students", 
       title = "Histogram Plot of the Salaries Offered") +
  scale_x_continuous(labels = comma)


median(as.numeric(placement$salary),na.rm=TRUE)

# Remark: In the report we had mentioned the presence of salaries that were not available for
# students who were not placed. The histogram plot of salaries generated a warning sign for this
# reason, pointing out 67 observations without the salary information.
#
# As a verification, if we divide 67 by 215 we get ~31%, same as generated from earlier results.
# This is a way to confirm that we are not missing any salary information on students who were placed.
#
# The salary offered ranges from roughly Rs.200,000 to Rs.950,000.
# And the median salary is around Rs.265,000.


# Third question - what is the status by etest_p grade?
# Hypothesis is that the higher the grade the higher the chance of being placed
# Testing with histogram

# Stacked
ggplot(placement, aes(x = etest_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students", 
       x = "etest_p (binwidth = 1)", 
    title = "Placement Status based on etest_p grade")

# Overlayed
ggplot(placement, aes(x = etest_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students", 
       x = "etest_p (binwidth = 1)", 
       title = "Placement Status based on etest_p grade")

# Density Plot
ggplot(placement, aes(x = etest_p, fill = status)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students", 
       x = "etest_p", 
       title = "Placement Status based on etest_p grade")

# Testing with boxplot
ggplot(placement, aes(x = status, y = etest_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(y = "etest_p", 
       x = "status", 
       title = "Placement Status based on etest_p grade")

# Remark 3: 


# Fourth question - what is the status by work experience?
# Hypothesis is that candidates with previous work experiences are likely to be placed.
# The default is geom_bar(position = "stack")
ggplot(placement, aes(x = workex, fill = status)) + 
  theme_bw() + 
  geom_bar() + 
  labs(y = "Number of Students", 
       title = "Placement Status by Work Experience")


# Of importance, the position = "fill" (note the quotation marks) will make “proportion bars”,
# so the height of the bar goes up to 100%. This is obviously only of use if different colors indicate the relative height (frequencies) of the categories in the bar.
# The following will be a bar chart of proportion for the entire category
ggplot(placement, aes(x = workex, fill = status)) + 
  theme_bw() + 
  geom_bar() + 
  labs(y = "Number of Students", 
       title = "Placement Status by Work Experience")

# This will give the same plot as above but is structured a little different
ggplot(placement, aes(x = workex)) + 
  theme_bw() + 
  geom_bar(aes(fill = status), position = "fill") + 
  labs(y = "Number of Students", 
       title = "Placement Status by Work Experience")

# Now with position = "dodge"
ggplot(placement, aes(x = workex, fill = status)) + 
  theme_bw() + 
  geom_bar(position = "dodge") + 
  labs(y = "Number of Students", 
       title = "Placement Status by Work Experience")


# Remark: The first bar plot shows that more students did not have work experience.
# The proportion bar plot shows that almost 60% of students without work experience were placed, 
# whereas almost 90% with work experience were. This result matches with our hypothesis.




# Now, perhaps we can test the effect of work experience and etest_p on placement status.
# Fifth question - how does the status depend on both etest_p and work experience?
ggplot(placement, aes(x = etest_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ workex) +
  geom_histogram(binwidth = 5) +
  labs(y = "Student Count",
       title = "Placement Status by Work Experience and etest_p")

# Remark: This first plot's layout is confusing and does not help us to visually determine
# the relationship asked in the question.

# Stacked
ggplot(placement, aes(x = etest_p, fill = workex)) + 
  theme_bw() +
  facet_wrap(~ status) +
  geom_histogram(binwidth = 5) +
  labs(y = "Student Count",
       title = "Placement Status by Work Experience and etest_p")

# Overlayed
ggplot(placement, aes(x = etest_p, fill = workex)) + 
  theme_bw() +
  facet_wrap(~ status) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.6) +
  labs(y = "Student Count",
       title = "Placement Status by Work Experience and etest_p")

# Density Plot
ggplot(placement, aes(x = etest_p, fill = workex)) + 
  theme_bw() +
  facet_wrap(~ status) +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       title = "Placement Status by Work Experience and etest_p")


# Box Plot
ggplot(placement, aes(x = etest_p, fill = workex)) + 
  theme_bw() +
  facet_wrap(~ status) +
  geom_boxplot(varwidth = TRUE) +
  labs(y = "Proportion of Students",
       title = "Placement Status by Work Experience and etest_p")




# Remark: From the second plot it is much clear that a very small percentage of students
# with previous work experience were not placed, and more among these students had low
# etest_p score than high.
# However, for the students who were placed, etest_p scores does not seem to have any
# effect on the placement decision.
#
# Notice the comparision between the density and box plots, specifically how clear the
# distinction is for the box plot.
#
# In the box plot, we see that among the students who were not placed, the median etest score
# for those with work exprience were lower than those without. Among the students who were placed,
# we see the opposite, but the interquartile range almost overlaps. Comparing the status and etest
# scores for students without work experience, we see that the interquartile range overlaps.



# Sixth question - what is the effect of mba percentage on placement status?
# Hypothesis is that mba_p is an influence in some way.

# Stacked
ggplot(placement, aes(x = mba_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students", 
       x = "mba_p (binwidth = 1)", 
       title = "Placement Status based on MBA Score")

# Overlayed
ggplot(placement, aes(x = mba_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students", 
       x = "mba_p (binwidth = 1)", 
       title = "Placement Status based on MBA Score")

# Density Plot
ggplot(placement, aes(x = mba_p, fill = status)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students", 
       x = "mba_p (binwidth = 1)", 
       title = "Placement Status based on MBA Score")

# Box Plot
ggplot(placement, aes(x = status, y = mba_p)) + 
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status by Work Experience and MBA Score")


# Remark: The plot shows the hypothesis to hold for the lowest and the highest mba_p scores, 
# but for the scores in between the visual evidence is not strong. So we cannot correctly conclude
# that the hypothesis holds.
#
# Specifically check out the box plot. The interquaritlie range for Placed category is wider and overlaps
# that of the Not Placed category.



# Seventh question - Did gender play a role?
# Hypothesis is no
ggplot(placement, aes(x = gender, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on Gender")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = gender, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Count", 
       title = "Placement Status Based on Gender")

# Remark: The first bar plot shows that there are less female than male candidates.
# The proportion bar plot shows that 60% of female students were placed and 70% of male students wer placed.
# At this point it is not evident if gender did play a role in the placement process.


# Eighth question - Does MBA specialization have any effect on status?
ggplot(placement, aes(x = specialisation, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on MBA specialization")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = specialisation, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Proportion", 
       title = "Placement Status Based on MBA specialization")

# Remark: We see that more students specialized in Mkt&Fin than Mkt&HR.
# In addition, we see that a more candidates with Mkt&Fin specialization were hired.
# Based on the proportion bar plot, the ratio is ~80% : ~55%.

# Ninth question - Does the degree stream have any effect on status?
ggplot(placement, aes(x = degree_t, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on Degree Stream")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = degree_t, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Proportion", 
       title = "Placement Status Based on Degree Stream")

# Remark: The first bar plots shows that the number of students in Comm&Mgmt are a lot more than
# Sci&Tech and other streams combined. But from the proportion bar plot We see that equal proportion
# of students from Comm&Mgmt and Sci&Tech were hired. And less than 50% of students from other
# streams were hired.


# Tenth question - Does the degree percentage have any effect on status?
# Hypothesis is yes

# Stacked
ggplot(placement, aes(x = degree_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students", 
       x = "degree_p (binwidth = 1)", 
       title = "Placement Status based on degree_p grade")

# Overlayed
ggplot(placement, aes(x = degree_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students", 
       x = "degree_p (binwidth = 1)", 
       title = "Placement Status based on degree_p grade")

# Density Plot
ggplot(placement, aes(x = degree_p, fill = status)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students", 
       x = "degree_p (binwidth = 1)", 
       title = "Placement Status based on degree_p grade")

# Box Plot
ggplot(placement, aes(x = status, y = degree_p)) + 
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status by Work Experience and Degree Score")


# Remark: The histogram shows that overall candidates with higher degree percentage received placement.
# Note that the outliers in the box plot are not relevant when it comes to test scoes in this case.


# 11th question - How does both degree stream and degree percentage reflect on status?

# Stacked
ggplot(placement, aes(x = degree_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ degree_t) +
  geom_histogram(binwidth = 2) +
  labs(y = "Student Count",
       title = "Placement Status by Degree Stream and Degree Percentage")

# Overlayed
ggplot(placement, aes(x = degree_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ degree_t) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(y = "Student Count",
       title = "Placement Status by Degree Stream and Degree Percentage")

# Density Plot
ggplot(placement, aes(x = degree_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ degree_t) +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       title = "Placement Status by Degree Stream and Degree Percentage")

# Box Plot
ggplot(placement, aes(x = status, y = degree_p)) + 
  theme_bw() +
  facet_wrap(~ degree_t) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status by Work Experience and MBA Score")



# Remark: We see the overall trends observed for questions 10 and 11.
# Candidates with higher degree percentage were preferred, the box plot verifies this clearly.



# 12th question - How does the hsc_p affect status?

# Stacked
ggplot(placement, aes(x = hsc_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students", 
       x = "hsc_p (binwidth = 1)", 
       title = "Placement Status Based on HSC Score")

# Overlayed
ggplot(placement, aes(x = hsc_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students", 
       x = "hsc_p (binwidth = 1)", 
       title = "Placement Status Based on HSC Score")

# Density Plot
ggplot(placement, aes(x = hsc_p, fill = status)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students", 
       x = "hsc_p (binwidth = 1)", 
       title = "Placement Status Based on HSC Score")

# Box Plot
ggplot(placement, aes(x = status, y = hsc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status Based on HSC Score")


# Remark 12: Candidates with higher hsc percentages were preferred for placement.


# 13th question - How does the ssc_p affect status?

# Stacked
ggplot(placement, aes(x = ssc_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students", 
       x = "ssc_p (binwidth = 1)", 
       title = "Placement Status Based on SSC Score") +
  scale_y_continuous(limits=c(0, 13))

# Overlayed
ggplot(placement, aes(x = ssc_p, fill = status)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students", 
       x = "ssc_p (binwidth = 1)", 
       title = "Placement Status Based on SSC Score") +
  scale_y_continuous(limits=c(0, 13))

# Density Plot
ggplot(placement, aes(x = ssc_p, fill = status)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students", 
       x = "ssc_p (binwidth = 1)", 
       title = "Placement Status Based on SSC Score")

# Box Plot
ggplot(placement, aes(x = status, y = ssc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status Based on SSC Score")


# Remark 13: Candidates with higher ssc percentages were preferred for placement.


# Note: It seems like the degree, hsc, and ssc test scores were take into consideration when
# selecting candidates. The evidence was not strong for MBA and employment test scores.


# 14th question - We saw that hsc_p have a a positivel correlation with status.
# Now let us explore how hsc_s might affect the status?
ggplot(placement, aes(x = hsc_s, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on hsc_s")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = hsc_s, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Count", 
       title = "Placement Status Based on hsc_s")


# Remark 14a: We see that that most candidates are in the Commerce stream followed by
# the Science stream, and very few students are in the Arts stream. But we also see
# that a higher percentage of Commerce and Science sudents were placed (equal percentages)
# than Arts, for which stream the percentage placed is approximately 55%.

# Stacked
ggplot(placement, aes(x = hsc_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ hsc_s) +
  geom_histogram(binwidth = 2) +
  labs(y = "Number of Students",
       title = "Placement Status by hsc Stream and hsc Percentage")

# Overlayed
ggplot(placement, aes(x = hsc_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ hsc_s) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students",
       title = "Placement Status by hsc Stream and hsc Percentage")

# Density Plot
ggplot(placement, aes(x = hsc_p, fill = status)) + 
  theme_bw() +
  facet_wrap(~ hsc_s) +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       title = "Placement Status by hsc Stream and hsc Percentage")

# Box Plot
ggplot(placement, aes(x = status, y = hsc_p)) +
  theme_bw() +
  facet_wrap(~ hsc_s) +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Placement Status by hsc Stream and hsc Percentage")


# Remark 14b: Dividing the placement status according to the science, commerce, and arts streams,
# we see that students who were placed generally had higher hsc scores than those who were not placed.


# 15th question - Do the hsc and ssc boards have any effect
# hsc
ggplot(placement, aes(x = hsc_b, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on hsc_b")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = hsc_b, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Count", 
       title = "Placement Status Based on hsc_b")


# ssc
ggplot(placement, aes(x = ssc_b, fill = status)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Placement Status Based on ssc_b")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = ssc_b, fill = status)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Count", 
       title = "Placement Status Based on ssc_b")

# Remark 15: There does not seem to be any visual evidence to suggest any particular is preferred than the other.


# We noticed a higher preference for candidates who specialized in Mkt&Fin, and
# we also noticed that students with higher test scores in degree_p, hsc_p, and ssc_p
# were preferred.
#
# 16 - How does the MBA score vary over different specializations?

# Stacked
ggplot(placement, aes(x = mba_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "mba_p (bandwidth = 1)",
       title = "MBA stream vs MBA score")

# Overlayed
ggplot(placement, aes(x = mba_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students",
       x = "mba_p (bandwidth = 1)",
       title = "MBA stream vs MBA score")

# Densiity Plot
ggplot(placement, aes(x = mba_p, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "specialisation",
       x = "mba_p",
       title = "MBA Stream vs MBA Score")

# Box Plot
ggplot(placement, aes(x = specialisation, y = mba_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA Stream vs MBA Score")

# Remark 16: The trend between MBA test scores and specializations is not clear 
# from the histograms. But the density and box plots show that more students 
# specializing Mkt&HR had lower scores and more students specializingin Mkt&Fin 
# had higher scores. We also see from the boxplot that students in Mkt&Fin had 
# a higher median score than those in Mkt&HR.



# 17 - How does the etest score vary for MBA students based on their specialization?

# Stacked
ggplot(placement, aes(x = etest_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Studets",
       x = "etest_p (bandwidth = 1)",
       title = "MBA stream vs employment test score")

# Overlayed
ggplot(placement, aes(x = etest_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Studets",
       x = "etest_p (bandwidth = 1)",
       title = "MBA stream vs employment test score")

# Density
ggplot(placement, aes(x = etest_p, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "etest_p",
       title = "MBA stream vs employment test score")

# Box Plot
ggplot(placement, aes(x = specialisation, y = etest_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA stream vs employment test score")

# Remark 17: We see that more students in Mkt&HR had lower employment test scores,
# and more students in Mkt&Fin had higher test scores. The median score for Mkt&Fin
# students is 75 and that for Mkt&HR students is 66.


# 18th question - How does the degree test score vary based on MBA specialization?

# Stacked
ggplot(placement, aes(x = degree_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "degree_p (bandwidth = 1)",
       title = "MBA stream vs degree score")

# Overlayed
ggplot(placement, aes(x = degree_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students",
       x = "degree_p (bandwidth = 1)",
       title = "MBA stream vs degree score")

# Density Plot
ggplot(placement, aes(x = degree_p, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "degree_p",
       title = "MBA stream vs degree score")

# Box Plot
ggplot(placement, aes(x = specialisation, y = degree_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA stream vs degree score")


# Remark 18: The plots show that more students with Mkt&HR had lower degree scores, 
# and more students specializing in Mkt&Fin scored higher.



# 19 - How about the hsc_p scores by specialisation?

# Stacked histogram
ggplot(placement, aes(x = hsc_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "hsc_p (bandwidth = 1)",
       title = "MBA stream vs hsc test score")

# Overlayed histogram
ggplot(placement, aes(x = hsc_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Number of Students",
       x = "hsc_p (bandwidth = 1)",
       title = "MBA stream vs hsc test score")

# Density Plot
ggplot(placement, aes(x = hsc_p, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "hsc_p",
       title = "MBA stream vs hsc test score")

# Box Plot
ggplot(placement, aes(x = specialisation, y = hsc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA stream vs hsc test score")


# Remark 19: Based on the density and box plots we see that scores for students in Mkt&Fin are in the 
# range of 62-77, with a median around 67. For students in Mkt&HR the test score range is 57-70. and
# the median score is 63.
#
# Note that both groups have a few outliers in the hsc_p scores.


# 20 - How about the ssc_p by specialisation?

# Stacked Histogram
ggplot(placement, aes(x = ssc_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "ssc_p",
       title = "MBA stream vs ssc test score") +
  scale_y_continuous(limits=c(0, 13))

# Overlayed Histogram
ggplot(placement, aes(x = ssc_p, fill = specialisation)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "ssc_p",
       title = "MBA stream vs ssc test score")

# Density Plot
ggplot(placement, aes(x = ssc_p, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "ssc_p",
       title = "MBA stream vs ssc test score")

# Box Plot
ggplot(placement, aes(x = specialisation, y = ssc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA stream vs ssc test score")



# Remark 20: Although a higher number of Mkt&Fn students scored between 75-90,
# we can conclude no noticeable difference based on students specializations as
# more students are enrolled in Mkt&Fin than Mkt&HR.


# 21 - Do more students have work experience?
ggplot(placement, aes(x = workex)) +
  theme_bw() +
  geom_bar() +
  labs(title = "Student Work Experience Rate")

# Remark 21: We see that more students with work experience than not at the time
# of the campus placement event.

# 22 - Is there a relationship between MBA stream and work experience?
ggplot(placement, aes(x = specialisation, fill = workex)) +
  theme_bw() +
  geom_bar() +
  labs(x = "specialisation", 
       title = "MBA Specialization vs Work Experience")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = specialisation, fill = workex)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(x = "specialisation", 
       title = "MBA Specialization vs Work Experience")


# Remark 22: We see that more students in Mkt&Fin with work experience than those
# in the Mkt&HR specialization.


# 23 - Is there a relationship between MBA score vs Work Experience?

# Stacked Histogram
ggplot(placement, aes(x = mba_p, fill = workex)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "MBA Test Score (bandwidth = 1)",
       title = "MBA score vs Work Experience")

# Overlayed Histogram
ggplot(placement, aes(x = mba_p, fill = workex)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "MBA Test Score (bandwidth = 1)",
       title = "MBA score vs Work Experience")

# Density plot
ggplot(placement, aes(x = mba_p, fill = workex)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "MBA Test Score",
       title = "MBA score vs Work Experience")

# Box Plot
ggplot(placement, aes(x = workex, y = mba_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA score vs work experience")


# Remark 23: Students with work exprience generally have higher MBA score.


# 24 Gender vs work experience
ggplot(placement, aes(x = workex, fill = gender)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "Work Experience Based on Gender")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = workex, fill = gender)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Proportion", 
       title = "Work Experience Based on Gender")

# Remark 24: Slightly less female students with work experience than male.

# 25 Gender vs MBA specialization
ggplot(placement, aes(x = specialisation, fill = gender)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Student Count", 
       title = "MBA Specialization Based on Gender")

# With position = "fill" for proportion within each group
ggplot(placement, aes(x = specialisation, fill = gender)) +
  theme_bw() +
  geom_bar(position = "fill") +
  labs(y = "Student Proportion", 
       title = "MBA Specialization Based on Gender")

# Remark 25: More females in Mkt&HR than Mkt&Fin

# Let's check the test scores with respect to the genders

# 26 - Gender vs MBA test score

# Stacked Histogram
ggplot(placement, aes(x = mba_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "MBA Test Score (bandwidth = 1)",
       title = "MBA score vs Gender")

# Overlayed Histogram
ggplot(placement, aes(x = mba_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "MBA Test Score (bandwidth = 1)",
       title = "MBA score vs Gender")

# Density plot
ggplot(placement, aes(x = mba_p, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "MBA Test Score",
       title = "MBA Test Score vs Gender")

# Box Plot
ggplot(placement, aes(x = gender, y = mba_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "MBA Test Score vs Gender")


# Remark 26: The denisty plot shows that more males scored in the 50-64 score range, 
# and  more females scored above 62.


# 27 - Gender vs employment test score

# Stacked Histogram
ggplot(placement, aes(x = etest_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "Employment Test Score (bandwidth = 1)",
       title = "Employment Test Score vs Gender")

# Overlayed Histogram
ggplot(placement, aes(x = etest_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "Employment Test Score (bandwidth = 1)",
       title = "Employment Test Score vs Gender")

# Density plot
ggplot(placement, aes(x = etest_p, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Employment Test Score",
       title = "Employment Test Score vs Gender")

# Box Plot
ggplot(placement, aes(x = gender, y = etest_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Employment Test Score vs Gender")


# Remark 27: The density plot shows that relatively more female students scored below 77, 
# and relatively more male students scored above 77.
#
# Overall, based on the boxplot, the interquatile range for the employement test scores for
# male and female students are similar as are the median scores. It is possible that students
# with more work exprience are likely to score higher in this score. And as we noted earlier,
# there are more male students with work experience than female.



# 28 - Gender vs degree test score

# Stacked Histogram
ggplot(placement, aes(x = degree_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "Degree Test Score (bandwidth = 1)",
       title = "Degree Test Score vs Gender")

# Overlayed Histogram
ggplot(placement, aes(x = degree_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "Degree Test Score (bandwidth = 1)",
       title = "Degree Test Score vs Gender")

# Density plot
ggplot(placement, aes(x = degree_p, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Degree Test Score)",
       title = "Degree Test Score vs Gender")

# Box Plot
ggplot(placement, aes(x = gender, y = degree_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Degree Test Score vs Gender")


# Remark 28: The density plot shows that more male students scored lower than 68, 
# and more female students scored higher than 68.


# 29 - Gender vs hsc test score

# Stacked Histogram
ggplot(placement, aes(x = hsc_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "Hsc Test Score (bandwidth = 1)",
       title = "Hsc Test Score vs Gender")

# Overlayed Histogram
ggplot(placement, aes(x = hsc_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "Hsc Test Score (bandwidth = 1)",
       title = "Hsc Test Score vs Gender")

# Density plot
ggplot(placement, aes(x = hsc_p, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Hsc Test Score",
       title = "Hsc Test Score vs Gender")

# Box Plot
ggplot(placement, aes(x = gender, y = hsc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Hsc Test Score vs Gender")


# Remark 29: Based on the density plot, it cannot be concluded which gender had
# overall higher or lower scores.
#
# Based on the box plot, we see that for both genders have almost the same lower and
# upper quartiles and the median.


# 30 - Gender vs ssc test score

# Stacked Histogram
ggplot(placement, aes(x = ssc_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1) +
  labs(y = "Number of Students",
       x = "Ssc Test Score (bandwidth = 1)",
       title = "Ssc Test Score vs Gender")

# Overlayed Histogram
ggplot(placement, aes(x = ssc_p, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  labs(y = "Proportion of Students",
       x = "Ssc Test Score (bandwidth = 1)",
       title = "Ssc Test Score vs Gender")

# Density plot
ggplot(placement, aes(x = ssc_p, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Ssc Test Score",
       title = "Ssc Test Score vs Gender")

# Box Plot
ggplot(placement, aes(x = gender, y = ssc_p)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Ssc Test Score vs Gender")


# Remark 30: Based on the density plot, it cannot be concluded which gender had
# overall higher or lower scores.
#
# Based on the boxplot, however, the lower and upper quartiles and median for the
# female students are slightly higher that that of the male students.



# Check percentages for each categorical variable level
# gender - M / F
table(gender)
prop.table(table(gender)) * 100

# ssc_b - Central / Others
table(ssc_b)
prop.table(table(ssc_b)) * 100

# hsc_b - Centra / Others
table(hsc_b)
prop.table(table(hsc_b)) * 100

#hsc_s - Arts / Commerce / Science
table(hsc_s)
prop.table(table(hsc_s)) * 100

# degree_t - Comm&Mgmt / Others / Sci&Tech
table(degree_t)
prop.table(table(degree_t)) * 100

# workex Yes / No
table(workex)
prop.table(table(workex)) * 100

# specialisation - Mkt&Fin / Mkt&HR
table(specialisation)
prop.table(table(specialisation)) * 100

# staus - Placed / Not Placed
table(status)
prop.table(table(status)) * 100


# Cross Classification for workex and status
table(workex, status)
prop.table(table(workex, status)) * 100

# salary vs workex

# Box plot
plot(as.factor(workex), salary)
# 





plot(as.factor(workex), salary, varwidth=TRUE)

placedData <- placement[status == "Placed", ]

notplacedData <- placement[status == "Not Placed", ]

summary(as.factor(status))

summary(as.factor(specialisation))

table(status)
