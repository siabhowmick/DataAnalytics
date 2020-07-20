# Factors affecting campus placemnt
# Part 2: Focus on the salary offered to placed students

library(ggplot2)
library(readr)
placement <- read.csv("~\\R\\win-library\\4.0\\My R Projects and Files\\Kaggle - Campus Placement\\Placement_Data_Full_Class.csv")
View(placement)
attach(placement)

# The first variable in the placement dataset is sl_no, ranging from 1..215, 
# and does not contribute to the information in the data. 
# So let's delete that column - remains 14 variables we can work 
# 6 - numerical & 8 - categorical

# Check which columns have missing values, if any
colSums(is.na(placement))

# Remark: Only the salary column has missing values (67) for those students who were not placed.
# Verify with the following.
summary(status)

# Or
table(status)


# Create a new dataset by excluding rows in the salary column that have
# missing values for students who were not placed.
placed <- placement[complete.cases(placement$salary), ]
attach(placed)

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


# Second question - what is the numeric range of the salaries offered?
ggplot(placed, aes(x = salary)) +
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
  labs(x = "Salary (Rs.)", 
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


# 1 - Salary vs Gender

# Overlayed histogram
ggplot(placed, aes(x = salary, fill = gender)) +
  theme_bw() +
  geom_histogram(position = "identity", alpha = 0.6) +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on Gender") +
  scale_x_continuous(labels = comma)

# Density plot
ggplot(placed, aes(x = salary, fill = gender)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Salary (Rs.)",
       title = "Salary Bsed on Gender") +
  scale_x_continuous(labels = comma)

# Box plot
ggplot(placed, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(y = "Salary (Rs.)",
       title = "Salary Based on Gender") +
  scale_y_continuous(labels = comma)


# Remark 1: There are higher proportion of women who received lower salary below Rs.250,000.
# And a higher proportion of men who received salary in the range Rs.250,000 - Rs.350,000.
# The trend between the range above Rs.350,000 cannot be definitely determined.
#
# Based on the box plot we see that the median salary for women are almost equal to the lower
# quartile salary for men. The number of outlierrs for men past the range is a lot higher than
# that for women. Shows that overall, more men were offered extremely high salary than women.
# We see that the lowest salary offered to both genders were Rs.200,000.


# 2 - Salary based on mba specialization

# Stacked histogram
ggplot(placed, aes(x = salary, fill = specialisation)) +
  theme_bw() +
  geom_histogram() +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on MBA Specialization") +
  scale_x_continuous(labels = comma)

# Overlayed histogram
ggplot(placed, aes(x = salary, fill = specialisation)) +
  theme_bw() +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on MBA Specialization") +
  scale_x_continuous(labels = comma)

# Density plot
ggplot(placed, aes(x = salary, fill = specialisation)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Salary (alpha = 0.5)",
       title = "Salary Based on MBA Specialization") +
  scale_x_continuous(labels = comma)

# Box plot
ggplot(placed, aes(x = specialisation, y = salary)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Salary Based on MBA Specialization") +
  scale_y_continuous(labels = comma)


# Remark 2: Based on the overlayed histogram and box plot we see that the median salary offered
# to placed students from Mkt&Fin are higher than that of Mkt&HR. We also see that really high
# salaries in the range Rs.500,000 - Rs.950,000 were offered to students in Mkt&Fin. Highest
# salary offered to students placed in Mkt&HR is approximately Rs.460,000.


# 3 - Salary vs work experience

# Stacked histogram
ggplot(placed, aes(x = salary, fill = workex)) +
  theme_bw() +
  geom_histogram() +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on Work Experience") +
  scale_x_continuous(labels = comma)

# Overlayed histogram
ggplot(placed, aes(x = salary, fill = workex)) +
  theme_bw() +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on Work Experience") +
  scale_x_continuous(labels = comma)

# Density plot
ggplot(placed, aes(x = salary, fill = workex)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Salary (alpha = 0.5)",
       title = "Salary Based on Work Experience") +
  scale_x_continuous(labels = comma)

# Box plot
ggplot(placed, aes(x = workex, y = salary)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Salary Based on Work Experience") +
  scale_y_continuous(labels = comma)


# Remark 3: From the density plot we see that a similar proportion of stduents with and without work
# experience received salaries between Rs.200,000 - Rs.450,000. However, those in the salaries in the
# range of Rs.600,000 - Rs.800,000 were exclusively offered to those with work experience.
#
# Note that when we compare this density plot with that from comparison of salary based on 
# MBA specialization (remark 2), we see that the students in the higher salary range are excluseively
# from Mkt&Fin.
#
# Upon comparison with the graph for salary and gender (remark 1), we see that both male and female
# students belong to that salary bracket.


# 4 - Salary vs college degree stream

# Stacked histogram
ggplot(placed, aes(x = salary, fill = degree_t)) +
  theme_bw() +
  geom_histogram() +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on College Degree Stream") +
  scale_x_continuous(labels = comma)

# Overlayed histogram
ggplot(placed, aes(x = salary, fill = degree_t)) +
  theme_bw() +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(y = "Number of Students",
       x = "Salary (Rs.)",
       title = "Salary Based on College Degree Stream") +
  scale_x_continuous(labels = comma)

# Density plot
ggplot(placed, aes(x = salary, fill = degree_t)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  labs(y = "Proportion of Students",
       x = "Salary (alpha = 0.5)",
       title = "Salary Based on College Degree Stream") +
  scale_x_continuous(labels = comma)

# Box plot
ggplot(placed, aes(x = degree_t, y = salary)) +
  theme_bw() +
  geom_boxplot(varwidth = TRUE) +
  labs(title = "Salary Based on College Degree Stream") +
  scale_y_continuous(labels = comma)


# Remark 4: Sci&Tech and Comm&Mgmt students received highest salaries.


# 5 - Salary vs MBA test score

# Box Plot
ggplot(placed, aes(x = salary, y = mba_p, group = workex)) +
  theme_bw() +
  facet_wrap(~ workex) +
  geom_boxplot() +
  scale_x_continuous(labels = comma)


# 6 - Salary vs employment test score

# Box Plot
ggplot(placed, aes(x = salary, y = etest_p, group = workex)) +
  theme_bw() +
  facet_wrap(~ workex) +
  geom_boxplot() +
  scale_x_continuous(labels = comma)


# 7 - Salary vs degree test score

# Box Plot
ggplot(placed, aes(x = salary, y = degree_p, group = workex)) +
  theme_bw() +
  geom_boxplot() +
  scale_x_continuous(labels = comma)


# 8 - Salary vs hsc test score

# Box Plot
ggplot(placed, aes(x = salary, y = hsc_p, group = workex)) +
  theme_bw() +
  facet_wrap(~ workex) +
  geom_boxplot() +
  scale_x_continuous(labels = comma)


# 9 - Salary vs ssc test score

# Box Plot
ggplot(placed, aes(x = salary, y = ssc_p, group = workex)) +
  theme_bw() +
  facet_wrap(~ workex) +
  geom_boxplot() +
  scale_x_continuous(labels = comma)
