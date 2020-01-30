# Installing Packages
install.packages("ggplot2")
install.packages("mosaic")
install.packages("Sleuth3")
library(ggplot2)        # plotting & data
library(dplyr)          # data manipulation
library(mosaic)
library(Sleuth3)
# Loading Survey data in RStudio
Survey_Data <- read.csv("/cloud/project/Survey_Data.csv")
summary(Survey_Data)
# Exploratory Analysis
# We Explored data for App category and its usage based on country to get the distribution. We got the visibility of most popular Appilcation category among different countries
table(Survey_Data$App.Category, Survey_Data$Country)
# Here we explored data from different prispective to get the distribution of 3 ethicities among different age group
table(Survey_Data$Age , Survey_Data$Country)
# Average Screen time distribution
ggplot(Survey_Data, aes(x=Survey_Data$Average.Screen.time.last.10.days, color=Survey_Data$Country)) +
  geom_histogram(fill="white", position="dodge", bins = 10)+
  theme(legend.position="bottom") + labs(x = "Average Screen Time", color = "Legend: Country wise average screen time")
# Age Group Vs Average Screen time 
ggplot(Survey_Data, aes(x = Survey_Data$Age, y = Survey_Data$Average.Screen.time.last.10.days, color = factor(Survey_Data$Country))) +
  geom_point() +
  labs(x = "Age-Group", y = "Average Screen Time", color = "Legend: Country wise average screen time")+ theme(legend.position = "bottom")
# Sample distribution among Age-group 
ggplot(Survey_Data, aes(x = Survey_Data$Age, fill = Survey_Data$Country)) +
  geom_bar() +   labs(x = "Age-Group", y = "No of people", 
                      color = "Legend: Country wise count")+ theme(legend.position = "bottom")
# Ethnicities wise Screen time comparison
boxplot( Survey_Data$Average.Screen.time.last.10.days~Survey_Data$Country, ylab = 'Average Screen Time', xlab = 'Country', main = 'Ethnicitieswise screen time comparison', col = 'skyblue')
# App category Vs Average screen time : Screen Time here shows its usage across app categories along with outliers to get more visibility
ggplot(Survey_Data, aes(x = Survey_Data$App.Category, y = Survey_Data$Average.Screen.time.last.10.days)) +
  geom_boxplot() + guides(fill = TRUE) +
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+ labs(x = "App Category", y = "Average Screen Time")
pairs(Survey_Data$Average.Screen.time.last.24.hour~Survey_Data$Age, col = Survey_Data$Country)
# Statistical Analysis
# Sampling data for 32 Indians and Chinese for calculating average screen time
library(dplyr)
set.seed(0)
India <- filter(Survey_Data, Survey_Data$Country == "India")
India_2 <- India[sample(nrow(India),32),]

China <- filter(Survey_Data, Survey_Data$Country == "China")
China_2 <- China[sample(nrow(China),32),]

sampledata <- rbind(China_2,India_2)
sampledata

# Summary for Mean & median for sample population
summary(sampledata)

# QQ Plot
qqnorm(Survey_Data$Average.Screen.time.last.10.days)
qqnorm(India_2$Average.Screen.time.last.10.days)
qqnorm(China_2$Average.Screen.time.last.10.days)
# Sample Mean
X_bar_i <- mean(India_2$Average.Screen.time.last.10.days)
X_bar_c <- mean(China_2$Average.Screen.time.last.10.days)
X_bar_i
X_bar_c
# Sample Variance
s_i <- sd(India_2$Average.Screen.time.last.10.days)**2
s_c <- sd(China_2$Average.Screen.time.last.10.days)**2
s_i
s_c
# Sample Size
n_i <- length(India_2$Average.Screen.time.last.10.days)
n_c <- length(China_2$Average.Screen.time.last.10.days)
n_i
n_c
#Null Hypothises
mu <- 0
# T Test
t <- (X_bar_c - X_bar_i - mu)/sqrt((s_i/n_i) + (s_c/n_c))
t
# p-value for two sided upper
two_sided_diff <- pt(q=t, df = min(n_i, n_c)-1, lower.tail = FALSE) * 2
two_sided_diff
Alpha <- 0.05
Confidence_Interval <- 0.95
# Lower Bound
L_bound <- (X_bar_c - X_bar_i) + (qt(0.025, min(n_i, n_c)-1)* sqrt((s_i/n_i) +(s_c/n_c)))
L_bound
# Upper Bound
U_bound <- (X_bar_c - X_bar_i) + (qt(0.975, min(n_i, n_c)-1)* sqrt((s_i/n_i) +(s_c/n_c)))
U_bound
# R built in t-test function
t.test(India_2$Average.Screen.time.last.10.days, China_2$Average.Screen.time.last.10.days)
#histograms
histogram(~ Survey_Data$Average.Screen.time.last.10.days | Survey_Data$Country)
histogram (India_2$Average.Screen.time.last.10.days)
histogram(China_2$Average.Screen.time.last.10.days)
# Histogram of the sampling distribution
mu <- mean(Survey_Data$Average.Screen.time.last.10.days)
sd <- sd(Survey_Data$Average.Screen.time.last.10.days)
h <- hist(Survey_Data$Average.Screen.time.last.10.days, xlim = c(0,13))
lb <- mu - 1.96*sd
ub <- mu + 1.96*sd
abline(v = c(mu, lb, ub), lty = 2)
x_axis <- seq(min(Survey_Data$Average.Screen.time.last.10.days),max(Survey_Data$Average.Screen.time.last.10.days),length=80)
y_axis <- dnorm(x_axis, mu, sd)*length(x_axis)
lines(x_axis, y_axis, col = "blue")
# T- Test distribution graph
n <- min(n_c, n_i)
X <- seq(-4, 4, .01)
Y <- dt(X, n-1)
plot(X, Y, type = 'l')
abline(v = c(t, -t),  col = "blue")
# Confidence Interval graph
plot(X, Y, type = 'l')
abline(v = qnorm(0.975), col = "Green")
abline(v = qnorm(0.025), col = "Green")
abline(v = 0, col = "black")
# Difference between normal distribution & T distribution
plot(X,Y,type = 'l')
lines(X,dnorm(X), col = 'yellow')
Survey_Data
sampledata
India_2
China_2