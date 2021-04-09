#First we import the data

student_data <- read.csv2("student_mat.csv")

#Check the structure to see how the variables are qualified
str(student_data)

#Change variables to the appropriate format

student_data[,c(1:2,4:6,9:12,16:23)] <- lapply(student_data[,c(1:2,4:6,9:12,16:23)], factor)

str(student_data)

#A quick summary allow us to check if there is problems in the different values
summary(student_data)

#We see that everything seems alright


# Barplots & Histograms of the Data Description 

# School of the student
sschool <- barplot(table(student_data$school), main="Student's school", names.arg=c("Gabriel Pereira", "Mousinho da Silveira"), col="#D93E2B")
text(sschool,paste(table(student_data$school)), cex=1, pos=3)
# Gender
sgender <- barplot(table(student_data$sex), main="Student's gender", names.arg=c("Female", "Male"),ylab="Number of Students", col = c("#D93E2B"))
text(sgender,paste(table(student_data$sex)), cex=1, pos=3)
# Home address type
saddress <- barplot(table(student_data$address), main="Students' address", names.arg=c("Rural Area", "Urban Area"),ylab="Number of Students", col = "#D93E2B")
text(saddress,paste(table(student_data$address)), cex=1, pos=3)
# Family size
ssize <- barplot(table(student_data$famsize), main="Students' Family Size", names.arg=c("More than 3 members", "Less than 3 members"),ylab="Number of Students", col = "#D93E2B")
text(ssize,paste(table(student_data$famsize)), cex=1, pos=3)
# Parents cohabitation status
spstat <- barplot(table(student_data$Pstatus), main="Status of the Parents", names.arg=c("Living Apart", "Living Together"),ylab="Number of Students", col = "#D93E2B")
text(spstat,paste(table(student_data$Pstatus)), cex=1, pos=3)
# Mother's professional occupation
par(mar=c(11,4,4,4))
smjob <- barplot(table(student_data$Mjob), main="Mother's Profession", names.arg=c("At Home", "Health Sector", "Other", "Services", "Teacher"), las=2,ylab="Number of Parents", col = "#D93E2B")
text(smjob,paste(table(student_data$Mjob)), cex=1, pos=3)
# Father's professional occupation
par(mar=c(11,4,4,4))
sfjob <- barplot(table(student_data$Fjob), main="Father's Profession", names.arg=c("At Home", "Health Sector", "Other", "Services", "Teacher"), las=2,ylab="Number of Parents", col = "#D93E2B")
text(sfjob,paste(table(student_data$Fjob)), cex=1, pos=3)
# Student's guardian
sguard <- barplot(table(student_data$guardian), main="Student's Guardian", names.arg=c("Father", "Mother", "Other"),ylab="Number of Students", col = "#D93E2B")
text(sguard,paste(table(student_data$guardian)), cex=1, pos=3)
# Motivation to chose the school
sreason <- barplot(table(student_data$reason), main="Motivation to choose this school", las=2, names.arg=c("Course preference", "Close to home", "Other", "School’s reputation"),ylab="Number of Students", col = "#D93E2B")
text(sreason,paste(table(student_data$reason)), cex=1, pos=3)
# Extra educational support
ssup <- barplot(table(student_data$schoolsup), main="Extra Educational Support", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(ssup,paste(table(student_data$schoolsup)), cex=1, pos=3)
# Family educational support
fsup <- barplot(table(student_data$famsup), main="Family Educational Support", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(fsup,paste(table(student_data$famsup)), cex=1, pos=3)
# Extra paid classes
spaid <- barplot(table(student_data$paid), main="Extra Paid Classes", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(spaid,paste(table(student_data$paid)), cex=1, pos=3)
# Extra curricular activities
sact <- barplot(table(student_data$activities), main="Extra Curricular Activities", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(sact,paste(table(student_data$activities)), cex=1, pos=3)
# Has attended the school nursery 
snurse <- barplot(table(student_data$nursery), main="Did the student attend nursery", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(snurse,paste(table(student_data$nursery)), cex=1, pos=3)
# Willing to pursue higher education
shigh <- barplot(table(student_data$higher), main="Willing to Pursue Higher Education", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(shigh,paste(table(student_data$higher)), cex=1, pos=3)
# Internet connection
sint <- barplot(table(student_data$internet), main="Student has Internet Connection", names.arg=c("No", "Yes"),ylab="Number of Students", col = "#D93E2B")
text(sint,paste(table(student_data$internet)), cex=1, pos=3)
# Engaged in romantic relationship
srom <- barplot(table(student_data$romantic), main="Student Engaged in Romantic Relationship", names.arg=c("No", "Yes"),las=3,ylab="Number of Students", col = "#D93E2B")
text(srom,paste(table(student_data$romantic)), cex=1, pos=3)
# Age of the student
sage <- data.frame(student_data$age)

library(tidyverse)
library(hrbrthemes)

sage %>%
  ggplot( aes(x=student_data.age)) +
  geom_histogram( binwidth=1, fill="#d93e2b", color="grey", alpha=0.8) +
  ggtitle("Age of the Students") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
# Mother's education
par(mar=c(11,4,4,4))
smedu <- barplot(table(student_data$Medu), main="Mother's Education", names.arg=c("None", "Primary education (4th grade)", "5th to 9th grade","Secondary educaion","Higher education"),las=2,ylab="Number of Students", col = "#D93E2B")
text(smedu,paste(table(student_data$Medu)), cex=1, pos=3)
# Father's education
par(mar=c(11,4,4,4))
sfedu <- barplot(table(student_data$Fedu), main="Father's Education", names.arg=c("None", "Primary education (4th grade)", "5th to 9th grade","Secondary educaion","Higher education"),las=2,ylab="Number of Students", col = "#D93E2B")
text(sfedu,paste(table(student_data$Fedu)), cex=1, pos=3)
# Travel time between student's home and school
strav <- barplot(table(student_data$traveltime), names.arg=c("< 15min", "15-30min", "30-60min", "> 60min"), main="Travel Time", col = c("#d93E2B"), ylab= "Travel time of the Student") 
text(strav, paste(table(student_data$traveltime)))
# Hours studied per week
sstudy <- barplot(table(student_data$studytime), names.arg=c("< 2 hours", "2-5 jours", "5-10 Hours", "> 10 hours"), main="Time Spent Studied", col = c("#d93E2B"), ylab= "Travel time of the Student") 
text(sstudy, paste(table(student_data$studytime)))
# Number of past class failures 
sfail <- data.frame(student_data$failures)

sfail %>%
  ggplot( aes(x=student_data.failures)) +
  geom_histogram( binwidth=1, fill="#d93e2b", color="black", alpha=0.8) +
  ggtitle("Number of Past Class Failures") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
# Quality of the family relationship
srel <- barplot(table(student_data$famrel), main="Quality of the Family Relationship", names.arg=c("Very Bad","Bad","Average","Good","Very Good"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(srel, paste(table(student_data$famrel)))
# Amount of free time of the student
sfree <- barplot(table(student_data$freetime), main="Amount of Free Time of the Student After School", names.arg=c("Very low","Low","Average","High","Very High"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(sfree, paste(table(student_data$freetime)))
# Time sepent going out with friends
sout <- barplot(table(student_data$goout), main="Amount of Time Spent Going Out with Friends",names.arg=c("Very low","Low","Average","High","Very High"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(sout, paste(table(student_data$goout)))
# Consumption of alcohol during weekdays
sdalc <- barplot(table(student_data$Dalc), main="Amount of Alcohol Consumed during Weekdays",names.arg=c("Very low","Low","Average","High","Very High"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(sdalc, paste(table(student_data$Dalc)))
# Consumption of alcohol during weekends 
swalc <- barplot(table(student_data$Walc), main="Amount of Alcohol Consumed during Weekends",names.arg=c("Very low","Low","Average","High","Very High"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(swalc, paste(table(student_data$Walc)))
# Health status of the students
shealth <- barplot(table(student_data$health), main="Health Status of the Student", names.arg=c("Very Bad","Bad","Average","Good","Very Good"),las=1,ylab="Number of Studentss", col = "#D93E2B")
text(shealth, paste(table(student_data$health)))
# Number of school absences 
summary(student_data$absences)
sabs <- data.frame(student_data$absences)

sabs %>%
  ggplot( aes(x=student_data.absences)) +
  geom_histogram( binwidth=1, fill="#d93e2b", color="black", alpha=0.8) +
  ggtitle("Number of School Absences") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )



# Study of the correlation among quantitative variables

cor_data <- student_data[c(3,7,8,13:15,24:33)]
str(cor_data)
cor(cor_data)
#install.packages("corrplot")
library(corrplot)
cor_data_matrix <- cor(cor_data)
corrplot(cor_data_matrix, type = "upper", diag = FALSE, tl.col="black", tl.srt=45)

# As we can see a big correlation with G1/G2 and G3 we decided only to keep G3 as a reference variable
# We also have correlation between MEDU and FEDU, We keep MEDU
# For the correlation between Dalc and Walc we compute the mean of the two columns to create a unique variable corresponding to
# the consumption of alcohol during the whole week

#We compute an "alcohol" column with the mean of Dalc & Walc

alcohol <- apply(student_data[27:28], 1, mean)
student_data <- cbind(student_data,alcohol)

# We delete all the unwanted variables (FEDU, G1, G2, Walc & Dalc)

student_data <- student_data[-(c(31:32,8,27:28))]
str(student_data)

#Re check the correlation plot to be sure everything is alright
cor_data <- student_data[c(3,7,12:14,23:29)]
str(cor_data)
cor(cor_data)
#install.packages("corrplot")
library(corrplot)
cor_data_matrix <- cor(cor_data)
corrplot(cor_data_matrix, type = "upper", diag = FALSE, tl.col="black", tl.srt=45)

###########################
## The linear regression ##
###########################

# Model the graduation using a multiple linear regression model


linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob) + factor(Fjob)
             + factor(reason) + factor(guardian) + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences + alcohol, data=student_data)
summary(linreg)


#ANOVA Test
# We reject H0 because pvalue<5%
# It mean that at least one variable is correlated to Y (reference variable (G3))

# Which variables are significant to the model ? We use the backward method
# to remove one by one the less significant variable

#Remove reason
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob) + factor(Fjob)
             + factor(guardian) + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences + alcohol, data=student_data)
summary(linreg)

#remove guardian
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob) + factor(Fjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences + alcohol, data=student_data)
summary(linreg)

#remove Fjob
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences + alcohol, data=student_data)
summary(linreg)

#remove alcohol
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove nursery
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + Medu + factor(Mjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove Pstatus
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + Medu + factor(Mjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(internet) + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove internet
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + Medu + factor(Mjob)
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove Mjob
linreg <- lm(G3~factor(school) + factor(sex) + age + factor(address) + factor(famsize) + Medu
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove school
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + famrel + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove famrel
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#Remove traveltime
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove famrel
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + traveltime + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(activities) + factor(higher)
             + factor(romantic) + freetime + goout + health
             + absences, data=student_data)
summary(linreg)


#remove activities
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(paid) + factor(higher)
             + factor(romantic) + freetime + goout + health
             + absences, data=student_data)
summary(linreg)

#remove paid
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(higher)
             + factor(romantic) + freetime + goout + health
             + absences, data=student_data)
summary(linreg)


#remove health
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(higher)
             + factor(romantic) + freetime + goout
             + absences, data=student_data)
summary(linreg)

#remove freetime
linreg <- lm(G3~factor(sex) + age + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(higher)
             + factor(romantic) + goout
             + absences, data=student_data)
summary(linreg)

#remove age
linreg <- lm(G3~factor(sex) + factor(address) + factor(famsize) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(higher)
             + factor(romantic) + goout
             + absences, data=student_data)
summary(linreg)

#remove famsize
linreg <- lm(G3~factor(sex) + factor(address) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(famsup) + factor(higher)
             + factor(romantic) + goout
             + absences, data=student_data)
summary(linreg)

#remove famsup
linreg <- lm(G3~factor(sex) + factor(address) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(higher)
             + factor(romantic) + goout
             + absences, data=student_data)
summary(linreg)

#remove higher
linreg <- lm(G3~factor(sex) + factor(address) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(romantic) + goout
             + absences, data=student_data)
summary(linreg)

#remove absences
linreg <- lm(G3~factor(sex) + factor(address) + Medu
             + studytime + failures + factor(schoolsup)
             + factor(romantic) + goout, data=student_data)
summary(linreg)

#remove studytime
linreg <- lm(G3~factor(sex) + factor(address) + Medu
             + failures + factor(schoolsup)
             + factor(romantic) + goout, data=student_data)
summary(linreg)

#remove adress
linreg <- lm(G3~factor(sex) + Medu
             + failures + factor(schoolsup)
             + factor(romantic) + goout, data=student_data)
summary(linreg)

#remove schoolsup
linreg <- lm(G3~factor(sex) + Medu + failures
             + factor(romantic) + goout, data=student_data)
summary(linreg)



#Write the equation
#G3 = 10.6342 + 0.9557 sex M + 0.6157 Medu - 1.8943 failures - 0.9279 romantic yes - 0.4571 goout

# To explain this equation, the G3 graduation start for all students at 10.6342
# and change regarding to the coefficients. If the student is a men it go up by 0.9557
# and evry time the student have a failure the graduation down by 1.8943

##################################
##Verify hypotheses on residuals##
##################################

res <- linreg$residuals
summary(res)

#OK mean of residuals is equal to zero

# We verify if the residuals are normally distributed
qqnorm(res, main="Are residuals normally distributed?")
qqline (res)

shapiro.test(linreg$residuals)

# Are residuals homoskedastic ?
plot(linreg$residuals~linreg$fitted)

# Breusch pagan test
install.packages ("lmtest")
library(lmtest)
bptest(linreg,studentize=FALSE, data=data)

# pvalue >5%, we accept H0
# residuals are homoskedastic

# Durbin Watson test where H0 : residuals are not autocorrelated against H1 : residuals are aucorrelated
durbinWatsonTest (linreg,max.lag=1)

# pvalue>5%, we accept H0, residuals are not autocorrelated


#check for multicolinearity

install.packages("car")
library (car)
vif(linreg)

# all the VIF are <5, so there is no multicollinearity

###############
##Predictions##
###############

#Let's make predictions with the model
#What should be the final grade for this 2 students
# 1- Male, Medu=1, failures=3, romantic=yes, goout=4
# 2- Female, Medu=3, failures=0, romantic=No, goout=1

predictors1 <- data.frame(sex='M', Medu=1, failures=3, romantic ='yes', 
                          goout=4)
predict(linreg, predictors1, type="response",interval = "prediction", level = 0.95)

predictors1 <- data.frame(sex='F', Medu=3, failures=0, romantic ='no', 
                          goout=1)
predict(linreg, predictors1, type="response",interval = "prediction", level = 0.95)

#The second student have more chances of success