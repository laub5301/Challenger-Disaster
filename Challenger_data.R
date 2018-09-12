#modified from https://www.udemy.com/machine-learning-in-r/learn/v4/t/lecture/3584702?start=0
#read Challenger dataset
#bimodal model classification problem with two possible output classes (0 = no fail, 1 = fail)
#TRUE means dataset includes header and data is separated by commas
challenger_dataset <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/DAAG/orings.csv")

#print dataset
head(challenger_dataset)
challenger_dataset

#question: did o-ring fail=0 or not=1
challenger_dataset["Fail"] <- ifelse(challenger_dataset$Erosion < 1, 0, 1)

#look at binary values: FAIL or no fail
challenger_dataset$Fail

#attach dataset; use columns directly
attach(challenger_dataset)

#train logisitic regression model on dataset
model <- glm(Fail~Temperature, challenger_dataset, family = "binomial")
#run statistics
summary(model)
#p < 0.05 as seen in Temperature = 0.0363

#plot the logistic model
plot(Fail~Temperature)
lines(Temperature, model$fitted, type = "l", col = "red")

#predict probability of FAIL when temperature is low at 53F
predict(model, data.frame(Temperature=53), type = "response")
#according to logistic regression model, probability of failure is ~99%