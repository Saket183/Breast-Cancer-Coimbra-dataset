#Read the dataset Breast Cancer.csv
cancerdata <- read.csv("E:/Assignment/Exam Assignments/R/Breast Cancer.csv")

#check the structure of the Dataset
str(cancerdata)

#changing the response variable to factor
cancerdata$Classification <- as.factor(cancerdata$Classification)

#Run the model with all the variables
m_full <- glm(Classification ~ . , data = cancerdata,
           family = binomial())
summary(m_full)

#Remove variable Adiponectin with less significance and high p value to create new model
m1 <- glm(Classification ~ Age + BMI + Glucose + Insulin + HOMA + Leptin + 
            Resistin + MCP.1, data = cancerdata,
          family = binomial())
summary(m1)

anova(m1,m_full,test="Chisq")
#model m1 better than model m_full since p>0.5

#Remove variable HOMA with less significance and high p value to create new model
m2 <- glm(Classification ~ Age + BMI + Glucose + Insulin + Leptin + 
            Resistin + MCP.1, data = cancerdata,
          family = binomial())
summary(m2)

anova(m2,m1,test="Chisq")
#model m2 better than model m1 since p>0.5

#Remove Leptin less significance and high p value to create new model
m3 <- glm(Classification ~ Age + BMI + Glucose + Insulin  + 
            Resistin + MCP.1, data = cancerdata,
          family = binomial())
summary(m3)

anova(m3,m2,test="Chisq")
#model 3 better than model m2 since p>0.5

#Remove MCP.1 less significance and high p value to create new model
m4 <- glm(Classification ~ Age + BMI + Glucose + Insulin  + 
            Resistin, data = cancerdata,
          family = binomial())
summary(m4)

anova(m4,m3,test="Chisq")
#model m4 better than model m3 since p>0.5

#Remove Age less significance and high p value to create new model
m5 <- glm(Classification ~ BMI + Glucose + Insulin  + 
            Resistin, data = cancerdata,
          family = binomial())
summary(m5)

anova(m5,m4,test="Chisq")
#model m5 better than  model m4 since p>0.5

#Remove Insulin less significance and high p value to create new model
m6 <- glm(Classification ~ BMI + Glucose  + 
            Resistin, data = cancerdata,
          family = binomial())
summary(m6)

anova(m6,m5,test="Chisq")
#but here model m5 is better than model m6
#So m5 is the final model since P< 0.5 which is significant

#find the varibales with interaction
add1(m5,~ .^2,test = "Chisq")

#find the interaction between the variables in the model m5
m5_inter <- glm(Classification ~ BMI + Glucose + Insulin  + 
                  Resistin + BMI*Resistin + Glucose * Resistin, data = cancerdata,
                family = binomial())
summary(m5_inter)


#find the interaction between the variables in the model m5
m5_inter1 <- glm(Classification ~ BMI + Glucose + Insulin  + 
                  Resistin + BMI*Resistin*Glucose*Insulin, data = cancerdata,
                family = binomial())
summary(m5_inter1)

# 95% CI
confint(m5_inter)

# odds-ratio
exp(coef(m5_inter))

# 95% CI around coefficient
exp(confint(m5_inter))

#predict the Independent variable using Model m5
p <- predict(m5,type = "response")
head(predic)
tail(predic)

predic <- predict(m5_inter,type = "response")
head(predic)


#anova to check the better model between 
anova(m5,m5_inter,test = "Chisq")


#checking the accuracy of the model m5 using confucion matrix
pred1 < - ifelse(p>0.5, 1,0)
conmatrix <- table(Actual_Value= cancerdata$Classification,Predicted_Value = pred1)
conmatrix
(conmatrix[[1,1]]+conmatrix[[2,2]])/sum(conmatrix)

