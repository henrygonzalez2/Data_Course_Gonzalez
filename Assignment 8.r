#1. 
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(readr)

getwd()
mushroom <- read_csv("./mushroom_growth.csv")

view(mushroom)

#2. 



mod1 = lm(GrowthRate ~ Nitrogen, data = mushroom)
summary(mod1)


#GrowthRate as a function of Nitrogen
ggplot(mushroom, aes(x=Nitrogen,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()



mod2 = lm(GrowthRate ~ Light, data = mushroom)
summary(mod2)

#GrowthRate as a function of Light 
ggplot(mushroom, aes(x=Light,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod3 = lm(GrowthRate ~ Humidity, data = mushroom)
summary(mod3)

#GrowthRate as a function of Humidity 
ggplot(mushroom, aes(x=Humidity,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#No linear model can be generated as the X variable is categorical instead of numerical.
#The multiple R -Sqaured vlue is 0.1926, which is pretty bad compared to the other R-Squared values.

mod4 = lm(GrowthRate ~ Temperature, data = mushroom)
summary(mod4)

#GrowthRate as a function of Temperature 
ggplot(mushroom, aes(x=Temperature,y=GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

#GrowthRate as a function of Temperature + Humidity
mod5 = lm(GrowthRate ~ Temperature + Humidity, data = mushroom)
summary(mod5)

#GrowthRate as a function of Temperature + Humidity + Nitrogen
mod6 = lm(GrowthRate ~ Temperature + Humidity + Nitrogen, data = mushroom)
summary(mod6)

#GrowthRate as a function of Temperature + Nitrogen
mod7 = lm(GrowthRate ~ Temperature + Nitrogen, data = mushroom)
summary(mod7)

#GrowthRate as a function of Temperature + Nitrogen
mod8 = lm(GrowthRate ~ Light + Nitrogen, data = mushroom)
summary(mod8)

#3.


#4. 
#Now we calculate the Mean-Squared-Error of each model, the smaller the better.

mean(mod1$residuals^2)
# = 9723

mean(mod2$residuals^2)
# = 7702

mean(mod3$residuals^2)
# = 7855

mean(mod4$residuals^2)
# = 9397

mean(mod5$residuals^2)
# = 7763

mean(mod6$residuals^2)
# = 7757

mean(mod7$residuals^2)
# = 9631

mean(mod8$residuals^2)
# = 7697

#Light is the best predictor of growth rate according to the Mean_Sqaured_Value

# mod8 improves on the Mean_Sqaured_Value defined by GrowthRate as a function of Light


#5
#We will make predictions based on mod8, GrowthRate as a function of Light and Nitrogen

#Model 8 has the best R^2 value

#6


newdf <- data.frame(
  Light = c(5,15,25,35,45),
  Nitrogen  = c(15,20,25,30,35)) # anything specified in the model needs to be here with exact matching column names

df <- mushroom %>% 
  add_predictions(mod2) 
df %>% dplyr::select("GrowthRate","pred")

# Make a new dataframe with the predictor values we want to assess
# mod1 only has "disp" as a predictor so that's what we want to add here


# making predictions
pred <- predict(mod8, newdata = newdf)

# combining hypothetical input data with predictions
hyp_preds <- data.frame(
  Light = newdf$Light,
  Nitrogen  = newdf$Nitrogen,
  pred  = pred
)

# Add column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining real data and hypothetical data
fullpreds <- dplyr::full_join(df, hyp_preds)


ggplot(fullpreds,aes(x=Light + Nitrogen,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=GrowthRate),color="Black") +
  theme_minimal()
#7.
# gather predictions from all 2 models
mushroom %>% 
  gather_predictions(mod2,mod8) %>% 
  ggplot(aes(x=Light + Nitrogen,y=GrowthRate)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label="Light") +
  annotate("text",x=250,y=30,label="Light + Nitrogen")
 

##extra code##
# put all models into a list
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4,mod5=mod5,mod6=mod6,mod7=mod7,mod8=mod8)
# apply "performance" function on all in the list and combine 
map(mods,performance) %>% reduce(full_join)

# put all models into a list (without categorical variables)
mods1 <- list(mod1=mod1,mod2=mod2,mod4=mod4,mod7=mod7,mod8=mod8)
# apply "performance" function on all in the list and combine 
map(mods1,performance) %>% reduce(full_join)







