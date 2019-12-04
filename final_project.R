library(readxl)
page1 <- read.csv(file.choose())
page2 <- read.csv(file.choose())
page3 <- read.csv(file.choose())
page4 <- read.csv(file.choose())
page5 <- read.csv(file.choose())
page6 <- read.csv(file.choose())
page7 <- read.csv(file.choose())
page8 <- read.csv(file.choose())
page9 <- read.csv(file.choose())
page10 <- read.csv(file.choose())
page11 <- read.csv(file.choose())
page12 <- read.csv(file.choose())
page13 <- read.csv(file.choose())
page14 <- read.csv(file.choose())

final_data = rbind(page1,page2,page3,page4,page5,page6,page7,page8,page9,page10,page11,page12,page13,page14)
new_final_data <- na.omit(final_data)
#5352 obs

#cleaning further
final_data=new_final_data[which(new_final_data$Price!="Request a Price"),]
#exporting to csv
write.csv(final_data,"final_data1.csv",row.names=FALSE)

boattrader<-read.csv(file.choose())
attach(boattrader)

#changing webscrap datatype to numeric
boattrader$Year = as.numeric(as.character(boattrader$Year))
boattrader$Price = as.numeric(as.character(boattrader$Price))
boattrader$Length = as.numeric(as.character(boattrader$Length))

# removing NA introduced in length in webscraping
boattrader <- na.omit(boattrader)

#removing duplicates
duplicate=duplicated(boattrader)
boattrader_clean = boattrader[!duplicate,]

# we got 4541 data points which are our final 

#calculating age
current_age = 2019
boattrader_clean$age = current_age - boattrader_clean$Year

#csv export of final data set
write.csv(boattrader_clean,"sdm_final_project2.csv",row.names=FALSE)

attach(boattrader_clean)


#Simple price-length regression model 
plot(Length,Price)
mod1 = lm(Price~ Length, data = boattrader_clean)
summary(mod1)
plot(Price,mod1$fitted.values)
par(mfrow = c(2, 2))
plot(mod1)


#Price-Age model
plot(age,Price)
mod2 = lm(Price~age, data = boattrader_clean)
summary(mod2)
plot(Price,mod2$fitted.values)

#Price-age log
mod3 = lm(log(Price)~age, data = boattrader_clean)
summary(mod3)
plot(Price,mod3$fitted.values)

#Price-Length log
plot(Length,Price)
mod4 = lm(Price~ log(Length), data = boattrader_clean)
summary(mod4)

#Price-length+age
mod5 = lm(Price~ Length+age, data = boattrader_clean)
summary(mod5)
plot(Price,mod5$fitted.values)

#Price,length,hull.material
mod6 = lm(Price~ Length+Hull.Material, data = boattrader_clean)
summary(mod6)
plot(Price,mod6$fitted.values)



#Price,length,hull.material,age
mod7 = lm(Price~ Length+Hull.Material+age, data = boattrader_clean)
summary(mod7)
plot(Price,mod7$fitted.values)

#Price,Length,Engine.Type
mod8 = lm(Price~ Length+Engine.Type+age, data = boattrader_clean)
summary(mod8)
plot(Price,mod8$fitted.values)

#Price,Length,Engine.Type,age
mod_random = lm(Price~ Length+Engine.Type+age, data = boattrader_clean)
summary(mod_random)
plot(Price,mod_random$fitted.values)

#Final_model
mod_final = lm(log(Price)~ Length+Engine.Type+age+Hull.Material+State+Class, data = boattrader2)
summary(mod_final)
plot(Price,mod_final$fitted.values)

#logPrice,length,hull.material,age
mod8 = lm(log(Price)~ Length+Hull.Material+age, data = boattrader_clean)
summary(mod8)
plot(Price,mod7$fitted.values)


#with age

mod9 = lm(log(Price)~ age +Class+Length, data = boattrader_clean1)
summary(mod9)
plot(Price,mod9$fitted.values)

#without age


# age and engine type
mod9 = lm(Price~ Engine.Type+age+Length, data = boattrader_clean)
summary(mod9)
plot(Price,mod9$fitted.values)


#Interaction variable
mod10 <- lm(log(Price)~ Length + age +age*Engine.Type)
summary(mod10)
plot(Price,mod10$fitted.values)



# Regression on class 
boattrader_clean_class=boattrader_clean[Class=="PWC",]
boattrader_clean_class_2=boattrader_clean[Class=="Power",]
boattrader_clean_class_3=boattrader_clean[Class=="Sails",]
boattrader2 = rbind(boattrader_clean_class,boattrader_clean_class_2,boattrader_clean_class_3)
write.csv(boattrader2,"boattrader2.csv",row.names=FALSE)

boattrader2 =read.csv("boattrader2.csv")
mod_class <- lm(boattrader2$Price~boattrader2$Hull.Material + boattrader2$Class + boattrader2$Engine.Type)
summary(mod_class)

mod_class <- lm(log(boattrader2$Price)~boattrader2$age + boattrader2$Length*boattrader2$Class)
summary(mod_class)

