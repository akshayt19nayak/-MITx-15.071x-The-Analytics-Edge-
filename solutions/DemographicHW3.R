cps <- read.csv(file.choose())

#1.1 and 1.2
summary(cps)

#1.3
sort(table(cps$State))

#1.4 summary(cps)
((116639+7073)/(7590+7073+116639))

#1.5
as.factor(cps$Hispanic)
table((cps[cps$Hispanic==1,])$Race)

#2.1
summary(cps)

#2.2 
table(cps$Region,is.na(cps$Married))
table(cps$Sex,is.na(cps$Married))
table(cps$Age,is.na(cps$Married)) #cps doesn't ask for marital status under the age of 14

#2.3
table(cps$State, is.na(cps$MetroAreaCode))

#2.4
table(cps$Region,is.na(cps$MetroAreaCode))

#2.5
tapply(is.na(cps$MetroAreaCode),cps$State,mean)

#3.1
CountryCodes = read.csv(file.choose())
MetroCodes = read.csv(file.choose())

#3.2
cps = merge(cps,MetroCodes, by.x="MetroAreaCode",by.y="Code",all.x=TRUE)
summary(cps)

#3.3
sort(table(cps$MetroArea))

#3.4
sort(tapply(cps$Hispanic, cps$MetroArea, mean))

#3.5
sort(tapply((cps$Race == 'Asian'),cps$MetroArea, mean))

#3.6
sort(tapply((cps$Education=='No high school diploma'),cps$MetroArea, mean, na.rm = TRUE))

#4.1
cps=merge(cps, CountryCodes, by.x="CountryOfBirthCode",by.y='Code',all.x=TRUE)
str(cps)
summary(cps)

#4.2
summary(cps)

#4.3
tapply(dps$Country!="United States",
  dps$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",mean, na.rm=TRUE)

dps = cps[cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA',]

#4.4
sort(tapply(cps$Country=='India',cps$MetroArea,sum,na.rm=TRUE))
sort(tapply(cps$Country=='Brazil',cps$MetroArea,sum,na.rm=TRUE))
sort(tapply(cps$Country=='Somalia',cps$MetroArea,sum,na.rm=TRUE))
