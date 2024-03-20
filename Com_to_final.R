rawdata <- read.csv("F:/VIT/SY/SEM2/DS/CP/LitModel-main/content/combined_data.csv")
Total.loss <- rawdata$Estimated.Property.Loss + rawdata$Estimated.Content.Loss
rawdata <- data.frame(rawdata,Total.loss)
rawdata$Fire <- ifelse(rawdata$Total.loss >=500, 1,0)
dataset <- rawdata[rawdata$Fire ==1,]
sp <- count(dataset,vars = "Fire")
a <-rawdata[ sample( which( rawdata$Fire == 0 ) , sp[[2]][1] ) , ]
dataset <- rbind(dataset,a)
write.csv(dataset,"F:/VIT/SY/SEM2/DS/CP/LitModel-main/content/final_data.csv")

