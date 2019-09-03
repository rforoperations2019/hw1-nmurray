
library("anytime")

#Exploratory Data Analysis & Cleaning 
ny_brew <- read.csv("Wineries__Breweries__and_Distilleries.csv")
df_brew <- data.frame(ny_brew)
dim(df_brew)
names(df_brew)
colSums(is.na(df_brew))

#Remove Null Values
df_brew <- na.omit(df_brew)

#There is location data listed as 0--remove these cases 
df_brew <-df_brew[df_brew$Latitude != 0 & df_brew$Longitude != 0, ]

lapply(df_brew, class)
lapply(df_brew, typeof)

# Convert License Dates to Timeseries data 
df_brew$License.Original.Issue.Date <- anytime::anydate(df_brew$License.Original.Issue.Date)
df_brew$License.Effective.Date <- anytime::anydate(df_brew$License.Effective.Date)
df_brew$License.Expiration.Date <- anytime::anydate(df_brew$License.Expiration.Date)

#Drop Data Error
df_brew <- subset(df_brew, df_brew$License.Expiration.Date != "2100-06-30")

#Save and write for Shiny Use
write.csv(df_brew, file = "ny_brew_data.csv")
saveRDS(df_brew, file = "ny_brewery")
save(df_brew, file = "ny_brewery.Rdata")
