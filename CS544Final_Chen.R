#========================================
# Final project
# CS 544
# 2021 Fall
# By Yuepeng Chen, Jisun Lee  and Huiyu Yi
#========================================

library(plotly)
library(stats)
library(prob)
library(scales)
library(sampling)
library(wordcloud2)

#===========read the data and filtrate out something useless for this project=============
data <- read.csv("https://raw.githubusercontent.com/CocoYi3327/cs544final/main/startbucks.csv")
head(data)
Sys.setlocale('LC_ALL','C') # for encoding formate

#==================number of stores in each country======================================
####### show the overall view at the beginning

fig <- data
fig <- fig %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~latitude,
    lon = ~longitude,
    coloraxis = 'coloraxis',
    radius = 10) 
fig <- fig %>%
  layout(
    mapbox = list(
      style="stamen-terrain",
      center= list(lon=180)), coloraxis = list(colorscale = "Viridis"))

fig
a <- sort(table(data$countryCode),decreasing = TRUE)
a <- as.data.frame(a)
fig1 <- plot_ly( a, x = a$Var1,y = a$Freq,type = 'bar')
fig1 <- fig1 %>% layout(title = "Number of Starbucks in Each Countries", yaxis = list(type = 'linear'))
fig1


#=================summary of the number of stores around the world=======================
data1 <- table(data$countryCode)    #count the number of stores in each country
dataforsum <- as.data.frame(data1)
allsum <- summary(dataforsum$Freq)
paste("The",names(allsum), "number of the Starbucks in low denisity country is", allsum)
top5<-sort(data1,decreasing = TRUE)[1:5]
top5
fig3 <- plot_ly(y = dataforsum$Freq,type = 'box',name = "World")
fig3

#===============================delete outliers===============================
outliers <- boxplot.stats(dataforsum$Freq)$out   
Out <- which(dataforsum$Freq %in% c(outliers)) #position of outliers in the data frame
min(dataforsum[Out,]$Freq)      #the smallest outlier set as the boundary of extremely high density

#==================== Categorical variables ===========================================
#set density as below:
# <= 375 low
# 375-675 moderate
# 675-975 high
# >= 975 extremely high

LtoM <- 375
MtoH <- 675
HtoE <- 975


#filter out each density
dataL <- as.data.frame(data1[data1<=LtoM])
dataM <- data1[data1>LtoM]
dataM <- as.data.frame(dataM[dataM<=MtoH])
dataH <- data1[data1>MtoH]
dataH <- as.data.frame(dataH[dataH<=HtoE])
dataE <- as.data.frame(data1[data1>HtoE])
#count each density
low <- nrow(dataL)
moderate <- nrow(dataM)
high <- nrow(dataH)
ehigh <- nrow(dataE)
#make the table to show number of each density
names(low) <- "number"
categorical <- rbind(low = low,moderate = moderate,high = high,Extremly_high = ehigh)
categorical <- data.frame(categorical)
categorical         #show the table
# plot out
x2 <- c("Low","Moderate","High","Extremely High")
fixedx2 <- factor(x2,levels = c("Low","Moderate","High","Extremely High"))
fig2 <- plot_ly(x = fixedx2 ,y = categorical$number,type = 'bar')
fig2 <- fig2 %>% layout(title = "Number of Countries in Each Level of Density")
fig2

#==============================Summary of low density===================================
#because most of countries are in low density, we are going to analyze low density only
#analyze in words
lsum <- summary(dataL$Freq)
lsum
paste("The",names(lsum), "number of Starbucks in low denisity country is", lsum)

#=============================inside the US ============================================
#because the US has the most number of Starbucks, focus on only US

#==============================================
data2US <- subset(data,data$countryCode=="US")

#map of US

g <- list(
  scope = 'US',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

figMap <- plot_geo(data2US, lat = ~latitude, lon = ~longitude)
figMap <- figMap %>% add_markers(
  text = ~paste(streetAddressLine1, streetAddressLine2, streetAddressLine3, paste("OwnershipTypeCode:", ownershipTypeCode), sep = "<br />"),
  color = ~ownershipTypeCode, symbol = I("square"), size = I(8), hoverinfo = "text"
)
figMap <- figMap %>% layout(
  title = 'Starbucks in US', geo = g
)
figMap


data2 <- table(data2US$countrySubdivisionCode) 
#count the number of Starbucks in each state
#####
##### make a pie char here represent the percentage of each state
#####
data2_1 <- as.data.frame(data2)

figUSState <- plot_ly(data2_1, labels = ~Var1, values = ~Freq, type = 'pie')
figUSState <- figUSState %>% layout(title = 'Starbucks in US states',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figUSState
#===================================distribution======================================
size = nrow(data2US)
percentage = data2/size
percentage
#probability for 20 US Starbucks stores located in MA
probMA <- percentage[names(percentage)=='MA']
probMA
pmfMA <- dbinom(1:20,size,probMA)
pmfMA
pmfMA1 <- as.data.frame(table(pmfMA))
pmfMA1$Freq <- c(1:20)
pmfMA1
###### maybe replaced by a interface graph
######

plot(1:20,pmfMA,type="h",xlab = "20 times")   #pmf plot - each probability


# probability for picking one store in each state
pmfUS <- dbinom(1,size,percentage)
pmfUS

#=================================sampling=============================================
mean <- mean(data2)
sd <- sd(data2)
mean
sd
#with a huge sd, what will the sampling show with US data
set.seed(9672)
# generate randomize 50 virtual states with normal distribution

normalrandom <- rnorm(50,mean=mean,sd = sd);normalrandom

######
###### maybe replaced by a interface graph
######
hist(normalrandom)

#sample from random

# get samples with sample size 50,100,500,1000 from the US Starbucks.
ssize <- c(50,100,500,1000)

#simple random sampling
for(size in ssize){
  s1 <- srswr(size,nrow(data2))
  pickedrow1 <- (1:nrow(data2))[s1!=0]
  pickedrow1 <- rep(pickedrow1,s1[s1!=0])
  simplesample1 <- data2US[pickedrow1, ]
  output <- table(simplesample1$countrySubdivisionCode);output
  output1 <- as.data.frame(table(output));output1
  output1$Freq<- c('CO','WA','NY');output1
  cat("The simple random sample lays out with sample size", size, "is\n", names(output), "\n", output, "\n")
  
}

#pie
s1 <- srswr(50,nrow(data2))
pickedrow1 <- (1:nrow(data2))[s1!=0]
pickedrow1 <- rep(pickedrow1,s1[s1!=0])
simplesample1 <- data2US[pickedrow1, ]
output <- table(simplesample1$countrySubdivisionCode)
output <- as.data.frame(output)
figRandomSampling50 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figRandomSampling50 <- figRandomSampling50 %>% layout(title = 'Simple Random Sampling with sample size = 50',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figRandomSampling50
s1 <- srswr(100,nrow(data2))
pickedrow1 <- (1:nrow(data2))[s1!=0]
pickedrow1 <- rep(pickedrow1,s1[s1!=0])
simplesample1 <- data2US[pickedrow1, ]
output <- table(simplesample1$countrySubdivisionCode)
output <- as.data.frame(output)
figRandomSampling100 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figRandomSampling100 <- figRandomSampling100 %>% layout(title = 'Simple Random Sampling with sample size = 100',
                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figRandomSampling100
s1 <- srswr(500,nrow(data2))
pickedrow1 <- (1:nrow(data2))[s1!=0]
pickedrow1 <- rep(pickedrow1,s1[s1!=0])
simplesample1 <- data2US[pickedrow1, ]
output <- table(simplesample1$countrySubdivisionCode)
output <- as.data.frame(output)
figRandomSampling500 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figRandomSampling500 <- figRandomSampling500 %>% layout(title = 'Simple Random Sampling with sample size = 500',
                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figRandomSampling500
s1 <- srswr(1000,nrow(data2))
pickedrow1 <- (1:nrow(data2))[s1!=0]
pickedrow1 <- rep(pickedrow1,s1[s1!=0])
simplesample1 <- data2US[pickedrow1, ]
output <- table(simplesample1$countrySubdivisionCode)
output <- as.data.frame(output)
figRandomSampling1000 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figRandomSampling1000 <- figRandomSampling1000 %>% layout(title = 'Simple Random Sampling with sample size = 1000',
                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figRandomSampling1000


#systematic sampling
for(size in ssize){
  N <- nrow(data2)
  k <- ceiling(N/size)
  r <- sample(k,1)
  s <- seq(r,by=k,length = size)
  systematicsample <- data2US[s, ]
  output <- table(systematicsample$countrySubdivisionCode)
  cat("The systematic sample lays out with sample size", size, "is\n", names(output), "\n", output, "\n")
  
}
#pie
N <- nrow(data2)
k <- ceiling(N/50)
r <- sample(k,1)
s <- seq(r,by=k,length = 50)
systematicsample <- data2US[s, ]
output <- table(systematicsample$countrySubdivisionCode);output
output <- as.data.frame(output);output
figSystematicSampling50 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figSystematicSampling50 <- figSystematicSampling50 %>% layout(title = 'Systematic Sampling with sample size = 50',
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figSystematicSampling50

N <- nrow(data2)
k <- ceiling(N/100)
r <- sample(k,1)
s <- seq(r,by=k,length = 100)
systematicsample <- data2US[s, ]
output <- table(systematicsample$countrySubdivisionCode);output
output <- as.data.frame(output);output
figSystematicSampling100 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figSystematicSampling100 <- figSystematicSampling100 %>% layout(title = 'Systematic Sampling with sample size = 100',
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figSystematicSampling100

N <- nrow(data2)
k <- ceiling(N/500)
r <- sample(k,1)
s <- seq(r,by=k,length = 500)
systematicsample <- data2US[s, ]
output <- table(systematicsample$countrySubdivisionCode);output
output <- as.data.frame(output);output
figSystematicSampling500 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figSystematicSampling500 <- figSystematicSampling500 %>% layout(title = 'Systematic Sampling with sample size = 500',
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figSystematicSampling500


N <- nrow(data2)
k <- ceiling(N/1000)
r <- sample(k,1)
s <- seq(r,by=k,length = 1000)
systematicsample <- data2US[s, ]
output <- table(systematicsample$countrySubdivisionCode);output
output <- as.data.frame(output);output
figSystematicSampling1000 <- plot_ly(output, labels = ~Var1, values = ~Freq, type = 'pie')
figSystematicSampling1000 <- figSystematicSampling1000 %>% layout(title = 'Systematic Sampling with sample size = 1000',
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
figSystematicSampling1000


#conclusion because of the sd is too huge, these samples cannot represent the original dataset.


####
#### extra part
####
wordclouddata <- as.data.frame(table(data2US$countrySubdivisionCode));wordclouddata
wordcloud2(wordclouddata)

