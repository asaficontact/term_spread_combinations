##################################################################################
#                          Macroeconomic Finance: Final Project                  #
#                                April  12, 2019                                 #
#                             Abdul Tawab Ajmal Safi                             #
##################################################################################

##################################################################################
#Main Setup
##################################################################################

# preliminaries (install packages if necessary)

install.packages("xts")
install.packages("pdfetch")
install.packages("pscl")
install.packages("stargazer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("car")
install.packages("readxl")
install.packages("sandwich")
install.packages("quantmod")
install.packages("Quandl")
install.packages("PerformanceAnalytics")
install.packages("factoextra")
install.packages("corrplot")
install.packages("dygraphs")
install.packages("igraph")
install.packages("visNetwork")
install.packages("vars")
install.packages("tframePlus")
install.packages("plot3D")
install.packages("plot3Drgl")
install.packages("plotly")
install.packages("rgl")
install.packages("scatterplot3d")
install.packages("lattice")
install.packages("lme4")
##############################
#Loading Libraries

library(xts)       # time series
library(pdfetch)   # loads data from the internet: https://www.rdocumentation.org/packages/pdfetch/versions/0.2.3
library(pscl)      # pseudo R2 for probit
library(stargazer) # nice format for tables
library(lubridate) # handle dates
library(tidyverse)
library(ggplot2)   # beautiful graphs
library(tidyr)
library(dplyr)
library(car)      #used for scatterplots
library(readxl)   #Read EXcel
library(sandwich)  # Newey-West standard errors 
library(quantmod)  #standard errors 
library(Quandl)  #standard errors 
library(PerformanceAnalytics)   # to calculate returns
library (factoextra)            # to do awesome PCA charts
library(corrplot)               # correlation matrix
library(dygraphs)
library(igraph) # Network plots https://igraph.org/r/doc/
library(visNetwork)
library(vars)
library(tframePlus)
library(plot3D)
library(plot3Drgl)
library(plotly)
library(rgl)
library(scatterplot3d)
library(lattice)
library(lme4)

#########################################################
#Set Up
#########################################################

##################################################################################
# Variable Scrubbing
rm(list=ls(all=TRUE))

#####################################
#pdfetch_FRED
us.cpi.FRED = pdfetch_FRED("CPIAUCSL")
us.unemp.FRED = pdfetch_FRED("UNRATE")


#####################################
#Excel Files
username = readline(prompt="Enter name: ")
setwd(paste("C:/Users/",username,"/Desktop/Macro-Finance Final Project/Data", collapse='', sep=''))

#YLDS
us.ylds.e = read_xlsx("ALL_YLDS/ALL_YLDS_US.xlsx")

#EXP
us.exp.e = read_xlsx("ALL_EXP/ALL_EXP_US.xlsx")

#TP 
us.tp.e = read_xlsx("ALL_TP/ALL_TP_US.xlsx")

#US TEN Year - 3 months
tenthree.FRED = pdfetch_FRED("T10Y3MM")

###########################################
#Change this

start.date = "1995-01-30"
end.date = "2018-10-31"
l = 120 #This indicates that we will be taking returns from 2 months up until 120 months

############################################
#Percentage change
#CPI
us.cpi.change = diff(log(us.cpi.FRED), lag=1)
us.cpi = window(us.cpi.change, start = start.date, end = end.date)
us.cpi = us.cpi*100

#UNEMP
us.unemp = window(us.unemp.FRED, start = start.date, end = end.date)

#TEN Year minus Three Months
tenthree = window(tenthree.FRED, start = start.date, end = end.date)

############################################
#Start and End Date of the yield
us.ylds.e = us.ylds.e[which(us.ylds.e[,1]>=start.date & us.ylds.e[,1]<=end.date),]
us.exp.e = us.exp.e[which(us.exp.e[,1]>=start.date & us.exp.e[,1]<=end.date),]
us.tp.e = us.tp.e[which(us.tp.e[,1]>=start.date & us.tp.e[,1]<=end.date),]

###########################################
#End of Setup 
###########################################


##########################################
##########################################
# US Yield Curve
##########################################
##########################################



#Term Spread Setup
###################################
#10 year - 3 months
#Long term = 24 months - 120 months
#Short term = 2 months - 18 months

longterm = 24:120 
shortterm = 2:18
nspread = 1649     #Number of possible term spread combinations

us.termspread = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread = cbind(us.termspread,us.ylds.e[,i]-us.ylds.e[,j])
  }
}
us.termspread = us.termspread[,-1]
# row.names(us.termspread) = data.matrix(us.ylds.e[,1])

#Label the rows of termspread as date
row.names(us.termspread) = format(seq(as.Date("1995-01-30"), by = "month", length.out = 286),"%Y-%m-%d")


###########################################
#Running a regression on all the term spreads and storing all Betas

#Regression with CPI


#Beta coefficient Extraction from Regression
beta.ylds.cpi.us = c()
for(i in 1:nspread){
  beta.ylds.cpi.us[i] = coefficients(lm(us.termspread[,i] ~ us.cpi))[2]
}

#P value Extraction from the Regression
p.ylds.cpi.us = c()
for(i in 1:nspread){
  p.ylds.cpi.us[i] = summary(lm(us.termspread[,i] ~ us.cpi))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.cpi.us = c()
for(i in 1:nspread){
  r.ylds.cpi.us[i] = summary(lm(us.termspread[,i] ~ us.cpi))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.cpi.us = c()
for(i in 1:nspread){
  adjr.ylds.cpi.us[i] = summary(lm(us.termspread[,i] ~ us.cpi))$adj.r.squared
}


#######################################################

# Rough Work for figuring out how to extract p value and R squared

# x = lm(us.termspread[,1] ~ us.cpi)
# View(x)
# x[["R"]]
# stargazer(x, title="Recesssion Forecasting Model [Australian Spread]", aline = TRUE , type = "text", keep.stat = "all")
# pR2(x)
# summary(x)$r.squared
# 
# lmp <- function (modelobject) {
#   if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
#   f <- summary(modelobject)$fstatistic
#   p <- pf(f[1],f[2],f[3],lower.tail=F)
#   attributes(p) <- NULL
#   return(p)
# }
# y = summary(x)$coefficients[8]
# y = summary(x)[12]
# y = summary(x)
# data.matrix(y)
# y[1]
# pVal <- anova(x)$'Pr(>F)'[1]

########################################################

#Regression with UNEMP

#Beta coefficient Extraction from Regression
beta.ylds.unemp.us = c()
for(i in 1:nspread){
  beta.ylds.unemp.us[i] = coefficients(lm(us.termspread[,i] ~ us.unemp))[2]
}

#P value Extraction from the Regression
p.ylds.unemp.us = c()
for(i in 1:nspread){
  p.ylds.unemp.us[i] = summary(lm(us.termspread[,i] ~ us.unemp))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.unemp.us = c()
for(i in 1:nspread){
  r.ylds.unemp.us[i] = summary(lm(us.termspread[,i] ~ us.unemp))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.unemp.us = c()
for(i in 1:nspread){
  adjr.ylds.unemp.us[i] = summary(lm(us.termspread[,i] ~ us.unemp))$adj.r.squared
}



#################################################
#3D Graphing 


####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]



########################################
#3D Graphing for Regression with CPI
########################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.cpi.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.ylds.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

################################

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240 
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

################################

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))


#R value
##################################

data.3d = cbind(r.ylds.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))



#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))




########################################
#3D Graphing for Regression with UNEMP
########################################


#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.unemp.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.ylds.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.ylds.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))

##########################################
##########################################
# End of US Yield Curve
##########################################
##########################################


##########################################
##########################################
# US Expectation Component
##########################################
##########################################


#Term Spread Setup
###################################
#10 year - 3 months
#Long term = 24 months - 120 months
#Short term = 2 months - 18 months

longterm = 24:120 
shortterm = 2:18
nspread = 1649     #Number of possible term spread combinations

us.termspread.exp = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.exp = cbind(us.termspread.exp,us.exp.e[,i]-us.exp.e[,j])
  }
}
us.termspread.exp = us.termspread.exp[,-1]

#Label the rows of termspread as date
row.names(us.termspread.exp) = format(seq(as.Date("1995-01-30"), by = "month", length.out = 286),"%Y-%m-%d")

###########################################
#Running a regression on all the term spreads and storing all Betas

#Regression with CPI

#Beta Coefficent Extraction from the Regression

beta.exp.cpi.us = c()
for(i in 1:nspread){
  beta.exp.cpi.us[i] = coefficients(glm(us.termspread.exp[,i] ~ us.cpi))[2]
}

#P value Extraction from the Regression
p.exp.cpi.us = c()
for(i in 1:nspread){
  p.exp.cpi.us[i] = summary(lm(us.termspread.exp[,i] ~ us.cpi))$coefficients[8]
}

#R squared Extraction From the Regression

r.exp.cpi.us = c()
for(i in 1:nspread){
  r.exp.cpi.us[i] = summary(lm(us.termspread.exp[,i] ~ us.cpi))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.exp.cpi.us = c()
for(i in 1:nspread){
  adjr.exp.cpi.us[i] = summary(lm(us.termspread.exp[,i] ~ us.cpi))$adj.r.squared
}


#Regression with UNEMP

#Beta Coefficent Extraction from the Regression

beta.exp.unemp.us = c()
for(i in 1:nspread){
  beta.exp.unemp.us[i] = coefficients(glm(us.termspread.exp[,i] ~ us.unemp))[2]
}

#P value Extraction from the Regression
p.exp.unemp.us = c()
for(i in 1:nspread){
  p.exp.unemp.us[i] = summary(lm(us.termspread.exp[,i] ~ us.unemp))$coefficients[8]
}

#R squared Extraction From the Regression

r.exp.unemp.us = c()
for(i in 1:nspread){
  r.exp.unemp.us[i] = summary(lm(us.termspread.exp[,i] ~ us.unemp))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.exp.unemp.us = c()
for(i in 1:nspread){
  adjr.exp.unemp.us[i] = summary(lm(us.termspread.exp[,i] ~ us.unemp))$adj.r.squared
}

#################################################
#3D Graphing 


####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]

########################################
#3D Graphing for Regression with CPI
########################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.exp.cpi.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.exp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.exp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.exp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.exp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Expectation Components] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Expectation Components] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Expectation Components] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P-Value')))

#R value
##################################

data.3d = cbind(r.exp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.exp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Value')))


#Adjusted R value
##################################

data.3d = cbind(adjr.exp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.exp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))



########################################
#3D Graphing for Regression with UNEMP
########################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.exp.unemp.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.exp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.exp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.exp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.exp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Expectation Components] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Expectation Components] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Expectation Components] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.exp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.exp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.exp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.exp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Expectation Components] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))


##########################################
##########################################
# End of US Expectation Component
##########################################
##########################################


##########################################
##########################################
# US Term Premium Component
##########################################
##########################################


#Term Spread Setup
###################################
#10 year - 3 months
#Long term = 24 months - 120 months
#Short term = 2 months - 18 months

longterm = 24:120 
shortterm = 2:18
nspread = 1649     #Number of possible term spread combinations

us.termspread.tp = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.tp = cbind(us.termspread.tp,us.tp.e[,i]-us.tp.e[,j])
  }
}
us.termspread.tp = us.termspread.tp[,-1]

#Label the rows of termspread as date
row.names(us.termspread.tp) = format(seq(as.Date("1995-01-30"), by = "month", length.out = 286),"%Y-%m-%d")

###########################################
#Running a regression on all the term spreads and storing all Betas

#Regression with CPI

#Beta Coefficent Extraction from the Regression

beta.tp.cpi.us = c()
for(i in 1:nspread){
  beta.tp.cpi.us[i] = coefficients(glm(us.termspread.tp[,i] ~ us.cpi))[2]
}

#P value Extraction from the Regression
p.tp.cpi.us = c()
for(i in 1:nspread){
  p.tp.cpi.us[i] = summary(lm(us.termspread.tp[,i] ~ us.cpi))$coefficients[8]
}

#R squared Extraction From the Regression

r.tp.cpi.us = c()
for(i in 1:nspread){
  r.tp.cpi.us[i] = summary(lm(us.termspread.tp[,i] ~ us.cpi))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.tp.cpi.us = c()
for(i in 1:nspread){
  adjr.tp.cpi.us[i] = summary(lm(us.termspread.tp[,i] ~ us.cpi))$adj.r.squared
}

#Regression with UNEMP

#Beta Coefficent Extraction from the Regression

beta.tp.unemp.us = c()
for(i in 1:nspread){
  beta.tp.unemp.us[i] = coefficients(glm(us.termspread.tp[,i] ~ us.unemp))[2]
}

#P value Extraction from the Regression
p.tp.unemp.us = c()
for(i in 1:nspread){
  p.tp.unemp.us[i] = summary(lm(us.termspread.tp[,i] ~ us.unemp))$coefficients[8]
}

#R squared Extraction From the Regression

r.tp.unemp.us = c()
for(i in 1:nspread){
  r.tp.unemp.us[i] = summary(lm(us.termspread.tp[,i] ~ us.unemp))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.tp.unemp.us = c()
for(i in 1:nspread){
  adjr.tp.unemp.us[i] = summary(lm(us.termspread.tp[,i] ~ us.unemp))$adj.r.squared
}


#################################################
#3D Graphing 


####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]

########################################
#3D Graphing for Regression with CPI
########################################

s.color = c()
for (i in 1:1649)
  if (p.tp.cpi.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.tp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.tp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.tp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.tp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Term Premium Component] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.tp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.tp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.tp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.tp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))



########################################
#3D Graphing for Regression with UNEMP
########################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.tp.unemp.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.tp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.tp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.tp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.tp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Term Premium Component] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.tp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.tp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Value')))


#Adjusted R value
##################################

data.3d = cbind(adjr.tp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.tp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Term Premium Component] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))





##########################################
##########################################
# End of US Term Premium Component
##########################################
##########################################


############################################################
############################################################
# US Long Run Yield Curve - Short Run Expectation Component
############################################################
############################################################


#Term Spread Setup
###################################
#10 year - 3 months
#Long term = 24 months - 120 months
#Short term = 2 months - 18 months

longterm = 24:120 
shortterm = 2:18
nspread = 1649     #Number of possible term spread combinations

us.termspread.yldsexp = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.yldsexp = cbind(us.termspread.yldsexp,us.ylds.e[,i]-us.exp.e[,j])
  }
}
us.termspread.yldsexp = us.termspread.yldsexp[,-1]

#Label the rows of termspread as date
row.names(us.termspread.yldsexp) = format(seq(as.Date("1995-01-30"), by = "month", length.out = 286),"%Y-%m-%d")

###########################################
#Running a regression on all the term spreads and storing all Betas

#Regression with CPI

#Beta Coefficent Extraction from the Regression

beta.yldsexp.cpi.us = c()
for(i in 1:nspread){
  beta.yldsexp.cpi.us[i] = coefficients(glm(us.termspread.yldsexp[,i] ~ us.cpi))[2]
}

#P value Extraction from the Regression
p.yldsexp.cpi.us = c()
for(i in 1:nspread){
  p.yldsexp.cpi.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.cpi))$coefficients[8]
}

#R squared Extraction From the Regression

r.yldsexp.cpi.us = c()
for(i in 1:nspread){
  r.yldsexp.cpi.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.cpi))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.yldsexp.cpi.us = c()
for(i in 1:nspread){
  adjr.yldsexp.cpi.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.cpi))$adj.r.squared
}

#Regression with UNEMP

beta.yldsexp.unemp.us = c()
for(i in 1:nspread){
  beta.yldsexp.unemp.us[i] = coefficients(glm(us.termspread.yldsexp[,i] ~ us.unemp))[2]
}

#P value Extraction from the Regression
p.yldsexp.unemp.us = c()
for(i in 1:nspread){
  p.yldsexp.unemp.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.unemp))$coefficients[8]
}

#R squared Extraction From the Regression

r.yldsexp.unemp.us = c()
for(i in 1:nspread){
  r.yldsexp.unemp.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.unemp))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.yldsexp.unemp.us = c()
for(i in 1:nspread){
  adjr.yldsexp.unemp.us[i] = summary(lm(us.termspread.yldsexp[,i] ~ us.unemp))$adj.r.squared
}


#################################################
#3D Graphing 


####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]

########################################
#3D Graphing for Regression with CPI
########################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.yldsexp.cpi.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.yldsexp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.yldsexp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.yldsexp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.yldsexp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.yldsexp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.yldsexp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.yldsexp.cpi.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.yldsexp.cpi.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and CPI Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))




########################################
#3D Graphing for Regression with UNEMP
########################################


#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.yldsexp.unemp.us[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.yldsexp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.yldsexp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "Change of beta coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.yldsexp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.yldsexp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "Change of p-value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "Change of P-Value coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.yldsexp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.yldsexp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "Change of R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.yldsexp.unemp.us, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.yldsexp.unemp.us

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and unemp Regression",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "Change of Adjusted R Squared coefficient of Term Spread [Long Run YLDS - Short Run EXP] and UNEMP Regression",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))


##################################################################
##################################################################
# End of US Long Run Yield Curve - Short Run Expectation Component
##################################################################
##################################################################



##################################################################
##################################################################
# Forecasting Macro Variables with Term Spread
##################################################################
###################################################################

######################################
#Basic Setup
######################################

#Short Term Setup (3 months)
T = length(us.cpi)
h = 3
us.cpi.3 = us.cpi[(h+1):T]
us.unemp.3 = us.unemp[(h+1):T]
us.ylds.e.3 = us.ylds.e[1:(T-h),]
us.exp.e.3 = us.exp.e[1:(T-h),]
us.tp.e.3 = us.tp.e[1:(T-h),]


#Short Term Setup (10 year)
T = length(us.cpi)
h = 120
us.cpi.120 = us.cpi[(h+1):T]
us.unemp.120 = us.unemp[(h+1):T]
us.ylds.e.120 = us.ylds.e[1:(T-h),]
us.exp.e.120 = us.exp.e[1:(T-h),]
us.tp.e.120 = us.tp.e[1:(T-h),]

#Term Spread Setup
###################################
#10 year - 3 months
#Long term = 24 months - 120 months
#Short term = 2 months - 18 months

longterm = 24:120 
shortterm = 2:18
nspread = 1649     #Number of possible term spread combinations

us.termspread.ylds.3 = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.ylds.3 = cbind(us.termspread.ylds.3,us.ylds.e.3[,i]-us.exp.e.3[,j])
  }
}
us.termspread.ylds.3 = us.termspread.ylds.3[,-1]

us.termspread.ylds.120 = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.ylds.120 = cbind(us.termspread.ylds.120,us.ylds.e.120[,i]-us.exp.e.120[,j])
  }
}
us.termspread.ylds.120 = us.termspread.ylds.120[,-1]

###########################################
#Running a regression on all the term spreads and storing all Betas

###############################
#3 Months
#Regression with CPI

#Beta Value Extraction From the Regression 

beta.ylds.cpi.us.3 = c()
for(i in 1:nspread){
  beta.ylds.cpi.us.3[i] = coefficients(lm(us.cpi.3 ~ us.termspread.ylds.3[,i]))[2]
}

#P value Extraction from the Regression
p.ylds.cpi.us.3 = c()
for(i in 1:nspread){
  p.ylds.cpi.us.3[i] = summary(lm(us.cpi.3 ~ us.termspread.ylds.3[,i]))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.cpi.us.3 = c()
for(i in 1:nspread){
  r.ylds.cpi.us.3[i] = summary(lm(us.cpi.3 ~ us.termspread.ylds.3[,i]))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.cpi.us.3 = c()
for(i in 1:nspread){
  adjr.ylds.cpi.us.3[i] = summary(lm(us.cpi.3 ~ us.termspread.ylds.3[,i]))$adj.r.squared
}

#Regression with UNEMP

#Beta Value Extraction From the Regression 

beta.ylds.unemp.us.3 = c()
for(i in 1:nspread){
  beta.ylds.unemp.us.3[i] = coefficients(lm(us.unemp.3 ~ us.termspread.ylds.3[,i]))[2]
}

#P value Extraction from the Regression
p.ylds.unemp.us.3 = c()
for(i in 1:nspread){
  p.ylds.unemp.us.3[i] = summary(lm(us.unemp.3 ~ us.termspread.ylds.3[,i]))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.unemp.us.3 = c()
for(i in 1:nspread){
  r.ylds.unemp.us.3[i] = summary(lm(us.unemp.3 ~ us.termspread.ylds.3[,i]))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.unemp.us.3 = c()
for(i in 1:nspread){
  adjr.ylds.unemp.us.3[i] = summary(lm(us.unemp.3 ~ us.termspread.ylds.3[,i]))$adj.r.squared
}

###############################
#10 Year
#Regression with CPI

#Beta Value Extraction From the Regression 

beta.ylds.cpi.us.120 = c()
for(i in 1:nspread){
  beta.ylds.cpi.us.120[i] = coefficients(lm(us.cpi.120 ~ us.termspread.ylds.120[,i]))[2]
}

#P value Extraction from the Regression
p.ylds.cpi.us.120 = c()
for(i in 1:nspread){
  p.ylds.cpi.us.120[i] = summary(lm(us.cpi.120 ~ us.termspread.ylds.120[,i]))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.cpi.us.120 = c()
for(i in 1:nspread){
  r.ylds.cpi.us.120[i] = summary(lm(us.cpi.120 ~ us.termspread.ylds.120[,i]))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.cpi.us.120 = c()
for(i in 1:nspread){
  adjr.ylds.cpi.us.120[i] = summary(lm(us.cpi.120 ~ us.termspread.ylds.120[,i]))$adj.r.squared
}

#Regression with UNEMP

#Beta Value Extraction From the Regression 

beta.ylds.unemp.us.120 = c()
for(i in 1:nspread){
  beta.ylds.unemp.us.120[i] = coefficients(lm(us.unemp.120 ~ us.termspread.ylds.120[,i]))[2]
}

#P value Extraction from the Regression
p.ylds.unemp.us.120 = c()
for(i in 1:nspread){
  p.ylds.unemp.us.120[i] = summary(lm(us.unemp.120 ~ us.termspread.ylds.120[,i]))$coefficients[8]
}

#R squared Extraction From the Regression

r.ylds.unemp.us.120 = c()
for(i in 1:nspread){
  r.ylds.unemp.us.120[i] = summary(lm(us.unemp.120 ~ us.termspread.ylds.120[,i]))$r.squared
}

#adjusted R Squared Extraction From Regression 

adjr.ylds.unemp.us.120 = c()
for(i in 1:nspread){
  adjr.ylds.unemp.us.120[i] = summary(lm(us.unemp.120 ~ us.termspread.ylds.120[,i]))$adj.r.squared
}


##################################################
#3D Graphing (3 Months - SHORT RUN)[CPI]
##################################################

####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]

########################################
#3D Graphing for Regression with CPI

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.cpi.us.3[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.ylds.cpi.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.cpi.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "CPI forecast with Term Spread over Short Run [Beta Coefficient] ")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "CPI forecast with Term Spread over Short Run [Beta Coefficient]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "CPI forecast with Term Spread over Short Run [Beta Coefficient]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.cpi.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.cpi.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "CPI forecast with Term Spread over Short Run [P Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "CPI forecast with Term Spread over Short Run [P Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "CPI forecast with Term Spread over Short Run [P Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Short Run [P Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Short Run [P Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Short Run [P Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.ylds.cpi.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.cpi.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "CPI forecast with Term Spread over Short Run [R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "CPI forecast with Term Spread over Short Run [R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "CPI forecast with Term Spread over Short Run [R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Short Run [R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Short Run [R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Short Run [R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))

#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.cpi.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.cpi.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))



##################################################
#3D Graphing (3 Months - SHORT RUN)[UNEMP]
##################################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.unemp.us.3[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)


#Beta
##################################

data.3d = cbind(beta.ylds.unemp.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.unemp.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "unemp forecast with Term Spread over Short Run [Beta Coefficient] ")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "unemp forecast with Term Spread over Short Run [Beta Coefficient]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "unemp forecast with Term Spread over Short Run [Beta Coefficient]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Short Run [Beta Coefficient]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.unemp.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.unemp.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "unemp forecast with Term Spread over Short Run [P Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "unemp forecast with Term Spread over Short Run [P Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "unemp forecast with Term Spread over Short Run [P Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Short Run [P Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Short Run [P Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Short Run [P Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.ylds.unemp.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.unemp.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "unemp forecast with Term Spread over Short Run [R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "unemp forecast with Term Spread over Short Run [R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "unemp forecast with Term Spread over Short Run [R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Short Run [R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Short Run [R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Short Run [R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.unemp.us.3, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.unemp.us.3

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Short Run [Adjusted R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))

##################################################
#3D Graphing (10 Year - LONG RUN)[CPI]
##################################################

####################
# Basic Setup

#long term 
long.term = c()
for (i in longterm){
  for(j in shortterm){
    long.term = rbind(long.term,rep(i,j))
  }
}
long.term = long.term[,-1]

#short term
short.term = c()
for (i in longterm){
  for(j in shortterm){
    short.term = rbind(short.term,rep(j,i))
  }
}
short.term = short.term[,1]

########################################
#3D Graphing for Regression with CPI

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.cpi.us.120[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.ylds.cpi.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.cpi.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "CPI forecast with Term Spread over Long Run [Beta Coefficient] ")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "CPI forecast with Term Spread over Long Run [Beta Coefficient]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "CPI forecast with Term Spread over Long Run [Beta Coefficient]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "CPI forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.cpi.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.cpi.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "CPI forecast with Term Spread over Long Run [P Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "CPI forecast with Term Spread over Long Run [P Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "CPI forecast with Term Spread over Long Run [P Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Long Run [P Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Long Run [P Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "CPI forecast with Term Spread over Long Run [P Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.ylds.cpi.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.cpi.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "CPI forecast with Term Spread over Long Run [R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "CPI forecast with Term Spread over Long Run [R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "CPI forecast with Term Spread over Long Run [R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Long Run [R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Long Run [R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "CPI forecast with Term Spread over Long Run [R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.cpi.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.cpi.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "CPI forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))



##################################################
#3D Graphing (10 Years - Long RUN)[UNEMP]
##################################################

#Significance Setup
s.color = c()
for (i in 1:1649)
  if (p.ylds.unemp.us.120[i] <= 0.1){
    s.color[i] = 1
  } else {
    s.color[i] = 0
  }

s.color[which(s.color == 0)] <- 'Not Significant'
s.color[which(s.color == 1)] <- 'Signifcant'
s.color <- as.factor(s.color)

#Beta
##################################

data.3d = cbind(beta.ylds.unemp.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- beta.ylds.unemp.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Beta",
              main = "unemp forecast with Term Spread over Long Run [Beta Coefficient] ")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "unemp forecast with Term Spread over Long Run [Beta Coefficient]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Beta",
       main = "unemp forecast with Term Spread over Long Run [Beta Coefficient]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Beta",
          main = "unemp forecast with Term Spread over Long Run [Beta Coefficient]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Beta')))

#P value
##################################

data.3d = cbind(p.ylds.unemp.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- p.ylds.unemp.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "P-Value",
              main = "unemp forecast with Term Spread over Long Run [P Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "unemp forecast with Term Spread over Long Run [P Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "P-Value",
       main = "unemp forecast with Term Spread over Long Run [P Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Long Run [P Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Long Run [P Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "P-Value",
          main = "unemp forecast with Term Spread over Long Run [P Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'P Value')))

#R value
##################################

data.3d = cbind(r.ylds.unemp.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- r.ylds.unemp.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 120
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "R Squared",
              main = "unemp forecast with Term Spread over Long Run [R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "unemp forecast with Term Spread over Long Run [R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "R Squared",
       main = "unemp forecast with Term Spread over Long Run [R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Long Run [R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Long Run [R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "R Squared",
          main = "unemp forecast with Term Spread over Long Run [R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'R Squared')))


#Adjusted R value
##################################

data.3d = cbind(adjr.ylds.unemp.us.120, long.term, short.term)

# x, y and z coordinates
x <- short.term
y <- long.term
z <- adjr.ylds.unemp.us.120

#3d Scatterplot
scatterplot3d(x,y,z, pch = 16, highlight.3d = T, angle = 240
              ,type = "h",
              xlab = "Short Run",
              ylab = "Long Run",
              zlab = "Adjusted R Squared",
              main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]")
#rgl 
plot3d(x,y,z,
       type = "p",
       col = "red",
       size = 4,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]")

plot3d(x,y,z,
       col = "red",
       type = "s",
       radius = 0.2,
       xlab = "Short Run",
       ylab = "Long Run",
       zlab = "Adjusted R Squared",
       main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]")

#scatter3d
scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = T)

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = T,
          fit = "smooth")

scatter3d(x,y,z,
          type = "p",
          col = "red",
          size = 4,
          xlab = "Short Run",
          ylab = "Long Run",
          zlab = "Adjusted R Squared",
          main = "unemp forecast with Term Spread over Long Run [Adjusted R Squared Value]",
          surface = F,
          grid = F,
          ellipsoid = T,
          axis.col = c("black","black","black"))

#Plotly

plot_ly(x = x, y = y, z = z,color = s.color, size = 0.3, colors = c('#BF382A', '#0C4B8E') )%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Short Run'),
                      yaxis = list(title = 'Long Run'),
                      zaxis = list(title = 'Adjusted R Squared')))



##################################################################
##################################################################
# End of Forecasting Macro Variables with Term Spread
##################################################################
###################################################################


#########################################################
#Term Spread Graphs
#########################################################
#Note: To run the term spread graph codes first run all the term spread code above
#Set up 

#number of combinations
ncom = c(1:1649)

#Dates
dates = format(seq(as.Date("1995-01-30"), by = "month", length.out = 286),"%Y-%m-%d")

#Term Spread(YLDS)

plot_ly(x = ncom, y = dates, z = ~data.matrix(us.termspread)) %>% add_surface()%>%
  layout(title = "Yields Term Spread",
         scene = list(xaxis = list(title = 'Long & Short Run Combinations'),
                      yaxis = list(title = 'Times (Monthly)', labels=T),
                      zaxis = list(title = 'Term Spread (YLDS)')))




#Term Spread(EXP)

plot_ly(x = ncom, y = dates, z = ~data.matrix(us.termspread.exp)) %>% add_surface()%>%
  layout(title = "Expectation Component Term Spread",
         scene = list(xaxis = list(title = 'Long & Short Run Combinations'),
                      yaxis = list(title = 'Times (Monthly)', labels=T),
                      zaxis = list(title = 'Term Spread (EXP)')))



# plot_ly(z = ~data.matrix(us.termspread.exp)) %>% add_surface(
#   contours = list(
#     z = list(
#       show=TRUE,
#       usecolormap=TRUE,
#       highlightcolor="#ff0000",
#       project=list(z=TRUE)
#     )
#   )
# ) %>%
#   layout(
#     scene = list(
#       camera=list(
#         eye = list(x=1.87, y=0.88, z=-0.64)
#       )
#     )
#   )

#Term Spread(TP)

plot_ly(x = ncom, y = dates, z = ~data.matrix(us.termspread.tp)) %>% add_surface()%>%
  layout(title = "Term Permium Component Term Spread",
         scene = list(xaxis = list(title = 'Long & Short Run Combinations'),
                      yaxis = list(title = 'Times (Monthly)', labels=T),
                      zaxis = list(title = 'Term Spread (TP)')))


# 
# plot_ly(z = ~data.matrix(us.termspread.tp)) %>% add_surface(
#   contours = list(
#     z = list(
#       show=TRUE,
#       usecolormap=TRUE,
#       highlightcolor="#ff0000",
#       project=list(z=TRUE)
#     )
#   )
# ) %>%
#   layout(
#     scene = list(
#       camera=list(
#         eye = list(x=1.87, y=0.88, z=-0.64)
#       )
#     )
#   )


##########################################################
#Principle Component Analyis
##########################################################

pca = prcomp(us.termspread, center = TRUE, scale = TRUE)

plot(pca, type = "l") #This is called the Scret test which draws the line graph to show you how much variance each component accounts for.
fviz_eig(pca)

us.termspread.pca1 = pca$x[,1]
us.termspread.pca2 = pca$x[,2]
us.termspread.pca3 = pca$x[,3]

#1st PCA Graph
plot(us.termspread.pca1, type = "l",
     main="YLDS Term Spread PCA1", ylab="Percent", xlab="Time", ylim=c(), las=1, col="black",xaxt="n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))

#Second PCA Graph
plot(us.termspread.pca2, type = "l",
     main="YLDS Term Spread PCA1", ylab="Percent Change", xlab="Time", ylim=c(), las=1, col="black", lwd=3,xaxt="n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))

#3rd PCA Graph
plot(us.termspread.pca3, type = "l",
     main="YLDS Term Spread PCA1", ylab="Percent Change", xlab="Time", ylim=c(), las=1, col="black", lwd=3,xaxt="n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))

#fviz_pca_var(pca$rotation[,1], col.var = "contrib", repel = TRUE) + scale_color_gradient2(low="pink", mid="blue", high="red", midpoint=9)

##################################
#Spanning Test on PCA1
##################################

reg_PCA1_1 = lm(us.cpi ~ us.termspread.pca1)
stargazer(reg_PCA1_1, aline = TRUE , type = "latex", keep.stat = "all")

reg_PCA1_1 = lm(us.unemp ~ us.termspread.pca1)
stargazer(reg_PCA1_1, aline = TRUE , type = "latex", keep.stat = "all")

reg_PCA1_1 = lm(us.termspread.pca1 ~ us.cpi)
reg_PCA1_2 = lm(us.termspread.pca1 ~ us.cpi + us.unemp)
stargazer(reg_PCA1_1, reg_PCA1_2, aline = TRUE , type = "latex", keep.stat = "all")

#Scatterplot of PCA1 against CPI
spreadscatter = c(1:(length(us.termspread.pca1)))
for (i in 1:(length(us.termspread.pca1))){
  spreadscatter[i] = us.termspread.pca1[i]
}

cpiscatter = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  cpiscatter[i] = us.cpi[i]
}


plot(x = spreadscatter,y = cpiscatter,
     xlab = "YLDS Term Spread PCA 1",
     ylab = "CPI Change",
     xlim = c(-82,82),
     ylim = c(-2,2),		 
     main = "CPI Change vs YLDS Term Spread PCA 1"
)

#Scatterplot of PCA1 against UNEMP
spreadscatter = c(1:(length(us.termspread.pca1)))
for (i in 1:(length(us.termspread.pca1))){
  spreadscatter[i] = us.termspread.pca1[i]
}

unempscatter = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  unempscatter[i] = us.unemp[i]
}

plot(x = spreadscatter,y = unempscatter,
     xlab = "YLDS Term Spread PCA 1",
     ylab = "UNEMP Change",
     xlim = c(-82,82),
     ylim = c(3,11),		 
     main = "UNEMP Change vs YLDS Term Spread PCA 1"
)

#PCA1 against CPI
us.cpi.plot = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  us.cpi.plot[i] = us.cpi[i]
}
plot(us.termspread.pca1, type = "l",
     main="CPI and YLDS Term Spread PCA 1", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.cpi.plot, col = "brown")
text(50, 6, "CPI Curve", col = "brown")
text(250, 50, "YLDS Term Spread PCA 1")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")

#PCA1 against UNEMP
us.unemp.plot = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  us.unemp.plot[i] = us.unemp[i]
}
plot(us.termspread.pca1, type = "l",
     main="CPI and YLDS Term Spread PCA 1", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.unemp.plot, col = "brown")
text(50, 9, "UNEMP Curve", col = "brown")
text(250, 50, "YLDS Term Spread PCA 1")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")

#####################################
#Spanning Test on PCA2
#####################################

reg_PCA2_1 = lm(us.termspread.pca2 ~ us.cpi)
reg_PCA2_2 = lm(us.termspread.pca2 ~ us.cpi + us.unemp)
stargazer(reg_PCA2_1, reg_PCA2_2, title="Regression for Spanning Hypothesis test of PCA2", aline = TRUE , type = "text", keep.stat = "all")

#Scatterplot of PCA2 against CPI
spreadscatter = c(1:(length(us.termspread.pca2)))
for (i in 1:(length(us.termspread.pca2))){
  spreadscatter[i] = us.termspread.pca2[i]
}

cpiscatter = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  cpiscatter[i] = us.cpi[i]
}


plot(x = spreadscatter,y = cpiscatter,
     xlab = "YLDS Term Spread PCA 2",
     ylab = "CPI Change",
     xlim = c(-30,20),
     ylim = c(-2,2),		 
     main = "CPI Change vs YLDS Term Spread PCA 2"
)

#Scatterplot of PCA1 against UNEMP
spreadscatter = c(1:(length(us.termspread.pca2)))
for (i in 1:(length(us.termspread.pca2))){
  spreadscatter[i] = us.termspread.pca2[i]
}

unempscatter = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  unempscatter[i] = us.unemp[i]
}

plot(x = spreadscatter,y = unempscatter,
     xlab = "YLDS Term Spread PCA 2",
     ylab = "UNEMP Change",
     xlim = c(-30,20),
     ylim = c(3,11),		 
     main = "UNEMP Change vs YLDS Term Spread PCA 2"
)

#PCA2 against CPI
us.cpi.plot = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  us.cpi.plot[i] = us.cpi[i]
}
plot(us.termspread.pca2, type = "l",
     main="CPI and YLDS Term Spread PCA 2", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.cpi.plot, col = "brown")
text(50, 7, "CPI Curve", col = "brown")
text(120, 15, "YLDS Term Spread PCA 2")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")

#PCA2 against UNEMP
us.unemp.plot = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  us.unemp.plot[i] = us.unemp[i]
}
plot(us.termspread.pca2, type = "l",
     main="CPI and YLDS Term Spread PCA 2", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.unemp.plot, col = "brown")
text(50, 7, "UNEMP Curve", col = "brown")
text(120, 15, "YLDS Term Spread PCA 2")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")

############################################
#Spanning Test on PCA3
############################################

reg_PCA3_1 = lm(us.termspread.pca3 ~ us.cpi)
reg_PCA3_2 = lm(us.termspread.pca3 ~ us.cpi + us.unemp)
stargazer(reg_PCA3_1, reg_PCA3_2, title="Regression for Spanning Hypothesis test of PCA3", aline = TRUE , type = "text", keep.stat = "all")

#Scatterplot of PCA3 against CPI
spreadscatter = c(1:(length(us.termspread.pca3)))
for (i in 1:(length(us.termspread.pca3))){
  spreadscatter[i] = us.termspread.pca3[i]
}

cpiscatter = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  cpiscatter[i] = us.cpi[i]
}


plot(x = spreadscatter,y = cpiscatter,
     xlab = "YLDS Term Spread PCA 3",
     ylab = "CPI Change",
     xlim = c(-15,9),
     ylim = c(-2,2),		 
     main = "CPI Change vs YLDS Term Spread PCA 3"
)

#Scatterplot of PCA3 against UNEMP
spreadscatter = c(1:(length(us.termspread.pca3)))
for (i in 1:(length(us.termspread.pca3))){
  spreadscatter[i] = us.termspread.pca3[i]
}

unempscatter = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  unempscatter[i] = us.unemp[i]
}

plot(x = spreadscatter,y = unempscatter,
     xlab = "YLDS Term Spread PCA 3",
     ylab = "UNEMP Change",
     xlim = c(-15,9),
     ylim = c(3,11),		 
     main = "UNEMP Change vs YLDS Term Spread PCA 3"
)

#PCA3 against CPI
us.cpi.plot = c(1:(length(us.cpi)))
for (i in 1:(length(us.cpi))){
  us.cpi.plot[i] = us.cpi[i]
}
plot(us.termspread.pca3, type = "l",
     main="CPI and YLDS Term Spread PCA 3", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.cpi.plot, col = "brown")
text(50, 2, "CPI Curve", col = "brown")
text(250, 7, "YLDS Term Spread PCA 3")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")

#PCA3 against UNEMP
us.unemp.plot = c(1:(length(us.unemp)))
for (i in 1:(length(us.unemp))){
  us.unemp.plot[i] = us.unemp[i]
}
plot(us.termspread.pca3, type = "l",
     main="CPI and YLDS Term Spread PCA 3", ylab="Percent", xlab="Time", las=1, col="gold", lwd=3, xaxt = "n")
axis(1, at = c(0,50,100,150,200,250,286), labels = c("1995","1999","2003","2007","2011","2015","2018"))
lines(us.unemp.plot, col = "brown")
text(50, 7, "UNEMP Curve", col = "brown")
text(250, 7, "YLDS Term Spread PCA 3")
mtext("",font=2, side=0, adj=-1.3, cex=0.75, line=20)
box(lty=1, lwd=1, col="deepskyblue3")


#######################################################
#Measuing the Dispersion of Spread
######################################################
plot(tenthree, type = "l")

#Term Spread Dispersion Creation
us.termspread.dispersion = matrix()
for (i in 1:1649){
  us.termspread.dispersion = cbind(us.termspread.dispersion,us.termspread[,i]-tenthree[,1])
}
us.termspread.dispersion = us.termspread.dispersion[,-1]

#Set Up for regression
TermPremium = as.numeric(unlist(us.termspread.tp))
Dispersion = as.numeric(unlist(us.termspread.dispersion))
YLDS.Term.Spread = as.numeric(unlist(us.termspread))
Expectation = as.numeric(unlist(us.termspread.exp))

#Regression of TP against dispersion, YLDS and EXPECTATIOn
reg_d.tp_1 = glm(TermPremium ~ Dispersion)
reg_d.tp_2 = glm(TermPremium ~ Dispersion + Expectation)
reg_d.tp_3 = glm(TermPremium ~ Dispersion + YLDS.Term.Spread + Expectation)
stargazer(reg_d.tp_1,reg_d.tp_2, aline = TRUE , type = "latex", keep.stat = "all")

#Regression of EXPECTATION against dispersion and YLDS.
reg_d.exp_1 = glm(Expectation ~ Dispersion)
reg_d.exp_2 = glm(Expectation ~ Dispersion + YLDS.Term.Spread)
stargazer(reg_d.exp_1,reg_d.exp_2, title="Regression for Spanning Hypothesis test of PCA3", aline = TRUE , type = "text", keep.stat = "all")


#Creating a lag 
#Short Term Setup (3 months)
T = 286
h = 72
us.tp.e.3 = us.tp.e[(h+1):T,]
tenthree.e.3 = tenthree[1:(T-h)]
us.ylds.e.3 = us.ylds.e[1:(T-h),]


#lagged term spread
longterm = 24:120 
shortterm = 2:18
nspread = 1649    

us.termspread.3 = matrix()
for (i in longterm){
  for(j in shortterm){
    us.termspread.3 = cbind(us.termspread.3,us.ylds.e.3[,i]-us.ylds.e.3[,j])
  }
}
us.termspread.3 = us.termspread.3[,-1]

#lagged term Premium
us.tp.3 = matrix()
for (i in longterm){
  for(j in shortterm){
    us.tp.3 = cbind(us.tp.3,us.tp.e.3[,i]-us.tp.e.3[,j])
  }
}
us.tp.3 = us.tp.3[,-1]

#lagged term Dispersion

us.termspread.dispersion.3 = matrix()
for (i in 1:1649){
  us.termspread.dispersion.3 = cbind(us.termspread.dispersion.3,us.termspread.3[,i]-tenthree.e.3[,1])
}
us.termspread.dispersion.3 = us.termspread.dispersion.3[,-1]

#Set Up for regression
TermPremium.60 = as.numeric(unlist(us.tp.3))
Dispersion.60 = as.numeric(unlist(us.termspread.dispersion.3))
YLDS.Term.Spread.60 = as.numeric(unlist(us.termspread.3))

#Regression of TP against dispersion, YLDS and EXPECTATIOn
reg_d.tp_1 = glm(TermPremium.60 ~ Dispersion.60)
reg_d.tp_2 = glm(TermPremium.3 ~ Dispersion.3 + YLDS.Term.Spread.3)
stargazer(reg_d.tp_1, aline = TRUE , type = "text", keep.stat = "all")

