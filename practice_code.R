library(car)
library(devtools)
library(easyGgplot2)
LRM050_1 <- read.csv("LRM_050_1_summer_raw_data.csv")

pH050 <- lm(pH ~ sin(2*pi*DateTimeFormatExt)+cos(2*pi*DateTimeFormatExt),data=LRM050_1)
pHfitted <- pH050$fitted.values
LRM050_1$pHfitted <- pHfitted

Temp050 <- lm(Temp ~ sin(2*pi*DateTimeFormatExt)+cos(2*pi*DateTimeFormatExt),data=LRM050_1)
Tempfitted <- Temp050$fitted.values
LRM050_1$Tempfitted <-Tempfitted

ODO050 <- lm(ODO ~ sin(2*pi*DateTimeFormatExt)+cos(2*pi*DateTimeFormatExt),data=LRM050_1)
ODOfitted <- ODO050$fitted.values
LRM050_1$ODOfitted <- ODOfitted

pH050plot <- ggplot(LRM050_1, aes(x=DateTimeFormatExt, y=pHfitted))+
  geom_line(col="red",lwd=0.5)+
  geom_point(aes(y=pH), size=2.5)+
  ggtitle("LRM050 Meter Data")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x=NULL, y="pH s.u.")

Temp050plot <- ggplot(LRM050_1, aes(x=DateTimeFormatExt, y=Tempfitted))+
  geom_line(col="red",lwd=0.5)+
  geom_point(aes(y=Temp), size=2.5)+
  labs(x=NULL, y="Temperature C")

ODO050plot <- ggplot(LRM050_1, aes(x=DateTimeFormatExt, y=ODOfitted))+
  geom_line(col="red",lwd=0.5)+
  geom_point(aes(y=ODO), size=2.5)+
  labs(x="Elapsed Time", y= "DO mg/L")

ggplot2.multiplot(pH050plot,Temp050plot,ODO050plot, cols=1)

summary(pH050)
outlierTest(pH050)

summary(Temp050)
outlierTest(Temp050)

summary(ODO050)
outlierTest(ODO050)
