require(ggplot2)
require(ggthemes)
require(captioner)
require(gridExtra)
require(ggmosaic)
require(lsr)

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

figs <- captioner(prefix="Figure")
tbls <- captioner(prefix="Table")

setwd("/Users/alexeimarcilio/Desktop/portfolio/telecom")
getwd()
freedom <- read.csv("freedom.csv") 

str(freedom)

summary(freedom$comp_rate)
summary(freedom$quiz)

table(freedom$channel)

p <- qplot(freedom$comp_rate, freedom$quiz, data=freedom, xlab="Completion Rate", ylab="Quiz Scores", main="", color=channel) + theme_economist_white()
p
#plot(freedom$comp_rate, freedom$quiz, main = "", ylab = "Quiz Scores", xlab = "Completion Rate", cex = .01)

plot1 <- qplot(freedom$comp_rate,
      geom="histogram", fill=I("orange"),
      xlab = "Completion Rate") + theme_economist_white()


plot2 <- qplot(freedom$quiz,
      geom="histogram", fill=I("blue"),
      xlab = "Quiz Scores") + theme_economist_white()



grid.arrange(plot1, plot2, ncol=2)

Mode <- function(x) {
    if (is.numeric(x)) {
        x_table <- table(x)
        return(as.numeric(names(x_table)[which.max(x_table)]))
    }
}

box.c <- ggplot(freedom, aes(x=factor(channel), y=comp_rate))


box.c <- box.c + geom_boxplot(aes(fill = channel)) + theme_economist_white() + theme(legend.position="none") + 
  theme(axis.title.x=element_blank()) + labs(y = "Completion Rate")

box.q <- ggplot(freedom, aes(x=factor(channel), y=quiz))


box.q <- box.q + geom_boxplot(aes(fill = channel)) + theme_economist_white() + theme(legend.position="none") + 
    theme(axis.title.x=element_blank()) + labs(y = "Quiz Scores")


grid.arrange(box.c, box.q, ncol=2)
#pl3 <- pl2 + ggtitle("Weight by Sex") + xlab("Sex 1=M, 0=F")





freedom.care <- subset(freedom, freedom$channel=="Care")

care.c <- ggplot(freedom.care, aes(x=factor(group_name), y=comp_rate))


care.c <- care.c + geom_boxplot(aes(fill = group_name)) + theme_economist_white() + theme(legend.position="none") + 
  theme(axis.title.x=element_blank()) + labs(y = "Completion Rate")

care.q <- ggplot(freedom.care, aes(x=factor(group_name), y=quiz))


care.q <- care.q + geom_boxplot(aes(fill = group_name)) + theme_economist_white() + theme(legend.position="none") + 
    theme(axis.title.x=element_blank()) + labs(y = "Quiz Scores")


grid.arrange(care.c, care.q, ncol=2)


freedom.corp <- subset(freedom, freedom$channel=="Corp-retail")

corp.c <- ggplot(freedom.corp, aes(x=factor(group_name), y=comp_rate))


corp.c <- corp.c + geom_boxplot(aes(fill = group_name)) + theme_economist_white() + theme(legend.position="none") + 
  theme(axis.title.x=element_blank()) + labs(y = "Completion Rate") + 
  #theme(axis.text.x = element_text(angle = 0, hjust = -1)) + 
  scale_x_discrete(labels=c("Clgry", "Edmon", "Ottawa", "ON East","ON Cen",
                            "ON SE","ON SW","Van East","Van West"))

corp.c
#grid.arrange(corp.c, corp.c, ncol=2)

#CORPCA01???Calgary CORPED01???Edmonton CORPON01???Ottawa CORPON02???ON East CORPON03???ON Central CORPON04???ON Southeast CORPON05???ON Southwest CORPVA01???Vancouver Est CORPVA02???Vancouver W


freedom.deal <- subset(freedom, freedom$channel=="Dealer-retail")

corp.d <- ggplot(freedom.deal, aes(x=factor(group_name), y=comp_rate))

#DEALCA01DEALED02DEALON01DEALON02DEALON03DEALON04DEALON05DEALON06DEALVA01

corp.d <- corp.d + geom_boxplot(aes(fill = group_name)) + theme_economist_white() + theme(legend.position="none") + 
  theme(axis.title.x=element_blank()) + labs(y = "Completion Rate") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  scale_x_discrete(labels=c("CA01", "ED02", "ON01", "ON02","ON03","ON04","ON05","ON06","VA01"))

corp.d
#grid.arrange(corp.c, corp.c, ncol=2)

#CORPCA01???Calgary CORPED01???Edmonton CORPON01???Ottawa CORPON02???ON East CORPON03???ON Central CORPON04???ON Southeast CORPON05???ON Southwest CORPVA01???Vancouver Est CORPVA02???Vancouver W


correl <- read.csv("correl.csv") 



p <- qplot(as.Date(as.character(correl$Day), "%Y%m%d"), correl$Completion, data=correl, xlab="Day", ylab="Completion Rate", main="") + theme_economist_white()
p

meanChannel = aggregate(list(mean = freedom$comp_rate), list(channel =factor(freedom$channel)), mean)

STDChannel = aggregate(list(std = freedom$comp_rate), list(channel = factor(freedom$channel)), sd)

channel <- merge(meanChannel,STDChannel, by  = "channel") 

meanCare = aggregate(list(mean = freedom.care$comp_rate), list(group =factor(freedom.care$group_name)), mean)

STDCare = aggregate(list(std = freedom.care$comp_rate), list(group = factor(freedom.care$group_name)), sd)

Care <- merge(meanCare,STDCare, by  = "group") 


meanDeal = aggregate(list(mean = freedom.deal$comp_rate), list(group =factor(freedom.deal$group_name)), mean)

STDDeal = aggregate(list(std = freedom.deal$comp_rate), list(group = factor(freedom.deal$group_name)), sd)

Deal <- merge(meanDeal,STDDeal, by  = "group") 
#grid.arrange(channel, Care, ncol=2)

channel
Care
Deal


m1 <- lm(quiz ~ comp_rate, data = freedom)
m1


p <- qplot(freedom$quiz, freedom$comp_rate, data=freedom, xlab="Completion Rate", ylab="Quiz Score", main="") + theme_economist_white() +  geom_smooth(method='lm')

qqnorm(m1$residuals) + theme_economist_white()
qqline(m1$residuals) + theme_economist_white()
p


#grid.arrange(p, p, ncol=2)
#p + abline(lm(freedom$quiz ~ freedom$comp_rate))


#plot(freedom$quiz ~ freedom$comp_rate) + theme_economist_white()
#abline(m1)


m1 <- lm(Completion ~ as.Date(as.character(correl$Day), "%Y%m%d"), data = correl)
m1
summary(m1)

p <- qplot(as.Date(as.character(correl$Day), "%Y%m%d"), correl$Completion, data=correl, xlab="Day", ylab="Completion Rate", main="") + theme_economist_white() +  geom_smooth(method='lm')

qqnorm(m1$residuals) + theme_economist_white()
qqline(m1$residuals) + theme_economist_white()
p


aovTest1 <- aov(comp_rate ~ channel, data = freedom)
summary.aov(aovTest1)

aovTest2 <- aov(comp_rate ~ group_name, data = freedom.care)
summary.aov(aovTest2)

aovTest3 <- aov(comp_rate ~ group_name, data = freedom.corp)
summary.aov(aovTest3)

aovTest3b <- aov(comp_rate ~ group_name, data = freedom.deal)
summary.aov(aovTest3b)

m1 <- lm(quiz ~ comp_rate, data = freedom)
summary(m1)

posthocPairwiseT( aovTest1, p.adjust.method = "bonferroni")

posthocPairwiseT( aovTest2, p.adjust.method = "bonferroni")

posthocPairwiseT( aovTest3b, p.adjust.method = "bonferroni")

print("Care channel")
inference(y=freedom.care$comp_rate, est="mean", type="ci", method="theoretical", graphics.off())
print("Corporate Retail channel")
inference(y=freedom.corp$comp_rate, est="mean", type="ci", method="theoretical")
print("Dealer Retail channel")
inference(y=freedom.deal$comp_rate, est="mean", type="ci", method="theoretical")

temp = aggregate(list(comp_rate = freedom$comp_rate), list(channel = factor(freedom$channel)), mean)

mean1 <- ggplot(temp, aes(x=channel, y = comp_rate, fill = channel)) + geom_bar(stat = "identity") +   
  theme_economist_white() + 
  theme(axis.title.x=element_blank()) + theme(legend.position="none") + labs(y = "Completion Rate") + 
  geom_text(aes(label=round(comp_rate,3))) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  scale_x_discrete(labels=c("Care", "Corp", "Deal"))

temp2 = aggregate(list(comp_rate = freedom.care$comp_rate), list(group_name = factor(freedom.care$group_name)), mean)

mean2 <- ggplot(temp2, aes(x=group_name, y = comp_rate, fill = group_name)) + geom_bar(stat = "identity") +   
  theme_economist_white() + 
  theme(axis.title.x=element_blank()) + theme(legend.position="none") + labs(y = "Completion Rate") + 
  geom_text(aes(label=round(comp_rate,3))) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  scale_x_discrete(labels=c("Star", "Tele", "Wind"))


temp3 = aggregate(list(comp_rate = freedom.deal$comp_rate), list(group_name = factor(freedom.deal$group_name)), mean)

mean3 <- ggplot(temp3, aes(x=group_name, y = comp_rate, fill = group_name)) + geom_bar(stat = "identity") +   
  theme_economist_white() + 
  theme(axis.title.x=element_blank()) + theme(legend.position="none") + labs(y = "Completion Rate") + 
  geom_text(aes(label=round(comp_rate,2))) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  scale_x_discrete(labels=c("C1", "E2", "O1", "O2", "O3", "O4", "O5","O6", "V1"))



grid.arrange(mean1, mean2, mean3, ncol=2)


## 
