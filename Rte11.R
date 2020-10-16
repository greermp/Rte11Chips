library(tidyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
# library(ggmap)
library(ggpubr)
library(treemap)
# library(ggExtra)
# raw_df <- read.csv('fb_anova.csv')
raw_df <- read.csv('rt11.csv')
raw_df <- raw_df[1:4]
raw_df$Date2 <- mdy(raw_df$Date)
raw_df$Date3 <- as.Date(raw_df$Date,format = "%m/%d/%y")
raw_df$Month = strftime(raw_df$Date3, '%m')
raw_df$Year = strftime(raw_df$Date3, '%Y')

ggplot(filter(raw_df,Size=='6oz' | Size=='2oz'), aes(x=Date2, y = Chips.Sold, group=Chip),size=5) + 
  geom_line(aes(color=Chip), alpha=.5, size=1) + facet_wrap(~Size, nrow = 2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + scale_x_date(breaks = "3 month", minor_breaks = "1 month", labels=date_format("%b%y"))  +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 2000))+ labs(title = "Average Sold by Type",subtitle = "2 and 6 Oz Only")


sumdf <- raw_df %>% 
  group_by(Date2, Size) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))
  
ggplot(filter(sumdf,mean.sold>0), aes(x=Date2, y = mean.sold, group=Size),size=5) + 
  geom_line(alpha=.5, size=1, color="#3B5998") + facet_wrap(~Size, ncol = 2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + scale_x_date(breaks = "3 month", minor_breaks = "2 months", labels=date_format("%b%y")) +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 2000)) + labs(title = "Average Sold by Size")


sumdf <- raw_df %>% 
  group_by(Date2) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))

ggplot(filter(sumdf,mean.sold>0), aes(x=Date2, y = mean.sold),size=5) + 
  geom_line(alpha=.5, size=1, color="#3B5998") + #facet_wrap(~Size, ncol = 2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + scale_x_date(name = "", breaks = "3 month", minor_breaks = "1 month", labels=date_format("%b")) +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 2000)) + labs(title = "Average Sold by Month")


bymon = raw_df
sumdf <- bymon %>% 
  group_by(Month,Year) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))

ggplot(filter(sumdf,mean.sold>0), aes(x=Month, y = mean.sold),size=5) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") + facet_wrap(~Year, ncol = 1) + 
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 5000)) + labs(title = "Average Sold by Month/Year")

bymon = raw_df
bymon$Month = strftime(raw_df$Date3, '%m')
sumdf1 <- bymon %>% 
  group_by(Month) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))

ggplot(filter(sumdf1,mean.sold>0), aes(x=Month, y = mean.sold),size=5) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 60000, 5000)) + labs(title = "Average Sold by Month")



# bytye <- raw_df %>% 
#   group_by(Chip) %>% 
#   summarise(sum_sold = sum(Chips.Sold))


percent_chip <- raw_df %>% 
  group_by(Chip) %>% 
  summarise(sum=sum(Chips.Sold)) 
percent_chip <- filter(percent_chip, sum>0)
percent_chip$total = sum(percent_chip$sum)
percent_chip$Percentage = percent_chip$sum / percent_chip$total


percent_chip$label <- paste(percent_chip$Chip,'\n', (round(percent_chip$Percentage*100,1)), '%')
percent_chip$per <- paste((round(percent_chip$Percentage*100,1)), '%')
# treemap
treemap(percent_chip,
        index="Chip",
        vSize="sum",
        type="index"
)

treemap(percent_chip,
        index="label",
        vSize="sum",
        type="index",
        palette="Set3",
        title="Route 11 Chip Popularity",
        fontsize.title=20
)

ggplot(percent_chip, aes(reorder(Chip,-sum), sum)) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") + coord_flip()+ 
  geom_text(aes(label = per), hjust = -0.5, size=2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Average Sold 2008-2010")  + labs(y="Units Sold", x="")

ggplot(bytye) + geom_bar(x=sum_sold)


percent_chip <- raw_df %>% 
  group_by(Chip, Size) %>% 
  summarise(sum=sum(Chips.Sold)) 
percent_chip <- filter(percent_chip, sum>0)
percent_chip$total = sum(percent_chip$sum)
percent_chip$Percentage = percent_chip$sum / percent_chip$total

