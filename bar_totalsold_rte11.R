library(tidyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(RColorBrewer)
library(ggrepel)
# library(ggmap)
library(ggpubr)
library(treemap)
library(gghighlight)
# library(ggExtra)
# raw_df <- read.csv('fb_anova.csv')
raw_df <- read.csv('rt11.csv')
raw_df <- raw_df[1:4]
raw_df$Date2 <- mdy(raw_df$Date)
raw_df$Date3 <- as.Date(raw_df$Date,format = "%m/%d/%y")
raw_df$Month = strftime(raw_df$Date3, '%b')
raw_df$Year = strftime(raw_df$Date3, '%Y')
# raw_df$Month =  format(as.Date(raw_df$Date3), "%b")
raw_df$Month= factor(raw_df$Month, levels = month.abb)

ggplot(filter(raw_df,Size=='6oz' | Size=='2oz'), aes(x=Date2, y = Chips.Sold, group=Chip),size=5) + 
  geom_line(aes(color=Chip), alpha=.5, size=1) + facet_wrap(~Size, nrow = 2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + scale_x_date(breaks = "3 month", minor_breaks = "1 month", labels=date_format("%b%y"))  +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 2000))+ labs(title = "Average Sold by Type",subtitle = "2 and 6 Oz Only")


sumdf <- raw_df %>% 
  group_by(Date2, Size) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))
  
ggplot(filter(sumdf,mean.sold>0), aes(x=Date2, y = mean.sold, group=Size),size=5) + 
  geom_line(alpha=.5, size=1, color="#3B5998", lineend="round") + facet_wrap(~Size, ncol = 2) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + scale_x_date(breaks = "3 month", minor_breaks = "2 months", labels=date_format("%b%y")) +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 30000, 2000)) + labs(title = "Average Sold by Size")


sumdf <- raw_df %>% 
  group_by(Date2) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))

ggplot(filter(sumdf,mean.sold>0), aes(x=Date2, y = mean.sold),size=5) + 
  geom_line(alpha=.5, size=1, color="#3B5998", lineend="round") + #facet_wrap(~Size, ncol = 2) +
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
x <- bymon %>% 
  group_by(Month) %>% 
  summarise(mean.sold = mean(sum(Chips.Sold)))
x <- filter(x, mean.sold>0)
x <-ungroup(x)


ggplot(x, aes(x=Month, y = mean.sold, group=1)) +geom_smooth()
       
       
  

bymon = raw_df


sumdf1 <- bymon %>% 
  group_by(Year,Month,Chip) %>% 
  summarise(mean.sold = sum(Chips.Sold))  ##MPG Took out mean
sumdf2 <- sumdf1 %>% 
  group_by(Month,Chip) %>% 
  summarise(mean.sold = mean.sold)##MPG Took out mean


#With Chip filler
ggplot(filter(sumdf2,mean.sold>0), aes(x=Month, y = mean.sold,  fill=Chip),size=5) +
  geom_col(alpha=.7, size=1) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() +scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name="Average Sold",breaks=seq(0, 60000, 1000)) + labs(title = "Average Sold by Month")

sumdf1 <- bymon %>% 
  group_by(Month) %>% 
  summarise(mean.sold = sum(Chips.Sold))

ggplot(filter(sumdf1,mean.sold>0), aes(x=Month, y = mean.sold),size=5) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() +
  scale_y_continuous(name="Average Sold",breaks=seq(0, 60000, 1000)) + labs(title = "Average Sold by Month")



#Using the data on actual wholesale purchases during this same time period, confirm (or refute) 
#that these are accurate market share predictions for these flavors.
percent_chip <- filter(raw_df, raw_df$Year == 2006 | raw_df$Year == 2007 | raw_df$Year == 2008)
percent_chip <- raw_df %>% 
  group_by(Chip) %>% 
  summarise(sum=sum(Chips.Sold)) 
percent_chip <- filter(percent_chip, sum>0)
percent_chip$total = sum(percent_chip$sum)
percent_chip$Percentage = percent_chip$sum / percent_chip$total
percent_chip$label <- paste(percent_chip$Chip,'\n', (round(percent_chip$Percentage*100,1)), '%')
percent_chip$per <- paste((round(percent_chip$Percentage*100,1)), '%')
# treemap
# treemap(percent_chip,
#         index="Chip",
#         vSize="sum",
#         type="index"
# )

treemap(percent_chip,
        index="label",
        vSize="sum",
        type="index",
        palette="Set3",
        title="Route 11 Chip Popularity '06-'08",
        fontsize.title=20
)

ggplot(percent_chip, aes(reorder(Chip,-sum), sum)) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") + coord_flip()+ 
  geom_text(aes(label = per), hjust = -0.003, size=4) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Route 11 Chip Popularity '06-'08")  + labs(y="Units Sold", x="")


percent_chip0910 <- filter(raw_df, raw_df$Year == 2009 | raw_df$Year == 2010)
percent_chip0910 <- percent_chip0910 %>% 
  group_by(Chip) %>% 
  summarise(sum=sum(Chips.Sold)) 
percent_chip0910 <- filter(percent_chip0910, sum>0)
percent_chip0910$total = sum(percent_chip0910$sum)
percent_chip0910$Percentage = percent_chip0910$sum / percent_chip0910$total
percent_chip0910$label <- paste(percent_chip0910$Chip,'\n', (round(percent_chip0910$Percentage*100,1)), '%')
percent_chip0910$per <- paste((round(percent_chip0910$Percentage*100,1)), '%')

treemap(percent_chip0910,
        index="label",
        vSize="sum",
        type="index",
        palette="Set3",
        title="Route 11 Chip Popularity '09-'10",
        fontsize.title=20
)

ggplot(percent_chip0910, aes(reorder(Chip,-sum), sum)) + 
  geom_col(alpha=.7, size=1, fill="#3B5998") + coord_flip()+ 
  geom_text(aes(label = per), hjust = -.01, size=4) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Route 11 Chip Popularity '09-'10")  + labs(y="Units Sold", x="")

## How did the price increase in January 2008 affect the company's sales?

soldbyyear = raw_df %>% 
  group_by(Year,Month) %>% 
  summarise(sold=sum(Chips.Sold))
soldbyyear = filter(soldbyyear, sold>0)
soldbyyear1 <- soldbyyear %>% group_by(Year) %>% mutate(cumsum = cumsum(sold))



soldbyyear1 <-soldbyyear1 %>%
  mutate(label = if_else(Month == "Dec", as.character(Year), NA_character_))
ungroup(soldbyyear1)
#Plot by year
ggplot(soldbyyear1, aes(x=Month, y = cumsum, group=Year,color=Year)) +
  geom_line( alpha=.5, size=3, lineend="round") +
  geom_label_repel(aes(label = label),nudge_x = 100, nudge_y = 40,na.rm = TRUE)+
  scale_color_discrete(guide = FALSE) + labs(title = "Decrease in Sales in 2008", y="Cumulative Sold") +   scale_y_continuous(labels = scales::number_format(big.mark = ',')) + 
  theme_pubclean()
  
  


# ggplot(filter(sumdf2,mean.sold>0), aes(x=Month, y = mean.sold,  fill=Chip),size=5) +
#   geom_col(alpha=.7, size=1) 
# How did eliminating some of its flavors affect its sales?


chipsoldbyyear = raw_df %>% 
  group_by(Chip,Year) %>% 
  summarise(sum=sum(Chips.Sold))

chipsoldbyyear = filter(chipsoldbyyear, sum>0)
  
  
ggplot(chipsoldbyyear, aes(reorder(Chip,-sum), sum)) +
  geom_col(alpha=.7, fill="#3B5998", ) + coord_flip()+ 
  # geom_text(aes(label = per), hjust = -.01, size=4) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Route 11 Chip Popularity '09-'10")  + labs(y="Units Sold", x="")

# Got rid of  
ggplot(chipsoldbyyear, aes(x=Year, y=sum, fill=Chip)) +
  geom_col(alpha=.7) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Effect of Eliminating Flavors", 
                          subtitle = "Green Chile Enchilada, Garlic & Herb, and Sweet Potato Cinnamon Sugar ")  + 
  labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))+
  gghighlight(Chip == "Green Chile Enchilada" 
              |  Chip == "Garlic and Herb" 
              | Chip == "Sweet Potato Cinnamon Sugar",label_key = Month)





#Thinking about
ggplot(chipsoldbyyear, aes(x=Year, y=sum, fill=Chip)) +
  geom_col(alpha=.7) +
  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))  +
  theme_pubclean() + labs(title = "Effect of Eliminating Flavors", 
                          subtitle = "Chesapeake Crab and Salt and Vinegar ")  + 
  labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  gghighlight(Chip == "Chesapeake Crab" 
              |  Chip == "Salt and Vinegar" ,label_key = Month)



profitability = raw_df %>% 
  group_by(Year,Chip,  Size) %>% 
  summarise(sum=sum(Chips.Sold))  ##MPG Took out mean
profitability = filter(profitability, sum>0)
ggplot(profitability, aes(x=Year, y=sum, fill=Size)) +
  geom_col(alpha=.7) +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +
  theme_pubclean() + labs(title = "Cases Sold By Size") + labs(y="Cases Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

profitability$caseCost = NA
profitability$unitPrice = NA
profitability = filter(profitability, sum>0)
profitability$caseCost[profitability$Size == '2oz'] <- .36*30
profitability$caseCost[profitability$Size == '6oz'] <- .81*12
profitability$caseCost[profitability$Size == '1.5oz'] <- .36*30
profitability$caseCost[profitability$Size == '5oz'] <- .81*12

# profitability$casepack[profitability$Size == '2oz'] <- 30
# profitability$casepack[profitability$Size == '6oz'] <- 12
# profitability$casepack[profitability$Size == '1.5oz'] <- 30
# profitability$casepack[profitability$Size == '5oz'] <- 12

profitability$unitPrice[profitability$Size == '2oz'] <- 20
profitability$unitPrice[profitability$Size == '6oz'] <- 18
profitability$unitPrice[profitability$Size == '2oz' & profitability$Year<2008] <- 18
profitability$unitPrice[profitability$Size == '6oz'& profitability$Year<2008] <- 16
profitability$unitPrice[profitability$Size == '5oz'] <- 26.40
profitability$unitPrice[profitability$Size == '1.5oz'] <- 22

#Without discount
# profitability$profit = (profitability$sum * profitability$unitPrice) - (profitability$sum*profitability$caseCost )

profitability$income = (profitability$sum/2 * profitability$unitPrice) - (profitability$sum/2*profitability$caseCost ) + (profitability$sum/2 * profitability$unitPrice*.75) - (profitability$sum/2*profitability$caseCost )


ggplot(profitability, aes(x=Year, y=income, fill=Chip)) +
  geom_col(alpha=.7) +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +  
  theme_pubclean() + labs(title = "Income", size=20) +# labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) + labs(x="") 

profitability %>% 
  group_by(Year) %>% 
  summarise(total=sum(income)) %>% 
ggplot( aes(x=Year, y=total)) +
  geom_col(alpha=.7, fill="chocolate3") +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +  
  geom_text(aes(x = Year, y = total, label =  paste0('$ ', as.character(formatC(as.numeric(total), format="f", digits=0, big.mark=","))), vjust=-.5)) +
  theme_pubclean() + labs(title = "Income", size=20) +# labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) + labs(x="") 

profitability1 = profitability %>% 
  group_by(Year) %>% 
  summarise(Profit=sum(income)-1000000)

profitability2 <- profitability1 %>%
  mutate(pos = Profit >= 0)

profitability2 <- profitability2  %>% mutate(cumulativeProfit = cumsum(Profit))


profitability2 <-profitability2 %>%
  mutate(label = if_else(Year==2010, paste0('Cumulative Profit\n', '$', as.character(formatC(as.numeric(cumulativeProfit), format="f", digits=0, big.mark=","))), NA_character_))


ggplot(profitability2, aes(x=Year, y=Profit, fill=pos)) +
  geom_col(alpha=.7, position="identity") +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +  theme_pubclean() + labs(title = "Rte 11 Profit") +
  scale_y_continuous(name='Net Profit',breaks=seq(-800000, 200000, 200000), limits = c(-900000,200000), labels = scales::number_format(big.mark = ',', prefix = '$ ')) + labs(x="") +
  geom_text(aes(x = Year, y = Profit, label =  paste0('$ ', as.character(formatC(as.numeric(Profit), format="f", digits=0, big.mark=","))), vjust=-.5)) +
     scale_fill_manual(values = c("red", "dark green"), guide=FALSE) +
  stat_smooth(geom='line',alpha=.4,data = profitability2, mapping=aes(x=Year, y=cumulativeProfit, group=1),alpha=.8,size=2, color='blue') +
  geom_label_repel(data = profitability2, aes(x=Year, y=cumulativeProfit, label = label),color='blue', fill="grey",nudge_x = -.8,na.rm = TRUE) 


## INCOME PER CHIP???


perchipsold = raw_df %>% 
  group_by(Chip, Year) %>% 
  summarise(sum=mean(sum(Chips.Sold)))

perchipsold %>%
  mutate(Chip = fct_reorder(Chip, sum)) %>%
ggplot(aes(x=Chip, y=sum, fill=Year)) +
  geom_col(alpha=.7) +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +  
  theme_pubclean() + labs(title = "Income before Operating Cost", size=20) +# labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) + labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9))


perchip = raw_df %>% 
  group_by(Chip,Size) %>% 
  summarise(sum=mean(sum(Chips.Sold)))

perchip= filter(perchip,sum>0)

perchip$caseCost = NA
perchip$unitPrice = NA
perchip$caseCost[perchip$Size == '2oz'] <- .36*30
perchip$caseCost[perchip$Size == '6oz'] <- .81*12
perchip$caseCost[perchip$Size == '1.5oz'] <- .36*30
perchip$caseCost[perchip$Size == '5oz'] <- .81*12
perchip$unitPrice[perchip$Size == '2oz'] <- 20
perchip$unitPrice[perchip$Size == '6oz'] <- 18
perchip$unitPrice[perchip$Size == '2oz' & perchip$Year<2008] <- 18
perchip$unitPrice[perchip$Size == '6oz'& perchip$Year<2008] <- 16
perchip$unitPrice[perchip$Size == '5oz'] <- 26.40
perchip$unitPrice[perchip$Size == '1.5oz'] <- 22
perchip$income = (perchip$sum/2 * perchip$unitPrice) - (perchip$sum/2*perchip$caseCost ) + (perchip$sum/2 * perchip$unitPrice*.75) - (perchip$sum/2*perchip$caseCost )

test=perchip
perchip1 = perchip %>% 
  group_by(Chip) %>% 
  summarise(metric=sum(income, total=sum(sum)))

test = test %>% 
  group_by(Chip) %>% 
  summarise( metric=sum(sum))



# combined=cbind(perchip1,test$total)

test$label='Total Sold'
perchip1$label='Income'

combined=rbind(perchip1,test)

# ggplot(combined, aes(reorder(Chip,-Income), Chip)) +
#   geom_col(alpha=.7) +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) +
#   theme_pubclean() + labs(title = "Cases Sold By Flavor") + labs(y="Cases Sold", x="")+ 
#   scale_y_continuous(labels = scales::number_format(big.mark = ',')) + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9))


combined %>%
  mutate(Chip = fct_reorder(Chip, desc(metric))) %>%
ggplot(aes(x=Chip, y=metric,fill=label)) +
  geom_col(alpha=.7, position='dodge') +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
  theme_pubclean() + labs(title = "Income by chip ('06-'10)", size=20) +# labs(y="Units Sold", x="")+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) + labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9))+ theme(legend.title = element_blank()) 


# ggplot(combined, aes(x=Chip, y=metric,fill=label)) +
#   geom_col(alpha=.7, position='dodge') +  theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
#   theme_pubclean() + labs(title = "Income by chip ('06-'10)", size=20) +# labs(y="Units Sold", x="")+ 
#   scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) + labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9))
