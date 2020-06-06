library("plyr")
colorABC <-c("#d9742b", "#e6b450", "#e3e6c3")

#Segment
ptmssub <- ptms 
recid_abc14<-make_abc(ptmssub, 2014, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc14)[3]<-"Segment"
recid_abc13<-make_abc(ptmssub, 2013, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc13)[3]<-"Segment"
recid_abc12<-make_abc(ptmssub, 2012, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc12)[3]<-"Segment"

# Number of terminal

recid_abc<-rbind(recid_abc14, recid_abc13, recid_abc12)
recid_abc$Freq<-1
recid_abcN<-aggregate(Freq~Year+Segment, data=recid_abc, sum)
recid_abcT<-aggregate(Freq~Year, data=recid_abcN, sum)
names(recid_abcT)[2]<-"Total"
recid_abcN<-inner_join(recid_abcN, recid_abcT)
recid_abcN$Share<-with(recid_abcN, percent(Freq/Total))

recid_abcA<- recid_abcN %>% select(Year, Segment, Freq) %>% tidyr::spread(Year, Freq)
recid_abcB<- recid_abcN %>% select(Year, Segment, Share) %>% tidyr::spread(Year, Share)
names(recid_abcB)[2:4]<-c("Share2012", "Share2013", "Share2014")
recid_abcAB<-inner_join(recid_abcA, recid_abcB)


#Terminal Number
ptmssub14 <- ptms %>% 
  dplyr::filter(Year == 2014) %>%
  dplyr::group_by_("Year", "Receiving_ID", "Channel_2014") %>%
  dplyr::summarize(vol = sum(TMS_Volume), val = sum(TMS_Value))

ptmssub14$count <- 1

ptmssub14 <- inner_join(ptmssub14, recid_abc14)
ptmssub14 <- ptmssub14 %>%  
  dplyr::group_by_("Channel_2014","Segment") %>%
  dplyr::summarize(count = sum(count))

ptmssub14 <- inner_join(ptmssub14, aggregate(count~Channel_2014, data=ptmssub14, sum), by="Channel_2014")
ptmssub14 <- inner_join(ptmssub14, aggregate(count.y~Segment, data=ptmssub14, sum), by="Segment")
ptmssub14$Sshare <-ptmssub14$count.x/ptmssub14$count.y.x
ptmssub14$Cshare <-ptmssub14$count.y.x/ptmssub14$count.y.y

ptmssub14S <- ptmssub14 %>% select(Channel_2014, Segment, Cshare, Sshare) %>% tidyr::spread(Segment, Sshare)

ptmssub14S$xmax <- cumsum(ptmssub14S$Cshare)
ptmssub14S$xmin <- ptmssub14S$xmax - ptmssub14S$Cshare
ptmssub14S$Cshare <- NULL

ptmssub14S <-tidyr::gather(ptmssub14S, Segment, Share, -Channel_2014, -xmax, -xmin)
ptmssub14S <- ddply(ptmssub14S, .(Channel_2014), transform, ymax = cumsum(Share))
ptmssub14S <- ddply(ptmssub14S, .(Channel_2014), transform, ymin = ymax - Share)

ptmssub14S$xtext <- with(ptmssub14S, xmin + (xmax - xmin)/2)
ptmssub14S$ytext <- with(ptmssub14S, ymin + (ymax - ymin)/2)

mekko1 <- ggplot(ptmssub14S, aes(ymin = ymin, ymax = ymax, xmin =xmin, xmax = xmax, fill = Segment))
mekko1 <- mekko1 + geom_rect(colour = I("grey"))
mekko1 <- mekko1 + geom_text(aes(x = xtext, y = ytext, label = ifelse(Channel_2014 == "Hospital", paste(Segment," - ", as.integer(Share*100), "%", sep = ""), paste(Segment," - ", as.integer(Share*100), "%", sep = ""))), size = 3.5)
mekko1 <- mekko1 + geom_text(aes(x = xtext, y = 1.03, label = paste("Channel:", Channel_2014)), size = 4)
mekko1 <- mekko1 + scale_y_continuous(name="Segment Contribution", label=percent)
mekko1 <- mekko1 + scale_x_continuous(name="Channel Contribution", label=percent)
mekko1 <- mekko1 + scale_fill_manual(values=colorABC)
mekko1 <- mekko1 + theme(legend.position="bottom")
mekko1 <- mekko1 + ggtitle(expression(atop("Fig2 # of Terminal by Segments and Channel", atop(italic("(ALL Products, # of Terminal)"), ""))))

# Sale Source
ptmssub14 <- ptms %>% 
  dplyr::filter(Year == 2014) %>%
  dplyr::group_by_("Year", "Receiving_ID", "Channel_2014") %>%
  dplyr::summarize(vol = sum(TMS_Volume), val = sum(TMS_Value))

ptmssub14 <- inner_join(ptmssub14, recid_abc14)
  ptmssub14 <- ptmssub14 %>%  
  dplyr::group_by_("Channel_2014","Segment") %>%
  dplyr::summarize(val = sum(val))

ptmssub14 <- inner_join(ptmssub14, aggregate(val~Channel_2014, data=ptmssub14, sum), by="Channel_2014")
ptmssub14 <- inner_join(ptmssub14, aggregate(val.y~Segment, data=ptmssub14, sum), by="Segment")
ptmssub14$Sshare <-ptmssub14$val.x/ptmssub14$val.y.x
ptmssub14$Cshare <-ptmssub14$val.y.x/ptmssub14$val.y.y

ptmssub14S <- ptmssub14 %>% select(Channel_2014, Segment, Cshare, Sshare) %>% spread(Segment, Sshare)

ptmssub14S$xmax <- cumsum(ptmssub14S$Cshare)
ptmssub14S$xmin <- ptmssub14S$xmax - ptmssub14S$Cshare
ptmssub14S$Cshare <- NULL

ptmssub14S <-tidyr::gather(ptmssub14S, Segment, Share, -Channel_2014, -xmax, -xmin)
ptmssub14S <- ddply(ptmssub14S, .(Channel_2014), transform, ymax = cumsum(Share))
ptmssub14S <- ddply(ptmssub14S, .(Channel_2014), transform, ymin = ymax - Share)

ptmssub14S$xtext <- with(ptmssub14S, xmin + (xmax - xmin)/2)
ptmssub14S$ytext <- with(ptmssub14S, ymin + (ymax - ymin)/2)

mekko2 <- ggplot(ptmssub14S, aes(ymin = ymin, ymax = ymax, xmin =xmin, xmax = xmax, fill = Segment))
mekko2 <- mekko2 + geom_rect(colour = I("grey"))
mekko2 <- mekko2 + geom_text(aes(x = xtext, y = ytext, label = ifelse(Channel_2014 == "Hospital", paste(Segment," - ", as.integer(Share*100), "%", sep = ""), paste(Segment," - ", as.integer(Share*100), "%", sep = ""))), size = 3.5)
mekko2 <- mekko2 + geom_text(aes(x = xtext, y = 1.03, label = paste("Channel:", Channel_2014)), size = 4)
mekko2 <- mekko2 + scale_y_continuous(name="Segment Contribution", label=percent)
mekko2 <- mekko2 + scale_x_continuous(name="Channel Contribution", label=percent)
mekko2 <- mekko2 + scale_fill_manual(values=colorABC)
mekko2 <- mekko2 + theme(legend.position="bottom")
mekko2 <- mekko2 + ggtitle(expression(atop("Fig3 Sales Value by Segments and Channel", atop(italic("(ALL Products, CNY)"), ""))))

# Summary
ptmssubVol<- aggregate(TMS_Volume ~ Year + Receiving_ID, data=ptmssub, sum)
ptmssubVal<- aggregate(TMS_Value ~ Year + Receiving_ID, data=ptmssub, sum)
ptmssubSum<- inner_join(ptmssubVol, ptmssubVal)

recid_abcSum<-rbind(recid_abc14, recid_abc13, recid_abc12)
ptmssubSum<- inner_join(recid_abcSum, ptmssubSum)

ptmssubVol<- aggregate(TMS_Volume ~ Year + Segment, data=ptmssubSum, sum)
ptmssubVal<- aggregate(TMS_Value ~ Year + Segment, data=ptmssubSum, sum)
ptmssubSum<- inner_join(ptmssubVol, ptmssubVal)

growth<-function (data) {
  # Calculate Growth
  data$Growth <- c(NA, tail(data$TMS_Value, -1) / head(data$TMS_Value, -1) - 1)
  data
}

ptmssubSum<- ddply(ptmssubSum, .(Segment), growth)
ptmssubSum<- subset(ptmssubSum, Growth >0)
ptmssubSum$TMS_Volume<-comma(ptmssubSum$TMS_Volume)
ptmssubSum$TMS_Value<-comma(as.integer(ptmssubSum$TMS_Value))
ptmssubSum$Growth<-percent(ptmssubSum$Growth)
ptmssubSum<-gather(ptmssubSum, Metrics, Value, -Year, -Segment)
ptmssubSum<-spread(ptmssubSum, Year, Value)