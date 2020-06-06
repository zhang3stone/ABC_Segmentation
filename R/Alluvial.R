
#Segment
ptmssub <-ptms
recid_abc14<-make_abc(ptmssub, 2014, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc14)[3]<-"Y2014"
recid_abc13<-make_abc(ptmssub, 2013, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc13)[3]<-"Y2013"
recid_abc12<-make_abc(ptmssub, 2012, "Receiving_ID") %>% select(Year, Receiving_ID, pabc)
names(recid_abc12)[3]<-"Y2012"

recid_abc<-merge(recid_abc12, recid_abc13, by=c("Receiving_ID"))
recid_abc<-merge(recid_abc, recid_abc14, by=c("Receiving_ID"))

recid_abc$Freq <-1
recid_abc <- aggregate( Freq ~ Y2012 + Y2013 + Y2014, data=recid_abc, sum)

pt<-alluvial(recid_abc[,1:3], freq=recid_abc$Freq, alpha=1, xw=0.2, gap.width=0.1,          
             col=ifelse( recid_abc$Y2014 == "A", "#d9742b", ifelse(recid_abc$Y2014 == "B", "#e6b450", "#e3e6c3")),
             border="grey",
             layer = recid_abc$Y2014 != "A")
