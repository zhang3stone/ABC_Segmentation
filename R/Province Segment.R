library(ggplot2)
library(alluvial)
library(scales)
library(grid)

colorABC <-c("#d9742b", "#e6b450", "#e3e6c3")
recid_abc_Province <- make_abc(ptms, 2014, "Receiving_Province_E")
names(recid_abc_Province)<-c("Year", "Province", "Volume", "Value", "Total", "Share", "Accumulate", "Acc_share", "Segment")

p1<-ggplot(recid_abc_Province, aes(x = reorder(Province,-Value), y = Value/1000)) + 
  geom_bar(stat = "identity", aes(fill=Segment), color="black") + 
  scale_fill_manual(values=colorABC) +
  geom_text(aes(label = paste(round(Share*100,0), "%", sep=""), y = Value/2000, ymax=10), size=3, color="black") +
  geom_text(aes(y = Value / 1000, label = comma(round(Value / 1000,  digits = 0)), hjust = 0, vjust=-1), size=3) +
  scale_y_continuous(name="Sales Value (thousand)", label=comma) +
  scale_x_discrete(name="") +
  #annotate("segment", x=31, xend=31, y=20000, yend=10000, colour="#364d6e", size=2, arrow=arrow()) +
  #annotate("text",x="Anhui",y=25000, label="Anhui") +
  theme(axis.line = element_line(colour="white"), 
        axis.text = element_text(size=rel(0.9), angle=90), 
        axis.title = element_text(size=rel(1.4))) +
  theme(legend.position="bottom") +
  ggtitle(expression(atop("Fig.1 Sales Value & Contribution by Province ", atop(italic("(ALL Products, CNY)"), ""))))

print(p1)


