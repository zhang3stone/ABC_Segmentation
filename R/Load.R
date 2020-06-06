library(dplyr)
library(lubridate)

# load file
path2csv <- "~/Documents/GitHub/ABC_Segmentation/Data/transaction_master2.txt"
transaction_master <- read.csv(path2csv, stringsAsFactors = FALSE, comment.char = "")

# format
t <- tbl_df(transaction_master) %>%
  mutate(date = ymd(YearMonthDay)) %>%
  select(date, Year:RDS_Value)
rm("transaction_master")

# only TMS
tms <- filter(t, TMS_Value != 0) %>% select(-contains("RDS"))
rm("t")

# product master
path2csv <- "~/Documents/GitHub/ABC_Segmentation/Data/product_master.csv"
product_master <- read.csv(path2csv, stringsAsFactors = FALSE, comment.char = "")

pm = tbl_df(product_master)
rm("product_master")

# sender master
path2csv <- "~/Documents/GitHub/ABC_Segmentation/Data/sender_master2.csv"
sender_master <- read.csv(path2csv, stringsAsFactors = FALSE, comment.char = "")

sm = tbl_df(sender_master)
rm("sender_master")

# reciever master
path2csv <- "~/Documents/GitHub/ABC_Segmentation/Data/receiver_master2.csv"
receiver_master <- read.csv(path2csv, stringsAsFactors = FALSE, comment.char = "")

rm = tbl_df(receiver_master)
rm("receiver_master")


prov_rm <- rm %>% select(Receiving_ID, Channel_2014, Receiving_Province_E, Receiving_City_E)
ptms <- inner_join(tms, prov_rm)
ptms <- inner_join(ptms, pm)
