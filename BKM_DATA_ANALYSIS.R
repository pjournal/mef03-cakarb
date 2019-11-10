library(rvest)
library(dplyr)
library(ggplot2)
library(scales)
library(plyr)

url <- "https://bkm.com.tr/secilen-aya-ait-sektorel-gelisim/?filter_year=2019&filter_month=1"
page <- read_html(url)
tablo <- html_table(page, fill = TRUE)[[4]][-c(1:2),]


for(i in 2:6) {
  url <- paste("https://bkm.com.tr/secilen-aya-ait-sektorel-gelisim/?filter_year=2019&filter_month=", i, sep = "")
  page <- read_html(url)
  tablo <- bind_rows(tablo, html_table(page, fill = TRUE)[[4]][-c(1:2),-1])
}

is_yeri <- c(tablo%>% select(X1) %>%  filter(X1 != "NA"))
is_yeri_1 <- c(rep(is_yeri[["X1"]], times=6))


tablo_1 <- tablo %>% mutate(X1 = is_yeri_1) %>% filter(X1 != "TOPLAM")


month_1 <- c(rep(1:6, times=1, each=26))
tablo_son <- tablo_1 %>% mutate(month = month_1)


tablo2  <- as.data.frame(lapply(tablo_son, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", x)))))
tablo2[,1] <- tablo_son[,1]


colnames(tablo2) <- c("sector", "transaction_num_cc", "transaction_num_debit","transaction_amount_cc_mio", "transaction_amount_debit_mio", "month")

tablo_son <- tablo2




#####2019 6. ay ciroya gore sektor siralamasi

number1 = tablo_son %>% filter(month == 6) %>% 
  transmute(sector, total_transaction_amount_mio = transaction_amount_cc_mio + transaction_amount_debit_mio) %>% 
  arrange(desc(total_transaction_amount_mio)) %>%
  mutate(sector = case_when(total_transaction_amount_mio > 4000 ~ sector, TRUE ~ "OTHER")) %>% group_by(sector) %>% 
  transmute(total_transaction_amount_mio = sum(total_transaction_amount_mio)) %>% distinct() %>%
  arrange(desc(total_transaction_amount_mio)) %>% ungroup() %>%
  mutate(share = round(total_transaction_amount_mio/sum(total_transaction_amount_mio)*100,2))

number1 %>% ggplot(data = ., aes(x = "", y = share, fill = sector)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(x = 1.3, y = share, label = percent(share/100) ),position = position_stack(vjust = 0.5),color = "black")+
  labs(fill = "Sectors")+
  theme_void()


###### 6 ayda islem sayisi en cok artan sektorun islem sayisi gelisimi

number2 <- tablo_son %>% filter(month %in% c(1,6)) %>% select(sector, transaction_num_cc, transaction_num_debit, month) %>%
  transmute(sector, total_transaction_num = transaction_num_cc+transaction_num_debit, month)

number2_1 <- number2[number2$month==1, ]
number2_6<- number2[number2$month==6, ]

number_all <- number2_1 %>% left_join(number2_6, by = "sector", suffix = c("_1","_6"))

top3 <- number_all %>% transmute(sector, transaction_difference = abs(total_transaction_num_6 - total_transaction_num_1)) %>%
  arrange(desc(transaction_difference)) %>% top_n(3) %>% select(sector)


top3_son <- top3 %>% left_join(tablo_son, by = "sector") %>% group_by(sector, month) %>% 
  mutate(total_transaction = transaction_num_debit+transaction_num_cc) %>% select(sector, total_transaction, month)


top3_son %>% ggplot(data=., aes(x=month, y=total_transaction, fill=sector)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "Jan-Jun 2019",y= "Transaction Number", title = "History of Transaction Numbers", fill= "Sector") +
  theme_minimal()