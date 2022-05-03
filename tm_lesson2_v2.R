library(tm)
library(jiebaR)
library(wordcloud2)
stringsAsFactors = FALSE
mixseg<-worker()

## Part I: wordCloud
# 1.read data
a='
〔記者沈佩瑤／台北報導〕目前各地藥局仍有不少排隊買口罩民眾，台北市政府日前推出口罩販賣機，市長柯文哲更下令下週12行政區全部上，不過副市長蔡炳坤昨受訪態度轉低，指要先滿足機身足夠、與中央資料庫介接、販賣數量等三個條件，而柯今天受訪進一步拿出「扶植產業」藉口，還說「現階段每個藥店口罩賣的還好」，所以不會那麼急著12個行政區都上，會請衛生局排序。
柯文哲11日才在口罩販賣機上線記者會上，說「看到大家排隊買口罩我受不了！」所以推出口罩販賣機，並揚言下周12行政區都要上，但北市資訊局局長呂新科昨卻說是「1週後進行相關評估」。
柯文哲今天受訪解釋，台北市做為創新的城市，還有扶植產業的角色，所以把口罩販賣機當作一個新創事業做試辦，如果機型都不變的話，要複製到十二個區很快，但希望在這個過程中帶動資通訊產業發展，而其他機型，還要測試不同控制器、健保資料庫連接。
「後來我決定是這樣，如果我們都用一套，下禮拜就可以上線，可是我們有扶植台灣產業目的，」柯說，且現階段每個藥店口罩賣的還好，讓口罩販賣機更有效率、排隊時間更短，就不是那麼急著12個行政區都上。
柯強調不打算只用一個機型，所以還要試辦、訓練人員等，衛生局正在排序各行政區上線的先後，陸陸續續上線。
'

# 2.segment
mixseg[a]
seg<-mixseg[a]

# 3. draw cloud
segAll=data.frame(table(seg))
wordcloud2(segAll, size = 0.5)

# 4. beautify
segTwo<-data.frame(table(seg[nchar(seg)>1]))
wordcloud2(segTwo, size = 0.5)

wordcloud2(segTwo, size = 0.4, shape='star',
           color = 'random-light', 
           backgroundColor = 'black',
           fontFamily = '微軟正黑體')

# 5. letter
letterCloud(segTwo, word = "R", color='random-light' , size = 0.2, backgroundColor="black")
letterCloud(segAll, word = "COVID", color="white",  size = 0.2, backgroundColor="pink")

# 5. DIY :
# change "shape", "color", "backgroundColor","fontFamily"
## shape: circle,cardioid,diamond,triangle-forward,triangle,pentagon,star
# change content
# post online

## 6. extra
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")
figPath = system.file("examples/cat.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.0,color = "black")



## Part II: txt to database
# 1.read data
csv <- data.frame(ID=c("ko1","ko2","han3","han4"),
                    content=c("台北市政府日前推出口罩販賣機，柯文哲更下令下週12行政區全部上，柯文哲11日才在口罩販賣機上線記者會上，說「看到大家排隊買口罩我受不了！」所以推出口罩販賣機，並揚言下周12行政區都要上。",
                              "台灣感染武漢肺炎病毒機率最高的前5名城市依序是台北市。柯文哲上午參加里長座談前受訪指出",
                              "高雄市長韓國瑜14日晚間的直播中卻出現罕見的「粉色襯衫」，談到高雄市的「一級開設」，韓國瑜也在直播中感謝市民。",
                              "韓國瑜昨晚穿起粉紅色襯衫、穿戴起粉紅色，反駁日前被高雄市議員質疑沒市政行程「市長真好當」，他強調沒公開行程，不代表沒行程。")
                              ,stringsAsFactors = FALSE)


## 2. segment
csv$seg = NULL
for (i in 1:nrow(csv)){
  csv$seg[i]<-paste(unlist(mixseg[csv$content[i]]),collapse=' ')
}

## 3.dtm
dtm <- DocumentTermMatrix(VCorpus(VectorSource(csv$seg))) 


## 4. similarity doc
mydata.df <- as.matrix(dtm) 
mydata.df.scale <- scale(mydata.df)
d<- dist(mydata.df.scale)
fit <- hclust(d)
plot(fit)

## 5. DIY:
# change series text
# and plot it
# explain it.



