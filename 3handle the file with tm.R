#读取文件夹，提取有用词汇。生成OnlyKeywords、TIandKeywords与ItemandFre。
library(tm)#R中数据挖掘包
#detach("package:tm")
#install.packages("NLP")
library(NLP)
library(raster)#trim()函数需要raster包。
corpus <- Corpus(DirSource(directory = "D:/Rdw/Novelty and Conventionality/SingleFiles_TXT",
                           encoding = "UTF-8",recursive = T,mode = "text"))###文件内有3693篇文章的导出记录，切分版。
library(SnowballC)
as.character(corpus[[1]])[1]
corpus_1 <- corpus

corpus <- tm_map(corpus,content_transformer(tolower))#全部变为小写
for(i in seq(from = 1,to = 3693)){
  #corpus[[1]]
  corpus[[i]] <- gsub(pattern = "[^a-z0-9]",replacement = " ",corpus[[i]])#把所有的非字母\非数字变为空格
}
corpus <- tm_map(corpus,PlainTextDocument)#转换格式。
TIandKeywords.start <- NULL
OnlyKeywords.start <- NULL
ItemandFre.start.start <- NULL
myStopwords <- c(stopwords("english"),c("my","custom","words","elsevier","rights","reserved"
                                        ,"make","play","important","role","difficult","prefer","time"
                                        ,"wet","design","type","common","nation","found","ti"
                                        ,"total","keyword","ab","base","based","z9","papers"))#定义停用词。
myStopwords <- c(myStopwords,stopwords("SMART"))
corpus <- tm_map(corpus,removeWords,myStopwords)
corpus <- tm_map(corpus,removeNumbers)#去掉了数字，注意在提取被引频次的时候和整理非内容指标的时候并没有去掉数字。
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,stemDocument)#
for (j in seq(from = 1, to = 3693))
{
  if (" noab"%in%as.character(corpus[[j]]))#只去除没有摘要的记录。
  {}else{#else尽量要紧挨着if语句的大括号，要不然会出错的。
    TI <- trim(as.character(corpus[[j]])[2])
    OnlyKeywords <- paste(trim(as.character(corpus[[j]])[3]),trim(as.character(corpus[[j]])[4]),sep = " ")
    OnlyKeywords <- gsub(pattern = " noidnod",replacement = "",OnlyKeywords)
    OnlyKeywords <- gsub(pattern = " ",replacement = ",",OnlyKeywords)#把OnlyKeywords中的空格都变为“，”。
    OnlyKeywords.hasdup <- OnlyKeywords
    #需要对记录进行去重------------------------------------------------------------------------------------start
    OnlyKeywords.split <- strsplit(OnlyKeywords,",")
    OnlyKeywords.split.df <- as.data.frame(OnlyKeywords.split)
    index1 <- duplicated(OnlyKeywords.split.df[,1])
    OnlyKeywords.nodup <- OnlyKeywords.split.df[!index1,]#去除重复字符串，为了得到空矩阵的行和列。
    OnlyKeywords.nodup <- as.character(OnlyKeywords.nodup)#去除重复后得到的
    #------------------------------------------------------------------------------------------------------end
    #------------------------------------------------------------------------------------------------------start
    OnlyKeywords.nodup.matrix <- as.matrix(OnlyKeywords.nodup)#转换为矩阵形式。
    Item.fre <- matrix(data = 0,nrow = length(OnlyKeywords.nodup),ncol = 1)
    rownames(Item.fre) <- OnlyKeywords.nodup.matrix[,1]
    #下面的循环是用来统计出词频的。
    for (i in seq(from = 1,to = length(unlist(OnlyKeywords.split)))){
      Item.fre[unlist(OnlyKeywords.split)[i],1] <- Item.fre[unlist(OnlyKeywords.split)[i],1] + 1
    }
    #Item.fre#统计出每个单词的频数。
    Item.nodup.fre <- cbind(OnlyKeywords.nodup.matrix,Item.fre)
    ItemandFre.start = ""
    for(j in seq(from = 1,to = length(OnlyKeywords.nodup))){
      ItemandFre.every <- paste(Item.nodup.fre[j,1],Item.nodup.fre[j,2],sep = ":")
      ItemandFre <- paste(ItemandFre.start,ItemandFre.every,sep = ",")
      ItemandFre.start <- ItemandFre
    }
    ItemandFre <- substr(ItemandFre,2,nchar(ItemandFre))
    #如："paper:1,present:1,idea:2,understand:2,instanc:3,interact:2,intellig:2......"
    #------------------------------------------------------------------------------------------------------end
    OnlyKeywords.nodup.start <- NULL
    for(k in seq(from = 1,to = length(OnlyKeywords.nodup))){
      OnlyKeywords <- paste(OnlyKeywords.nodup.start,OnlyKeywords.nodup[k])
      OnlyKeywords.nodup.start <- OnlyKeywords
    }
    OnlyKeywords <- gsub(pattern = " ",replacement = ",",trim(OnlyKeywords))#把OnlyKeywords中的空格都变为“，”。
    TIandKeywords <- paste(TI,OnlyKeywords,sep = "&&&")
    TIandKeywords.total <- rbind(TIandKeywords.start,TIandKeywords)
    TIandKeywords.start <- TIandKeywords.total
    OnlyKeywords.total <- rbind(OnlyKeywords.start,OnlyKeywords)
    OnlyKeywords.start<- OnlyKeywords.total
    ItemandFre.total <- rbind(ItemandFre.start.start,ItemandFre)
    ItemandFre.start.start<- ItemandFre.total
  }
}
write.table(TIandKeywords.total,"TIandKeywords.total-2011-2016.txt",row.names = F ,col.names = F)
write.table(OnlyKeywords.total,"OnlyKeywords.total-2011-2016.txt",row.names = F ,col.names = F)
#需要对关键词进行去重
write.csv(ItemandFre.total,"ItemandFre.total-2011-2016.txt")
ItemandFre.total[2:10,]
#write.csv(myStopwords,"stopwords0608.csv")查看R语言的默认停用词。
#write.table(stopwords("english"),"stopwords0612.txt")
#stopwords("englishadded")