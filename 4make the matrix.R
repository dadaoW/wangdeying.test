library(raster)#trim()函数需要raster包。


OnlyKeywords.total <- read.table("OnlyKeywords.total_newdataresource_0503.txt")
Keywords.start = NULL
for(i in seq(from = 1,to = nrow(OnlyKeywords.total))){
  Keywords.total <- paste(Keywords.start,OnlyKeywords.total[i,],sep = ",")
  Keywords.start <- Keywords.total
}
#Keywords.total <- tolower(Keywords.total)#全部转换为小写。
#Keywords.total#里面包含无用的空格,character类型。
Keywordstotal.str <- strsplit(trim(Keywords.total),",")
kwt.str <- unlist(Keywordstotal.str[1])
kwt.str.df <- as.data.frame(kwt.str[2:length(kwt.str)])
index <- duplicated(kwt.str.df[,1])

Keywords.remove <- kwt.str.df[!index,]#去除重复字符串，为了得到空矩阵的行和列。
#class(Keywords.remove)#去除重复后得到的
#length(Keywords.remove)
#Keywords.remove[1:200]
Keywords.matrix.start <- matrix(data = 0,nrow = length(Keywords.remove),ncol = length(Keywords.remove))
colnames(Keywords.matrix.start) <- Keywords.remove
rownames(Keywords.matrix.start) <- Keywords.remove
Keywords.matrix.start.noweight <- Keywords.matrix.start
#write.csv(Keywords.matrix.start,"2.csv")
##--------------------------------------------------------------------------------------------------------------------
ItemandFre.total <- read.table("ItemandFre.total_newdataresource_0503.txt")
ItemandFre.total <- as.matrix(ItemandFre.total)
#ItemandFre.total[2,2] <- substr(as.character(ItemandFre.total[2,2]),3,nchar(as.character(ItemandFre.total[2,2])) - 1)
#ncol(ItemandFre.total)

for (m in seq(from = 2,to = nrow(ItemandFre.total))) {
  ItemandFre.total[m,2] <- substr(as.character(ItemandFre.total[m,2]),3,nchar(as.character(ItemandFre.total[m,2])) - 1)
  Line.m.split <- unlist(strsplit(ItemandFre.total[m,2],","))
  coKeywords.paper.start <- NULL
  for(i in seq(from = 1,to = length(Line.m.split)-1)){
    for (j in seq(from = i+1,to = length(Line.m.split))) {
      i.one.split <- unlist(strsplit(Line.m.split[i],":"))
      j.one.split <- unlist(strsplit(Line.m.split[j],":"))
      
      coKeywords.one <- paste(i.one.split[1],j.one.split[1],sep = ",")
      coKeywords.two <- rbind(coKeywords.one,as.numeric(i.one.split[2])*as.numeric(j.one.split[2]))
      coKeywords.paper <- cbind(coKeywords.paper.start,coKeywords.two)
      coKeywords.paper.start <- coKeywords.paper
    }
  }
  coKeywords.paper <- t(coKeywords.paper)
  
  ##-----------有权的情况下，共现矩阵的构建-------------------------------------------------------------------------------------------------
  for (i in seq(from = 1,to = nrow(coKeywords.paper))) {
    coKeywords.first <- unlist(strsplit(coKeywords.paper[i,],","))[1]
    coKeywords.second <- unlist(strsplit(coKeywords.paper[i,],","))[2]
    Keywords.matrix.start[coKeywords.first,coKeywords.second] <- Keywords.matrix.start[coKeywords.first,coKeywords.second]+as.numeric(coKeywords.paper[i,2])
    #前提是 文章的关键词数量n>=2。
    Keywords.matrix.start[coKeywords.second,coKeywords.first] <- Keywords.matrix.start[coKeywords.second,coKeywords.first]+as.numeric(coKeywords.paper[i,2])
    #前提是 文章的关键词数量n>=2。
  }
  
  
  ##-----------无权的情况下，共现矩阵的构建-------------------------------------------------------------------------------------------------
  # coKeywords.paper[,2] <- "1"
  # for (i in seq(from = nrow(coKeywords.paper))) {
  #   coKeywords.noweight.first <- unlist(strsplit(coKeywords.paper[i,],","))[1]
  #   coKeywords.noweight.second <- unlist(strsplit(coKeywords.paper[i,],","))[2]
  #   Keywords.matrix.start.noweight[coKeywords.noweight.first,coKeywords.noweight.second] <- 
  #     Keywords.matrix.start.noweight[coKeywords.noweight.first,coKeywords.noweight.second]+as.numeric(coKeywords.paper[i,2])
  #   #前提是 文章的关键词数量n>=2。
  #   Keywords.matrix.start.noweight[coKeywords.noweight.second,coKeywords.noweight.first] <- 
  #     Keywords.matrix.start.noweight[coKeywords.noweight.second,coKeywords.noweight.first]+as.numeric(coKeywords.paper[i,2])
  #   #前提是 文章的关键词数量n>=2。
  # }
}
#Keywords.matrix.start
write.csv(Keywords.matrix.start,"Keywords.matrix.start_newresource_0503.csv")
write.csv(Keywords.matrix.start.noweight,"Keywords.matrix.start.noweight-2011-2016.csv")
#.libPaths()
