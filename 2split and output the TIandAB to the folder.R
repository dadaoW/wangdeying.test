###用untitled1得到的结果，将WOS的导出记录分解为500个记录，并在每条记录中寻取“作者关键词”“编辑补充关键词”“被引用次数Z9”
#install.packages("raster")
#install.packages("tm")
#install.packages("SnowballC")
library(raster)
TI.Z9.start = NULL
TI.Keyword.start = NULL
Keywords.start.2 = NULL
TI.AB.KEY.Z9.start = NULL


start = NULL

for (j in seq(from = 2,to = length(PT.ID.unlist))) {
  Wos <- Wos.test[c(as.numeric(PT.ID.unlist[j]):as.numeric(ER.ID.unlist[j])),]  
  Wos[Wos == ""] = NA
  #下面的代码是将列值为NA的位置补充完整。start-------------------------------------------------------------------------------------
  Wos1 <- as.matrix(Wos[,1])
  Wos.matrix <- as.matrix(cbind(rownames(Wos),Wos1))
  colnames(Wos.matrix)<-c("id","num")
  lessmax<-function(x,y) tail(x[x<=y],n=1)  ##函数用来求最近非NA下标
  lessmax<-Vectorize(lessmax,"y") ##对第二个参数向量化
  ind.na<-which(is.na(Wos.matrix[,"num"])) ##num为NA的行下标
  ind.nna<-which(!is.na(Wos.matrix[,"num"])) ##num不为NA的行下标
  #Wos.matrix ##原始的矩阵
  #ind.na
  #ind.nna
  
  if(!is.na(ind.na[1])){
    Wos.matrix[is.na(Wos.matrix[,"num"]),"num"]<-Wos.matrix[lessmax(ind.nna,ind.na),"num"] ##替换
  }
  #Wos.matrix##替换后的矩阵。---end--------------------------------------------------------------------------------------------------------
  Z9.start = NULL
  TI.start = NULL
  Keyword.start = NULL
  AB.start = NULL
  UT.start = NULL
  
  Z9 = NULL
  TI = NULL
  Keyword = NULL
  AB = NULL
  UT = NULL
  ##-------------------------------------------------------------------------------------------------------------------------------
  
  for (i in seq(from = 1,to = as.numeric(ER.ID.unlist[j]) - as.numeric(PT.ID.unlist[j])+ 1)){
    if(Wos.matrix[i,2] == "Z9" ){
      Z9.ID <-cbind(Z9.start,Wos.matrix[i,1])
      Z9.start <- Z9.ID}}
  Z9.ID#Z9所在行数。
  for (i in seq(from = 1,to = as.numeric(ER.ID.unlist[j]) - as.numeric(PT.ID.unlist[j])+ 1)){
    if(Wos.matrix[i,2] == "TI" ){
      TI.ID <-cbind(TI.start,Wos.matrix[i,1])
      TI.start <- TI.ID}}
  TI.ID#标题所在行数。
  #for (i in seq(from = 1,to = as.numeric(ER.ID.unlist[j]) - as.numeric(PT.ID.unlist[j])+ 1)) {
  #  if(Wos.matrix[i,2] == "UT" ){
  #    UT.ID <-cbind(UT.start,Wos.matrix[i,1])
  #    UT.start <- UT.ID}}
  #UT.ID#DI所在行数。
  
  
  if("ID"%in%Wos.matrix[,2]|"DE"%in%Wos.matrix[,2]){
    for (i in seq(from = 1,to = as.numeric(ER.ID.unlist[j]) - as.numeric(PT.ID.unlist[j])+ 1)){
      if(Wos.matrix[i,2] == "ID"|Wos.matrix[i,2] == "DE" ){
        Keyword.ID <-cbind(Keyword.start,Wos.matrix[i,1])
        Keyword.start <- Keyword.ID}}
    Keyword.ID#作者关键词所在行。
    
    for(i in seq(from = 1,to = ncol(Keyword.ID)))
    {Keyword.total = paste(Keyword,Wos.test[Keyword.ID[1,i],2],",")
    Keyword <- Keyword.total}
    Keyword.total <- gsub(pattern = ";|:", replacement = ",", Keyword.total)#将原文中作者关键词串里的“:与;”符号替换为“,”
    #以便于后续过程。
    #Keyword.total#输出作者关键词
  }
  else{Keyword.total <- "noIDNODE"}#没有“ID”标识的情况。
  
  if("AB"%in%Wos.matrix[,2]){
    for(i in seq(from = 1,to = as.numeric(ER.ID.unlist[j]) - as.numeric(PT.ID.unlist[j]) + 1)){
      if(Wos.matrix[i,2] == "AB"){
        AB.ID <- cbind(AB.start,Wos.matrix[i,1])
        AB.start <- AB.ID}}
    AB.ID#论文摘要所在行。
    
    for(i in seq(from = 1,to = ncol(AB.ID)))
    {AB.total <- paste(AB,Wos.test[AB.ID[1,i],2],",")
    AB <- AB.total}
  }
  else{AB.total <- "noAB"}#没有AB的情况。
  
  Keyword.total
  Keyword.total.unlist <- unlist(strsplit(Keyword.total,","))
  Keyword.total.unlist[!Keyword.total.unlist == " "]
  Keyword.total <- as.character(trim(Keyword.total.unlist[!Keyword.total.unlist == " "]))
  
  Keyword.total.start = NULL
  for(i in seq(from = 1,to = length(Keyword.total))){
    Keyword.total.end <- paste(Keyword.total.start,Keyword.total[i],sep = ",")
    Keyword.total.start <- Keyword.total.end
  }
  Keyword.total <- Keyword.total.end
  Keyword.total <- substr(Keyword.total,2,nchar(Keyword.total))
  
  ##-------------------------------------------------------------------------------------------------------------------------------
  for(i in seq(from = 1,to = ncol(TI.ID)))
  {TI.total = paste(TI,Wos.test[TI.ID[1,i],2])
  TI <- TI.total}
  TI.total#输出标题。
  
  #for(i in seq(from = 1,to = ncol(UT.ID)))
  #{UT.total = paste(UT,Wos.test[UT.ID[1,i],2])
  #UT <- UT.total}
  #UT.total#输出文献的唯一标识。
  
  for(i in seq(from = 1,to = ncol(Z9.ID)))
  {Z9.total = paste(Z9,Wos.test[Z9.ID[1,i],2])
  Z9 <- Z9.total}
  Z9.total#输出被引用频次。
  
  ##-------------------------------------------------------------------------------------------------------------------------------
  TI.AB.KEY.Z9 <- rbind(TI.total,rbind(AB.total,rbind(Keyword.total,Z9.total)))
  TI.AB.KEY.Z9.rbind <- rbind(TI.AB.KEY.Z9.start,TI.AB.KEY.Z9)
  TI.AB.KEY.Z9.start <- TI.AB.KEY.Z9.rbind
  TI.AB.KEY.Z9#输出标题，摘要，关键词，被引用频次。
  
  txtname <- paste("singletxt2",as.character(j - 1),sep = "+")
  txtname.txt <- paste(txtname,"txt",sep = ".")
  path.txtname.txt <- paste("D:/Rdw/Novelty and Conventionality/SingleFiles_TXT",txtname.txt,sep = "/")
  write.table(TI.AB.KEY.Z9,path.txtname.txt)
  
  
  TI.Z9 <- cbind(TI.total,Z9.total)
  TI.Z9.rbind <- rbind(TI.Z9.start,TI.Z9)
  TI.Z9.start <- TI.Z9.rbind
  TI.Z9#输出文章标题和被引频次。
  
  TI.rbind <- rbind(start,TI.total)
  start <- TI.rbind#输出文章标题。
  
  TI.Keyword <- paste(TI.total,Keyword.total,sep = ":|:")
  TI.Keyword.rbind <- rbind(TI.Keyword.start,TI.Keyword)
  TI.Keyword.start <- TI.Keyword.rbind
  TI.Keyword#输出文章标题和作者关键词。
  
  Keywords.rbind <- rbind(Keywords.start.2,Keyword.total)
  Keywords.start.2 <- Keywords.rbind#按行输出关键词。
  
  
}
#TI.rbind
#TI.Z9.rbind
#TI.Keyword.rbind
#Keywords.rbind
#AB.ID
#TI.AB.KEY.Z9.rbind


#Keywords.rbind.unupper <- tolower(Keywords.rbind)
#keywords.rbind.unupper#里面还包含有“noidnode”的，也就是说里面还有没有编辑关键词的记录。
#Keywords.rbind.unupper#matrix类型。
#去掉无关键词的记录。start---------------------------------------------------------------------------------------------------------------------
#Keywords.rbind.unupper <- Keywords.rbind.unupper[!Keywords.rbind.unupper == "noidnode"]
#Keywords.rbind.unupper#character类型
#--------------------end----------------------------------------------------------------------------------------------------------------------------
#Keywords.rbind.unupper <- as.matrix(Keywords.rbind.unupper)#将类型转换为matrix类型
#regexpr.matrix <- as.matrix((regexpr(",",Keywords.rbind.unupper)))
#Keywords.rbind.unupper[regexpr.matrix  == -1]
#Keywords.rbind.unupper <- as.matrix(Keywords.rbind.unupper[!regexpr.matrix  == -1])
#Keywords.rbind.unupper
###########下接Untitled4
