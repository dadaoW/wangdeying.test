###将表格存储为CSv格式的文件，然后用read.csv函数进行读取。暂时解决了无法读取数值型数据的问题。
###从Web of Science到处文件中，截取“PT”和“ER”的所在行数。
#Wos.test <- read.csv("savedrecs1-200(hasRef).csv",header = T)
Wos.test1 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source1-500.csv",header = T)
Wos.test2 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source501-1000.csv",header = T)
Wos.test3 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source1001-1500.csv",header = T)
Wos.test4 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source1501-2000.csv",header = T)
Wos.test4 <- Wos.test4[,1:2]
Wos.test5 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source2001-2500.csv",header = T)
Wos.test6 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source2501-3000.csv",header = T)
Wos.test7 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source3001-3500.csv",header = T)
Wos.test8 <- read.csv("D:/Rdw/Novelty and Conventionality/DATASOURCE_CSV/Data_Source_end.csv",header = T)
ncol(Wos.test1)
ncol(Wos.test2)
ncol(Wos.test3)
ncol(Wos.test4)
ncol(Wos.test5)
ncol(Wos.test6)
ncol(Wos.test7)
ncol(Wos.test8)

Wos.test <- rbind(Wos.test1,Wos.test2,Wos.test3,Wos.test4,Wos.test5,Wos.test6,Wos.test7,Wos.test8)
nrow(Wos.test)
#class(Wos.test)
rownames(Wos.test)
nrow(Wos.test)
#Wos.test[3000,]
Wos.test.id <- cbind(rownames(Wos.test),Wos.test)
nrow(Wos.test)
PT.start = NULL
ER.start = NULL
for(i in seq(from = 1,to = nrow(Wos.test))){
  if (Wos.test.id[i,2] == "PT"){
    PT.ID <- paste(PT.start,Wos.test.id[i,1],sep = ",")
    PT.start <- PT.ID }}
#PT.ID#输出PT所在行数。
for(i in seq(from = 1,to = nrow(Wos.test))){
  if (Wos.test.id[i,2] == "ER"){ 
    ER.ID <- paste(ER.start,Wos.test.id[i,1],sep = ",")
    ER.start <- ER.ID}}
#ER.ID#输出ER所在行数。

PT.ID.unlist <- unlist(strsplit(PT.ID,","))
#PT.ID.unlist[497]
ER.ID.unlist <- unlist(strsplit(ER.ID,","))
#ER.ID.unlist[497]
length(PT.ID.unlist)
length(ER.ID.unlist)
