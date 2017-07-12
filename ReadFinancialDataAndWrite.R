FinancialData<-function(files)
{
  library(jsonlite)
  library(gdata)
  library(RCurl)
  baseurl<-"https://www.screener.in/api/company/"
  files<-sort(files,decreasing = FALSE)
  notfound<-c()
  for(i in 1:length(files))
  {
    print("................")
    print(paste(files[i],"processing....",i,sep="  "))
    
    ###### step 1: Get Data ########
    URL<-paste(baseurl,files[i],sep="")
    input <- fromJSON(getURL(URL))
    if(length(input$detail)>0)
    {
      print(paste(files[i],"Details Not Found",i,sep="  "))
      notfound<-c(notfound,files[i])
      
    }else
    {
    resultlist<-list()
    CompanyDesc<-data.frame(input$name,input$short_name,input$bse_code,input$nse_code)
    names(CompanyDesc)<-gsub("input.","",names(CompanyDesc))
    names(CompanyDesc)<-c("CompanyFullName","CompanyShortName","bse_code","nse_code")
    resultlist[[length(resultlist)+1]]<-CompanyDesc
    
    datSheet<-input$number_set
    for(k in 1:length(datSheet))
    {
      print(paste(files[i],"processing....",names(datSheet[k]),sep="  "))
      
      sheetnameFin<-names(datSheet[k])
      assign(sheetnameFin,c())
      bs<-datSheet[[k]]
      for(j in 1:length(bs))
      {
        result<-unlist(bs[j])
        assign(sheetnameFin,cbind(get(sheetnameFin),result))
      }
      
      sheetname<-get(sheetnameFin)
      colnames(sheetname)<-NULL
      colnames(sheetname)<-sheetname[1,]
      sheetname<-data.frame(sheetname)
      sheetname<-sheetname[-1,]
      date<-rownames(sheetname)
      sheetname<-cbind(date,sheetname)
      rownames(sheetname)<-NULL
      resultlist[[length(resultlist)+1]]<-sheetname
    }
      ###### pros and cons
    Cons<-data.frame(cons=input$warehouse_set$analysis$bad)
    Pros<-data.frame(pros=input$warehouse_set$analysis$good)
      resultlist[[length(resultlist)+1]]<-Cons
      resultlist[[length(resultlist)+1]]<-Pros
      names(resultlist)<-c("CompanyDesc",names(datSheet),"Cons","Pros")
      
      ###### step 2: Write To DB #####
      ####### connet to database ############
      library(RODBC)
      ###Create DB connection
      print("writing to db")
      
      connection<-odbcDriverConnect('driver={SQL Server};server=192.168.2.104,1433;
                                  database=Financial_Information;uid=Sanket;pwd=Password@123')
      
      if(nrow(resultlist$balancesheet)>0)
      {
        balancesheet<-cbind(resultlist$CompanyDesc,resultlist$balancesheet)
        ### Write Data to Database 
        sqlSave(connection,balancesheet,tablename="balancesheet",append=TRUE,rownames=FALSE)
      }
      
      if(nrow(resultlist$annual)>0)
      {
       Annual<-cbind(resultlist$CompanyDesc,resultlist$annual)
       ### Write Data to Database 
       sqlSave(connection,Annual,tablename="Annual",append=TRUE,rownames=FALSE)
      }
   
      if(nrow(resultlist$quarters)>0)
      {
       Quarter<-cbind(resultlist$CompanyDesc,resultlist$quarters)
       ### Write Data to Database 
       sqlSave(connection,Quarter,tablename="Quarter",append=TRUE,rownames=FALSE)
      }
      
     
      if(nrow(resultlist$cashflow)>0)
      {
      cashflow<-cbind(resultlist$CompanyDesc,resultlist$cashflow)
      ### Write Data to Database 
      sqlSave(connection,cashflow,tablename="cashflow",append=TRUE,rownames=FALSE)
      }
      
      
      Cons<-resultlist$Cons
      Pros<-resultlist$Pros
       if(nrow(Pros)>0 & nrow(Cons)>0)
      {
        prosandcons<-cbind(resultlist$CompanyDesc,cbindX(Pros,Cons)) 
      }else if(nrow(Pros)==0 & nrow(Cons)==0)
      {
        Cons<-data.frame(Cons<-NA)
        names(Cons)<-c("cons")
        Pros<-data.frame(Pros<-NA)
        names(Pros)<-c("pros")
      
        prosandcons<-cbind(resultlist$CompanyDesc,cbindX(Pros,Cons)) 
       
      }else if(nrow(Pros)==0 | nrow(Cons)==0)
      {
        if(nrow(Pros)>0)
        {
          Cons<-data.frame(Cons<-NA)
          names(Cons)<-c("cons")
          prosandcons<-cbind(resultlist$CompanyDesc,cbindX(Pros,Cons)) 
        }else
        {
          Pros<-data.frame(Pros<-NA)
          names(Pros)<-c("pros")
          prosandcons<-cbind(resultlist$CompanyDesc,cbindX(Pros,Cons)) 
        }
      }else
      {
        print("...")
      }
      ### Write Data to Database 
      sqlSave(connection,prosandcons,tablename="ProsAndCons",append=TRUE,rownames=FALSE)

      ###Closing Connection ########
      odbcClose(connection)
      
    }
  }
  
  return(notfound)
 }

############### function Call #############
setwd("F://StocksData")
companyList1<-read.csv("500.csv")
companyList1<-as.character(companyList1$Symbol)
companyList2<-read.csv("non500.csv")
companyList2<-as.character(companyList2$Symbol)
companyname<-c(companyList1,companyList2)


## passing the vector of company names to function as an argument ###
FinancialData(companyname)






#found<-read.csv("Found.csv")
#found<-found$Symbols
#companyname<-sort(companyname,decreasing = FALSE)
#notfound<-companyname[!(companyname %in% found)]
#notfound
#write.csv(notfound,"CompanyDataNotFound.csv",row.names = FALSE)

############# from a name hit url and read json in r##########
 library(RCurl)
 baseurl<-"https://www.screener.in/api/company/"
 companyname<-c("SBIN","BEL","3MINDIA","WIPRO","SREINFRA","INFY","HDFCBANK","GATI","DRREDDY","CIPLA","SUNPHARMA",
  "ACC","ABB","ADANIPOWER")
 companyname<-sort(companyname,decreasing = FALSE)
 URL<-paste(baseurl,companyname[1],sep="")

###### step 1: Get Data ########
input <- fromJSON(getURL(URL))
 


