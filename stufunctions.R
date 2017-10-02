# To test the functions, just go to the bottom of the file and run the codes there
# First 5 functions are writen for question d.
# To get the result, just use cleanstu(dataframe, correct one)
# To clean the dataframe.
cleanstu<-function(df,dfclean){
  n<-nrow(dfclean)
  x<-nrow(df)/(n+1)
  y<-1
  df1<-data.frame(c("x.1",1:n),check.names=FALSE)
  for(i in 1:x){
    df1<-cbind(df1,df[seq(y,y+n),])
    y<-y+n+1
  }
  df1<-cnullcol(df1)
  df1<-cheader(df1)
  df1<-cleancol(df1)
  df1<-classcol(df1,dfclean)
  return(df1)
}

# To get rid of null colomns, serves for cleanstu function
cnullcol<-function(df){
  x<-c()
  for(i in 1:ncol(df)){
    if(sum(""==df[,i])==nrow(df)){
      x<-c(x,i)
    }
  }
  df<-df[,-x]
  return(df)
}

# To change the first column into header, serves for cleanstu function
cheader<-function(df){
  header<-as.vector(as.matrix(df[1,]))
  colnames(df)<-header
  df<-df[-1,]
  return(df)
}

# After cleaning, I discovered columns' names from nurse_visit are messed up
# The following function is just a combination of codes, cannot be used in other cases
cleancol<-function(cleanedstu){
  cleanedstu$nurse_visit<- paste(cleanedstu$nurse_visit,cleanedstu$absences)
  cleanedstu$absences<-cleanedstu$Grades
  cleanedstu$Grades<-cleanedstu[,35]
  cleanedstu[,35]<-NULL
  return(cleanedstu)
}

# To change the class of columns
# Similar to the above function, this also can only used in this case
classcol<-function(df,dfclean){
  for (i in 1:ncol(dfclean)){
      if(is.integer(dfclean[,i])){
         df[,i]<-as.integer(as.character(df[,i]))
         }
     }
  for(i in 1:ncol(df)){
    if(is.factor(dfclean[,i])){
      df[,i]<-as.factor(as.character(df[,i]))
    }
  }
  return(df)
}

# Following are functions for question e.
# To get the result, just run the function mjobmatter(dataframe)
# The dataframe used here must be the result of last question.
mjobmatter<-function(df){
  teacher<-c()
  nteacher<-c()
  df$Mjob<-as.character(df$Mjob)
  for (i in 1:nrow(df)){
    if(df[i,'Mjob'] == "teacher"){
      teacher<-c(teacher,getgrade(df[i,'Grades']))
    }else{
      nteacher<-c(nteacher,getgrade(df[i,'Grades']))
    }
  }
  print("Average grade of teacher's children:")
  print(mean(teacher))
  print("Average grade of other's children:")
  print(mean(nteacher))
  if (mean(teacher)>mean(nteacher)){
    print("Good to have a teacher mom!")
  }else{
    print("Teacher mom cannot help her own child!")
  }
  return(NULL)
}

# To get the grade of one student
getgrade<-function(gfact){
  gstr<-as.character(gfact)
  num<-sum(as.numeric(strsplit(gstr,split='/')[[1]]))
  return(num)
}

# Copy the following codes to test my functions (mind the location of your files might not be the same of mine)
#studentsfile<-read.table("students2.txt",fill=TRUE)
#studclean<-read.csv("students.csv",header=TRUE)
#studafterclean<-cleanstu(studentsfile,studclean)
#mjobmatter(studafterclean)
