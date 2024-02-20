

#--------------------
#   Import/reading
#--------------------

#Libraries and Packages
library(datasets)
library(ggplot2)
install.packages('readxl')
library(readxl)
install.packages('lubridate')
library(lubridate)
library(dplyr)
install.packages('broom')
library(broom)


#----------------------------
#   Data import and cleaning
#----------------------------
#Reading employee file
df = read.csv("C:\\Users\\kangs\\Desktop\\APU\\Y2S1\\PFDA\\Official Assignment\\employee_attrition.csv")

#1. Renaming
names(df) = c("EmployeeID","Record_Date","Birth_Date","Hired_Date","Termination_Date","Age","YearsWorked","City","Department","Job_Title","Store_Name","Gender_Short","Gender_Full","Termination_Reason","Termination_Type","Year_Status","Status","Head_Office")

#2. Removing unwanted data
#Remove 0:00 by only keeping first 10 characters
df$Record_Date<-substr(df$Record_Date,1,10)

#3. Replacing misspelling
df <-df %>%mutate(Termination_Reason=ifelse(Termination_Reason=="Resignaton","Resignation",Termination_Reason))

#4. Filter duplicate data
df_filtered = df%>%group_by(EmployeeID)%>%filter(Year_Status == max(Year_Status))

#5. Standardizing dates formats
#Remove 0:00 by only keeping first 10 characters
df$Record_Date<-substr(df$Record_Date,1,10)

#Format Record_Date
df$Record_Date<-as.Date(df$Record_Date,format= "%m/%d/%y")
df$Record_Date<-format(df$Record_Date,"%Y-%m-%d")

#Format Birth_Date
df$Birth_Date<-as.Date(df$Birth_Date,format= "%m/%d/%y")
df$Birth_Date<-format(df$Birth_Date,"%Y-%m-%d")

#Format Hired_Date
df$Hired_Date<-as.Date(df$Hired_Date,format= "%m/%d/%y")
df$Hired_Date<-format(df$Hired_Date,"%Y-%m-%d")

#Format Termination_Date
df$Termination_Date<-as.Date(df$Termination_Date,format= "%m/%d/%y")
df$Termination_Date<-format(df$Termination_Date,"%Y-%m-%d")


#------------------------------------------------------------------------------
#   Question 1 : What is the attrition for different aspects of the company?
#------------------------------------------------------------------------------
# -- Analysis 1.1 What is the attrition based on year?
#Summarizing total remained and total terminated in a year
YearStatus <- df%>%group_by(Year_Status)%>%summarize(Total_Remained = sum(Status=="ACTIVE"),Total_Terminated =sum(Status=="TERMINATED"))

#Calculating attrition
AttritionOfYear <- mutate(YearStatus,Attrition = YearStatus$Total_Terminated/(YearStatus$Total_Remained+YearStatus$Total_Terminated*0.5))

#Create the graph
G_AttritionofYear<-ggplot(AttritionOfYear,aes(x =Year_Status,y=Attrition))+
  geom_bar(stat ="identity",fill ="steelblue",col="white")+
  labs(x ="Status Year",y ="Attrition Rate")+
  ggtitle( "Attrition Rate by Year")+
  theme_minimal()+
  scale_x_continuous(breaks=seq(min(AttritionOfYear$Year_Status),max(AttritionOfYear$Year_Status),by =1))


# Display the graph
G_AttritionofYear

-- # Analysis 1.2 What is the attrition based on departments --
  #Group data by departments
DepartmentStatus <- df%>%group_by(Department)%>%summarize(Total_Remained = sum(Status=="ACTIVE"),Total_Terminated =sum(Status=="TERMINATED"))


#Calculate Attrition for each department
DepartmentAttrition <-DepartmentStatus %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Create graph
G_DepartmentAttrition <- ggplot(DepartmentAttrition,aes(x=DepartmentAttrition$Attrition,y=DepartmentAttrition$Department))+
  geom_bar(stat = "identity",fill="steelblue")+       
  labs(x ="Attrition",y ="Department")+
  geom_text(aes(label =Total_Terminated),vjust =0,hjust=1)+
  ggtitle( "Attrition Rate by Department")+
  theme_minimal()

#Display graph
G_DepartmentAttrition

-- # Analysis 1.3 What is the attrition based on departments with year --
  #Group data by departments and year
DepartmentStatusxYear <- df%>%group_by(Department,Year_Status)%>%summarize(Total_Remained = sum(Status=="ACTIVE"),Total_Terminated =sum(Status=="TERMINATED"))

#Calculate Attrition for each department
DepartmentAttritionxYear <-DepartmentStatusxYear %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Creating graph
G_DepartmentAttritionxYear <- ggplot(DepartmentAttritionxYear,aes(x=DepartmentAttritionxYear$Attrition,y=DepartmentAttritionxYear$Department,fill=factor(DepartmentAttritionxYear$Year_Status)))+
  geom_bar(stat = "identity")+       
  labs(x ="Attrition",y ="Department")+
  ggtitle( "Attrition Rate by Department")+
  theme_minimal()

#Display graph
G_DepartmentAttritionxYear


-- #Analysis 1.4 What is the attrition based on Job Title of all years --
  #Group data by Job title and year
JobTitlexStatus <- df%>%group_by(Job_Title,Year_Status)%>%summarize(Total_Remained = sum(Status=="ACTIVE"),Total_Terminated =sum(Status=="TERMINATED"))

#Calculate Attrition for each department
JobTitlexStatusAttrition <-JobTitlexStatus %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Creating graph
G_JobTitlexStatusAttrition <- ggplot(JobTitlexStatusAttrition,aes(x=Attrition,y=Job_Title,fill=factor(Year_Status)))+
  geom_bar(stat = "identity")+       
  labs(x ="Attrition",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title with year")+
  theme_minimal()


#Display graph
G_JobTitlexStatusAttrition

-- #Analysis 1.5 What is the attrition based on Stores of all years --
#Group data by Store and year
Store_NamexStatus <- df%>%group_by(Store_Name,Year_Status)%>%summarize(Total_Remained = sum(Status=="ACTIVE"),Total_Terminated =sum(Status=="TERMINATED"))

#Calculate Attrition for each store
Store_NamexStatusAttrition <-Store_NamexStatus %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Creating graph
G_Store_NamexStatusAttrition <- ggplot(Store_NamexStatusAttrition,aes(x=Attrition,y=Store_Name,fill=factor(Year_Status)))+
  geom_bar(stat="identity")+       
  labs(x ="Attrition")+
  ggtitle( "Attrition Rate by Store Name with year")+
  theme_minimal()+
  scale_y_discrete(limits =1:46)

#Display graph
G_Store_NamexStatusAttrition

#Analysis 1.6 Which City or store has closed down?
#Create Graph
G_CityxDepartment= ggplot(df_filtered,aes(y=Store_Name,fill=factor(City)))+geom_bar(col="white")+
  scale_y_discrete(limit=(1:46))

#Display Graph
G_CityxDepartment

#------------------------------------------------------------------------------
#   Question 2 : What happened to the company at 2009,2014 and 2015?
#------------------------------------------------------------------------------

#Analysis 2.1 What is the attrition for year 2009 Specifically?
#Filter year and calculate attrition
JobTitlexStatusAttrition2009 <-JobTitlexStatus%>%filter(Year_Status=="2009") %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Create Graph
G_JobTitlexStatusAttrition2009 <- ggplot(JobTitlexStatusAttrition2009,aes(x=Attrition,y=Job_Title))+
  geom_bar(stat = "identity",fill="palegoldenrod")+       
  labs(x ="Attrition",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title of year 2009")+
  theme_minimal()

#Display Graph
G_JobTitlexStatusAttrition2009

#Analysis 2.2 What is the attrition for year 2014 Specifically?
#Filter year and calculate attrition
JobTitlexStatusAttrition2014 <-JobTitlexStatus%>%filter(Year_Status=="2014") %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Create Graph
G_JobTitlexStatusAttrition2014 <- ggplot(JobTitlexStatusAttrition2014,aes(x=Attrition,y=Job_Title))+
  geom_bar(stat = "identity",fill="palegoldenrod")+       
  labs(x ="Attrition",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title of year 2014")+
  theme_minimal()

#Display Graph
G_JobTitlexStatusAttrition2014

#Analysis 2.3 What is the attrition for year 2015 Specifically?
#Filter year and calculate attrition
JobTitlexStatusAttrition2015 <-JobTitlexStatus%>%filter(Year_Status=="2015") %>%mutate(Attrition=Total_Terminated/(Total_Remained+Total_Terminated))

#Create Graph
G_JobTitlexStatusAttrition2015 <- ggplot(JobTitlexStatusAttrition2015,aes(x=Attrition,y=Job_Title))+
  geom_bar(stat = "identity",fill="palegoldenrod")+       
  labs(x ="Attrition",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title of year 2015")+
  theme_minimal()

#Display Graph
G_JobTitlexStatusAttrition2015

#Analysis 2.4 What is the reason of Job titles termination for 2009? --
#Filter for terminated dataset, ,group by year and reason , remove high volume department 
JobTitlexTermination_Reason <- df%>%filter(Status=="TERMINATED")%>%group_by(Job_Title,Year_Status,Termination_Reason)%>%summarize(Total_Terminated=sum(Status=="TERMINATED"))
JobTitlexTermination_Reason <- JobTitlexTermination_Reason%>%filter(!(Job_Title%in%c("Produce Clerk","Meat Cutter","Dairy Person","Cashier","Baker","Shelf Stocker")))

#Generate Graph
G_TerminationReasonxYear2009 <- ggplot(JobTitlexTermination_Reason%>%filter(Year_Status%in%c("2009")),aes(x=Total_Terminated,y=Job_Title,fill=factor(Termination_Reason)))+
  geom_bar(stat = "identity")+       
  labs(x ="Terminated Count",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title with reason 2009 ")+
  theme_minimal()

#Display Graph
G_TerminationReasonxYear2009

#Analysis 2.5 What is the reason of Job titles termination for 2014? --
#Generate Graph
G_TerminationReasonxYear2014 <- ggplot(JobTitlexTermination_Reason%>%filter(Year_Status%in%c("2014")),aes(x=Total_Terminated,y=Job_Title,fill=factor(Termination_Reason)))+
  geom_bar(stat = "identity")+       
  labs(x ="Terminated Count",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title with reason 2014")+
  theme_minimal()

#Display Graph
G_TerminationReasonxYear2014

#Analysis 2.6 What is the reason of Job titles termination for 2015? --
#Generate Graph
G_TerminationReasonxYear2015 <- ggplot(JobTitlexTermination_Reason%>%filter(Year_Status%in%c("2015")),aes(x=Total_Terminated,y=Job_Title,fill=factor(Termination_Reason)))+
  geom_bar(stat = "identity",)+       
  labs(x ="Terminated Count",y ="Job_Title")+
  ggtitle( "Attrition Rate by Job Title with reason 2015")+
  theme_minimal()

#Display Graph
G_TerminationReasonxYear2015


#Analysis 2.7 What is the age of people whom retired? --
#Create graph
G_AgeOfRetirement <- ggplot(df%>%filter(Termination_Reason=="Retirement"),aes(x=Age,y=Job_Title),fill)+
  geom_violin(color = "cadetblue3")+      
  labs(x ="Age",y ="Job Title")+
  ggtitle( "Age of people whom left for retirement")+
  theme_minimal()

#Display graph
G_AgeOfRetirement

#Analysis 2.8 Which department is at danger for retirement?
#Create Graph
AgeDistributionInDepartment<-ggplot(df%>%filter(Year_Status=="2015"),aes(y=Department,x=Age,fill=factor(Department)))+
  geom_violin()+
  labs(x ="Age",y ="Department")+
  ggtitle("Age distribution in department 2015")+
  theme_minimal()

#Display Graph
AgeDistributionInDepartment


#Analysis 2.9 Which store is at danger for retirement?
#Create Graph
AgeDistributionInStore<-ggplot(df%>%filter(Year_Status=="2015"),aes(y=Store_Name,x=Age,fill=factor(Store_Name)))+
  geom_boxplot()+
  labs(x ="Age",y ="Store_Name")+
  ggtitle("Age distribution in stores 2015")+
  theme_minimal()+
  scale_y_discrete(limit = 1:46)

#Display Graph
AgeDistributionInStore


#Analysis 2.10 How long did employees whom are laid off worked for?


#Create Graph
G_LayedOff_Length <-ggplot(df_filteredAgeLayedOff,aes(x=YearsWorked,y=Job_Title))+
  geom_violin(fill="cadetblue3")+
  labs(x ="YearsWorked",y ="Department")+
  ggtitle("Years Worked to Job Title in 2014 and 2015")+
  theme_minimal()

#Display Graph
G_LayedOff_Length


#Analysis 2.11 Is employees Layed of in 2015 and 2014 related to Age? --
#Create dataset 2015 and 2014 with the termination reason LayOff
df_filteredAgeLayedOff <-df%>%filter(Year_Status%in%c(2015,2014),Termination_Reason =="Layoff")

#Create Graph
G_LayedOff_2014n2015 <-ggplot(df_filteredAgeLayedOff,aes(x=Age,y=Job_Title))+
  geom_violin(fill="cadetblue3")+
  labs(x ="Age",y ="Department")+
  ggtitle("LayedOff in 2014 and 2015 by JobTitle")+
  theme_minimal()

#Display Graph
G_LayedOff_2014n2015


#Analysis 2.12 What is the of age managers whom Remained? (If yes, might be out of date skills)
#Create dataset of year 2015 and active data
df_Remained <-df%>%filter(Year_Status%in%c(2015),Status=="ACTIVE")

#Create Graph
G_Remained_2015 <-ggplot(df_Remained,aes(x=Age,y=Job_Title))+
  geom_boxplot()+
  labs(x ="Age",y ="Job Title")+
  ggtitle("Remaining employee age distribution")+
  theme_minimal()

#Display Graph
G_Remained_2015

#Analysis 2.13 What is the of age managers whom Remained excluding high volume jobs? (If yes, might be out of date skills)
#Create Graph
G_Remained_2015Filter <-ggplot(df_Remained%>%filter(!(Job_Title%in%c("Produce Manager","Dairy Person","Bakery Manager","Customer Service Manager","Processed Foodss Manager","Cashier"))),aes(x=Age,y=Job_Title))+
  geom_boxplot()+
  labs(x ="Age",y ="Job Title")+
  ggtitle("Remaining employee age distribution [filtered]")+
  theme_minimal()

#Display Graph
G_Remained_2015Filter

#Analysis 2.14 Is Employees Layed off in 2015 and 2014 related to Location Demographic? --
#Creating dataset
df_filteredAgeLayedOff <-df%>%filter(Year_Status%in%c(2015,2014),Termination_Reason =="Layoff")

#Create Graph
G_LayedOff_City <-ggplot(df_filteredAgeLayedOff,aes(y=City,fill=factor(Job_Title)))+
  geom_bar(stat = "count")+
  labs(x ="Count",y ="City")+
  ggtitle("LayedOff in 2014 and 2015 by JobTitle")+
  theme_minimal()
G_LayedOff_City

#Display Graph
G_LayedOff_City

#Analysis 2.15 Is high Layed Off due to generally having high employee count in that city or otherwise? --
#Create Graph
G_City <- ggplot(df,aes(y=City))+geom_bar(col="white",fill="cadetblue3")

#Display Graph
G_City

#Analysis 2.16 Is Layed off related to gender?
#Create Graph
G_LayedOff_Gender <-ggplot(df_filteredAgeLayedOff,aes(x=Gender_Full,fill=factor(Job_Title)))+
  geom_bar(stat="count")+
  labs(x ="Age",y ="Department")+
  ggtitle("LayedOff in 2014 and 2015 by JobTitle")+
  theme_minimal()

#Display Graph
G_LayedOff_Gender

#------------------------------------------------------------------------
#   Question 3 : Is there demographics imbalance in the company?
#-----------------------------------------------------------------------
######Gender########
#Analysis 3.1 What is the gender distribution across the whole company?
#Create Graph
G_Gender= ggplot(df_filtered,aes(x=Gender_Full))+geom_bar(col="white",fill="palegoldenrod")

#Display Graph
G_Gender

#Analysis 3.2 What is the gender distribution across department?
#Create Graph
G_DepartmentxGender <- ggplot(df_filtered,aes(y=Department,fill=factor(Gender_Full)))+geom_bar(col="white")

#Display Graph
G_DepartmentxGender

#Analysis 3.3 Is there gender equality across job title positions?
#Create Graph
G_GenderxJobTitles <- ggplot(df_filtered,aes(y=Job_Title,fill=factor(Gender_Full)))+geom_bar(col="white")

#Display Graph
G_GenderxJobTitles

#Analysis 3.4 Is there gender equality across Job title excluding the high volume jobs
#Create Graph
G_GenderxJobTitles <- df %>%
  filter(!Job_Title %in% 
           c("Shelf Stocker", "Produce Clerk", "Meat Cutter", "Dairy Person", "Cashier", "Baker")) %>%
  ggplot(aes(y = Job_Title, fill = factor(Gender_Full))) +
  geom_bar(col = "white")

#Display Graph
G_GenderxJobTitles

#Analysis 3.5 What is the gender distribution across stores?
#Create Graph
G_GenderxStores = ggplot(df_filtered,aes(y=Store_Name,fill=factor(Gender_Full)))+geom_bar(col="white")+
  scale_y_discrete(limit=1:46)

#Display Graph
G_GenderxStores


#-----------------------------------------------------------------------------------
#   Question 4 : What is the relationship between Age, Job title and years worked?
#-----------------------------------------------------------------------------------
#Analysis 4.1 Which year contained employees whom worked the longest? (To find best example for answering question)
G_MostEmployeeWorked <- ggplot(df,aes(x=factor(Year_Status),y=YearsWorked))+
                                 geom_boxplot()+
                                 labs(x="Year",y="YearsWorked")

#Display Graph
G_MostEmployeeWorked


#Analysis 4.2 What is the age of employee when they're first hired
dfAgeHired <- mutate(df,AgeHired = Age - YearsWorked)

#Create Graph
G_HiredAge <- ggplot(dfAgeHired,aes(x=AgeHired))+geom_bar(stat="count",color="cadetblue3")

#Display Graph
G_HiredAge

#Analysis 4.3 What is the relationship between hired age and years worked?
G_AgeYearsWorked <- ggplot(dfAgeHired,aes(x=AgeHired,y=YearsWorked))+
                    geom_line()

#Display Graph
G_AgeYearsWorked

#Analysis 4.4 What is the correlation and regression age and years worked?
#Calculate the correlation
correlation <-cor(dfAgeHired$Age,dfAgeHired$YearsWorked)

#Print the correlation value
print(correlation)


#Do linear regression analysis
regressionAge <-lm(YearsWorked ~AgeHired,data = dfAgeHired)

#Display summary
summary(regressionAge)
summary(regressionAge)$r.squared

#Analysis 4.5 What is the relationship between Job_Title and YearsWorked?
#Create Graph
G_JobTitleYearsWorked <- ggplot(dfAgeHired%>%filter(Year_Status=="2015"),aes(y = Job_Title,x = YearsWorked,fill=factor(Department))) +
  geom_boxplot(color="steelblue") +labs(y = "Job Title",x = "Years Worked") +
  theme_minimal()

#Display Graph
G_JobTitleYearsWorked

#Do linear regression analysis
regressionJobTitle<-lm(YearsWorked ~Job_Title,data = dfAgeHired%>%filter(Status =="TERMINATED"))

#Display summary
summary(regressionJobTitle)
summary(regressionJobTitle)$r.squared

#-----------------------------------------------------------------------------------------------------
#   Question 5 : What is the years prediction of how long more employees world work before leaving?
#-----------------------------------------------------------------------------------------------------
#Analysis 5.1 What is the prediction of How long each employee would stay based on Age
#Dataset of terminated employees
ProduceYearPrediction <-dfAgeHired %>%filter(Status=="TERMINATED")


#Dataset of remaining employees
dfRemainProduce <- dfAgeHired %>%filter(Year_Status =="2015")%>%filter(!Job_Title%in%c('CEO', 'CHief Information Officer', 'Director, Recruitment', 'Exec Assistant, Finance', 'Exec Assistant, Human Resources', 'Exec Assistant, Legal Counsel', 'Exec Assistant, VP Stores', 'Legal Counsel', 'VP Finance', 'VP Human Resources', 'VP Stores'))

#Calculate regression for each variable
RegressionOfProduceAge<-lm(YearsWorked ~Age,data = ProduceYearPrediction)
age_predictions <-predict(RegressionOfProduceAge,newdata =dfRemainProduce)
dfRemainProduce <- mutate(dfRemainProduce,AgeYearsWorkPrediction = predict(RegressionOfProduceAge,newdata =dfRemainProduce))

#Create Graph
G_YearsWorkPredictionBasedOnAge <- ggplot(dfRemainProduce,aes(y=AgeYearsWorkPrediction,x=Age))+
  geom_point()

#Display Graph
G_YearsWorkPredictionBasedOnAge


#Analysis 5.2 What is the prediction of How long each employee would stay based on Job Title
#Make predictions using terminated dataset
RegressionOfProduceJobTitle<-lm(YearsWorked ~Job_Title,data = ProduceYearPrediction)
jobtitle_predictions <-predict(RegressionOfProduceJobTitle,newdata =dfRemainProduce)
dfRemainProduce <- mutate(dfRemainProduce,JobTitleYearsPrediction = predict(RegressionOfProduceJobTitle,newdata =dfRemainProduce))

#Create Graph
G_YearsWorkPredictionBasedOnJobTitle <- ggplot(dfRemainProduce,aes(y=Job_Title,x=JobTitleYearsPrediction))+
  geom_point()

#Display Graph
G_YearsWorkPredictionBasedOnJobTitle

#Analysis 5.3 What is the weights of Job Title and Age's predictions?
#Calculate weights for predictions
age_weights <- summary(RegressionOfProduceAge)$r.squared
jobtitle_weights <-summary(RegressionOfProduceJobTitle)$r.squared

x = c("Age","Job Title")
y = c(100*round(age_weights/(age_weights+jobtitle_weights),digits=4),100*round(jobtitle_weights/(age_weights+jobtitle_weights),digits=4))
weights <- data.frame(x,y)
names(weights) = c("Variable","Percentage")

#Create Piechart
G_PieChart <-ggplot(weights,aes(x ="",y =Percentage,fill =Variable))+
  geom_bar(stat ="identity",width =1,color ="white")+
  coord_polar("y",start =0)+labs(fill ="Variable",x =NULL, y =NULL,title ="Proportion by Variable")+theme_void()
# Add labels to the pie chart

G_PieChart <-G_PieChart +geom_text(aes(label=paste(Variable,Percentage,"%"),y=Percentage/2),position=position_stack(vjust=0.5),color ="white",size =4)

#Display the pie chart
G_PieChart

#Analysis 5.4 What is the prediction with of length of work based on both Age and Job_Title
# Add new column for combined predictions
dfRemainProduce$YearsWorked_Prediction <-age_predictions*age_weights + jobtitle_predictions*jobtitle_weights

# View the updated dataset
dfRemainProduce

#Create graph
G_ProducePrediction  <- ggplot(dfRemainProduce,aes(x=YearsWorked_Prediction,y=Job_Title))+
  geom_boxplot(color="steelblue") +labs(x = "YearsWorked_Prediction",y = "Job_Title") +
  theme_minimal()

#Display Graph
G_ProducePrediction

#Analysis 5.5 What is the difference between prediction and actual data in departments (-ve = high chance of leaving soon)
#Add Predicted Years left column
ProduceYearPrediction<-mutate(dfRemainProduce,PredictedYearsLeft = YearsWorked-YearsWorked_Prediction)

#Create Graph
G_YearsWorkedDifference_Produce <- ggplot(ProduceYearPrediction,aes(x=PredictedYearsLeft,y=Department))+
  geom_boxplot(color="steelblue") +labs(y = "Department",x = "PredictedYearsLeft") +
  theme_minimal()

#Display Graph
G_YearsWorkedDifference_Produce

#Analysis 5.6 What is the difference between prediction and actual data in each Job Title (-ve = high chance of leaving soon)
#Create Graph
G_YearsWorkedDifference_JobTitle  <- ggplot(ProduceYearPrediction,aes(x=PredictedYearsLeft,y=Job_Title))+
  geom_boxplot(color="steelblue") +labs(y = "Job_Title",x = "PredictedYearsLeft") +
  theme_minimal()

#Display Graph
G_YearsWorkedDifference_JobTitle

#Analysis 5.7 How many employees are predicted to lose for the following years in each Job_Title?
#Generate new column for predicted termination year
PredictTerminationYear <- mutate(ProduceYearPrediction,PredictedTerminationYear = round(year(Hired_Date) + YearsWorked_Prediction,digits=0))

#Create Graph
G_NumberOfEmployeesToHire <- ggplot(PredictTerminationYear,aes(x=PredictedTerminationYear,fill=factor(Job_Title)))+
  geom_bar(stat="count")+
  geom_text(stat="count",aes (label=stat(count)),position =position_stack(vjust =0.5))


#Display Graph
G_NumberOfEmployeesToHire