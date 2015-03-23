# EDA School Analysis - part -1 : Read input files  

# set-up environment

# identify file colClasses for read optimization

attendance.sample <- read.csv("data/School_Attendance_and_Enrollment_Statistics_by_District__2010-11_.csv",
                              stringsAsFactors=FALSE, header=T, nrows=100)  

classSize.sample <- read.csv("data/2010-2011_Class_Size_-_School-level_detail.csv",  
                             stringsAsFactors=FALSE, header=T, nrows=100)  

sat2010.sample <- read.csv("data/SAT__College_Board__2010_School_Level_Results.csv", 
                           stringsAsFactors=FALSE, header=T, nrows=100) 

sat2014.sample <- read.csv("data/SAT_Results2014.csv",  
                           stringsAsFactors=FALSE, header=T, nrows=100)  

safety.sample <- read.csv("data/School_Safety_Report.csv",
                          stringsAsFactors=FALSE, header=T, nrows=100)

demographics.sample <- read.xlsx("data/DemographicSnapshot20078to201314Public.xls",
                                 3, header=TRUE, colIndex=c(1,2,19,23,25,27,29,31,33,35,37),
                                 nrows=100, stringsAsFactors=FALSE) 


attendance.colclass <- sapply(attendance.sample,class)
classSize.colclass <- sapply(classSize.sample,class)
sat2010.colclass <- sapply(sat2010.sample,class)
sat2014.colclass <- sapply(sat2014.sample,class)
safety.colclass <- sapply(safety.sample,class)
demographics.colclass <- sapply(demographics.sample,class)

# read input files

attendance.raw <- tbl_df(read.csv(
                        "data/School_Attendance_and_Enrollment_Statistics_by_District__2010-11_.csv",
                         stringsAsFactors=FALSE, header=T, colClasses=attendance.colclass, 
                         comment.char=""))  

classSize.raw <- tbl_df(read.csv(
                        "data/2010-2011_Class_Size_-_School-level_detail.csv",  
                         stringsAsFactors=FALSE, header=T, colClasses=classSize.colclass, 
                         comment.char="") )

demographics.raw <- tbl_df(read.xlsx(
                            "data/DemographicSnapshot20078to201314Public.xls",3,
                            header=TRUE, colIndex=c(1,2,19,23,25,27,29,31,33,35,37),
                            stringsAsFactors=FALSE)) 

sat2010.raw <- tbl_df(read.csv(
                      "data/SAT__College_Board__2010_School_Level_Results.csv", 
                       stringsAsFactors=FALSE, header=T,colClasses=sat2010.colclass, 
                       comment.char=""))

sat2014.raw <- tbl_df(read.csv("data/SAT_Results2014.csv",  stringsAsFactors=FALSE, 
                      header=T,colClasses=sat2014.colclass, 
                      comment.char=""))

safety.raw <- tbl_df(read.csv("data/School_Safety_Report.csv", stringsAsFactors=FALSE, 
                              header=T,colClasses=safety.colclass, 
                              comment.char="")) 

