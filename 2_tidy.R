# pre-process files and generate tidy data for analysis

# pre process attendance file

attendance.in <-  dplyr::rename(attendance.raw,
                                district = District, 
                                attendance.rate = YTD...Attendance..Avg.,
                                total.students = YTD.Enrollment.Avg.)

attendance.in <- mutate(attendance.in,
                        district = extract_numeric(district),
                        attendance.rate = extract_numeric(attendance.rate))

attendance.out <- filter(attendance.in,district > 0 & district < 33)

# pre process class size file

classSize.in <-  filter(classSize.raw, GRADE=="09-12", PROGRAM.TYPE == "GEN ED" , 
                        CORE.SUBJECT..MS.CORE.and.9.12.ONLY. %in% c("ENGLISH","MATH"))

classSize.in <- select(classSize.in, 
                       district = CSD, 
                       school.id = SCHOOL.CODE,
                       subject = CORE.SUBJECT..MS.CORE.and.9.12.ONLY.,
                       students = NUMBER.OF.STUDENTS...SEATS.FILLED,
                       classes = NUMBER.OF.SECTIONS)

# school level roll-up 
group.bySchool <- group_by(classSize.in,district,school.id,subject)
size.bySchool <- dplyr::summarise(group.bySchool, 
                                  students.total=sum(students), 
                                  classes.total=sum(classes),
                                  size=students.total/classes.total
)  
size.bySchool  <- select(size.bySchool, - students.total, -classes.total)
overall.size <- spread(size.bySchool,subject,size)

size.bySchool.out <-  dplyr::rename(overall.size,
                                   math.size = MATH, 
                                   english.size = ENGLISH)

# district level roll-up 

group.byDistrict <- group_by(classSize.in,district)
size.byDistrict <- dplyr::summarise(group.byDistrict, 
                                    students.total=sum(students), 
                                    classes.total=sum(classes),
                                    size=students.total/classes.total
)  
size.byDistrict.out  <- select(size.byDistrict, - students.total, -classes.total)

# 3.Preprocess Demographics File  

demographics.in <- filter(demographics.raw, Year == "2010-11")
demographics.in$district <-  as.numeric(demographics.in$District)

demographics.out <-  select(demographics.in,
                            district,  
                            female.ratio = X..Female, 
                            ethnic.asian = X..Asian,
                            ethnic.black = X..Black, 
                            ethnic.hispanic = X..Hispanic,
                            ethnic.White = X..White, 
                            disability.ratio = X..Students.with.Disabilities,
                            english.learners = X..English.Language.Learners,
                            poverty.ratio = X..Eligible.for.Free.or.Reduced.Lunches) 


# 4.Preprocess safety / crime file 

safety.in <- filter(safety.raw,NoCrim.N != "N/A")
safety.in <- filter(safety.in,Geographical.District.Code > 0 & Geographical.District.Code <33 )

safety.in$Major.N <- as.numeric(safety.in$Major.N) 
safety.in$Oth.N <- as.numeric(safety.in$Oth.N) 
safety.in$NoCrim.N <- as.numeric(safety.in$NoCrim.N) 
safety.in$AvgOfMajor.N <- as.numeric(safety.in$AvgOfMajor.N) 
safety.in$AvgOfOth.N <- as.numeric(safety.in$AvgOfOth.N) 
safety.in$AvgOfNoCrim.N <- as.numeric(safety.in$AvgOfNoCrim.N) 

safety.in <- mutate(safety.in, 
                    expected.crime=AvgOfMajor.N+AvgOfOth.N+AvgOfNoCrim.N,
                    total.crime=Major.N+Oth.N+NoCrim.N,
                    average.crime=total.crime/expected.crime)

safety.in <- select(safety.in, 
                    district= Geographical.District.Code,
                    average.crime)

safety.group.by <- group_by (safety.in,district)
safety.out <- dplyr::summarise(safety.group.by, crime=mean(average.crime))  

# 5. SAT score 2010 file.

sat2010.in <- sat2010.raw

sat2010.in$district <- as.integer(substring(sat2010.in$DBN , first=1 , last=2))
sat2010.in$school.id <- as.character(substring(sat2010.in$DBN , first=3 , last=6))
sat2010.in$Number.of.Test.Takers = as.integer(sat2010.in$Number.of.Test.Takers)
sat2010.in$Critical.Reading.Mean = as.numeric(sat2010.in$Critical.Reading.Mean)
sat2010.in$Mathematics.Mean = as.numeric(sat2010.in$Mathematics.Mean)
sat2010.in$Writing.Mean = as.numeric(sat2010.in$Writing.Mean)

sat2010.in = filter(sat2010.in,Number.of.Test.Takers > 0& district <= 32)
sat2010.in <- mutate(sat2010.in, 
                     math.total = Mathematics.Mean * Number.of.Test.Takers , 
                     reading.total =  Critical.Reading.Mean *  Number.of.Test.Takers ,  
                     writing.total =  Writing.Mean *  Number.of.Test.Takers )

sat2010.in = select(sat2010.in, -DBN, -School.Name, 
                    -Critical.Reading.Mean, -Mathematics.Mean, -Writing.Mean)

# generate school level roll-up for 2010 SAT scores : 
sat2010.group.bySchool <- group_by(sat2010.in,district,school.id)
sat2010.bySchool.out <- dplyr::summarise(sat2010.group.bySchool, 
                                         total2010 = (sum(math.total+reading.total+writing.total)/(sum(Number.of.Test.Takers))),
                                         reading2010.total=(sum(reading.total)/(sum(Number.of.Test.Takers))),
                                         writing2010.total=(sum(writing.total)/(sum(Number.of.Test.Takers))),
                                         english2010.total=(sum(reading.total+writing.total)/(sum(Number.of.Test.Takers))),
                                         math2010.total=(sum(math.total)/(sum(Number.of.Test.Takers))),
                                         total2010.percent=
                                           ((sum(math.total)+sum(reading.total)+sum(writing.total))/(sum(Number.of.Test.Takers)*24)),
                                         english2010.percent=((sum(reading.total)+sum(writing.total))/(sum(Number.of.Test.Takers)*16)),
                                         math2010.percent=(sum(math.total)/(sum(Number.of.Test.Takers)*8)))  


# generate district level roll-up for SAT scores: 
sat2010.group.byDistrict <- group_by (sat2010.in,district)
sat2010.byDistrict.out  <- dplyr::summarise(sat2010.group.byDistrict, 
                                            total.percent=((sum(math.total)+sum(reading.total) + 
                                                              sum(writing.total)) /     (sum(Number.of.Test.Takers)*2400)))

# 6.2014 SAT score

sat2014.in <- sat2014.raw

sat2014.in$district <- as.integer(substring(sat2014.in$DBN , first=1 , last=2))
sat2014.in$school.id <- as.character(substring(sat2014.in$DBN , first=3 , last=6))
sat2014.in$Num.of.SAT.Test.Takers = as.integer(sat2014.in$Num.of.SAT.Test.Takers)
sat2014.in$SAT.Critical.Reading.Avg..Score = as.numeric(sat2014.in$SAT.Critical.Reading.Avg..Score)
sat2014.in$SAT.Math.Avg..Score = as.numeric(sat2014.in$SAT.Math.Avg..Score)
sat2014.in$SAT.Writing.Avg..Score = as.numeric(sat2014.in$SAT.Writing.Avg..Score)

sat2014.in = filter(sat2014.in,Num.of.SAT.Test.Takers > 0 & district < 33)

sat2014.in <- mutate(sat2014.in, 
                     math.total = SAT.Math.Avg..Score * Num.of.SAT.Test.Takers, 
                     reading.total =  SAT.Critical.Reading.Avg..Score *  Num.of.SAT.Test.Takers,  
                     writing.total =  SAT.Writing.Avg..Score *  Num.of.SAT.Test.Takers)

sat2014.in = select(sat2014.in, -DBN, -SCHOOL.NAME, -SAT.Critical.Reading.Avg..Score,
                    -SAT.Math.Avg..Score,-SAT.Writing.Avg..Score,
                    -SAT.Critical.Reading.Avg..Score)
# Generate school level roll-up 

sat2014.group.bySchool <- group_by(sat2014.in,district,school.id)
sat2014.bySchool.out <- dplyr::summarise(sat2014.group.bySchool, 
                                         total2014 = (sum(math.total+reading.total+writing.total)/(sum(Num.of.SAT.Test.Takers))),
                                         reading2014.total=(sum(reading.total)/(sum(Num.of.SAT.Test.Takers))),
                                         writing2014.total=(sum(writing.total)/(sum(Num.of.SAT.Test.Takers))),
                                         english2014.total=(sum(reading.total+writing.total)/(sum(Num.of.SAT.Test.Takers))),
                                         math2014.total=(sum(math.total)/(sum(Num.of.SAT.Test.Takers))),
                                         total2014.percent=((sum(math.total)+sum(reading.total)+sum(writing.total))/(sum(Num.of.SAT.Test.Takers)*24)),
                                         english2014.percent=((sum(reading.total)+sum(writing.total))/(sum(Num.of.SAT.Test.Takers)*16)),
                                         math2014.percent=(sum(math.total)/(sum(Num.of.SAT.Test.Takers)*8)))  

# Generate district level roll-up 

sat2014.group.byDistrict <- group_by(sat2014.in,district)
sat2014.byDistrict.out <- dplyr::summarise(sat2014.group.byDistrict, 
                                           total.percent.2014=((sum(math.total)+sum(reading.total)+sum(writing.total)) / 
                                                                 (sum(Num.of.SAT.Test.Takers)*2400)))

# Generate consolidated district level file 

district.level.data <- join_all(list(size.byDistrict.out,
                                     attendance.out,
                                     safety.out,
                                     demographics.out,
                                     sat2010.byDistrict.out,
                                     sat2014.byDistrict.out), by = 'district', type = 'full')

district.level.data <- tbl_df(district.level.data)[,-1]  


# Transform / center data 

scaled.district.data <- as.data.frame(cbind(district.level.data[,1],scale(district.level.data[,-1],
                                                                          center = TRUE, scale = TRUE)))

# add category groups 

sizegroup <- cut(scaled.district.data$size, breaks = 4,include.lowest=TRUE, 
                 labels = c("small","medium","big","very big"))
povertygroup <- cut(scaled.district.data$poverty.ratio, breaks = 4,include.lowest=TRUE,
                    labels = c("low","medium","high","very high"))
scaled.district.data <- as.data.frame(cbind(scaled.district.data,sizegroup,povertygroup))

# Generate consolidated school level file

school.level.data <- join_all(list(size.bySchool.out,
                                   sat2010.bySchool.out,
                                   sat2014.bySchool.out), 
                              by = c('district','school.id'), type = 'full')


school.level.data <- tbl_df(na.omit(school.level.data))[,-1]

scaled.school.data <- as.data.frame(cbind(school.level.data[,c(1:3)],scale(school.level.data[,-c(1:3)], center = TRUE, scale = TRUE)))

scaled.school.data <- mutate(scaled.school.data,
                             chMath = math2014.total - math2010.total,
                             chEnglish = english2014.total - english2010.total,
                             chReading = reading2014.total - reading2010.total,
                             chWriting = writing2014.total - writing2010.total)

scaled.school.data <- scaled.school.data[,-c(4:19)]

math.sizegroup <- cut(scaled.school.data$math.size, breaks = c(0,20,25,30,35),
                      include.lowest=TRUE,labels = c("small","medium","big","very big"))

reading.sizegroup <- cut(scaled.school.data$english.size, breaks = c(0,20,25,30,35),
                         include.lowest=TRUE, labels = c("small","medium","big","very big"))

writing.sizegroup <- cut(scaled.school.data$english.size, breaks = c(0,20,25,30,35),
                         include.lowest=TRUE, labels = c("small","medium","big","very big"))

scaled.school.data <- as.data.frame(cbind(scaled.school.data,
                                          math.sizegroup,reading.sizegroup,
                                          writing.sizegroup))
