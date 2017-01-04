########################################################################
# Build a Sample Education Drop Out Dataset
# ----------------------------------------------------------------------
# AUTHORS:            Zhou Fang, Graham Williams.
# CONTRIBUTORS:       Zhou Fang, Graham Williams.
# DATE OF CREATION:   2016-12-15
# DEPARTMENT:         ADS Asia Pacific, IMML
# COMPANY:            Microsoft
########################################################################
#
# Create a new anonymous dataset similar to the AP dataset but based
# on the student dataset from UCI Machine Learning repository
# (https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip)

#### 00 SETUP ------------------------------------

library(magrittr)
library(ggplot2)
library(dplyr)

set.seed(42)

rawData <- "../../rawData"

#### 10 LOAD DATASETS ------------------------------------

## Target (Private) Dataset

ap <- readr::read_csv("dropAP.csv")

names(ap) %<>% rattle::normVarNames() %T>% print()

set.seed(42)
index <- sample(ap$school_id, 100, replace=FALSE)

ap %<>% filter(school_id %in% index) %T>% 
  {.$school_id %>% head() %>% print()}

## Public Dataset

mat <- readr::read_csv2(file.path("student", "student-mat.csv"))
por <- readr::read_csv2(file.path("student", "student-por.csv"))

stu <- merge(mat, por,
             by=c("school", "sex", "age", "address", "famsize",
                  "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                  "reason", "nursery", "internet"))

n <- nrow(stu) # 382 Students

names(stu) %<>%
  rattle::normVarNames() %>%
  stringr::str_replace("^g_", "g") %T>%
  print()

#### 20 CLEANUP ------------------------------------

## Generalise names from the source dataset.

# What about school directorate?

names(ap)

#[1] "new_child_id"            "school_id"               "studying_class"          #"gender"                 
#[5] "caste"                   "mother_tongue"           "disability"              "area"                   
#[9] "school_category_new"     "establishment_year"      "total_students"          "good_class_rooms"       
#[13] "bad_class_rooms"         "total_toilet_functional" "total_toilets"           "ramps"                  
#[17] "boundary_walls"          "language_1_marks"        "language_2_marks"        "english_marks"          
#[21] "science_marks"           "mathematics_marks"       "male_teacher"            "female_teacher"         
#[25] "general_caste_teacher"   "obc_caste_teacher"       "orc_caste_teacher"       "other_caste_teacher"    
#[29] "sc_caste_teacher"        "st_caste_teacher"        "unknown_caste_teacher"   "languages_teacher"      
#[33] "mathematics_teacher"     "science_teacher"         "social_studies_teacher"  "exp_under_5"            
#[37] "exp_5to_10"              "exp_over_10"             "age_25to_35"             "age_35to_45"            
#[41] "age_45above"             "continue_drop"      

names(stu)

#  [1] "school"       #"sex"          "age"          "address"      "famsize"     
#  [6] "pstatus"      "medu"         "fedu"         "mjob"         "fjob"        
# [11] "reason"       "nursery"      "internet"     "guardian_x"   "traveltime_x"
# [16] "studytime_x"  "failures_x"   "schoolsup_x"  "famsup_x"     "paid_x"      
# [21] "activities_x" "higher_x"     "romantic_x"   "famrel_x"     "freetime_x"  
# [26] "goout_x"      "dalc_x"       "walc_x"       "health_x"     "absences_x"  
# [31] "g1_x"         "g2_x"         "g3_x"         "guardian_y"   "traveltime_y"
# [36] "studytime_y"  "failures_y"   "schoolsup_y"  "famsup_y"     "paid_y"      
# [41] "activities_y" "higher_y"     "romantic_y"   "famrel_y"     "freetime_y"  
# [46] "goout_y"      "dalc_y"       "walc_y"       "health_y"     "absences_y"  
# [51] "g1_y"         "g2_y"         "g3_y"        

#  [1] "school"     "sex"        "age"        "address"    "famsize"   
#  [6] "Pstatus"    "Medu"       "Fedu"       "Mjob"       "Fjob"      
# [11] "reason"     "guardian"   "traveltime" "studytime"  "failures"  
# [16] "schoolsup"  "famsup"     "paid"       "activities" "nursery"   
# [21] "higher"     "internet"   "romantic"   "famrel"     "freetime"  
# [26] "goout"      "Dalc"       "Walc"       "health"     "absences"  
# [31] "G1"         "G2"         "G3"        

#### Transform continue_drop to be numeric

vnames <- c('continue_drop')

ap[vnames] %<>% 
  lapply(as.factor) %>% 
  lapply(as.numeric) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}


#### CONTINUE_DROP ------------------------------------------

ap %>% summarise(av=mean(continue_drop-1))

ds <- stu

set.seed(42)
ds %<>%
  mutate(continue_drop= sample(c(0, 1), n, replace=TRUE, prob=c(0.95, 0.05))) %T>%
  {.$continue_drop %>% table() %>% print()}

#### 30 Generate Scores ------------------------------------

# Heading towards 3 scores for everyone 

ap %>% ggplot(aes(x=mathematics_marks)) + geom_histogram()

ap %>% ggplot(aes(x=science_marks)) + geom_histogram()

ap %>% ggplot(aes(x=english_marks)) + geom_histogram()

#ap %>% ggplot(aes(x=language_1_marks)) + geom_histogram()

#ap %>% ggplot(aes(x=language_2_marks)) + geom_histogram()

ds %<>%
  mutate(mathematics_marks=(ds$g1_x+ds$g2_x+ds$g3_x) %>%	# Add the three maths scores.
           scales::rescale(c(0, 1)) %>%	# Rescale to similar to AP.
           "+"(rnorm(n, sd=0.05)) %>%	# Spread the data a bit more.
           ifelse(. < 0.10, . + 0.10, .) %>%	# Remove the small score outliers.
           round(digits=3),
         
          english_marks=(ds$g1_y+ds$g2_y+ds$g3_y) %>%	# Add the three portugese scores.
           scales::rescale(c(0, 1)) %>%	# Rescale to similar to AP.
           "+"(rnorm(n, sd=0.05)) %>%	# Spread the data a bit more.
           ifelse(. < 0.10, . + 0.10, .) %>% 	# Remove the small score outliers.
           round(digits=3),                             # Into 3 digits like AP.
         
          science_marks=mathematics_marks
         )

ds %>% ggplot(aes(x=mathematics_marks)) + geom_histogram()

ds %>% ggplot(aes(x=english_marks)) + geom_histogram()

ds %>% ggplot(aes(x=science_marks)) + geom_histogram()


#### 40 Create Student IDs ------------------------------------

ds %<>%
  mutate(student_id=sprintf('s%05d', 1:n)) %T>%
  {.$student_id %>% print()}

# Note that for AP many appear with just a single entry, many with
# two, and three and then drops off. Some sit multiple tests over and
# over?

ap %>%
  group_by(new_child_id) %>%
  summarise(count=length(new_child_id)) %>%
  group_by(count) %>%
  summarise(freq=length(count))

#count   freq
#<int>  <int>
#  1     5899


#### 42 GENDER ------------------------------------

# Aim for roughly the same proportions as in AP

table(ap$gender)/nrow(ap)

# Bias this with slightly lower proportion of drop out? 

ap %>% group_by(gender) %>% summarise(av=mean(continue_drop-1))
  
# Keep this as it is.

ds %<>%
  mutate(gender=sex) %T>%
  {.$gender %>% table() %>% print()}

table(ds$gender)/nrow(ds)

#### 50 GUARDIAN ------------------------------------  

# Some variables to keep almost as is.

ds %<>%
  mutate(guardian=ifelse(.$guardian_x == .$guardian_y, guardian_x, "mixed"))

#### CASTE -------------------------------------------------

table(ap$caste)/nrow(ap)

ds %<>%
  mutate(caste=sample(c("BC", "OC", "SC", "ST"), n, replace=TRUE, prob=c(0.55, 0.16, 0.24, 0.05))) %T>%
  {.$caste %>% print()}

table(ds$caste)/nrow(ds)

#### ADD TEACHER SKILLS

ap %>% select(science_teacher) %>% table()

table(ap$science_teacher)/nrow(ap)

ds %<>%
  mutate(science_teacher=sample(0:9, n, replace=TRUE, prob=c(0.09, 0.09, 0.12, 0.05, 0.24, 0.12, 0.13, 0.05, 0.08, 0.03))) %T>%
  {.$science_teacher %>% print()}

table(ds$science_teacher)/nrow(ds)

table(ap$languages_teacher)/nrow(ap)

ds %<>%
  mutate(languages_teacher=sample(0:12, n, replace=TRUE, prob=c(0.11, 0.03, 0.07, 0.07, 0.14, 0.10, 0.17, 0.10, 0.03, 0.04, 0.09, 0.03, 0.02))) %T>%
  {.$languages_teacher %>% print()}

table(ds$languages_teacher)/nrow(ds)

# Let's identify the variables to keep in the dataset.

vars <- c("continue_drop", "student_id", 
          "gender","caste", "mathematics_marks", "english_marks", "science_marks",
          "science_teacher", "languages_teacher", "guardian", "internet") 

#### Replicate --------------------------------------------

dsrep <- as.data.frame(sapply(ds[vars], rep, times=50))

# Random sample school_code from ap's school_id value and replicate it 50 times

school_id <- factor(unlist(replicate(50, sample(ap$school_id, 382, replace=TRUE), simplify=FALSE)))

dsrep$school_id <- school_id

dsrep %>% select(school_id) %>% table()

dsrep %>% select(school_id) %>% table() %>% length()

# Generate new student_id

dsrep %<>%
  mutate(student_id=sprintf('s%05d', 1:19100)) %T>%
  {.$student_id %>% print()}

dsrep %>% str()

#### 42 ADD TOTAL_STUDENTs ------------------------------------

table(dsrep$school_id)

dsrep %>%
  group_by(school_id) %>%
  summarise(total_students=length(student_id)) ->
total_students_table

dsrep <- left_join(dsrep, total_students_table, by=c("school_id"))

#### ADD TOTAL_TOILET ----------------------------------------------

ap %>% select(total_toilets) %>% table() %>% length()

ap$school_id <- factor(as.character(ap$school_id))

ap %>% 
  group_by(school_id) %>%
  select(total_toilets) %>%
  distinct(total_toilets) %>%
  data.frame() %>% 
  tbl_df() ->
total_toilets_table

total_toilets_table$total_toilets <- round(rescale(total_toilets_table$total_toilets, c(1, 100)) + (rnorm(100, sd=0.05)))

total_toilets_table

dsrep <- left_join(dsrep, total_toilets_table, by=c("school_id"))

dsrep %>% str()


#### ADD ESTABLISHED_YEAR

table(ap$establishment_year)

ap %>% select(establishment_year) %>% table() %>% length()

ap %>% 
  group_by(school_id) %>%
  select(establishment_year) %>%
  distinct(establishment_year) %>%
  data.frame() %>% 
  tbl_df() ->
  establishment_year_table

establishment_year_table$establishment_year <- round(rescale(establishment_year_table$establishment_year, c(1800, 2016)) + (rnorm(60, sd=5)))

establishment_year_table

dsrep <- left_join(dsrep, establishment_year_table, by=c("school_id"))

dsrep %>% str()

#### Change school_id -------------------------

levels(dsrep$school_id) <- sprintf('%05d', 300:399)

levels(dsrep$continue_drop) <- c("continue", "drop")

levels(dsrep$internet) <- c("FALSE", "TRUE")

dsrep %>% str()

#### TRANSFORM DATA TYPE

vnames <- c('internet')

dsrep[vnames] %<>% 
  lapply(as.character) %>%
  lapply(as.logical) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

vnames <- c('continue_drop', 'gender', 'caste', 'guardian', 'school_id')

dsrep[vnames] %<>% 
  lapply(as.character) %>%
  head() %>% 
  print()

vnames <- c('science_teacher', 'languages_teacher', 'establishment_year', 'total_toilets')

dsrep[vnames] %<>%
  lapply(as.character) %>%
  lapply(as.integer) %>%
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

vnames <- c('mathematics_marks', 'english_marks', 'science_marks')

dsrep[vnames] %<>% 
  lapply(as.character) %>%
  lapply(as.numeric) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}


dsrep %>% str()

#'data.frame':	19100 obs. of  15 variables:
#  $ continue_drop     : chr  "continue" "continue" "continue" "continue" ...
#$ student_id        : chr  "s00001" "s00002" "s00003" "s00004" ...
#$ gender            : chr  "F" "F" "F" "F" ...
#$ caste             : chr  "SC" "BC" "OC" "BC" ...
#$ mathematics_marks : num  0.409 0.29 0.602 0.378 0.536 0.594 0.177 0.48 0.821 0.418 ...
#$ english_marks     : num  0.514 0.512 0.666 0.526 0.614 0.519 0.525 0.457 0.728 0.322 ...
#$ science_marks     : num  0.409 0.29 0.602 0.378 0.536 0.594 0.177 0.48 0.821 0.418 ...
#$ science_teacher   : int  6 4 4 8 9 4 6 2 2 8 ...
#$ languages_teacher : int  0 7 2 7 4 8 7 9 2 10 ...
#$ guardian          : chr  "mother" "mother" "mother" "mother" ...
#$ internet          : logi  TRUE TRUE FALSE TRUE TRUE TRUE ...
#$ school_id         : chr  "00331" "00354" "00390" "00368" ...
#$ total_students    : int  486 184 173 234 433 319 319 265 197 191 ...
#$ total_toilets     : int  15 NA 14 15 44 15 15 60 14 15 ...
#$ establishment_year: int  1990 NA 1972 1894 1957 ...

#### 90 Save data ------------------------------------

readr::write_csv(dsrep, path="studentDropIndia_20161215.csv")

save(dsrep, file="studentDropIndia.RData")


