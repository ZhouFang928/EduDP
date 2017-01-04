########################################################################
# Build a Sample Education Score Dataset
# ----------------------------------------------------------------------
# AUTHORS:            Zhou Fang, Graham Williams.
# CONTRIBUTORS:       Zhou Fang, Graham Williams.
# DATE OF CREATION:   2016-10-30
# DEPARTMENT:         ADS Asia Pacific, IMML
# COMPANY:            Microsoft
########################################################################
#
# Create a new anonymous dataset similar to the BCE dataset but based
# on the student dataset from UCI Machine Learning repository
# (https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip)

#### 00 SETUP ------------------------------------

library(magrittr)
library(ggplot2)
library(dplyr)
library(lme4)

set.seed(42)

rawData <- "../../rawData"

#### 10 LOAD DATASETS ------------------------------------

## Target (Private) Dataset

bce <- readr::read_csv(file.path(rawData, "readingBCE.csv"))
names(bce) %<>% rattle::normVarNames() %T>% print()

## Public Dataset

mat <- readr::read_csv2(unz(file.path(rawData, "student.zip"), "student-mat.csv"))
por <- readr::read_csv2(unz(file.path(rawData, "student.zip"), "student-por.csv"))

stu <- merge(mat, por,
             by=c("school", "sex", "age", "address", "famsize",
                  "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                  "reason", "nursery", "internet"))

n <- nrow(stu) # 382 Students

n

names(stu) %<>%
  rattle::normVarNames() %>%
  stringr::str_replace("^g_", "g") %T>%
  print()

#### 20 CLEANUP ------------------------------------

## Generalise names from the source dataset.

# What about school directorate?

names(bce)
names(bce) %<>%
  stringr::str_replace("naplanscale_", "") %>%
  stringr::str_replace("naplantest_", "test_")

names(bce)

#  [1] "score"              "student_id"         "school_code"       
#  [4] "indigenous"         "english_language"   "parent_occupation" 
#  [7] "disability_name"    "book_loans_cat"     "attendance_rate"   
# [10] "school_directorate" "sesband"            "school_size"       
# [13] "test_year_level"    "test_year"         

names(stu)

#  [1] "school"       "sex"          "age"          "address"      "famsize"     
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

#### 30 Unify Scores ------------------------------------

# Heading towards 4 scores for everyone and then we mimic some with
# different collecitons of test year levels, like 3, 5, 7 or 5, 7,
# etc, like in the bce data.

bce %>% ggplot(aes(x=score)) + geom_histogram()

ds <- stu

ds %<>%
  mutate(mscore=ds$g1_x+ds$g2_x+ds$g3_x %>%	# Add the three maths scores.
           scales::rescale(c(175, 750)) %>%	# Rescale to similar to BCE.
           "+"(rnorm(n, sd=5)) %>%	# Spread the data a bit more.
           ifelse(. < 300, . + 260, .) %>%	# Remove the small score outliers.
           round(),
         
         pscore=ds$g1_y+ds$g2_y+ds$g3_y %>%	# Add the three portugese scores.
           scales::rescale(c(175, 750)) %>%	# Rescale to similar to BCE.
           "+"(rnorm(n, sd=5)) %>%	# Spread the data a bit more.
           ifelse(. < 300, . + 260, .) %>% 	# Remove the small score outliers.
           round(),                             # Into 3 digits like BCE.

         xscore=pscore,
         
         score=mscore)

ds %>% ggplot(aes(x=mscore)) + geom_histogram()

ds %>% ggplot(aes(x=pscore)) + geom_histogram()

#### 40 Create Student IDs ------------------------------------

ds %<>%
  mutate(student_id=sprintf('s%05d', 1:n)) %T>%
  {.$student_id %>% print()}

# Note that for BCE many appear with just a single entry, many with
# two, and three and then drops off. Some sit multiple tests over and
# over?

bce %>%
  group_by(student_id) %>%
  summarise(count=length(student_id)) %>%
  group_by(count) %>%
  summarise(freq=length(count))

#   count  freq
#   <int> <int>
# 1     1 43207
# 2     2 24032
# 3     3 12641
# 4     4  3691
# 5     5   131
# 6     6    85
# 7     7    15
# 8     8     1
# 9    10     1

# Should I replicate now?

#### 42 INDIGINEOUS ------------------------------------

# Aim for roughly the same proportions as in BCE

table(bce$indigenous)/nrow(bce)

# Bias this with slightly lower scores? 

bce %>% group_by(indigenous) %>% summarise(av=mean(score))

# Randomly choose internet (has roughly similar propotions) and
# convert to indigenous.

ds %<>%
  mutate(indigenous=
           internet == "no" &
           sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.15, 0.85)),
         mscore=ifelse(indigenous, mscore*0.9, mscore)) %T>%
  {.$indigenous %>% table() %>% print()}

table(ds$indigenous)/nrow(stu)
ds %>% group_by(indigenous) %>% summarise(av=mean(mscore))

#### 44 ENGLISH_LANGUAGE ------------------------------------

table(bce$english_language)/nrow(bce)
bce %>% group_by(english_language) %>% summarise(av=mean(score))

# Randomly choose freetime_x == 1 (roughly similar proportions) and
# convert to english_language.

ds %<>%
  mutate(english_language=
           freetime_x != 1) %T>%
  {.$english_language %>% table() %>% print()}

table(ds$english_language)/nrow(stu)
ds %>% group_by(english_language) %>% summarise(av=mean(mscore))

#### 46 PARENT_OCCUPATION ------------------------------------

table(bce$parent_occupation)/nrow(bce)
bce %>% group_by(parent_occupation) %>% summarise(av=mean(score)) %>% print.data.frame()

# Keep these two as they are.

ds %<>%
  mutate(father_occupation=fjob,
         mother_occupation=mjob) %T>%
  {.$father_occupation %>% table() %>% print()}

table(ds$father_occupation)/nrow(stu)
ds %>% group_by(father_occupation) %>% summarise(av=mean(mscore))

table(ds$mother_occupation)/nrow(stu)
ds %>% group_by(mother_occupation) %>% summarise(av=mean(mscore))

#### 48 ATTENDANCE RATE ------------------------------------

summary(bce$attendance_rate)
bce %>%
  ggplot(aes(x=attendance_rate)) +
  geom_density()

ds %<>%
  mutate(attendance_rate=
           (150 - absences_x - absences_y)/150) %T>%
  {.$attendance_rate %>% summary() %>% print()}

#### 50 GUARDIAN ------------------------------------  

# Some variables to keep almost as is.

ds %<>%
  mutate(guardian=ifelse(.$guardian_x == .$guardian_y, guardian_x, "mixed"))

#### 60 WRAP UP - REPLICATE AND ADD SCHOOL_CODE TEST_YEAR LEVEL

bce %>% select(school_code) %>% table()

bce %>% select(school_code) %>% table() %>% length()

bce %>% select(test_year) %>% table()

bce %>% select(test_year_level) %>% table()


# THIS WILL GET TRICKY BUT SHOULD BE DOABLE

# Let's identify the variables to keep in the dataset.

vars <- c("mscore", "pscore", "xscore", "score", "student_id", "school", 
          "indigenous","english_language", "father_occupation", 
          "mother_occupation", "attendance_rate", "guardian") 

#, "test_year", "level")

#### TODO CREATE THE REPLICATED DATASET GENERATING APPROPRIATE SCORE

### ADD TEST_YEAR AND LEVEL ACROSS MULTIPLE STUDENT IDs

library(tidyr)

ds <- gather(ds[vars], level, mark, mscore:score)

ds %>% str()

vnames <- c('level')

ds[vnames] %<>% 
  lapply(factor) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

levels(ds$level) <- c(3, 5, 7, 9)

ds$test_year <- ds$level

levels(ds$test_year) <- c(2009, 2011, 2013, 2015)

vnames <- c('test_year')

ds[vnames] %<>% 
  lapply(as.character) %>% 
  lapply(as.integer) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

names(ds)[names(ds) == 'mark'] <- 'score'

ds$school_code <- ifelse(ds$school=="GP", 301, 302)

ds$school <- NULL

ds %>% str()

ds %>%
  group_by(student_id) %>%
  summarise(count=length(student_id)) %>%
  group_by(count) %>%
  summarise(freq=length(count))


### Build a mixed effect regression model

# Initialise random numbers for repeatable results.

seed <- 123
set.seed(seed)

########################################################################
# TRAIN MODEL

# Train a mixed effect regression model.
m.lmer <- lmer(score ~ (1 | student_id) + (1 | school_code) + indigenous + english_language + 
                 father_occupation + mother_occupation + attendance_rate + 
                 guardian + level, data=ds)

model <- m.lmer
mtype <- "lmer"
mdesc <- "Linear Mixed Effect Model"

# Basic model summary.

model

### Random sample 

seed <- 123
set.seed(seed)

index <- sample(1:nrow(ds), 0.8*nrow(ds), replace=FALSE)

ds <- ds[index, ]

ds %>% str()

ds %>%
  group_by(student_id) %>%
  summarise(count=length(student_id)) %>%
  group_by(count) %>%
  summarise(freq=length(count))

#### TRANSFORM DATA TYPE

vnames <- c('indigenous', 'english_language')

ds[vnames] %<>% 
  lapply(as.character) %>%
  lapply(as.logical) %>% 
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

vnames <- c('father_occupation', 'mother_occupation', 'guardian')

ds[vnames] %<>% 
  lapply(as.character) %>%
  head() %>% 
  print()

vnames <- c('level', 'school_code')

ds[vnames] %<>%
  lapply(as.character) %>%
  lapply(as.integer) %>%
  data.frame() %>% 
  tbl_df() %T>%
  {head(.) %>% print()}

ds %>% str()

#'data.frame':	1222 obs. of  11 variables:
#  $ student_id       : chr  "s00058" "s00058" "s00243" "s00201" ...
#$ indigenous       : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#$ english_language : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#$ father_occupation: chr  "other" "other" "other" "services" ...
#$ mother_occupation: chr  "other" "other" "services" "health" ...
#$ attendance_rate  : num  0.933 0.933 0.96 1 0.84 ...
#$ guardian         : chr  "mother" "mother" "mixed" "mother" ...
#$ level            : int  5 7 5 7 7 3 9 7 9 5 ...
#$ score            : num  568 481 621 530 474 487 523 612 730 654 ...
#$ test_year        : int  2011 2013 2011 2013 2013 2009 2015 2013 2015 2011 ...
#$ school_code      : int  301 301 301 301 301 301 301 301 301 301 ...

ds %>% ggplot(aes(x=score)) + geom_histogram()


#### 90 Replicate to Larger Dataset ------------------------------------

readr::write_csv(ds, path="studentScoreAUS_20161215.csv")

save(dsreprs, file="studentScoreAUS.RData")





