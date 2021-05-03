library(dplyr)
library(qcc)
library(lubridate)
library(matrixStats)

# get the in-control data
getwd()
EDdata = read.csv("C:/Users/HP/Desktop/USF Spring21/Statistical Quality Control/proj_data/Incontrol_data.csv")
test1 = read.csv("C:/Users/HP/Desktop/USF Spring21/Statistical Quality Control/proj_data/Test_data_1.csv")
test2 = read.csv("C:/Users/HP/Desktop/USF Spring21/Statistical Quality Control/proj_data/Test_data_2.csv")
test3 = read.csv("C:/Users/HP/Desktop/USF Spring21/Statistical Quality Control/proj_data/Test_data_3.csv")

#check data types of columns
str(EDdata)
str(test1)
str(test2)

#Merge Ward 1 and Ward 2 data to facilitate calculations by getting rid of Null values, also since Ward have the same probability of
#receiving a patient, it should not matter a patient being assigned to a specific ward. Maybe, later on calculations will be done with wards separately
#for a more specific control?

transformed_data = EDdata %>% mutate(Ward_assigned = coalesce(Ward_1, Ward_2)) %>%
  select(Id, Check_In, Ward_assigned, Departure)

test1_transformed = test1 %>% mutate(Ward_assigned = coalesce(Ward_1, Ward_2)) %>%
  select(Id, Check_In, Ward_assigned, Departure)

test2_transformed = test2 %>% mutate(Ward_assigned = coalesce(Ward_1, Ward_2)) %>%
  select(Id, Check_In, Ward_assigned, Departure)

test3_transformed = test3 %>% mutate(Ward_assigned = coalesce(Ward_1, Ward_2)) %>%
  select(Id, Check_In, Ward_assigned, Departure)

#convert to date type necessary columns
transformed_data$Check_In = mdy_hm(transformed_data$Check_In)
transformed_data$Ward_assigned = mdy_hm(transformed_data$Ward_assigned)
transformed_data$Departure = mdy_hm(transformed_data$Departure)

test1_transformed$Check_In = mdy_hm(test1_transformed$Check_In)
test1_transformed$Ward_assigned = mdy_hm(test1_transformed$Ward_assigned)
test1_transformed$Departure = mdy_hm(test1_transformed$Departure)

test2_transformed$Check_In = ymd_hms(test2_transformed$Check_In)
test2_transformed$Ward_assigned = ymd_hms(test2_transformed$Ward_assigned)
test2_transformed$Departure = ymd_hms(test2_transformed$Departure)

test3_transformed$Check_In = ymd_hms(test3_transformed$Check_In)
test3_transformed$Ward_assigned = ymd_hms(test3_transformed$Ward_assigned)
test3_transformed$Departure = ymd_hms(test3_transformed$Departure)

#Check if data is in correct format (POSIXct is for date)
str(transformed_data)
str(test1_transformed)
str(test2_transformed)
str(test3_transformed)



#create table with our quality_measures for each transformed dataset
incontrol_quality_measures = data.frame( 
  id = transformed_data$Id,
  checked_in_day =  day(transformed_data$Check_In),
  checked_in_hour = hour(transformed_data$Check_In),
  waiting_time = as.numeric(difftime(transformed_data$Ward_assigned, transformed_data$Check_In, units = "mins")),
  assigned_ward_day = day(transformed_data$Ward_assigned),
  assigned_ward_hour = hour(transformed_data$Ward_assigned),
  treatment_time = as.numeric(difftime(transformed_data$Departure, transformed_data$Ward_assigned, units = "mins")))


test1_quality_measures = data.frame( 
  id = test1_transformed$Id,
  checked_in_day =  day(test1_transformed$Check_In),
  checked_in_hour = hour(test1_transformed$Check_In),
  waiting_time = as.numeric(difftime(test1_transformed$Ward_assigned, test1_transformed$Check_In, units = "mins")),
  assigned_ward_day = day(test1_transformed$Ward_assigned),
  assigned_ward_hour = hour(test1_transformed$Ward_assigned),
  treatment_time = as.numeric(difftime(test1_transformed$Departure, test1_transformed$Ward_assigned, units = "mins")))


test2_quality_measures = data.frame( 
  id = test2_transformed$Id,
  checked_in_day =  day(test2_transformed$Check_In),
  checked_in_hour = hour(test2_transformed$Check_In),
  waiting_time = as.numeric(difftime(test2_transformed$Ward_assigned, test2_transformed$Check_In, units = "mins")),
  assigned_ward_day = day(test2_transformed$Ward_assigned),
  assigned_ward_hour = hour(test2_transformed$Ward_assigned),
  treatment_time = as.numeric(difftime(test2_transformed$Departure, test2_transformed$Ward_assigned, units = "mins")))


test3_quality_measures = data.frame( 
  id = test3_transformed$Id,
  checked_in_day =  day(test3_transformed$Check_In),
  checked_in_hour = hour(test3_transformed$Check_In),
  waiting_time = as.numeric(difftime(test3_transformed$Ward_assigned, test3_transformed$Check_In, units = "mins")),
  assigned_ward_day = day(test3_transformed$Ward_assigned),
  assigned_ward_hour = hour(test3_transformed$Ward_assigned),
  treatment_time = as.numeric(difftime(test3_transformed$Departure, test3_transformed$Ward_assigned, units = "mins")))

#add a column with sample group id for each quality measure

incontrol_quality_measures <- transform(incontrol_quality_measures, sample = as.numeric(interaction(checked_in_hour, checked_in_day, drop=TRUE)))

test1_quality_measures <- transform(test1_quality_measures, sample = as.numeric(interaction(checked_in_hour, checked_in_day, drop=TRUE)))

test2_quality_measures <- transform(test2_quality_measures, sample = as.numeric(interaction(checked_in_hour, checked_in_day, drop=TRUE)))

test3_quality_measures <- transform(test3_quality_measures, sample = as.numeric(interaction(checked_in_hour, checked_in_day, drop=TRUE)))



#find subgroups of size 1 which must be dropped due to qcc control chart condition purposes
table = table(incontrol_quality_measures$sample)
print(table == 1)
size1_samples = c(11,47,50, 82, 86, 118, 121, 161, 163)

new_quality_measures = incontrol_quality_measures[(!incontrol_quality_measures$sample %in% size1_samples),]
#
table1 = table(test1_quality_measures$sample)
print(table1 == 1)
size1_samples = c(88,104,187,193,198)

new_t1_quality_measures = test1_quality_measures[(!test1_quality_measures$sample %in% size1_samples),]
#
table2 = table(test2_quality_measures$sample)
print(table2 == 1)
size2_samples = c(53,64,67,115,130,180)

new_t2_quality_measures = test2_quality_measures[(!test2_quality_measures$sample %in% size2_samples),]
#
table3 = table(test3_quality_measures$sample)
print(table3 == 1)
size3_samples = c(3,28,38,53,82,99,157,182,205)

new_t3_quality_measures = test3_quality_measures[(!test3_quality_measures$sample %in% size3_samples),]



############################  Creating quality measures samples ######################################
incontrol_waiting_time = with(new_quality_measures, qcc.groups(waiting_time, sample))
incontrol_treatment_time = with(new_quality_measures, qcc.groups(treatment_time, sample))

t1_waiting_time = with(new_t1_quality_measures, qcc.groups(waiting_time, sample))
t1_treatment_time = with(new_t1_quality_measures, qcc.groups(treatment_time, sample))

t2_waiting_time = with(new_t2_quality_measures, qcc.groups(waiting_time, sample))
t2_treatment_time = with(new_t2_quality_measures, qcc.groups(treatment_time, sample))

t3_waiting_time = with(new_t3_quality_measures, qcc.groups(waiting_time, sample))
t3_treatment_time = with(new_t3_quality_measures, qcc.groups(treatment_time, sample))


#summary(new_quality_measures)

######################################### s and r chart of waiting time #############################################
################################for waiting time
#completely incontrol data S
summary(qcc(incontrol_waiting_time, type = 'S' ))
#control charts with new out of control data
#t1 1
qcc(incontrol_waiting_time, type = 'S',newdata = t1_waiting_time[30:80,],chart.all = FALSE )
#t2 1
qcc(incontrol_waiting_time, type = 'S',newdata = t2_waiting_time[31:60,],chart.all = FALSE )
#t3 1
qcc(incontrol_waiting_time, type = 'S',newdata = t3_waiting_time[31:60,],chart.all = FALSE )

#completely incontrol data R
qcc(incontrol_waiting_time[136:166,], type = 'R', )
#control charts with new out of control data
#t1 1
qcc(incontrol_waiting_time, type = 'R',newdata = t1_waiting_time[1:30,],chart.all = FALSE )
#t2 1
qcc(incontrol_waiting_time, type = 'R',newdata = t2_waiting_time[31:60,],chart.all = FALSE )
#t3 1
qcc(incontrol_waiting_time, type = 'R',newdata = t3_waiting_time[31:60,],chart.all = FALSE )
################################for treatment time
#completely incontrol data S
qcc(incontrol_treatment_time[136:170,], type = 'S', )
#control charts with new out of control data
#t1 1
qcc(incontrol_treatment_time[136:170,], type = 'S',newdata = t1_treatment_time[1:30,],chart.all = FALSE )
#t2 1
qcc(incontrol_treatment_time[136:170,], type = 'S',newdata = t2_treatment_time[31:60,],chart.all = FALSE )
#t3 1
qcc(incontrol_treatment_time[136:170,], type = 'S',newdata = t3_treatment_time[31:60,],chart.all = FALSE )

#completely incontrol data R
qcc(incontrol_treatment_time[136:166,], type = 'R', )
#control charts with new out of control data
#t1 1
qcc(incontrol_treatment_time[136:166,], type = 'R',newdata = t1_treatment_time[1:30,],chart.all = FALSE )
#t2 1
qcc(incontrol_treatment_time[136:166,], type = 'R',newdata = t2_treatment_time[31:60,],chart.all = FALSE )
#t3 1
qcc(incontrol_treatment_time[136:166,], type = 'R',newdata = t3_treatment_time[1:30,],chart.all = FALSE ) # weird results


################################# CUSUM and EWMA chart  ############################################################
#All quality measures are in MINUTES

############ Waiting Time ##########
############# sample stdev data preparation for waiting time########################
incontrol_waiting_time = data.frame(incontrol_waiting_time)
incontrol_waiting_time$sd = rowSds(as.matrix(incontrol_waiting_time),na.rm = TRUE)
incontrol_waiting_time = incontrol_waiting_time %>% transmute(incontrol_waiting_time,ID = row_number())

sd_incontrol_waiting_time = data.frame(incontrol_waiting_time$sd)

t1_waiting_time = data.frame(t1_waiting_time)
t1_waiting_time$sd = rowSds(as.matrix(t1_waiting_time),na.rm = TRUE)
t1_waiting_time = t1_waiting_time %>% transmute(t1_waiting_time,ID = row_number())

sd_t1_waiting_time = data.frame(t1_waiting_time$sd)

t2_waiting_time = data.frame(t2_waiting_time)
t2_waiting_time$sd = rowSds(as.matrix(t2_waiting_time),na.rm = TRUE)
t2_waiting_time = t2_waiting_time %>% transmute(t2_waiting_time,ID = row_number())

sd_t2_waiting_time = data.frame(t2_waiting_time$sd)

t3_waiting_time = data.frame(t3_waiting_time)
t3_waiting_time$sd = rowSds(as.matrix(t3_waiting_time),na.rm = TRUE)
t3_waiting_time = t3_waiting_time %>% transmute(t3_waiting_time,ID = row_number())

sd_t3_waiting_time = data.frame(t3_waiting_time$sd)

################################ CUSUM
#completely incontrol data cusum chart

#k = abs(m1-m0)/2= 11.169/2 = 5.585
#h or decision interval ~~ 0.5
summary(cusum(
sd_incontrol_waiting_time, decision.interval = 0.5, se.shift = 5.5))
#control charts with new out of control data
#t1 1
cusum(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t1_waiting_time %>% slice(1:25),chart.all = FALSE, decision.interval = 0.5, se.shift = 5.5 )
#t2 1
cusum(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t2_waiting_time %>% slice(1:25),chart.all = FALSE, decision.interval = 0.5, se.shift = 5.5)
cusum(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t3_waiting_time %>% slice(1:25),chart.all = FALSE, decision.interval = 0.5, se.shift = 5.5)
############################### EWMA 
#completely incontrol data R
summary(ewma(
  sd_incontrol_waiting_time, lambda = 0.8, nsigmas = 3))
#control charts with new out of control data
#t1 1
ewma(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t1_waiting_time %>% slice(1:25),chart.all = FALSE, lambda = 0.8, nsigmas = 3)
#t2 1
ewma(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t2_waiting_time %>% slice(1:25),chart.all = FALSE, lambda = 0.8, nsigmas = 3 )
#t3 1
ewma(sd_incontrol_waiting_time %>% slice(1:25), newdata = sd_t3_waiting_time %>% slice(1:25),chart.all = FALSE, lambda = 0.8, nsigmas = 3 )


################################ For treatment time ###
############# sample stdev data preparation for treatment time########################
incontrol_treatment_time = data.frame(incontrol_treatment_time)
incontrol_treatment_time$sd = rowSds(as.matrix(incontrol_treatment_time),na.rm = TRUE)
incontrol_treatment_time = incontrol_treatment_time %>% transmute(incontrol_treatment_time,ID = row_number())

sd_incontrol_treatment_time = data.frame(incontrol_treatment_time$sd)

t1_treatment_time = data.frame(t1_treatment_time)
t1_treatment_time$sd = rowSds(as.matrix(t1_treatment_time),na.rm = TRUE)
t1_treatment_time = t1_treatment_time %>% transmute(t1_treatment_time,ID = row_number())

sd_t1_treatment_time = data.frame(t1_treatment_time$sd)

t2_treatment_time = data.frame(t2_treatment_time)
t2_treatment_time$sd = rowSds(as.matrix(t2_treatment_time),na.rm = TRUE)
t2_treatment_time = t2_treatment_time %>% transmute(t2_treatment_time,ID = row_number())

sd_t2_treatment_time = data.frame(t2_treatment_time$sd)

t3_treatment_time = data.frame(t3_treatment_time)
t3_treatment_time$sd = rowSds(as.matrix(t3_treatment_time),na.rm = TRUE)
t3_treatment_time = t3_treatment_time %>% transmute(t3_treatment_time,ID = row_number())

sd_t3_treatment_time = data.frame(t3_treatment_time$sd)

################################ CUSUM
#k = abs(m1-m0)/2= 11.169/2 = 3.1
#h or decision interval ~~ 0.8
#completely incontrol data cusum chart
summary(cusum(
  sd_incontrol_treatment_time %>% slice(25:48), decision.interval = 0.8, se.shift = 3.1))
#control charts with new out of control data
#t1 1
cusum(sd_incontrol_treatment_time %>% slice(25:48), newdata = sd_t1_treatment_time %>% slice(1:20),chart.all = FALSE, decision.interval = 0.8, se.shift = 3.1 )
#t2 1
cusum(sd_incontrol_treatment_time %>% slice(25:48), newdata = sd_t2_treatment_time %>% slice(25:50),chart.all = FALSE, decision.interval = 0.8, se.shift = 3.1 )
#t3 1
cusum(sd_incontrol_treatment_time %>% slice(25:48), newdata = sd_t3_treatment_time %>% slice(25:50),chart.all = FALSE, decision.interval = 0.8, se.shift = 3.1 )

############################### EWMA 
#completely incontrol data R
summary(ewma(
  sd_incontrol_treatment_time %>% slice(25:48), lambda = 0.8))
#control charts with new out of control data
#t1 1
ewma(sd_incontrol_treatment_time, newdata = sd_t1_treatment_time %>% slice(25:48),chart.all = FALSE, lambda = 0.8)
#t2 1
ewma(sd_incontrol_treatment_time %>% slice(25:48), newdata = sd_t2_treatment_time %>% slice(25:50),chart.all = FALSE , lambda = 0.8)
#t3 1
ewma(sd_incontrol_treatment_time %>% slice(25:48), newdata = sd_t3_treatment_time %>% slice(25:50),chart.all = FALSE , lambda = 0.8)


########## Possibility of clustering? #########################


#trying to create bins or group to categorize by treatment time. 
#Why? possibly because the cause of an emergency for a treatment time with 3 hours
#is different than the cause of an emergency for a treatment time of 10 minutes


scatter.smooth(new_quality_measures$treatment_time[1:200], new_quality_measures$treatment_time[1:200])
scatter.smooth(new_quality_measures$treatment_time[201:500], new_quality_measures$treatment_time[201:500])

scatter.smooth(new_t1_quality_measures$treatment_time[1:200], new_t1_quality_measures$treatment_time[1:200])
scatter.smooth(new_t1_quality_measures$treatment_time[201:500], new_t1_quality_measures$treatment_time[201:500])

scatter.smooth(new_t2_quality_measures$treatment_time[1:200], new_t2_quality_measures$treatment_time[1:200])
scatter.smooth(new_t2_quality_measures$treatment_time[201:500], new_t2_quality_measures$treatment_time[201:500])


scatter.smooth(new_t3_quality_measures$treatment_time[1:200], new_t3_quality_measures$treatment_time[1:200])
scatter.smooth(new_t3_quality_measures$treatment_time[201:500], new_t3_quality_measures$treatment_time[201:500])

