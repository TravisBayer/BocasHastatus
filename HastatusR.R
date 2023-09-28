{VISUALIZING 24 HRS OF HASTATUS ACC AND HR DATA}

  {FIRST UPLOAD THE DATA FOR EACH BAT AND THEN IDENTIFY THE CORRECT TIMEFRAMES TO CREATE A BAT NIGHT}

--------------
  BAT DAYS
--------------
Bat1 (PHYL33)
__________
ACC
_____

  {IMPORT DATA FROM BAT}

Phyl33 <- read.delim("C:/Users/userselu/Desktop/Research/Data/TechnoSmArt Europe/Downloads/Phyl33/Phyl33_S4.csv")

View(Phyl33)

  {DEFINE TIME RANGES FOR DAY 1. BAT DAY = 6PM TO 5:59:59 PM THE FOLLOWING DAY. GPS/ACC DATA TIME IS IN UTC, SO 6PM EST ~ 23:00:00 UTC.}

{DAY 1}

Phyl33Day1_X <- Phyl33[2070360:4230359, 3]
Phyl33Day1_Y <- Phyl33[2070360:4230359, 4]
Phyl33Day1_Z <- Phyl33[2070360:4230359, 5]
Phyl33Day1_time <- Phyl33[2070360:4230359, 2]

  {CREATE A DATA FRAME CONTAINING ALL THREE ACC AXES AND THE TIMESTAMP COLUMN}

Phyl33Day1 <- data.frame(Phyl33Day1_time, Phyl33Day1_X, Phyl33Day1_Y, Phyl33Day1_Z)

  {CREATE AN EQUATION FOR VEDBA WITH THE X Y AND Z AXES SPECIFIC TO THE DATA FRAME YOU JUST CREATED}

VeDBA_Phyl33Day1 = sqrt(Phyl33Day1$Phyl33Day1_X^2 + Phyl33Day1$Phyl33Day1_Y^2 + Phyl33Day1$Phyl33Day1_Z^2)

  {INCORPORATE THAT NEW VEDBA EQUATION INTO YOUR DATA FRAME}

Phyl33Day1$VeDBA_Phyl33Day1 <- VeDBA_Phyl33Day1

{DAY 2}

Phyl33Day2_X <- Phyl33[4230360:6390359, 3]
Phyl33Day2_Y <- Phyl33[4230360:6390359, 4]
Phyl33Day2_Z <- Phyl33[4230360:6390359, 5]
Phyl33Day2_time <- Phyl33[4230360:6390359, 2]

Phyl33Day2 <- data.frame(Phyl33Day2_time, Phyl33Day2_X, Phyl33Day2_Y, Phyl33Day2_Z)
VeDBA_Phyl33Day2 = sqrt(Phyl33Day2$Phyl33Day2_X^2 + Phyl33Day2$Phyl33Day2_Y^2 + Phyl33Day2$Phyl33Day2_Z^2)
Phyl33Day2$VeDBA_Phyl33Day2 <- VeDBA_Phyl33Day2

{DAY3}

Phyl33Day3_X <- Phyl33[6390360:8550359, 3]
Phyl33Day3_Y <- Phyl33[6390360:8550359, 4]
Phyl33Day3_Z <- Phyl33[6390360:8550359, 5]
Phyl33Day3_time <- Phyl33[6390360:8550359, 2]

Phyl33Day3 <- data.frame(Phyl33Day3_time, Phyl33Day3_X, Phyl33Day3_Y, Phyl33Day3_Z)
VeDBA_Phyl33Day3 = sqrt(Phyl33Day3$Phyl33Day3_X^2 + Phyl33Day3$Phyl33Day3_Y^2 + Phyl33Day3$Phyl33Day3_Z^2)
Phyl33Day3$VeDBA_Phyl33Day3 <- VeDBA_Phyl33Day3

{DAY 4}

Phyl33Day4_X <- Phyl33[8550360:10710359, 3]
Phyl33Day4_Y <- Phyl33[8550360:10710359, 4]
Phyl33Day4_Z <- Phyl33[8550360:10710359, 5]
Phyl33Day4_time <- Phyl33[8550360:10710359, 2]

Phyl33Day4 <- data.frame(Phyl33Day4_time, Phyl33Day4_X, Phyl33Day4_Y, Phyl33Day4_Z)
VeDBA_Phyl33Day4 = sqrt(Phyl33Day4$Phyl33Day4_X^2 + Phyl33Day4$Phyl33Day4_Y^2 + Phyl33Day4$Phyl33Day4_Z^2)
Phyl33Day4$VeDBA_Phyl33Day4 <- VeDBA_Phyl33Day4

  {CREATE A DATA FRAME THAT CONTAINS THE DATA FRAMES FROM EACH BAT DAY}

Phyl33Diary <- data.frame(Phyl33Day1, Phyl33Day2, Phyl33Day3, Phyl33Day4)

{GRAPH THE ACC DATA FROM EACH BAT DAY WE DEFINED ABOVE}

{DAY 1}
{ACC}


P33D1graph <- data.frame(VeDBA_Phyl33Day1)
P33D1graph_1 <- P33D1graph %>% mutate(time = row_number())
spline_P33D1 <- as.data.frame(spline(P33D1graph_1$time, P33D1graph_1$VeDBA_Phyl33Day1))

Bat33sample1 <- ggplot(P33D1graph_1, aes(x=P33D1graph_1$time, y= P33D1graph_1$VeDBA_Phyl33Day1, group = 1)) + 
  geom_line(data = spline_P33D1, aes(x = x, y = y))

Bat33sample1

  {GRAPH THE CORRESPONDING HEART RATE DATA FROM THE SAME BAT DURING THE SAME DEFINED TIME FRAMES FOR EACH BAT DAY}
  {TIMESTAMPS ARE IN LOCAL TIME USING A 24 HR CLOCK. 6 PM EST ~ 18:00:00 EST}

{HR}

  {IMPORT HEART RATE DATA FOR ALL THE BATS}

filteredHR <- read.csv("C:/Users/userselu/Desktop/Research/Data/Hastatus Heart Rate Tags/hastatus HR Filtered/hastatusHR/processedData/filteredHR.csv")
View(filteredHR)

  {DEFINE A 24 HR BAT DAY FOR THE HEART RATE DATA. BAT DAY = 6:00 PM - 05:59:59 PM THE FOLLOWING DAY}
  {REFER TO CAPTURE SHEETS IN GOOGLE DRIVE TO DETERMINE THE CORRECT CORRESPONDING HEART RATE TAG NUMBER TO THE BATS GPS NUMBER}
  
Phyl33HRDay1 <- filteredHR[11700:12835, 5]
Phyl33HRDay1time <- filteredHR[11700:12835, 3]

  {CREATE A DATA FRAME FOR THE BAT NIGHT OF HEART RATE DATA}

Phyl33HR1 <- data.frame(Phyl33HRDay1, Phyl33HRDay1time)

  {UNTIL I FIGURE OUT HOW TO CORRECTLY CORRELATE THE CORRECT TIMESTAMPS TO THE DATA, MUTATE A COLUMN THAT COUNTS THE NUMBER OF ROWS IN THE DATA FRAME TO SERVE AS A PLACE HOLDER FOR THE TIME UNTIL PROBLEM IS SOLVED}

Phyl33HR_df1 <- Phyl33HR1 %>% mutate(time = row_number())

  {SPLINE THE DATA FRAME}

spline_P33D1HR <- as.data.frame(spline(Phyl33HR_df1$time, Phyl33HR_df1$Phyl33HRDay1))

  {CREATE A LINE PLOT OF THE DATA}

Bat33HRsample1 <- ggplot(Phyl33HR_df1, aes(x=Phyl33HR_df1$time, y= Phyl33HR_df1$Phyl33HRDay1, group = 1)) + 
  geom_line(data = spline_P33D1HR, aes(x = x, y = y))

Bat33HRsample1

{DAY 2}
{ACC}
  
P33D2graph <- data.frame(VeDBA_Phyl33Day2)
P33D2graph_1 <- P33D2graph %>% mutate(time = row_number())
spline_P33D2 <- as.data.frame(spline(P33D2graph_1$time, P33D2graph_1$VeDBA_Phyl33Day2))

Bat33sample2 <- ggplot(P33D2graph_1, aes(x=P33D2graph_1$time, y= P33D2graph_1$VeDBA_Phyl33Day2, group = 1)) + 
  geom_line(data = spline_P33D2, aes(x = x, y = y))

Bat33sample2

{HR}
  
Phyl33HRDay2 <- filteredHR[12836:13768, 5]

Phyl33HR2 <- data.frame(Phyl33HRDay2)

Phyl33HR_df2 <- Phyl33HR2 %>% mutate(time = row_number())

spline_P33D2HR <- as.data.frame(spline(Phyl33HR_df2$time, Phyl33HR_df2$Phyl33HRDay2))

Bat33HRsample2 <- ggplot(Phyl33HR_df2, aes(x=Phyl33HR_df2$time, y= Phyl33HR_df2$Phyl33HRDay2, group = 1)) + 
  geom_line(data = spline_P33D2HR, aes(x = x, y = y))

Bat33HRsample2

{DAY 3}
{ACC}
  
 P33D3graph <- data.frame(VeDBA_Phyl33D3ay3)
P33D3graph_1 <- P33D3graph %>% mutate(time = row_number())
spline_P33D3 <- as.data.frame(spline(P33D3graph_1$time, P33D3graph_1$VeDBA_Phyl33Day3))

Bat33sample3 <- ggplot(P33D3graph_1, aes(x=P33D3graph_1$time, y= P3D33graph_1$VeDBA_Phyl33Day3, group = 1)) + 
  geom_line(data = spline_P33D3, aes(x = x, y = y))

Bat33sample3

{HR}
  
Phyl33HRDay3 <- filteredHR[13769:14658, 5]

Phyl33HR3 <- data.frame(Phyl33HRDay3)

Phyl33HR_df3 <- Phyl33HR3 %>% mutate(time = row_number())

spline_P33D3HR <- as.data.frame(spline(Phyl33HR_df3$time, Phyl33HR_df3$Phyl33HRDay3))

Bat33HRsample3 <- ggplot(Phyl33HR_df3, aes(x=Phyl33HR_df3$time, y= Phyl33HR_df3$Phyl33HRDay3, group = 1)) + 
  geom_line(data = spline_P33D3HR, aes(x = x, y = y))

Bat33HRsample3

{DAY 4}
{ACC}
  
P33D4graph <- data.frame(VeDBA_Phyl33Day4)
P33D4graph_1 <- P33D4graph %>% mutate(time = row_number())
spline_P33D4 <- as.data.frame(spline(P33D4graph_1$time, P33D4graph_1$VeDBA_Phyl33Day4))

Bat33sample4 <- ggplot(P33D4graph_1, aes(x=P33D4graph_1$time, y= P33D4graph_1$VeDBA_Phyl33Day4, group = 1)) + 
  geom_line(data = spline_P33D4, aes(x = x, y = y))

Bat33sample4

{HR}
  
Phyl33HRDay4 <- filteredHR[14659:15482, 5]

Phyl33HR4 <- data.frame(Phyl33HRDay4)

Phyl33HR_df4 <- Phyl33HR4 %>% mutate(time = row_number())

spline_P33D4HR <- as.data.frame(spline(Phyl33HR_df4$time, Phyl33HR_df4$Phyl33HRDay4))

Bat33HRsample4 <- ggplot(Phyl33HR_df4, aes(x=Phyl33HR_df4$time, y= Phyl33HR_df4$Phyl33HRDay4, group = 1)) + 
  geom_line(data = spline_P33D4HR, aes(x = x, y = y))

Bat33HRsample4

______________
Bat2 (PHYL34)
______________
ACC
_____

Phyl34 <- read.delim("C:/Users/userselu/Desktop/Research/Data/TechnoSmArt Europe/Downloads/Phyl34/Phyl34_S2.csv")
View(Phyl34)

Phyl34Day1_X <- Phyl34[2070246:4230245, 3]
Phyl34Day1_Y <- Phyl34[2070246:4230245, 4]
Phyl34Day1_Z <- Phyl34[2070246:4230245, 5]
Phyl34Day1_time <- Phyl34[2070246:4230245, 2]

Phyl34Day1 <- data.frame(Phyl34Day1_time, Phyl34Day1_X, Phyl34Day1_Y, Phyl34Day1_Z)
VeDBA_Phyl34Day1 = sqrt(Phyl34Day1$Phyl34Day1_X^2 + Phyl34Day1$Phyl34Day1_Y^2 + Phyl34Day1$Phyl34Day1_Z^2)
Phyl34Day1$VeDBA_Phyl34Day1 <- VeDBA_Phyl34Day1

Phyl34Day2_X <- Phyl34[4230246:6390245, 3]
Phyl34Day2_Y <- Phyl34[4230246:6390245, 4]
Phyl34Day2_Z <- Phyl34[4230246:6390245, 5]
Phyl34Day2_time <- Phyl34[4230246:6390245, 2]

Phyl34Day2 <- data.frame(Phyl34Day2_time, Phyl34Day2_X, Phyl34Day2_Y, Phyl34Day2_Z)
VeDBA_Phyl34Day2 = sqrt(Phyl34Day2$Phyl34Day2_X^2 + Phyl34Day2$Phyl34Day2_Y^2 + Phyl34Day2$Phyl34Day2_Z^2)
Phyl34Day2$VeDBA_Phyl34Day2 <- VeDBA_Phyl34Day2

Phyl34Day3_X <- Phyl34[6390246:8550245, 3]
Phyl34Day3_Y <- Phyl34[6390246:8550245, 4]
Phyl34Day3_Z <- Phyl34[6390246:8550245, 5]
Phyl34Day3_time <- Phyl34[6390246:8550245, 2]

Phyl34Day3 <- data.frame(Phyl34Day3_time, Phyl34Day3_X, Phyl34Day3_Y, Phyl34Day3_Z)
VeDBA_Phyl34Day3 = sqrt(Phyl34Day3$Phyl34Day3_X^2 + Phyl34Day3$Phyl34Day3_Y^2 + Phyl34Day3$Phyl34Day3_Z^2)
Phyl34Day3$VeDBA_Phyl34Day3 <- VeDBA_Phyl34Day3

Phyl34Day4_X <- Phyl34[8550246:10710245, 3]
Phyl34Day4_Y <- Phyl34[8550246:10710245, 4]
Phyl34Day4_Z <- Phyl34[8550246:10710245, 5]
Phyl34Day4_time <- Phyl34[8550246:10710245, 2]

Phyl34Day4 <- data.frame(Phyl34Day4_time, Phyl34Day4_X, Phyl34Day4_Y, Phyl34Day4_Z)
VeDBA_Phyl34Day4 = sqrt(Phyl34Day4$Phyl34Day4_X^2 + Phyl34Day4$Phyl34Day4_Y^2 + Phyl34Day4$Phyl34Day4_Z^2)
Phyl34Day4$VeDBA_Phyl34Day4 <- VeDBA_Phyl34Day4

Phyl34Diary <- data.frame(Phyl34Day1, Phyl34Day2, Phyl34Day3, Phyl34Day4)

{GRAPHING PHYL34}

{DAY 1}
{ACC}
  
P34D1graph <- data.frame(VeDBA_Phyl34Day1)
P34D1graph_1 <- P34D1graph %>% mutate(time = row_number())
spline_P34D1 <- as.data.frame(spline(P34D1graph_1$time, P34D1graph_1$VeDBA_Phyl34Day1))

Bat34sample1 <- ggplot(P34D1graph_1, aes(x=P34D1graph_1$time, y= P34D1graph_1$VeDBA_Phyl34Day1, group = 1)) + 
  geom_line(data = spline_P34D1, aes(x = x, y = y))

Bat34sample1

{HR}
  
Phyl34HRDay1 <- filteredHR[52747:54011, 5]

Phyl34HR1 <- data.frame(Phyl34HRDay1)

Phyl34HR_df1 <- Phyl34HR1 %>% mutate(time = row_number())

spline_P34D1HR <- as.data.frame(spline(Phyl34HR_df1$time, Phyl34HR_df1$Phyl34HRDay1))

Bat34HRsample1 <- ggplot(Phyl34HR_df1, aes(x=Phyl34HR_df1$time, y= Phyl34HR_df1$Phyl34HRDay1, group = 1)) + 
  geom_line(data = spline_P34D1HR, aes(x = x, y = y))

Bat34HRsample1

{DAY 2}
{ACC}
  
P34D2graph <- data.frame(VeDBA_Phyl34Day2)
P34D2graph_1 <- P34D2graph %>% mutate(time = row_number())
spline_P34D2 <- as.data.frame(spline(P34D2graph_1$time, P34D2graph_1$VeDBA_Phyl34Day2))

Bat34sample2 <- ggplot(P34D2graph_1, aes(x=P34D2graph_1$time, y= P34D2graph_1$VeDBA_Phyl34Day2, group = 1)) + 
  geom_line(data = spline_P34D2, aes(x = x, y = y))

Bat34sample2

{HR}
  
Phyl34HRDay2 <- filteredHR[54012:55071, 5]

Phyl34HR2 <- data.frame(Phyl34HRDay2)

Phyl34HR_df2 <- Phyl34HR2 %>% mutate(time = row_number())

spline_P34D2HR <- as.data.frame(spline(Phyl34HR_df2$time, Phyl34HR_df2$Phyl34HRDay2))

Bat34HRsample2 <- ggplot(Phyl34HR_df2, aes(x=Phyl34HR_df2$time, y= Phyl34HR_df2$Phyl34HRDay2, group = 1)) + 
  geom_line(data = spline_P34D2HR, aes(x = x, y = y))

Bat34HRsample2

{DAY3}
{ACC}
  
P34D3graph <- data.frame(VeDBA_Phyl34Day3)
P34D3graph_1 <- P34D3graph %>% mutate(time = row_number())
spline_P34D3 <- as.data.frame(spline(P34D3graph_1$time, P34D3graph_1$VeDBA_Phyl34Day3))

Bat34sample3 <- ggplot(P34D3graph_1, aes(x=P34D3graph_1$time, y= P34D3graph_1$VeDBA_Phyl34Day3, group = 1)) + 
  geom_line(data = spline_P34D3, aes(x = x, y = y))

Bat34sample3

{HR}
  
Phyl34HRDay3 <- filteredHR[55072:56147, 5]

Phyl34HR3 <- data.frame(Phyl34HRDay3)

Phyl34HR_df3 <- Phyl34HR3 %>% mutate(time = row_number())

spline_P34D3HR <- as.data.frame(spline(Phyl34HR_df3$time, Phyl34HR_df3$Phyl34HRDay3))

Bat34HRsample3 <- ggplot(Phyl34HR_df3, aes(x=Phyl34HR_df3$time, y= Phyl34HR_df3$Phyl34HRDay3, group = 1)) + 
  geom_line(data = spline_P34D3HR, aes(x = x, y = y))

Bat34HRsample3

{DAY 4}
{ACC}
  
P34D4graph <- data.frame(VeDBA_Phyl34Day4)
P34D4graph_1 <- P34D4graph %>% mutate(time = row_number())
spline_P34D4 <- as.data.frame(spline(P34D4graph_1$time, P34D4graph_1$VeDBA_Phyl34Day4))

Bat34sample4 <- ggplot(P34D4graph_1, aes(x=P34D4graph_1$time, y= P34D4graph_1$VeDBA_Phyl34Day4, group = 1)) + 
  geom_line(data = spline_P34D4, aes(x = x, y = y))

Bat34sample4

{HR}
  
Phyl34HRDay4 <- filteredHR[56148:57321, 5]

Phyl34HR4 <- data.frame(Phyl34HRDay4)

Phyl34HR_df4 <- Phyl34HR4 %>% mutate(time = row_number())

spline_P34D4HR <- as.data.frame(spline(Phyl34HR_df4$time, Phyl34HR_df4$Phyl34HRDay4))

Bat34HRsample4 <- ggplot(Phyl34HR_df4, aes(x=Phyl34HR_df4$time, y= Phyl34HR_df4$Phyl34HRDay4, group = 1)) + 
  geom_line(data = spline_P34D4HR, aes(x = x, y = y))

Bat34HRsample4

__________
Bat3(PHYL36)
__________
ACC
_____

Phyl36 <- read.delim("C:/Users/userselu/Desktop/Research/Data/TechnoSmArt Europe/Downloads/Phyl36/Phyl36_S3.csv")
View(Phyl36)

Phyl36Day1_X <- Phyl36[2070286:4230285, 3]
Phyl36Day1_Y <- Phyl36[2070286:4230285, 4]
Phyl36Day1_Z <- Phyl36[2070286:4230285, 5]
Phyl36Day1_time <- Phyl36[2070286:4230285, 2]

Phyl36Day1 <- data.frame(Phyl36Day1_time, Phyl36Day1_X, Phyl36Day1_Y, Phyl36Day1_Z)
VeDBA_Phyl36Day1 = sqrt(Phyl36Day1$Phyl36Day1_X^2 + Phyl36Day1$Phyl36Day1_Y^2 + Phyl36Day1$Phyl36Day1_Z^2)
Phyl36Day1$VeDBA_Phyl36Day1 <- VeDBA_Phyl36Day1

Phyl36Day2_X <- Phyl36[4230286:6390285, 3]
Phyl36Day2_Y <- Phyl36[4230286:6390285, 4]
Phyl36Day2_Z <- Phyl36[4230286:6390285, 5]
Phyl36Day2_time <- Phyl36[4230286:6390285, 2]

Phyl36Day2 <- data.frame(Phyl36Day2_time, Phyl36Day2_X, Phyl36Day2_Y, Phyl36Day2_Z)
VeDBA_Phyl36Day2 = sqrt(Phyl36Day2$Phyl36Day2_X^2 + Phyl36Day2$Phyl36Day2_Y^2 + Phyl36Day2$Phyl36Day2_Z^2)
Phyl36Day2$VeDBA_Phyl36Day2 <- VeDBA_Phyl36Day2

Phyl36Day3_X <- Phyl36[6390286:8550285, 3]
Phyl36Day3_Y <- Phyl36[6390286:8550285, 4]
Phyl36Day3_Z <- Phyl36[6390286:8550285, 5]
Phyl36Day3_time <- Phyl36[6390286:8550285, 2]

Phyl36Day3 <- data.frame(Phyl36Day3_time, Phyl36Day3_X, Phyl36Day3_Y, Phyl36Day3_Z)
VeDBA_Phyl36Day3 = sqrt(Phyl36Day3$Phyl36Day3_X^2 + Phyl36Day3$Phyl36Day3_Y^2 + Phyl36Day3$Phyl36Day3_Z^2)
Phyl36Day3$VeDBA_Phyl36Day3 <- VeDBA_Phyl36Day3

Phyl36Day4_X <- Phyl36[8550286:10710285, 3]
Phyl36Day4_Y <- Phyl36[8550286:10710285, 4]
Phyl36Day4_Z <- Phyl36[8550286:10710285, 5]
Phyl36Day4_time <- Phyl36[8550286:10710285, 2]

Phyl36Day4 <- data.frame(Phyl36Day4_time, Phyl36Day4_X, Phyl36Day4_Y, Phyl36Day4_Z)
VeDBA_Phyl36Day4 = sqrt(Phyl36Day4$Phyl36Day4_X^2 + Phyl36Day4$Phyl36Day4_Y^2 + Phyl36Day4$Phyl36Day4_Z^2)
Phyl36Day4$VeDBA_Phyl36Day4 <- VeDBA_Phyl36Day4

Phyl36Diary <- data.frame(Phyl36Day1, Phyl36Day2, Phyl36Day3, Phyl36Day4)

{GRAPHING PHYL36}

{DAY 1}
{ACC}

P36D1graph <- data.frame(VeDBA_Phyl36Day1)
P36D1graph_1 <- P36D1graph %>% mutate(time = row_number())
spline_P36D1 <- as.data.frame(spline(P36D1graph_1$time, P36D1graph_1$VeDBA_Phyl36Day1))

Bat36sample1 <- ggplot(P36D1graph_1, aes(x=P36D1graph_1$time, y= P36D1graph_1$VeDBA_Phyl36Day1, group = 1)) + 
  geom_line(data = spline_P36D1, aes(x = x, y = y))

Bat36sample1

{HR}

Phyl36HRDay1 <- filteredHR[399:1303, 5]

Phyl36HR1 <- data.frame(Phyl36HRDay1)

Phyl36HR_df1 <- Phyl36HR1 %>% mutate(time = row_number())

spline_P36D1HR <- as.data.frame(spline(Phyl36HR_df1$time, Phyl36HR_df1$Phyl36HRDay1))

Bat36HRsample1 <- ggplot(Phyl36HR_df1, aes(x=Phyl36HR_df1$time, y= Phyl36HR_df1$Phyl36HRDay1, group = 1)) + 
  geom_line(data = spline_P36D1HR, aes(x = x, y = y))

Bat36HRsample1

{DAY 2}
{ACC}

P36D2graph <- data.frame(VeDBA_Phyl36Day2)
P36D2graph_1 <- P36D2graph %>% mutate(time = row_number())
spline_P36D2 <- as.data.frame(spline(P36D2graph_1$time, P36D2graph_1$VeDBA_Phyl36Day2))

Bat36sample2 <- ggplot(P36D2graph_1, aes(x=P36D2graph_1$time, y= P36D2graph_1$VeDBA_Phyl36Day2, group = 1)) + 
  geom_line(data = spline_P36D2, aes(x = x, y = y))

Bat36sample2

{HR}

Phyl36HRDay2 <- filteredHR[1304:1886, 5]

Phyl36HR2 <- data.frame(Phyl36HRDay2)

Phyl36HR_df2 <- Phyl36HR2 %>% mutate(time = row_number())

spline_P36D2HR <- as.data.frame(spline(Phyl36HR_df2$time, Phyl36HR_df2$Phyl36HRDay2))

Bat36HRsample2 <- ggplot(Phyl36HR_df2, aes(x=Phyl36HR_df2$time, y= Phyl36HR_df2$Phyl36HRDay2, group = 1)) + 
  geom_line(data = spline_P36D2HR, aes(x = x, y = y))

Bat36HRsample2

{DAY 3}
{ACC}

P36D3graph <- data.frame(VeDBA_Phyl36Day3)
P36D3graph_1 <- P36D3graph %>% mutate(time = row_number())
spline_P36D3 <- as.data.frame(spline(P36D3graph_1$time, P36D3graph_1$VeDBA_Phyl36Day3))

Bat36sample3 <- ggplot(P36D3graph_1, aes(x=P36D3graph_1$time, y= P36D3graph_1$VeDBA_Phyl36Day3, group = 1)) + 
  geom_line(data = spline_P36D3, aes(x = x, y = y))

Bat36sample3

{HR}

Phyl36HRDay3 <- filteredHR[1887:2685, 5]

Phyl36HR3 <- data.frame(Phyl36HRDay3)

Phyl36HR_df3 <- Phyl36HR3 %>% mutate(time = row_number())

spline_P36D3HR <- as.data.frame(spline(Phyl36HR_df3$time, Phyl36HR_df3$Phyl36HRDay3))

Bat36HRsample3 <- ggplot(Phyl36HR_df3, aes(x=Phyl36HR_df3$time, y= Phyl36HR_df3$Phyl36HRDay3, group = 1)) + 
  geom_line(data = spline_P36D3HR, aes(x = x, y = y))

Bat36HRsample3

{DAY 4}
{ACC}

P36D4graph <- data.frame(VeDBA_Phyl36Day4)
P36D4graph_1 <- P36D4graph %>% mutate(time = row_number())
spline_P36D4 <- as.data.frame(spline(P36D4graph_1$time, P36D4graph_1$VeDBA_Phyl36Day4))

Bat36sample4 <- ggplot(P36D4graph_1, aes(x=P36D4graph_1$time, y= P36D4graph_1$VeDBA_Phyl36Day4, group = 1)) + 
  geom_line(data = spline_P36D4, aes(x = x, y = y))

Bat36sample4

{HR}

Phyl36HRDay4 <- filteredHR[2686:3855, 5]

Phyl36HR4 <- data.frame(Phyl36HRDay4)

Phyl36HR_df4 <- Phyl36HR4 %>% mutate(time = row_number())

spline_P36D4HR <- as.data.frame(spline(Phyl36HR_df4$time, Phyl36HR_df4$Phyl36HRDay4))

Bat36HRsample4 <- ggplot(Phyl36HR_df4, aes(x=Phyl36HR_df4$time, y= Phyl36HR_df4$Phyl36HRDay4, group = 1)) + 
  geom_line(data = spline_P36D4HR, aes(x = x, y = y))

Bat36HRsample4

__________
Bat4(PHYL37)
__________
ACC
_____

Phyl37 <- read.delim("C:/Users/userselu/Desktop/Research/Data/TechnoSmArt Europe/Downloads/Phyl37/Phyl37_S3.csv")
View(Phyl37)

Phyl37Day1_X <- Phyl37[2070210:4230209, 3]
Phyl37Day1_Y <- Phyl37[2070210:4230209, 4]
Phyl37Day1_Z <- Phyl37[2070210:4230209, 5]
Phyl37Day1_time <- Phyl37[2070210:4230209, 2]

Phyl37Day1 <- data.frame(Phyl37Day1_time, Phyl37Day1_X, Phyl37Day1_Y, Phyl37Day1_Z)
VeDBA_Phyl37Day1 = sqrt(Phyl37Day1$Phyl37Day1_X^2 + Phyl37Day1$Phyl37Day1_Y^2 + Phyl37Day1$Phyl37Day1_Z^2)
Phyl37Day1$VeDBA_Phyl37Day1 <- VeDBA_Phyl37Day1

Phyl37Day2_X <- Phyl37[4230210:6390209, 3]
Phyl37Day2_Y <- Phyl37[4230210:6390209, 4]
Phyl37Day2_Z <- Phyl37[4230210:6390209, 5]
Phyl37Day2_time <- Phyl37[4230210:6390209, 2]

Phyl37Day2 <- data.frame(Phyl37Day2_time, Phyl37Day2_X, Phyl37Day2_Y, Phyl37Day2_Z)
VeDBA_Phyl37Day2 = sqrt(Phyl37Day2$Phyl37Day2_X^2 + Phyl37Day2$Phyl37Day2_Y^2 + Phyl37Day2$Phyl37Day2_Z^2)
Phyl37Day2$VeDBA_Phyl37Day2 <- VeDBA_Phyl37Day2

Phyl37Day3_X <- Phyl37[6390210:8550209, 3]
Phyl37Day3_Y <- Phyl37[6390210:8550209, 4]
Phyl37Day3_Z <- Phyl37[6390210:8550209, 5]
Phyl37Day3_time <- Phyl37[6390210:8550209, 2]

Phyl37Day3 <- data.frame(Phyl37Day3_time, Phyl37Day3_X, Phyl37Day3_Y, Phyl37Day3_Z)
VeDBA_Phyl37Day3 = sqrt(Phyl37Day3$Phyl37Day3_X^2 + Phyl37Day3$Phyl37Day3_Y^2 + Phyl37Day3$Phyl37Day3_Z^2)
Phyl37Day3$VeDBA_Phyl37Day3 <- VeDBA_Phyl37Day3

Phyl37Day4_X <- Phyl37[8550210:10710209, 3]
Phyl37Day4_Y <- Phyl37[8550210:10710209, 4]
Phyl37Day4_Z <- Phyl37[8550210:10710209, 5]
Phyl37Day4_time <- Phyl37[8550210:10710209, 2]

Phyl37Day4 <- data.frame(Phyl37Day4_time, Phyl37Day4_X, Phyl37Day4_Y, Phyl37Day4_Z)
VeDBA_Phyl37Day4 = sqrt(Phyl37Day4$Phyl37Day4_X^2 + Phyl37Day4$Phyl37Day4_Y^2 + Phyl37Day4$Phyl37Day4_Z^2)
Phyl37Day4$VeDBA_Phyl37Day4 <- VeDBA_Phyl37Day4

Phyl37Diary <- data.frame(Phyl37Day1, Phyl37Day2, Phyl37Day3, Phyl37Day4)

{GRAPHING PHYL37}

{DAY 1}
{ACC}
  
P37D1graph <- data.frame(VeDBA_Phyl37Day1)
P37D1graph_1 <- P37D1graph %>% mutate(time = row_number())
spline_P37D1 <- as.data.frame(spline(P37D1graph_1$time, P37D1graph_1$VeDBA_Phyl37Day1))

Bat37sample1 <- ggplot(P37D1graph_1, aes(x=P37D1graph_1$time, y= P37D1graph_1$VeDBA_Phyl37Day1, group = 1)) + 
  geom_line(data = spline_P37D1, aes(x = x, y = y))

Bat37sample1

{HR}
  
Phyl37HRDay1 <- filteredHR[5631:6807, 5]

Phyl37HR1 <- data.frame(Phyl37HRDay1)

Phyl37HR_df1 <- Phyl37HR1 %>% mutate(time = row_number())

spline_P37D1HR <- as.data.frame(spline(Phyl37HR_df1$time, Phyl37HR_df1$Phyl37HRDay1))

Bat37HRsample1 <- ggplot(Phyl37HR_df1, aes(x=Phyl37HR_df1$time, y= Phyl37HR_df1$Phyl37HRDay1, group = 1)) + 
  geom_line(data = spline_P37D1HR, aes(x = x, y = y))

Bat37HRsample1

{DAY 2}
{ACC}
  
P37D2graph <- data.frame(VeDBA_Phyl37Day2)
P37D2graph_1 <- P37D2graph %>% mutate(time = row_number())
spline_P37D2 <- as.data.frame(spline(P37D2graph_1$time, P37D2graph_1$VeDBA_Phyl37Day2))

Bat37sample2 <- ggplot(P37D2graph_1, aes(x=P37D2graph_1$time, y= P37D2graph_1$VeDBA_Phyl37Day2, group = 1)) + 
  geom_line(data = spline_P37D2, aes(x = x, y = y))

Bat37sample2

{HR}
  
Phyl37HRDay2 <- filteredHR[6808:7899, 5]

Phyl37HR2 <- data.frame(Phyl37HRDay2)

Phyl37HR_df2 <- Phyl37HR2 %>% mutate(time = row_number())

spline_P37D2HR <- as.data.frame(spline(Phyl37HR_df2$time, Phyl37HR_df2$Phyl37HRDay2))

Bat37HRsample2 <- ggplot(Phyl37HR_df2, aes(x=Phyl37HR_df2$time, y= Phyl37HR_df2$Phyl37HRDay2, group = 1)) + 
  geom_line(data = spline_P37D2HR, aes(x = x, y = y))

Bat37HRsample2

{DAY 3}
{ACC}
  
P37D3graph <- data.frame(VeDBA_Phyl37Day3)
P37D3graph_1 <- P37D3graph %>% mutate(time = row_number())
spline_P37D3 <- as.data.frame(spline(P37D3graph_1$time, P37D3graph_1$VeDBA_Phyl37Day3))

Bat37sample3 <- ggplot(P37D3graph_1, aes(x=P37D3graph_1$time, y= P37D3graph_1$VeDBA_Phyl37Day3, group = 1)) + 
  geom_line(data = spline_P37D3, aes(x = x, y = y))

Bat37sample3

{HR}
  
Phyl37HRDay3 <- filteredHR[7900:8922, 5]

Phyl37HR3 <- data.frame(Phyl37HRDay3)

Phyl37HR_df3 <- Phyl37HR3 %>% mutate(time = row_number())

spline_P37D3HR <- as.data.frame(spline(Phyl37HR_df3$time, Phyl37HR_df3$Phyl37HRDay3))

Bat37HRsample3 <- ggplot(Phyl37HR_df3, aes(x=Phyl37HR_df3$time, y= Phyl37HR_df3$Phyl37HRDay3, group = 1)) + 
  geom_line(data = spline_P37D3HR, aes(x = x, y = y))

Bat37HRsample3

{DAY 4}
{ACC}
  
P37D4graph <- data.frame(VeDBA_Phyl37Day4)
P37D4graph_1 <- P37D4graph %>% mutate(time = row_number())
spline_P37D4 <- as.data.frame(spline(P37D4graph_1$time, P37D4graph_1$VeDBA_Phyl37Day4))

Bat37sample4 <- ggplot(P37D4graph_1, aes(x=P37D4graph_1$time, y= P37D4graph_1$VeDBA_Phyl37Day4, group = 1)) + 
  geom_line(data = spline_P37D4, aes(x = x, y = y))

Bat37sample4

{HR}
  
Phyl37HRDay4 <- filteredHR[8923:10066, 5]

Phyl37HR4 <- data.frame(Phyl37HRDay4)

Phyl37HR_df4 <- Phyl37HR4 %>% mutate(time = row_number())

spline_P37D4HR <- as.data.frame(spline(Phyl37HR_df4$time, Phyl37HR_df4$Phyl37HRDay4))

Bat37HRsample4 <- ggplot(Phyl37HR_df4, aes(x=Phyl37HR_df4$time, y= Phyl37HR_df4$Phyl37HRDay4, group = 1)) + 
  geom_line(data = spline_P37D4HR, aes(x = x, y = y))

Bat37HRsample4

_________
Bat5(PHYL38)
_________
ACC
____

Phyl38 <- read.delim("C:/Users/userselu/Desktop/Research/Data/TechnoSmArt Europe/Downloads/Phyl38/Phyl38_S3.csv")
View(Phyl38)

Phyl38Day1_X <- Phyl38[2070286:4230285, 3]
Phyl38Day1_Y <- Phyl38[2070286:4230285, 4]
Phyl38Day1_Z <- Phyl38[2070286:4230285, 5]
Phyl38Day1_time <- Phyl38[2070286:4230285, 2]

Phyl38Day1 <- data.frame(Phyl38Day1_time, Phyl38Day1_X, Phyl38Day1_Y, Phyl38Day1_Z)
VeDBA_Phyl38Day1 = sqrt(Phyl38Day1$Phyl38Day1_X^2 + Phyl38Day1$Phyl38Day1_Y^2 + Phyl38Day1$Phyl38Day1_Z^2)
Phyl38Day1$VeDBA_Phyl38Day1 <- VeDBA_Phyl38Day1

Phyl38Day2_X <- Phyl38[4230286:6390285, 3]
Phyl38Day2_Y <- Phyl38[4230286:6390285, 4]
Phyl38Day2_Z <- Phyl38[4230286:6390285, 5]
Phyl38Day2_time <- Phyl38[4230286:6390285, 2]

Phyl38Day2 <- data.frame(Phyl38Day2_time, Phyl38Day2_X, Phyl38Day2_Y, Phyl38Day2_Z)
VeDBA_Phyl38Day2 = sqrt(Phyl38Day2$Phyl38Day2_X^2 + Phyl38Day2$Phyl38Day2_Y^2 + Phyl38Day2$Phyl38Day2_Z^2)
Phyl38Day2$VeDBA_Phyl38Day2 <- VeDBA_Phyl38Day2

Phyl38Day3_X <- Phyl38[6390286:8550285, 3]
Phyl38Day3_Y <- Phyl38[6390286:8550285, 4]
Phyl38Day3_Z <- Phyl38[6390286:8550285, 5]
Phyl38Day3_time <- Phyl38[6390286:8550285, 2]

Phyl38Day3 <- data.frame(Phyl38Day3_time, Phyl38Day3_X, Phyl38Day3_Y, Phyl38Day3_Z)
VeDBA_Phyl38Day3 = sqrt(Phyl38Day3$Phyl38Day3_X^2 + Phyl38Day3$Phyl38Day3_Y^2 + Phyl38Day3$Phyl38Day3_Z^2)
Phyl38Day3$VeDBA_Phyl38Day3 <- VeDBA_Phyl38Day3

{*Device dies on the 4th day*}
  
Phyl38Diary <- data.frame(Phyl38Day1, Phyl38Day2, Phyl38Day3)

{GRAPHING PHYL38}

{DAY 1}
{ACC}
  
P38D1graph <- data.frame(VeDBA_Phyl38Day1)
P38D1graph_1 <- P38D1graph %>% mutate(time = row_number())
spline_P38D1 <- as.data.frame(spline(P38D1graph_1$time, P38D1graph_1$VeDBA_Phyl38Day1))

Bat38sample1 <- ggplot(P38D1graph_1, aes(x=P38D1graph_1$time, y= P38D1graph_1$VeDBA_Phyl38Day1, group = 1)) + 
  geom_line(data = spline_P38D1, aes(x = x, y = y))

Bat38sample1

{HR}
  
Phyl38HRDay1 <- filteredHR[24848:26140, 5]

Phyl38HR1 <- data.frame(Phyl38HRDay1)

Phyl38HR_df1 <- Phyl38HR1 %>% mutate(time = row_number())

spline_P38D1HR <- as.data.frame(spline(Phyl38HR_df1$time, Phyl38HR_df1$Phyl38HRDay1))

Bat38HRsample1 <- ggplot(Phyl38HR_df1, aes(x=Phyl38HR_df1$time, y= Phyl38HR_df1$Phyl38HRDay1, group = 1)) + 
  geom_line(data = spline_P38D1HR, aes(x = x, y = y))

Bat38HRsample1

{DAY 2}
{ACC}
  
P38D2graph <- data.frame(VeDBA_Phyl38Day2)
P38D2graph_1 <- P38D2graph %>% mutate(time = row_number())
spline_P38D2 <- as.data.frame(spline(P38D2graph_1$time, P38D2graph_1$VeDBA_Phyl38Day2))

Bat38sample2 <- ggplot(P38D2graph_1, aes(x=P38D2graph_1$time, y= P38D2graph_1$VeDBA_Phyl38Day2, group = 1)) + 
  geom_line(data = spline_P38D2, aes(x = x, y = y))

Bat38sample2

{HR}
  
Phyl38HRDay2 <- filteredHR[26141:27434, 5]

Phyl38HR2 <- data.frame(Phyl38HRDay2)

Phyl38HR_df2 <- Phyl38HR2 %>% mutate(time = row_number())

spline_P38D2HR <- as.data.frame(spline(Phyl38HR_df2$time, Phyl38HR_df2$Phyl38HRDay2))

Bat38HRsample2 <- ggplot(Phyl38HR_df2, aes(x=Phyl38HR_df2$time, y= Phyl38HR_df2$Phyl38HRDay2, group = 1)) + 
  geom_line(data = spline_P38D2HR, aes(x = x, y = y))

Bat38HRsample2

{DAY 3}
{ACC}
  
P38D3graph <- data.frame(VeDBA_Phyl38Day3)
P38D3graph_1 <- P38D3graph %>% mutate(time = row_number())
spline_P38D3 <- as.data.frame(spline(P38D3graph_1$time, P38D3graph_1$VeDBA_Phyl38Day3))

Bat38sample3 <- ggplot(P38D3graph_1, aes(x=P38D3graph_1$time, y= P38D3graph_1$VeDBA_Phyl38Day3, group = 1)) + 
  geom_line(data = spline_P38D3, aes(x = x, y = y))

Bat38sample3

{HR}
  
Phyl38HRDay3 <- filteredHR[27435:28697, 5]

Phyl38HR3 <- data.frame(Phyl38HRDay3)

Phyl38HR_df3 <- Phyl38HR3 %>% mutate(time = row_number())

spline_P38D3HR <- as.data.frame(spline(Phyl38HR_df3$time, Phyl38HR_df3$Phyl38HRDay3))

Bat38HRsample3 <- ggplot(Phyl38HR_df3, aes(x=Phyl38HR_df3$time, y= Phyl38HR_df3$Phyl38HRDay3, group = 1)) + 
  geom_line(data = spline_P38D3HR, aes(x = x, y = y))

Bat38HRsample3