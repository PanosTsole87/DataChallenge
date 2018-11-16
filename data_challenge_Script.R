library(fBasics)

dental = read.csv("perio_study.csv")

#### Find all data classes - assumes data class is uniform for entire col ###########################################

dental_classes = data.frame() # Initiate empty dataframe
NA_locs = data.frame() # Initiate empty dataframe

for (i in 1:ncol(dental))
  dental_classes[2,i] = class(dental[1,i])
  dental_classes[1,] = colnames(dental)

# Equivalent to str()
str(dental)  

# Find the NAs in data
for (i in 1:ncol(dental))
  NA_locs = c(NA_locs, (length(which(is.na(dental[,i])))))

# Equivalent to summary()
summary(dental) # There are 688 NAs in gum_disease and 6 NAs in tp_fluoride

#### Get general stats of the data - For continuous variables ###############################################################
Sum_data = basicStats(dental[,c(2,5,6,8,11,14,15)])[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs"),] # use this method so values can be called later if required


boxplot(dental$age, main = "Age") ### lots of people in their 30s
boxplot(dental[,5], main = "Height")
boxplot(dental[,6], main = "Mins_Brush") ### 13 mins a day???
boxplot(dental[,8], main = "n.fillings")
boxplot(dental[,11], main = "tp_flouride")
boxplot(dental[,14], main = "weight") ### some very fat people are ruining the plot
boxplot(dental[(dental$weight<250),14],main = "weight with outliers deleted") 
boxplot(dental[,15], main = "probing_depth")


length(unique(dental$alcohol)) ### need to spell check
plot(as.factor(dental$gum_disease)) #### need to put NAs as 0
plot(as.factor(dental$mouthwash)) ##20% use MW
plot(as.factor(dental$sex)) ### well balanced
plot(as.factor(dental$smoke)) ### 20% smoke!
plot(as.factor(dental$typ2_diabetes))
plot(as.factor(dental$water_fluor)) ## very low - 10%




length(which(dental$tp_fluoride==0))/length(dental$tp_fluoride) *100 ### 95% of toothpastes have flouride

#### Data Cleaning ##############################################################################################
clean_d = dental # Make a copy of the initial df

## Change gum disease NAs to 0
for (i in 1:length(clean_d$gum_disease))
  if (is.na(clean_d$gum_disease[i])) {clean_d$gum_disease[i] = 0}

plot(as.factor(clean_d$gum_disease))


### Delete outlier weights (10)
basicStats(dental[which(dental$weight>250), c(2,5,6,8,11,14,15)])[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs"),] 
## basic stats show no significant dif to rest of the data

clean_d = clean_d[-which(dental$weight>250),] ###delete the fatties

#### Reduce alcohol co to 0 for non/low and 1 for high
clean_d$alcohol = as.character(clean_d$alcohol)
for (i in 1:length(clean_d$alcohol))
  if (clean_d$alcohol[i] != "light/none" || clean_d$alcohol[i] != "heavy"){
     if (substring(clean_d$alcohol[i],1,1) == "h") {clean_d$alcohol[i] = 1 }
      else{
        clean_d$alcohol[i] = 0
     }}

plot(as.factor(clean_d$alcohol))

### delete  NAs in flouride_level(6)

clean_d = clean_d[-which(is.na(clean_d$tp_fluoride)),]

# Delete 1st column containing row numbers
clean_d$X = NULL

########### Data Exploration ############


mosaicplot(table(clean_d$alcohol, clean_d$mouthwash))
mosaicplot(table(clean_d$alcohol, clean_d$sex))
mosaicplot(table(clean_d$alcohol, clean_d$smoke))
mosaicplot(table(clean_d$alcohol, clean_d$typ2_diabetes))
mosaicplot(table(clean_d$alcohol, clean_d$water_fluor))

mosaicplot(table(clean_d$mouthwash, clean_d$sex))
mosaicplot(table(clean_d$mouthwash, clean_d$smoke))
mosaicplot(table(clean_d$mouthwash, clean_d$typ2_diabetes))
mosaicplot(table(clean_d$mouthwash, clean_d$water_fluor))

mosaicplot(table(clean_d$sex, clean_d$smoke))
mosaicplot(table(clean_d$sex, clean_d$typ2_diabetes))
mosaicplot(table(clean_d$sex, clean_d$water_fluor))

mosaicplot(table(clean_d$smoke, clean_d$typ2_diabetes))
mosaicplot(table(clean_d$smoke, clean_d$water_fluor))

mosaicplot(table(clean_d$typ2_diabetes, clean_d$water_fluor))

# Write clean_d
write.csv(clean_d, "clean_d.csv", row.names = F)
############### Data transformations ####################
# Create a new df
trans_d = clean_d

# BMI - Combine weight-height

trans_d$BMI = trans_d$weight /((trans_d$height/100)^2) 
boxplot(trans_d$BMI)

# Remove height and weight 
trans_d$height = NULL
trans_d$weight = NULL

# Flouride*brush mins

trans_d$Flmins = trans_d$tp_fluoride * trans_d$mins_brushing
boxplot(trans_d$Flmins)

# Remove tp_fluoride and mins_brushing
trans_d$tp_fluoride = NULL
trans_d$mins_brushing = NULL


# Have gum disease or not - Transform dependent variable gum_disease to two-level variable. Now we have a binomial problem
for (i in 1:length(trans_d$gum_disease))
  if (trans_d$gum_disease[i] >0) {trans_d$gumd_binary[i] = 1
  } else{
    trans_d$gumd_binary[i] = 0
  }
plot(as.factor(trans_d$gumd_binary))

# Remove gum_disease
trans_d$gum_disease = NULL

# How many are recorded as having gum disease but have p depth less than 3mm
nrow(trans_d[trans_d$gumd_binary == 1 & trans_d$probing_depth< 3, ]) # 127

# How many are recorded as having gum disease but have p depth less than 3mm
nrow(trans_d[trans_d$gumd_binary == 1 & trans_d$tp_fluoride < 1000, ]) # 0

# Save trans_d
write.csv(trans_d, "trans_d.csv", row.names = F)
