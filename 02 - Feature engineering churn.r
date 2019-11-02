#load all the libraries 

library(data.table)
library(tidyverse)
library(lubridate)
library(sqldf)

#setwd("")

#load payment file
payment <- fread("FactPayment_UTS.csv")
#convert to a datetype variable
payment$DonationDateSKey <- ymd(payment$DonationDateSKey)

#load regular giving file
filtered_RG <- fread("regular_giving.csv")
#filter out unnecessary columns
filter_RG <- filtered_RG %>% select(PrimaryConstituentID) %>% distinct()
#select data before 2017
payment_2016 <- payment %>% filter (DonationDateSKey < ymd("20170101"))
#join RG file to payment file
payment_2016_RG <- left_join(filter_RG, payment_2016, 
                             by = c("PrimaryConstituentID" = "DonorConstituentID") )
#rename column
colnames(payment_2016_RG)[1] <- "DonorConstituentID"

#load iteration file
iteration <- fread("DimIteration.csv")

#assign to payment
payment <- payment_2016_RG

#Joining iteration with payments to do event level analyses
pay_events <- payment %>%
  left_join(iteration %>% select(IterationSKey, IterationGroupingName, ProgramName), by = c("IterationSKey"))
#remove the datasets
rm(payment, iteration)

#load instance file
instance <- fread("FactInstance_UTS.csv")

instance_counts <- instance %>%
  group_by(IndConstituentID) %>%
  summarise(instance_count = n_distinct(InstanceDetailSKey)) %>%
  arrange(-instance_count)

rm(instance)

#Distinct events

dist_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(dist_events = n_distinct(ProgramName)) %>%
  arrange(-dist_events) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

#table(dist_events$dist_events) #87% are only one event

#Tenure of constituent

tenure <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2') %>%
  filter(DonationDateSKey != "9999-12-31") %>%
  group_by(DonorConstituentID) %>%
  summarise(tenure_yrs = max(DonationDateSKey) - min(DonationDateSKey)) %>%
  arrange(-tenure_yrs)

tenure$tenure_yrs <- as.numeric(tenure$tenure_yrs)

#nrow(tenure %>% filter(tenure_yrs <= 3))/nrow(tenure) #85% of const. less than 3 yrs

#Join tenure and dist_events

const_base <- sqldf("select a.DonorConstituentID as donor,
                    a.dist_events,
                    b.tenure_yrs as tenure
                    from dist_events a
                    left join tenure b
                    on a.DonorConstituentID = b.DonorConstituentID
                    order by a.DonorConstituentID")

rm(tenure, dist_events)

#Years donated in for constituents

yrs_donated <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(yrs_donated = n_distinct(year(DonationDateSKey))) %>%
  arrange(-yrs_donated) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

#nrow(yrs_donated %>% filter(yrs_donated <= 3))/nrow(yrs_donated) #89%

const_base <- sqldf("select a.*,
                    b.yrs_donated
                    from const_base a
                    left join yrs_donated b
                    on a.donor = b.DonorConstituentID
                    order by a.donor")

rm(yrs_donated)

#Number of credit cards
credit <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  mutate(card = paste(CreditCardExpirationMonth,"-",CreditCardExpirationYear, sep = "")) %>%
  group_by(DonorConstituentID) %>%
  summarise(dist_cards = n_distinct(card)) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

const_base <- sqldf("select a.*,
                    b.dist_cards
                    from const_base a
                    left join credit b
                    on a.donor = b.DonorConstituentID
                    order by a.donor")

rm(credit)
#Hosted events

hosted_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(hosted_events = n()) %>%
  arrange(-hosted_events) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#nrow(hosted_events %>% filter(hosted_events <= 3))/nrow(hosted_events) #65.5%

const_base <- sqldf("select a.*,
                    b.hosted_events
                    from const_base a
                    left join hosted_events b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(hosted_events)

#Distinct instances based on InstanceDetailSKey
dist_instance <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(paid_instances = n_distinct(InstanceDetailSKey)) %>%
  arrange(- paid_instances) %>%
  filter(DonorConstituentID != 'Const-1') %>%
  filter(DonorConstituentID != 'Const-2')

const_base <- sqldf("select a.*,
                    b.paid_instances
                    from const_base a
                    left join dist_instance b
                    on a.donor = DonorConstituentID
                    order by a.donor")

rm(dist_instance)



#Participated events

participated_events <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  summarise(participated_events = n()) %>%
  arrange(-participated_events) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#nrow(participated_events %>% filter(participated_events <= 3))/nrow(participated_events)

const_base <- sqldf("select a.*,
                    b.participated_events
                    from const_base a
                    left join participated_events b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(participated_events)

#Most common payment type

pay_type <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID, PaymentMethodType) %>%
  summarise(pay_type = n()) %>%
  arrange(-pay_type) %>%
  filter(row_number() == 1) %>%
  select(-pay_type) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Join with const_base

const_base <- sqldf("select a.*,
                    b.PaymentMethodType as pay_type
                    from const_base a
                    left join pay_type b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(pay_type)


#Donated in last 3 yrs flag

last_3 <- pay_events %>%
  filter(year(DonationDateSKey) < 2018) %>%
  group_by(DonorConstituentID) %>%
  mutate(last_3 = case_when(max(DonationDateSKey) > 2015 ~ 1,
                            TRUE ~ 0)) %>%
  distinct(DonorConstituentID, last_3) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

#Join with const_base

const_base <- sqldf("select a.*,
                    b.last_3 as last_3
                    from const_base a
                    left join last_3 b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(last_3)

#Ratio of failed to successful payments

fail <- pay_events %>%
  filter(year(DonationDateSKey) < 2018 & PaymentStatus == "Error") %>%
  group_by(DonorConstituentID, PaymentStatus) %>%
  summarise(fail = n()) %>%
  distinct(DonorConstituentID, fail) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

success <- pay_events %>%
  filter(year(DonationDateSKey) < 2018 & PaymentStatus == "Processed") %>%
  group_by(DonorConstituentID, PaymentStatus) %>%
  summarise(success = n()) %>%
  distinct(DonorConstituentID, success) %>%
  rename(ConstituentID = DonorConstituentID) %>%
  filter(ConstituentID != 'Const-1') %>%
  filter(ConstituentID != 'Const-2')

fail_ratio <- success %>%
  left_join(fail, by = c("ConstituentID")) %>%
  mutate(fail_ratio = fail/success) %>%
  distinct(ConstituentID, fail_ratio)

#Join with const_base

const_base <- sqldf("select a.*,
                    b.fail_ratio as fail_ratio
                    from const_base a
                    left join fail_ratio b
                    on a.donor = b.ConstituentID
                    order by a.donor")

rm(fail_ratio, fail, success)

const_base$fail_ratio <- ifelse(is.na(const_base$fail_ratio), 
                                0, const_base$fail_ratio)

#Export const_base
write.table(const_base, "const_base.csv", row.names = F)



#Past donations
past_don <- pay_events %>%
  filter(!is.na(ProgramName)) %>%
  filter(PaymentStatus == "Processed") %>%
  mutate(don_year = year(DonationDateSKey)) %>%
  group_by(DonorConstituentID, ProgramName, don_year) %>%
  summarise(count = n(),
            donated_amt = sum(PaymentAmount))

#Donation by event type
donor_donations_by_event <- pay_events %>%
  filter(PaymentStatus == "Processed") %>%
  group_by(DonorConstituentID, ProgramName) %>%
  summarise(total_amt = sum(PaymentAmount),
            count = n())

donor_total <- donor_donations_by_event %>%
  group_by(DonorConstituentID) %>%
  summarise(total_donated = sum(total_amt),
            total_count = sum(count))

write.table(donor_total, "donor_total.csv", row.names = F)
write.table(past_don, "past_don.csv", row.names = F)
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#Joining tables to get constituent level info in one table

#rm(dist_events, participated_events, tenure, yrs_donated, hosted_events)

const_base <- fread("/home/rstudio-user/const_base.csv")
donor_total <-  fread("/home/rstudio-user/donor_total.csv")
past_don <-  fread("/home/rstudio-user/past_don.csv")

#--------------------------------------------------------------------------------
#For RG 
#--------------------------------------------------------------------------------

rg_base <- sqldf("select a.*,
                 
                 c.total_donated,
                 c.total_count
                 from const_base a
                
                 left join donor_total c
                 on a.donor = c.DonorConstituentID
                 order by a.donor")

# left join donor_donations b
#on a.donor = b.DonorConstituentID
#Join with pay_don table to identify payment in 2018 or not - dependent variable

rg_base <- sqldf("select distinct a.*,
                 b.status
                 from rg_base a
                 left join 
                 (select DonorConstituentID, don_year, count, 1 as status
                 from past_don
                 where ProgramName = \"Regular Giving\"
                 and don_year = 2018
                 and count > 0) b
                 on a.donor = b.DonorConstituentID
                 order by a.donor")

rg_base$status <- ifelse(is.na(rg_base$status), 0, 1)

write.table(rg_base, "rg_base_new.csv", row.names = F)

#---------------------------------------------------------------------------------------------

#Run the below code only if it has not be run before (this code is present in 
#  02 - Feature engineering and ADS creation.R code

# Get ABS data for average income, clean it and join with constituents id

library(rsdmx)
library(stringdist)

# function to find the closest match
ClosestMatch2 <- function(string, stringVector) {
  stringVector[amatch(string, stringVector, maxDist = Inf)]
}

#########################################################################
#To download data from ABS, will take a lot of time
# get income ddata from ABS through SDMX
sdmx <- readSDMX(providerId = "ABS", resource = "data",
                 flowRef = "ABS_C16_T13_LGA", dsd = TRUE)

#convert to a dataframe 
df <- as.data.frame(sdmx, labels = TRUE)

#store the data
write.csv(df, "abs_merged.csv", row.names = F)
#########################################################################
#read ABS income data
demo <- fread("abs_merged.csv")

#select required columns and filter data out for NSW

demo_filtered <- demo %>%
  dplyr::filter(State == "New South Wales") %>%
  dplyr::select(
    "Region",
    "Sex",
    "Age",
    "Total Personal Income - weekly",
    "Total Personal Income - weekly",
    "Value"
  )

# drop () from region name
demo_filtered$Region <-
  gsub("\\s*\\([^\\)]+\\)", "", as.character(demo_filtered$Region))


# drop () from weekly income
demo_filtered$`Total Personal Income - weekly` <-
  gsub(
    "\\s*\\([^\\)]+\\)",
    "",
    as.character(demo_filtered$`Total Personal Income - weekly`)
  )

# remove commas fromm income
demo_filtered$`Total Personal Income - weekly` <-
  gsub(
    "$|,",
    "",
    as.character(demo_filtered$`Total Personal Income - weekly`)
  )

# split income ranges
res <-
  as.data.frame(str_match(demo_filtered$`Total Personal Income - weekly`, "^(.*)-(.*)$")[, -1])

# add income range back to dataset
demo_filtered <- cbind(demo_filtered, res)

# parse numeric values
demo_filtered$V1 <- parse_number(as.character(demo_filtered$V1))
demo_filtered$V2 <- parse_number(as.character(demo_filtered$V2))

# calculate mean of income range
demo_filtered$average_income <-
  (demo_filtered$V1 + demo_filtered$V2) / 2

# remove NAs
demo_filtered <-
  demo_filtered %>% filter(!is.na(average_income) &
                             !Sex == "Persons")

# calculated weighted mean across LGA, sex and age group
tot_average <-
  demo_filtered %>%
  mutate(avg_inc_weekly = round(weighted.mean(average_income, Value)), 2) %>%
  dplyr::select(avg_inc_weekly) %>%
  distinct()

#filter the dataset out
demo_filtered <- demo_filtered %>%
  group_by(Region, Sex) %>%
  mutate(avg_inc_weekly = round(weighted.mean(average_income, Value)), 2) %>%
  dplyr::select("Region", "Sex", "Age", "avg_inc_weekly") %>%
  unique()

#clean gender data
demo_filtered$Sex <- gsub("s", "", demo_filtered$Sex)

#load constituents dataset---------------------------------------
constituents <- fread("C:/uts/ilab1/dimConstituent_UTS.csv")
constituents <- constituents %>% dplyr::select(MasterConstituentId, LGA, Gender)

#clean LGA data in constituents table and assign it to a new column
constituents$LGA_clean <-
  trimws(
    gsub(
      "City|Council|of|The|the|Regional|Municipality|Shire",
      "",
      constituents$LGA
    ),
    which = "both"
  )

#clean LGA data in income table and assign it to a new column
demo_filtered$Region_clean <-
  trimws(
    gsub(
      "City|Council|of|The|the|Regional|Municipality|Shire",
      "",
      demo_filtered$Region
    ),
    which = "both"
  )

#create a new column
constituents$LGA_match <- NA
constituents$LGA_match <- as.character(constituents$LGA_match)

#loop through LGA data in constituents table to find a match in income table
for (i in unique(constituents$LGA_clean)) {
  #skipping some of the missing LGAs
  if (!i %in% c(
    "NULL",
    "Unknown",
    "Bayside",
    "Dubbo",
    "Unincorporated Far West",
    "Lord Howe Island Board"
  )) {
    #print(paste(i, "-", ClosestMatch2(i, demo_filtered$Region_clean)))
    constituents[constituents$LGA_clean == i, ]$LGA_match <-
      ClosestMatch2(i, demo_filtered$Region_clean)
  }
}

#rename the columns
colnames(demo_filtered) <-
  c("LGA", "Gender", "AgeBand", "avg_weekly_income", "LGA_match")


# join datasets constituents and income on LGA name and gender

constituents <-
  constituents %>% left_join(demo_filtered, by = c("LGA_match", "Gender"))

# set avg_income for constituents without matches
constituents$avg_weekly_income <-
  ifelse(
    is.na(constituents$avg_weekly_income),
    as.numeric(tot_average),
    constituents$avg_weekly_income
  )

# dataset for model

avg_income <-
  constituents %>% select (MasterConstituentId, avg_weekly_income)

avg_inc <-
  avg_income %>% distinct(MasterConstituentId, avg_weekly_income)

write.csv(avg_inc, "avg_inc_dis.csv", row.names = F)


