### Liam Abbott
### STATS 506, Fall 2015
### Final Project

##### PROCESS AND FILTER DATA ##############################################################################

# clear workspace
rm(list=ls())

# load necessary packages
require("data.table")
require("tidyr")
require("dplyr")

## process Medicare data ##

# read in Medicare data
dt_med = data.table(read.delim("Medicare_Provider_Util_Payment_PUF_CY2013.txt", header=TRUE, sep="\t"))

# select columns of interest
dt_med = dt_med[,.(NPI, 
                   HCPCS_CODE, 
                   PLACE_OF_SERVICE, 
                   NPPES_PROVIDER_STATE,
                   PROVIDER_TYPE,
                   LINE_SRVC_CNT,
                   BENE_UNIQUE_CNT,
                   AVERAGE_MEDICARE_PAYMENT_AMT)]

# rename columns
colnames(dt_med) = c("provider", 
                     "hcpcs_code", 
                     "place_of_service", 
                     "state",
                     "specialty",
                     "line_srvc_cnt",
                     "beneficiary_count",
                     "avg_medicare_payment_amt")

# set key to data table
setkey(dt_med, provider, hcpcs_code, place_of_service)

# restrict to providers in Massachusetts and Tennessee
dt_med = dt_med[state=="MA" | state=="TN",]

# recode states with full names
levels(dt_med$state) = c( levels(dt_med$state), "Massachusetts", "Tennessee" )
dt_med$state[dt_med$state=="MA"] = "Massachusetts"
dt_med$state[dt_med$state=="TN"] = "Tennessee"
dt_med$state = factor(dt_med$state, levels=c("Massachusetts", "Tennessee"))

# create gross Medicare payment amount variable
dt_med$gross_medicare_payment_amt = dt_med[,avg_medicare_payment_amt*line_srvc_cnt]

# aggregate across place of service to provider/code level, 
# calculating gross Medicare payment amount for each state
dt_med_provider_code = dt_med[, .( gross_medicare_amt_prov_code = sum(gross_medicare_payment_amt),
                                   state = state[1],
                                   specialty = specialty[1],
                                   line_srvc_cnt_prov_code = sum(line_srvc_cnt),
                                   beneficiary_count_prov_code = sum(beneficiary_count) ),
                              , by=.( provider, hcpcs_code )]

# write filtered and aggregated Medicare data to text file
write.table(dt_med_provider_code, file="medicare_data_ma_tn.txt", sep="\t")


## process population data ##

# read in population data
df_pop = read.csv("PEP_2013_PEPSYASEX_with_ann.csv", header=T, stringsAsFactors=F)
df_pop = df_pop[-1,]
df_pop = df_pop[,-c(1,2)]

# reformat population data from wide to long format
df_pop = gather_(data=df_pop, 
                 key="cen_sex_age", 
                 value="population", 
                 gather_cols=colnames(df_pop[,2:dim(df_pop)[2]]))

# split key variable created by reformatting into different variables
df_pop = separate(df_pop, 
                  col=cen_sex_age, 
                  into=c("source", "year", "discard1", "sex", "discard2", "age"), 
                  sep=c(4, 8, 11, 12, 16))

# discard newly-created variables that aren't needed
df_pop = df_pop[,-c(4,6)]

# filter out some unnecessary rows
df_pop = df_pop[!df_pop$age=="age",]

# recode columns as factor or numeric variables, as appropriate
df_pop[,1:4] = lapply(df_pop[,1:4], as.factor)
df_pop[,5:6] = lapply(df_pop[,5:6], as.numeric)

# recode "missing" age variables as age 85 and above
df_pop$age[is.na(df_pop$age)] = 85

# rename state variable
names(df_pop)[names(df_pop)=="GEO.display.label"] = "state"

# filter to include only male and female specific rows (not rows accounting for both)
df_pop = df_pop[df_pop$sex=="1" | df_pop$sex=="2",]

# recode levels of sex variable
levels(df_pop$sex) = c( levels(df_pop$sex), "male", "female")
df_pop$sex[df_pop$sex=="1"] = "male"
df_pop$sex[df_pop$sex=="2"] = "female"
df_pop$sex = factor(df_pop$sex, levels=c("male", "female"))

# filter to include only population data for Massachusetts and Tennessee
df_pop = df_pop[df_pop$state=="Massachusetts" | df_pop$state=="Tennessee",]
df_pop$state = factor(df_pop$state, levels=c("Massachusetts", "Tennessee"))

# filter to include only 2013 population data
df_pop = df_pop[df_pop$year==2013,]

# filter out median age rows
df_pop = df_pop[!df_pop$age==999,]

# keep only variables of interest
df_pop = df_pop[,c("state", "age", "population")]

# write filtered and reformatted population data to text file
write.table(df_pop, file="population_data_ma_tn.txt", sep="\t")

# clear workspace
rm(list=ls())


##### ANALYZE DATA #########################################################################################

# clear workspace
rm(list=ls())

# load necessary packages
require("dplyr")
require("ggplot2")
require("scales")

# read in Medicare and population data
df_med = read.delim("medicare_data_ma_tn.txt", header=T, sep="\t")
df_pop = read.delim("population_data_ma_tn.txt", header=T, sep="\t")


### Comparison of Total Payments

# aggregate Medicare payment data to state level, aggregating across code levels
df_med_state = group_by(df_med, state) %>%
  summarise(gross_medicare_amt_state = sum(gross_medicare_amt_prov_code))

# aggregate population data to state level, aggregating across age. also, calculate the 65 and over
# population in each state (Medicare-eligible population)
df_pop_state = group_by(df_pop, state) %>%
  summarise(state_population_total = sum(population),
            state_population_65over = sum(population[age>=65]))

# join Medicare and population data, calculate Medicare payments per eligible adult for each state
df_combine_state = inner_join(df_med_state, df_pop_state, by="state") %>%
  mutate(gross_medicare_per_capita = gross_medicare_amt_state/state_population_65over)

# compare results
print (df_combine_state[,c("state","gross_medicare_per_capita", 
                           "gross_medicare_amt_state", "state_population_65over")])


### Comparison of Payments by Service Type and Specialty

## compare by service type 

# aggregate Medicare data to state, procedure type level (aggregating across providers), also
# calculate proportion of gross Medicare payments accounted for by each service type and cumulative
# proportion of the top n service types
df_med_state_srvc = group_by(df_med, state, hcpcs_code) %>%
  summarise(gross_medicare_amt = sum(gross_medicare_amt_prov_code)) %>%
  arrange(state, desc(gross_medicare_amt)) %>%
  mutate(medicare_payment_pct = gross_medicare_amt/sum(gross_medicare_amt),
         medicare_payment_cum_pct = cumsum(gross_medicare_amt)/sum(gross_medicare_amt))

# extract top service types from each state as those comprising 50% of gross Medicare payments
top50pct = filter(df_med_state_srvc, medicare_payment_cum_pct <= 0.50)
top50pct$hcpcs_code = factor(top50pct$hcpcs_code)
include_codes = levels(top50pct$hcpcs_code)

# new data frame with top 31 service types, calculate per capita code payments
top50pct = inner_join(df_med_state_srvc, df_combine_state[,c("state", "state_population_65over")], by="state") %>%
  filter(hcpcs_code %in% include_codes) %>%
  mutate(medicare_amt_per_capita = gross_medicare_amt/state_population_65over)

# reorder levels of hcpcs code variable, according to per capita Medicare payment values
top50pct$hcpcs_code = factor(top50pct$hcpcs_code)
top50pct$hcpcs_code = factor(top50pct$hcpcs_code,
                             levels = unique(top50pct$hcpcs_code[order(top50pct$medicare_amt_per_capita, decreasing=TRUE)]))

# plot the distribution of per capita Medicare payments across service types, factored by state
ggplot(top50pct, aes(fill=state, x=hcpcs_code, y=medicare_amt_per_capita)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y="Medicare Payment Amount Per Eligible Adult", x="HCPCS Code (Service Type)") +
  scale_fill_manual(values=c("steelblue2", "indianred2")) +
  scale_y_continuous(breaks=seq(0, 180, 10), labels=dollar_format()) +
  theme_bw()


## compare by physician specialty

# aggregate Medicare data to state, specialty level (aggregating across providers), also
# calculate proportion of gross Medicare payments accounted for by each specialty type and cumulative
# proportion of the top n specialties
df_med_state_spec = group_by(df_med, state, specialty) %>%
  summarise(gross_medicare_amt = sum(gross_medicare_amt_prov_code)) %>%
  arrange(state, desc(gross_medicare_amt)) %>%
  mutate(medicare_payment_pct = gross_medicare_amt/sum(gross_medicare_amt),
         medicare_payment_cum_pct = cumsum(gross_medicare_amt)/sum(gross_medicare_amt))

# extract top specialties from each state as those comprising 50% of gross Medicare payments
top50pct = filter(df_med_state_spec, medicare_payment_cum_pct <= 0.50)
top50pct$specialty = factor(top50pct$specialty)
include_specs = levels(top50pct$specialty)

# new data frame with top 9 specialties, calculate per capita code payments
top50pct = inner_join(df_med_state_spec, df_combine_state[,c("state", "state_population_65over")], by="state") %>%
  filter(specialty %in% include_specs) %>%
  mutate(medicare_amt_per_capita = gross_medicare_amt/state_population_65over)

# reorder levels of specialties, according to per capita Medicare payment values
top50pct$specialty = factor(top50pct$specialty)
top50pct$specialty = factor(top50pct$specialty,
                            levels = unique(top50pct$specialty[order(top50pct$medicare_amt_per_capita, decreasing=TRUE)]))

# plot the distribution of per capita Medicare payments across specialties, factored by state
ggplot(top50pct, aes(fill=state, x=specialty, y=medicare_amt_per_capita)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y="Medicare Payment Amount Per Eligible Adult", x="Provider Specialty") +
  scale_fill_manual(values=c("steelblue2", "indianred2")) +
  scale_y_continuous(breaks=seq(0, 280, 25), labels=dollar_format()) +
  theme_bw()


### Comparison of Services Per Beneficiary and Payments Per Beneficiary

## comparison of services per beneficiary

# aggregate to state level (across providers and hcpcs codes), finding gross "line service count" and "beneficiary
# count" per state, then dividing these values to find services per beneficiary 
df_med_srvc_bene = group_by(df_med, state) %>%
  summarise(gross_line_srvc_cnt = sum(line_srvc_cnt_prov_code),
            gross_bene_cnt = sum(beneficiary_count_prov_code)) %>%
  mutate(services_per_beneficiary = gross_line_srvc_cnt/gross_bene_cnt)

# print results
print (df_med_srvc_bene)


## comparison of Medicare payments per beneficiary

# aggregate to state level (across providers and hcpcs codes), finding gross Medicare payment amounts and beneficiary
# counts per state, then dividing these values to find Medicare payments per beneficiary for each state
df_med_payment_bene = group_by(df_med, state) %>%
  summarise(gross_medicare_amt = sum(gross_medicare_amt_prov_code),
            gross_bene_cnt = sum(beneficiary_count_prov_code)) %>%
  mutate(payment_per_beneficiary = gross_medicare_amt/gross_bene_cnt)

# print results
print (df_med_payment_bene)

