## Tiratelli, Matteo (2024) "Evidence from the transformation of socialist parties in western Europe, 1945-2021", Social Science History.

devtools::install_github("https://github.com/MatteoTiratelli/SparsePanelMatch")
library(SparsePanelMatch)
library(tidyverse)

set.seed(123456789)
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

manifestoR::mp_setapikey("XXXXX") # Users will need to enter their CMP API key location here

#################### Import ParlGov data #########################

cabs <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv")
parties <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv")

cabs <- merge(cabs, parties[names(parties) %in% c("party_id", "cmp", "party_name", "family_name")],
              by = "party_id", all.x = TRUE, suffixes = c('','_ParlGovParties'))

cab_list <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv")
cab_list %>%
  select(country_name, start_date, cabinet_id) %>%
  unique() %>%
  group_by(country_name) %>%
  mutate(end_date = lead(start_date)) %>%
  ungroup() %>%
  select(start_date, end_date, cabinet_id) -> cab_list
cab_list$end_date <- replace_na(cab_list$end_date, as.Date("2021-12-31"))
cab_list$timediff <- difftime(cab_list$end_date, cab_list$start_date,
                              units = "days")

cabs <- merge(cabs, cab_list, by = c('cabinet_id','start_date'), all.x = TRUE)

rm(parties, cab_list)


#################### Import Party Facts data #############################
## Party Facts data set - alternative CMP identifyer

# download and read Party Facts mapping table
file_name <- 'partyfacts-mapping.csv'
if(! file_name %in% list.files())
  download.file("https://partyfacts.herokuapp.com/download/external-parties-csv/", file_name)
partyfacts <- read.csv(file_name, as.is=TRUE)  # maybe conversion of character encoding

# show and select available datasets
cat(paste(unique(partyfacts$dataset_key), collapse='\n'))
dataset_1 <- 'manifesto'
dataset_2 <- 'parlgov'

# merge two datasets selected
first <- partyfacts[partyfacts$dataset_key == dataset_1, ]
second <- partyfacts[partyfacts$dataset_key == dataset_2, ]
merge_table <- merge(first, second, by='partyfacts_id', all=TRUE)

# write results into file with dataset names in file name
file_out <- sprintf('partyfacts-%s-%s.csv', dataset_1, dataset_2)

cabs <- merge(cabs, merge_table[names(merge_table) %in% c("name.y", "dataset_party_id.y", "dataset_party_id.x")],
              by.x = "party_id", by.y = "dataset_party_id.y", all = TRUE, suffixes = c('','_PartyFacts'))

names(cabs)[names(cabs) == "cmp"] <- "cmp_ParlGovParties"
names(cabs)[names(cabs) == "dataset_party_id.x"] <- "cmp_PartyFacts"
names(cabs)[names(cabs) == "name.y"] <- "party_name_PartyFacts"
cabs$cmp_ParlGovParties <- as.character(cabs$cmp_ParlGovParties)

rm(first, merge_table, partyfacts, second, dataset_1, dataset_2, file_name, file_out)


#################### Subset by country #######################
## Subset by membership of "UN regional group: Western European and Others" & Left parties only

cabs <- cabs[!(is.na(cabs$party_id)),]
cabs <- cabs[!(is.na(cabs$country_id)),]

country_codes <- c(59, 64, 21, 67, 43, 54, 41, 56, 37, 26, 7, 72, 8, 9, 63, 27, 35, 40, 44)
# UN regional group: Western European and Others: Andorra (NA), Austria, Belgium, Denmark,
# Finland, France, Germany, Greece, Iceland, Ireland, Italy, Liechtenstein (NA), Luxembourg,
# Malta, Monaco (NA), Netherlands, Norway, Portugal, San Marino (NA), Spain, Sweden,
# Switzerland, United Kingdom
cabs <- cabs[cabs$country_id %in% country_codes,]

#################### Check CMP identifyer ####################
## 

cabs$cmp_ParlGovParties <- replace_na(cabs$cmp_ParlGovParties, "Not found")
cabs$cmp_PartyFacts <- replace_na(cabs$cmp_PartyFacts, "Not found")

cabs$cmp_PartyFacts <- ifelse(cabs$cmp_PartyFacts == "Not found", cabs$cmp_ParlGovParties, cabs$cmp_PartyFacts)
cabs$cmp_ParlGovParties <- ifelse(cabs$cmp_ParlGovParties == "Not found", cabs$cmp_PartyFacts, cabs$cmp_ParlGovParties)

cabs$match <- (cabs$cmp_PartyFacts == cabs$cmp_ParlGovParties)



#################### Download CMP ####################

#install.packages("manifestoR")
cmp <- manifestoR::mp_maindataset(version = "MPDS2021a")

country_codes <- c(11,12,13,14,15,21,22,23,31,32,33,34,35,41,42,43,51,52,53,54)
cmp <- cmp[cmp$country %in% country_codes,]

cabs[cabs$match == FALSE & cabs$family_name %in% c("Social democracy","Communist/Socialist"),] -> notmatching
unique(notmatching$party_name)
unique(notmatching$party_name_PartyFacts)
unique(notmatching$party_name_ParlGovParties)

## Issues:
# 1
# The Flemish Socialist Party in the 2003 and 2007 elections ran as part of a cartel
# with the liberal Spirit Party.
# CMP codes those two elections as a different party (with two entries for them in 2007):
# 21321 for the long running Flemish Socialist Party which exists before and after 2003-7
# 21221 for the two years it ran as part of larger cartel
# => Keep separate but record government presence, don't compare new manifesto to cartel manifesto
cabs$cmp_ParlGovParties[cabs$party_name == "Socialistische Partij Anders / Sociaal-Liberale Partij"] <- "Not found"

# 2
# L'Unione-Prodi was direct hier of the Olive Tree coalition coalition which
# represented the centre-left in the 1996 and 2001 general elections. However,
# The Union also included parties of the radical left, which were not affiliated
# with The Olive Tree. CMP has L'Unione on its own for 2006 election (32955), alongside
# Oliver Tree (32329) - which was a smaller federation of parties which merged
# to form the Democratic Party in October 2007
# => Code both L'Unione and Olive Tree in 2006 as having been in gov, don't compute change
cabs$cmp_ParlGovParties[cabs$party_name == "L'Unione-Prodi"] <- "Not found"

# 3
# CMP divids Sinn Fein in Ireland in 1980s as Workers Party (53220), from nationalist
# party afterwards (53951)
# => keep separate but recode both as Left parties
cmp$parfam[cmp$party == 53951] <- 30

# 4
# CMP separates Die Linke pre/post 2007 election
# => Default to CMP coding as two separate parties
# Never in government
rm(notmatching)


#################### Merge with CMP data, create mean voter scores & subset by party family #############

cmp %>%
  drop_na(pervote) %>%
  group_by(edate) %>%
  summarise(mean_per103 = weighted.mean(per103, w = (pervote/100), na.rm = FALSE),
            mean_per104 = weighted.mean(per104, w = (pervote/100), na.rm = FALSE),
            mean_per105 = weighted.mean(per105, w = (pervote/100), na.rm = FALSE),
            mean_per203 = weighted.mean(per203, w = (pervote/100), na.rm = FALSE),
            mean_per204 = weighted.mean(per204, w = (pervote/100), na.rm = FALSE),
            mean_per414 = weighted.mean(per414, w = (pervote/100), na.rm = FALSE),
            mean_per701 = weighted.mean(per701, w = (pervote/100), na.rm = FALSE),
            mean_rile = weighted.mean(rile, w = (pervote/100), na.rm = FALSE)) -> means
cmp <- merge(cmp, means, by = 'edate', all = TRUE)

cmp$mean_per103 <- abs(cmp$per103 - cmp$mean_per103) # take absolute differences between each value and average voter at that election
cmp$mean_per104 <- abs(cmp$per104 - cmp$mean_per104)
cmp$mean_per105 <- abs(cmp$per105 - cmp$mean_per105)
cmp$mean_per203 <- abs(cmp$per203 - cmp$mean_per203)
cmp$mean_per204 <- abs(cmp$per204 - cmp$mean_per204)
cmp$mean_per414 <- abs(cmp$per414 - cmp$mean_per414)
cmp$mean_per701 <- abs(cmp$per701 - cmp$mean_per701)

cmp <- cmp[cmp$parfam %in% c(20,30),] # subset by 20 LEFT and 30 Social Democratic party family (40,50,60 are Liberal, Christian Democratic and Conservative)
cabs <- cabs[cabs$cabinet_party == 1,] # subset by only parties of government
cabs %>% group_by(cabinet_id) %>%
  mutate(singlepartygov = ifelse(n()==1,1,0)) -> cabs

cabs %>%
  group_by(cmp_ParlGovParties,country_name,election_date) %>%
  summarise(timeingov = sum(as.numeric(timediff)),
            prime_minister = prime_minister,
            singlepartygov = singlepartygov) -> cabs
cabs$cabinet_party <- 1

cmp <- merge(cmp, unique(cabs[,c(1:4,7)]),
             by.x = c('party', 'countryname','edate'),
             by.y = c('cmp_ParlGovParties','country_name','election_date'),
             all.x = TRUE) # merge to create indicator variable in cmp of whether they were in gov or not after that election
cmp$cabinet_party <- replace_na(cmp$cabinet_party, 0)
cmp$timeingov <- replace_na(cmp$timeingov, 0)

cabs_pm <- cabs[cabs$prime_minister == 1,] # subset by only prime ministerial parties
cmp <- merge(cmp, unique(cabs_pm[,c(1:3,5)]),
             by.x = c('party', 'countryname','edate'),
             by.y = c('cmp_ParlGovParties','country_name','election_date'),
             all.x = TRUE) # merge to create indicator variable in cmp of whether they were in gov or not after that election
cmp$prime_minister <- replace_na(cmp$prime_minister, 0)

cabs_single <- unique(cabs[cabs$singlepartygov == 1,]) # subset by only prime ministerial parties
cmp <- merge(cmp, unique(cabs_single[,c(1:3,6)]),
             by.x = c('party', 'countryname','edate'),
             by.y = c('cmp_ParlGovParties','country_name','election_date'),
             all.x = TRUE) # merge to create indicator variable in cmp of whether they were in gov or not after that election
cmp$singlepartygov <- replace_na(cmp$singlepartygov, 0)

cmp %>%
  group_by(party) %>%
  arrange(edate, .by_group = TRUE) %>%
  mutate(wasingov = lag(cabinet_party),
         willgointogov = lead(cabinet_party),
         lag_yearsingov = lag(timeingov)/365,
         prime_minister = lag(prime_minister),
         singlepartygov = lag(singlepartygov)) -> cmp # take lag to indicate if they were in gov in the previous period

cmp %>%
  group_by(party) %>%
  arrange(edate, .by_group = TRUE) %>%
  mutate(days_date_diff = difftime(edate, lag(edate), units = 'days')) -> cmp

cmp$sd_rile <- (cmp$rile - mean(cmp$rile, na.rm = TRUE))/sd(cmp$rile, na.rm = TRUE)

cmp %>%
  group_by(party) %>%
  arrange(edate, .by_group = TRUE) %>%
  mutate(lag_sd_rile = lag(sd_rile)) -> cmp

rm(cabs, means, country_codes, cabs_pm, cabs_single)

#################### Decline in left-party vote shares: Figure 1 ####

cmp %>%
  group_by(edate, countryname) %>%
  summarise(Summed = sum(pervote, na.rm = T)) %>%
  filter(edate > as.Date('1945-01-01') & Summed > 0) %>%
  mutate(group = ifelse(countryname == 'United Kingdom','Labour Party (UK)','Western Europe')) %>%
  ggplot(aes(x = edate, y = Summed)) +
  geom_smooth(method = 'loess', colour = 'black') +
  geom_point() + 
  theme_base() +
  xlab(NULL) + ylab('Vote share') +
  facet_grid(rows = vars(group))

#################### Filter by parties ever in gov ###########

cmp <- cmp[!is.na(cmp$wasingov),]
cmp_all <- cmp
cmp <- filter(cmp, party %in% unique(cmp$party[cmp$wasingov == 1]))

cmp %>%
  group_by(party) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(days_date_diff = difftime(edate, lag(edate), units = 'days')) -> cmp

(mean(as.numeric(cmp$days_date_diff), na.rm = T)/365)*3 # 10.76899 years is average 3 election cycle period

#################### Graph of left governments: Figure 2 ############

# Graph of Social democratic parliamentary strength in western Europe

days <- c('01','02','03','04','05','06','07','08','09',10:31)
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
yearmonths_country <- tibble(date = character())

for (i in 1901:2021) {
  for (x in 1:12) {
    for (y in 1:31) {
      temp <- tibble(date = paste0(i,'-',months[x],'-',days[y]))
      yearmonths_country <- bind_rows(yearmonths_country, as.tibble(temp)) 
    }
  }
}
rm(temp,i,x,y)

cab_party_share <- read_csv("https://raw.githubusercontent.com/hdigital/parlgov-snippets/master/cabinet-party-family/cabinet-party-family.csv")

cab_party_share %>%
  group_by(country_name) %>%
  mutate(end_date = lead(start_date)) -> cab_party_share
cab_party_share$end_date <- replace_na(cab_party_share$end_date, "2021-12-31")
cab_party_share$end_date <- as.character(cab_party_share$end_date)
cab_party_share$start_date <- as.character(cab_party_share$start_date)

cab_party_share <- cab_party_share[cab_party_share$soc > 0 | cab_party_share$com > 0,]
cab_party_share$weight <- cab_party_share$com + cab_party_share$soc

countries <- unique(cab_party_share$country_name_short)
for (i in 1:length(countries)) {yearmonths_country[, paste0(countries[[i]])] <- ifelse(yearmonths_country$date %in% cab_party_share$start_date[cab_party_share$country_name_short == countries[i]], "start",
                                                         ifelse(lead(yearmonths_country$date) %in% cab_party_share$end_date[cab_party_share$country_name_short == countries[i]], "end", NA))
}

for (i in 2:length(yearmonths_country)) {
  names(yearmonths_country)[i] <- unique(cab_party_share$country_name_short[cab_party_share$country_name_short == names(yearmonths_country)[i]])
}

for (i in 2:length(yearmonths_country)) {
  start_ind <- which(yearmonths_country[,i] == "start")
  end_ind <- which(yearmonths_country[,i] == "end")
  for (x in 1:length(start_ind)) {
    score <- cab_party_share$weight[cab_party_share$country_name_short == names(yearmonths_country[,i]) & cab_party_share$start_date == yearmonths_country[[start_ind[[x]],1]]]
    yearmonths_country[start_ind[[x]]:end_ind[[x]],i] <- as.character(score)
  }
}

yearmonths_country[is.na(yearmonths_country)] <- '0'

yearmonths_country %>%
  pivot_longer(-date, names_to = 'Countries', values_to = 'Value') %>%
  filter(Countries %in% c('AUT', 'BEL','CHE','CZE','DEU','DNK','ESP','FIN','FRA','GBR',
                          'GRC','IRL','ISL','ITA','LUX','NLD','NOR','PRT','SWE')) %>%
  mutate(year = as.numeric(substr(date,1,4))) %>%
  group_by(Countries, year) %>%
  summarise(Value = sum(as.numeric(Value))) -> graphdata

graphdata$Countries <- factor(graphdata$Countries, levels = c('DNK','NOR','SWE','FIN','ISL',
                                                              'AUT','DEU','CZE',
                                                              'NLD','BEL','LUX','CHE','FRA',
                                                              'ITA','PRT','ESP','GRC',
                                                              'GBR','IRL'))

ggplot(graphdata, aes(x = year, y = Countries, fill = Value)) + 
  geom_tile(color = "transparent") + 
  scale_fill_gradient(low = "white", high = "darkgrey") +
  xlab(NULL) + ylab(NULL) + 
  theme_base() +
  scale_x_continuous(limits = c(1910, 2015)) +
  scale_y_discrete(limits = rev(levels(graphdata$Countries))) +
  theme(legend.position = "none") 

rm(cab_party_share, graphdata, yearmonths_country, countries, days, end_ind, i, months, score, start_ind, x, y)


#################### Descriptives: Figure 3 ############

cmp %>%
  ungroup() %>%
  select(per104, per105, 
         per203, per204,
         per414,
         per701,
         lag_yearsingov,
         pervote,
         rile, wasingov) %>%
  pivot_longer(!wasingov) %>%
  drop_na(name) -> plotdata
plotdata$name <- gsub('per104', 'MilitPos', plotdata$name)
plotdata$name <- gsub('per105', 'MilitNeg', plotdata$name)
plotdata$name <- gsub('per203', 'ConstPos', plotdata$name)
plotdata$name <- gsub('per204', 'ConstNeg', plotdata$name)
plotdata$name <- gsub('per414', 'EconOrth', plotdata$name)
plotdata$name <- gsub('per701', 'Labour', plotdata$name)
plotdata$name <- gsub('lag_yearsingov', 'YearsinGov', plotdata$name)
plotdata$name <- gsub('pervote', 'PercentVote', plotdata$name)
plotdata$name <- gsub('rile', 'LeftRight', plotdata$name)
plotdata$name <- factor(plotdata$name, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg',
                                                  'EconOrth','Labour','YearsinGov','PercentVote','LeftRight'))
plotdata$wasingov <- recode(plotdata$wasingov, '0' = "Not in government", '1' = "In government")
plotdata$wasingov <- factor(plotdata$wasingov, levels = c("In government", "Not in government"))

plotdata %>%
  drop_na(wasingov) %>%
  ggplot(aes(x = value, colour = factor(wasingov))) + geom_density() + 
  xlab(NULL) + ylab(NULL) + scale_color_viridis_d() +
  theme_base() +
  facet_wrap(~ name,  scales = 'free', ncol = 3, strip.position = "top") +
  theme(panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        legend.position = 'bottom',
        panel.border = element_blank(),
        axis.ticks = element_blank())

rm(plotdata)

#################### Set variables for analysis #########

# Dependent variables

cmp$pervote <- scale(cmp$pervote)[,1]
cmp$per103 <- scale(cmp$per103)[,1]
cmp$per104 <- scale(cmp$per104)[,1]
cmp$per105 <- scale(cmp$per105)[,1]
cmp$per203 <- scale(cmp$per203)[,1]
cmp$per204 <- scale(cmp$per204)[,1]
cmp$per414 <- scale(cmp$per414)[,1]
cmp$per701 <- scale(cmp$per701)[,1]

cmp %>%
  group_by(party) %>%
  arrange(date, .by_group = TRUE) %>%
  select(party, partyname, date, edate, countryname, # Logic is that novel experience of governance will push you towards the normal way of doing things: i.e. traditional foreign & domestic policy, support for constitution, traditional party role (back away from union connection)
         per103, per104, per105, # anti-imperialism, military_pos, military_neg
         per203, per204, # constitutionalism_pos, constitutionalism_neg
         per414, # economic orthodoxy
         per701, # labour groups_pos [no labour_neg because it is almost entirely 0s, only two left parties ever make negative mentions of labour groups]
         wasingov, lag_yearsingov, days_date_diff, willgointogov,
         pervote, lag_sd_rile, rile, prime_minister, singlepartygov,
         mean_rile, mean_per103, mean_per104, mean_per105,
         mean_per203, mean_per204, mean_per414, mean_per701) -> cmp

cmp %>%
  group_by(party) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(days_date_diff = as.numeric(days_date_diff)) %>%
  mutate(days_outofgov = ifelse(wasingov == 0, NA,
                                ifelse(wasingov == 1 & lag(wasingov) == 0, lag(days_date_diff),
                                       ifelse(wasingov == 1 & lag(wasingov) == 0 & lag(wasingov, n = 2) == 0, sum(lag(days_date_diff), lag(days_date_diff, n = 2)),
                                              ifelse(wasingov == 1 & lag(wasingov) == 0 & lag(wasingov, n = 2) == 0 & lag(wasingov, n = 3) == 0, sum(lag(days_date_diff), lag(days_date_diff, n = 2), lag(days_date_diff, n = 3)),
                                                     ifelse(wasingov == 1 & lag(wasingov) == 0 & lag(wasingov, n = 2) == 0 & lag(wasingov, n = 3) == 0 & lag(wasingov, n = 4) == 0, sum(lag(days_date_diff), lag(days_date_diff, n = 2), lag(days_date_diff, n = 3), lag(days_date_diff, n = 4)),
                                                            ifelse(wasingov == 1 & lag(wasingov) == 0 & lag(wasingov, n = 2) == 0 & lag(wasingov, n = 3) == 0 & lag(wasingov, n = 4) == 0 & lag(wasingov, n = 5) == 0, sum(lag(days_date_diff), lag(days_date_diff, n = 2), lag(days_date_diff, n = 3), lag(days_date_diff, n = 4), lag(days_date_diff, n = 5)),
                                                                   NA))))))) -> cmp

cmp$days_outofgov[cmp$party == 12221 & cmp$date == 200909] <- as.numeric(difftime(cmp$edate[cmp$party == 12221 & cmp$date == 200909], cmp$edate[cmp$party == 12221 & cmp$date == 196509], units = 'days'))
cmp$days_outofgov[cmp$party == 13230 & cmp$date == 201506] <- as.numeric(difftime(cmp$edate[cmp$party == 13230 & cmp$date == 201506], cmp$edate[cmp$party == 13230 & cmp$date == 196409], units = 'days'))
cmp$days_outofgov[cmp$party == 14221 & cmp$date == 197003] <- as.numeric(difftime(cmp$edate[cmp$party == 14221 & cmp$date == 197003], cmp$edate[cmp$party == 14221 & cmp$date == 194807], units = 'days'))
cmp$days_outofgov[cmp$party == 31320 & cmp$date == 198806] <- as.numeric(difftime(cmp$edate[cmp$party == 31320 & cmp$date == 198806], cmp$edate[cmp$party == 31320 & cmp$date == 195811], units = 'days'))
cmp$days_outofgov[cmp$party == 32220 & cmp$date == 200105] <- as.numeric(difftime(cmp$edate[cmp$party == 32220 & cmp$date == 200105], cmp$edate[cmp$party == 32220 & cmp$date == 194804], units = 'days'))

cmp$date <- paste0(substr(cmp$date,1,4),'-',substr(cmp$date,5,6),'-01')
cmp$date <- as.Date(cmp$date)

#################### Matching ##########

dvs <- c('per104', 'per105',
         'per203', 'per204',
         'per414',
         'per701')

listmethods <- c('none',"CBPS.weight", "CBPS.match", "ps.weight", "ps.match",
                 "mahalanobis")

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets <- lapply(dvs, matching) # This is one takes time
MatchedSets <- flatten(MatchedSets)

#################### Figure 4 ##########

df1 <- data.table::setDT(MatchedSets[[26]]$summary)
plotdata <- data.frame(Outcome = c(weighted.mean(df1$lag_outcome[df1$treatment == 1], 
                                                 w = df1$weight[df1$treatment == 1]), 
                                   weighted.mean(df1$lag_outcome[df1$treatment == 0], 
                                                 w = df1$weight[df1$treatment == 0]), 
                                   weighted.mean(df1$outcome[df1$treatment == 1], 
                                                 w = df1$weight[df1$treatment == 1]), 
                                   weighted.mean(df1$outcome[df1$treatment == 0], 
                                                 w = df1$weight[df1$treatment == 0])))
plotdata$Time <- c("Pre", "Pre", "Post", "Post")
plotdata$Time <- factor(plotdata$Time, levels = c("Pre", "Post"))
plotdata$Treated <- c("Recently in government (treated)", "Not in government (control)","Recently in government (treated)", "Not in government (control)")
ggplot2::ggplot(plotdata, aes(x = Time, y = Outcome, colour = Treated)) + 
  geom_point(position = position_dodge(width = 0.05)) + 
  ylab("EconOrth") + xlab(NULL) + 
  scale_colour_viridis_d() +
  geom_line(aes(group = Treated), position = position_dodge(width = 0.05)) + 
  theme_bw() + theme(axis.line = element_blank(), panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), plot.title.position = "plot", 
                     plot.title = element_text(hjust = 0.5), panel.background = element_blank(), 
                     plot.background = element_blank(), legend.background = element_blank(), 
                     legend.box.background = element_blank(), legend.title = element_blank(), 
                     legend.position = "bottom", strip.background = element_blank())


#################### Main analysis: Figure 5 ##########

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output <- lapply(MatchedSets, estimation) # This one runs slow

results <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output)) {
    temp <- tibble(dv = output[[i]]$outcome, refinement = output[[i]]$refinement_method, 
                   coef = output[[i]]$summary$coefs,
                   low = output[[i]]$summary$bootstrap_low, high = output[[i]]$summary$bootstrap_high)
    results <- bind_rows(results, temp)
}

plot_function <- function (data, title, caption) {
  results <- data
  results <- na.omit(results)
  results$refinement <- gsub('CBPS.match', 'cbps.match', results$refinement)
  results$refinement <- gsub('CBPS.weight', 'cbps.weight', results$refinement)
  results$refinement <- gsub('none', 'exact only', results$refinement)
  results$refinement <- factor(results$refinement, levels = c('mahalanobis','ps.match','ps.weight','cbps.match','cbps.weight','exact only'))
  results$dv <- gsub('per104', 'MilitPos', results$dv)
  results$dv <- gsub('per105', 'MilitNeg', results$dv)
  results$dv <- gsub('per203', 'ConstPos', results$dv)
  results$dv <- gsub('per204', 'ConstNeg', results$dv)
  results$dv <- gsub('per414', 'EconOrth', results$dv)
  results$dv <- gsub('per701', 'Labour', results$dv)
  results$dv <- factor(results$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))
  
  ggplot(results, aes(x = coef, y = as.factor(refinement))) +
    geom_pointrange(aes(xmin = low, xmax = high), colour = 'darkgrey') +
    geom_pointrange(data = results[(results$low<0 & results$high<0) | (results$low>0 & results$high>0),], aes(xmin = low, xmax = high), colour = 'black') +
    xlab(NULL) + ylab(NULL) + 
    facet_grid(rows = vars(dv), scales = 'free_y', space = 'free_y') +
    geom_vline(xintercept = 0) +
    scale_colour_viridis_d() + labs(title = title,
                                    caption = caption) +
    theme_base() +
    theme(legend.position = 'none', axis.ticks = element_blank()) -> p1
  return(p1)
}

plot_function(data = results,
              title = NULL,
              caption = NULL) # Figure 5


#################### Variation in effect sizes over time: Figure 6 ##########

output <- tibble(coef = numeric(), group = character(), year = numeric(), dv = character())
for(i in c(1,13,25,31)){
  df1 <- MatchedSets[[i]]$summary
  dv <- MatchedSets[[i]]$outcome
  df1$ideological_change <- (df1$outcome - df1$lag_outcome)
  df1$weight[df1$treatment == 0] <- df1$weight[df1$treatment == 0]*-1
  groups <- unique(df1$group)
  did <- function(x) {
    df1 %>%
      filter(group == x) %>%
      summarise(coef = sum(ideological_change*weight)) %>%
      mutate(group = x,
             year = as.numeric(substr(x,7,10)),
             dv = dv) %>%
      return()
  }
  testres <- lapply(groups, did)
  testres %>% map_dfr(as_tibble, .name_repair = "universal") -> coefs
  output <- bind_rows(output, coefs)
}
output$dv <- gsub('per104', 'MilitPos', output$dv)
output$dv <- gsub('per105', 'MilitNeg', output$dv)
output$dv <- gsub('per203', 'ConstPos', output$dv)
output$dv <- gsub('per204', 'ConstNeg', output$dv)
output$dv <- gsub('per414', 'EconOrth', output$dv)
output$dv <- gsub('per701', 'Labour', output$dv)
output$dv <- factor(output$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))

ggplot(output, aes(x = year, y = coef)) +
  geom_smooth(method = 'lm', colour = 'black', size = 0.8, alpha = 1) +
  geom_point(size = 0.8, colour = 'black') +
  scale_x_continuous(breaks = c(1960,1980,2000,2020)) +
  xlab(NULL) + ylab('Standardised difference-in-difference coefficient') + 
  facet_wrap(facet = vars(dv), ncol = 1, strip.position = "right", scales = "free_y") +
  theme_base() +
  theme(legend.position = 'none', axis.ticks = element_blank()) # Figure 6

output %>%
  group_by(dv) %>%
  do(model = lm(coef ~ as.numeric((.$year-1945)), data = .)) -> fitted_models
for (i in 1:6) {
  print(fitted_models$dv[i])
  print(summary(fitted_models$model[[i]]))
}

rm(coefs, df1, output, results, temp, testres, dv, groups, i, fitted_models, did)


#################### A1: Table of parties used in analysis ##########

cmp %>%
  filter(wasingov == 1) %>%
  distinct(partyname, .keep_all = TRUE) %>%
  select(countryname, partyname) # Table A1

#################### A3 Robustness: OLS binary ##########

estimation <- function (x) {
  data <- x
  data$summary$ideological_change <- (data$summary$outcome - data$summary$lag_outcome)
  estimates <- estimatr::lm_robust(ideological_change ~ factor(treatment) + control1 + control2 + factor(countryname),
                                   clusters = group, weights = weight,
                                   data = data$summary)
  estimates$refinement_method <- x$refinement_method
  estimates$outcome <- x$outcome
  return(estimates)
}
OLSoutput <- lapply(MatchedSets, estimation)

OLSresults <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(OLSoutput)) {
    temp <- tibble(dv = OLSoutput[[i]]$outcome, refinement = OLSoutput[[i]]$refinement_method, 
                   coef = OLSoutput[[i]]$coefficients[['factor(treatment)1']],
                   low = OLSoutput[[i]]$conf.low[['factor(treatment)1']], high = OLSoutput[[i]]$conf.high[['factor(treatment)1']])
    OLSresults <- bind_rows(OLSresults, temp)
}

plot_function(data = OLSresults,
              title = "A3: Matched OLS estimator with binary treatment",
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis except, instead of using a difference-in-difference estimator, coefficients are from an OLS regression with country-level fixed effects, vote share and (lagged and standardised) right-left position as controls, with robust standard errors (clustered on each matched set).",100))

#################### A4 Robustness: OLS continuous ###############

estimation <- function (x) {
  data <- x
  data$summary$ideological_change <- (data$summary$outcome - data$summary$lag_outcome)
  estimates <- estimatr::lm_robust(ideological_change ~ lag_yearsingov + control1 + control2  + factor(countryname),
                                   clusters = group, weights = weight,
                                   data = data$summary)
  estimates$refinement_method <- x$refinement_method
  estimates$outcome <- x$outcome
  return(estimates)
}
OLSoutput <- lapply(MatchedSets, estimation)

OLSresults <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(OLSoutput)) {
  temp <- tibble(dv = OLSoutput[[i]]$outcome, refinement = OLSoutput[[i]]$refinement_method, 
                 coef = OLSoutput[[i]]$coefficients[['lag_yearsingov']],
                 low = OLSoutput[[i]]$conf.low[['lag_yearsingov']], high = OLSoutput[[i]]$conf.high[['lag_yearsingov']])
  OLSresults <- bind_rows(OLSresults, temp)
}

plot_function(data = OLSresults,
              title = "A4: Matched OLS estimator with continuous treatment",
              caption = wrapper("Notes: Analysis as in Appendix 3 above except the treatment variable is continuous (the number of years the party has been in government since the last election).",100))

#################### A5 Robustness: 2 election cycles ###############

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 2, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_2 <- lapply(dvs, matching)
MatchedSets_2 <- flatten(MatchedSets_2)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output2 <- lapply(MatchedSets_2, estimation)

results2 <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output2)) {
  temp <- tibble(dv = output2[[i]]$outcome, refinement = output2[[i]]$refinement_method, 
                 coef = output2[[i]]$summary$coefs,
                 low = output2[[i]]$summary$bootstrap_low, high = output2[[i]]$summary$bootstrap_high)
  results2 <- bind_rows(results2, temp)
}

plot_function(data = results2,
              title = "A5: DiD estimator matched on two treatment lags",
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis except the exact matching is done using only two treatment lags. The effective sample is n = 450 with 78 matched sets.",100))

#################### A6 Robustness: 4 election cycles ###############

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 4, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_4 <- lapply(dvs, matching)
MatchedSets_4 <- flatten(MatchedSets_4)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output4 <- lapply(MatchedSets_4, estimation)

results4 <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output4)) {
  temp <- tibble(dv = output4[[i]]$outcome, refinement = output4[[i]]$refinement_method, 
                 coef = output4[[i]]$summary$coefs,
                 low = output4[[i]]$summary$bootstrap_low, high = output4[[i]]$summary$bootstrap_high)
  results4 <- bind_rows(results4, temp)
}

plot_function(data = results4,
              title = "A6: DiD estimator matched on four treatment lags",
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis except the exact matching is done using four treatment lags. The effective sample is n = 138 with 38 matched sets.",100))


#################### A7: 10 year window ###############

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 120, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_10 <- lapply(dvs, matching)
MatchedSets_10 <- flatten(MatchedSets_10)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output_10 <- lapply(MatchedSets_10, estimation)

results_10 <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output_10)) {
  temp <- tibble(dv = output_10[[i]]$outcome, refinement = output_10[[i]]$refinement_method, 
                 coef = output_10[[i]]$summary$coefs,
                 low = output_10[[i]]$summary$bootstrap_low, high = output_10[[i]]$summary$bootstrap_high)
  results_10 <- bind_rows(results_10, temp)
}

plot_function(data = results_10,
              title = 'A7: DiD estimator with 10 year time window',
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis except the time window is extended to ten years. The effective sample is n = 388 with 69 matched sets.",100))


#################### A8: All left parties ##########

cmp_all$date <- paste0(substr(cmp_all$date,1,4),'-',substr(cmp_all$date,5,6),'-01')
cmp_all$date <- as.Date(cmp_all$date)
cmp_all %>% drop_na(wasingov) -> cmp_all

cmp_all$pervote <- scale(cmp_all$pervote)[,1]
cmp_all$per103 <- scale(cmp_all$per103)[,1]
cmp_all$per104 <- scale(cmp_all$per104)[,1]
cmp_all$per105 <- scale(cmp_all$per105)[,1]
cmp_all$per203 <- scale(cmp_all$per203)[,1]
cmp_all$per204 <- scale(cmp_all$per204)[,1]
cmp_all$per414 <- scale(cmp_all$per414)[,1]
cmp_all$per701 <- scale(cmp_all$per701)[,1]

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp_all, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_all <- lapply(dvs, matching)
MatchedSets_all <- flatten(MatchedSets_all)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output_all <- lapply(MatchedSets_all, estimation)

results_all <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output_all)) {
  temp <- tibble(dv = output_all[[i]]$outcome, refinement = output_all[[i]]$refinement_method, 
                 coef = output_all[[i]]$summary$coefs,
                 low = output_all[[i]]$summary$bootstrap_low, high = output_all[[i]]$summary$bootstrap_high)
  results_all <- bind_rows(results_all, temp)
}

plot_function(data = results_all,
              title = 'A8: DiD estimator for all left wing parties',
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis, but includes all left wing parties in western Europe (79 parties, and 678 party-election observations) The effective sample after matching is n = 388 with 65 matched sets.",100))

#################### A9 Testing CMP assumptions: absdiff matching ###############

absdiffs <- c('mean_per104', 'mean_per105','mean_per203', 'mean_per204', 'mean_per414', 'mean_per701')

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_placDV <- lapply(absdiffs, matching)
MatchedSets_placDV <- flatten(MatchedSets_placDV)

estimation <- function (x) {
  df1 <- data.table::as.data.table(x$summary)
  df1 <- df1[treatment == 0, `:=`(weight, (weight * -1))]
  Estimate <- tibble(coefs = sum(df1$outcome * df1$weight)/length(unique(df1$group)))
  Estimate$lead <- "t+0"
  cat("Coefficients calculated. Beginning bootstrapping\n")
  boots <- matrix(NA, nrow = 1000, ncol = 1)
  for (k in 1:1000) {
    clusters <- unique(df1[["unit"]])
    units <- sample(clusters, size = length(clusters), replace = T)
    df.bs <- lapply(units, function(x) which(df1[, "unit"] == x))
    d.sub1 <- df1[unlist(df.bs), ]
    boots[k, 1] <- sum((d.sub1$outcome)*d.sub1$weight)/length(unique(d.sub1$group))
  }
  Estimate$bootstrap_coefs <- apply(boots, 2, function(x) mean(x))
  Estimate$bootstrap_sd <- apply(boots, 2, function(x) sd(x))
  Estimate$bootstrap_low <- apply(boots, 2, quantile, probs = 0.05/2)
  Estimate$bootstrap_high <- apply(boots, 2, quantile, probs = (1 - (0.05/2)))
  output <- list(summary = Estimate, qoi = x$qoi, covs = x$covs, 
                 treatment_lags = x$treatment_lags, outcome_leads = x$outcome_leads, 
                 refinement_method = x$refinement_method, time_window_in_months = x$time_window_in_months, 
                 outcome = x$outcome, n_iterations = 1000, 
                 alpha = 0.05)
  class(output) <- "SparsePanelEstimate"
  return(output)
}
outputplacdv <- lapply(MatchedSets_placDV, estimation)

resultsplacdv <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(outputplacdv)) {
  temp <- tibble(dv = outputplacdv[[i]]$outcome, refinement = outputplacdv[[i]]$refinement_method, 
                 coef = outputplacdv[[i]]$summary$coefs,
                 low = outputplacdv[[i]]$summary$bootstrap_low, high = outputplacdv[[i]]$summary$bootstrap_high)
  resultsplacdv <- bind_rows(resultsplacdv, temp)
}

resultsplacdv <- na.omit(resultsplacdv)
resultsplacdv$refinement <- gsub('CBPS.match', 'cbps.match', resultsplacdv$refinement)
resultsplacdv$refinement <- gsub('CBPS.weight', 'cbps.weight', resultsplacdv$refinement)
resultsplacdv$refinement <- gsub('none', 'exact only', resultsplacdv$refinement)
resultsplacdv$refinement <- factor(resultsplacdv$refinement, levels = c('mahalanobis','ps.match','ps.weight','cbps.match','cbps.weight','exact only'))
resultsplacdv$dv <- gsub('mean_per104', 'MilitPos', resultsplacdv$dv)
resultsplacdv$dv <- gsub('mean_per105', 'MilitNeg', resultsplacdv$dv)
resultsplacdv$dv <- gsub('mean_per203', 'ConstPos', resultsplacdv$dv)
resultsplacdv$dv <- gsub('mean_per204', 'ConstNeg', resultsplacdv$dv)
resultsplacdv$dv <- gsub('mean_per414', 'EconOrth', resultsplacdv$dv)
resultsplacdv$dv <- gsub('mean_per701', 'Labour', resultsplacdv$dv)
resultsplacdv$dv <- factor(resultsplacdv$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))


ggplot(resultsplacdv, aes(x = coef, y = as.factor(refinement), colour = as.factor(refinement))) +
  geom_pointrange(aes(xmin = low, xmax = high), alpha = 0.3) +
  geom_pointrange(data = resultsplacdv[(resultsplacdv$low<0 & resultsplacdv$high<0) | (resultsplacdv$low>0 & resultsplacdv$high>0),], aes(xmin = low, xmax = high), alpha = 1) +
  xlab(NULL) + ylab(NULL) + 
  facet_grid(rows = vars(dv), scales = 'free_y', space = 'free_y') +
  geom_vline(xintercept = 0) + MyThemes::theme_base() +
  scale_colour_viridis_d() + labs(title = "A9: Differences between parties and the average voter (matched)",
                                  caption = wrapper("Notes: Analysis as in Figure 5 in main analysis except the outcome variable is the absolute difference between the party's score and the average voter (proxied by the mean position of parties in that election weighted by their vote share).", 100)) +
  theme(legend.position = 'none', axis.ticks = element_blank())


#################### A10: absdiff full sample ###############

estimation <- function(x) {
  df1 <- data.table::as.data.table(cmp)
  df1 <- data.table::setnames(df1, x, 'outcome')
  Estimate <- tibble(coefs = (mean(df1$outcome[df1$wasingov == 1], na.rm = TRUE) - mean(df1$outcome[df1$wasingov == 0], na.rm = TRUE)))
  cat("Coefficients calculated. Beginning bootstrapping\n")
  boots <- matrix(NA, nrow = 1000, ncol = 1)
  for (k in 1:1000) {
    clusters <- unique(df1[["party"]])
    units <- sample(clusters, size = length(clusters), replace = T)
    df.bs <- lapply(units, function(y) which(df1[, "party"] == y))
    d.sub1 <- df1[unlist(df.bs), ]
    boots[k, 1] <- (mean(d.sub1$outcome[d.sub1$wasingov == 1], na.rm = TRUE) - mean(d.sub1$outcome[d.sub1$wasingov == 0], na.rm = TRUE))
  }
  Estimate$bootstrap_low <- quantile(boots[,1], probs = 0.05/2)[[1]]
  Estimate$bootstrap_high <- quantile(boots[,1], probs = (1 - (0.05/2)))[[1]]
  Estimate$refinement_method <- 'no matching'
  Estimate$dv <- x
  return(Estimate)
}
outputplacdv2 <- lapply(absdiffs, estimation)

resultsplacdv2 <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(outputplacdv2)) {
  temp <- tibble(dv = outputplacdv2[[i]]$dv, refinement = outputplacdv2[[i]]$refinement_method, 
                 coef = outputplacdv2[[i]]$coefs,
                 low = outputplacdv2[[i]]$bootstrap_low, high = outputplacdv2[[i]]$bootstrap_high)
  resultsplacdv2 <- bind_rows(resultsplacdv2, temp)
}

resultsplacdv2 <- na.omit(resultsplacdv2)
resultsplacdv2$dv <- gsub('mean_per104', 'MilitPos', resultsplacdv2$dv)
resultsplacdv2$dv <- gsub('mean_per105', 'MilitNeg', resultsplacdv2$dv)
resultsplacdv2$dv <- gsub('mean_per203', 'ConstPos', resultsplacdv2$dv)
resultsplacdv2$dv <- gsub('mean_per204', 'ConstNeg', resultsplacdv2$dv)
resultsplacdv2$dv <- gsub('mean_per414', 'EconOrth', resultsplacdv2$dv)
resultsplacdv2$dv <- gsub('mean_per701', 'Labour', resultsplacdv2$dv)
resultsplacdv2$dv <- factor(resultsplacdv2$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))

ggplot(resultsplacdv2, aes(x = coef, y = as.factor(refinement), colour = as.factor(refinement))) +
  geom_pointrange(aes(xmin = low, xmax = high), alpha = 0.2) +
  geom_pointrange(data = resultsplacdv2[(resultsplacdv2$low<0 & resultsplacdv2$high<0) | (resultsplacdv2$low>0 & resultsplacdv2$high>0),], aes(xmin = low, xmax = high), alpha = 1) +
  xlab(NULL) + ylab(NULL) + 
  facet_grid(rows = vars(dv), scales = 'free_y', space = 'free_y') +
  geom_vline(xintercept = 0) + MyThemes::theme_base() +
  scale_colour_viridis_d() + labs(title = "A10: Differences between parties and the average voter (full sample)",
                                  caption = wrapper("Notes: Sample includes all western European social democratic or communist parties who have ever been in government (n = 37) resulting in 473 party-election observations between 1944 and 2021. Points represent the difference in group means (treated vs untreated) of the absolute difference between each party's score and the average voter (proxied by the mean position of parties in that election weighted by their vote share) with block boostrapped standard errors.", 100)) +
  theme(legend.position = 'none', axis.ticks = element_blank())



#################### A12: Placebo treatment lead ##########

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp[!(is.na(cmp$willgointogov)),], 
                                 time = "date", unit = "party", treatment = "willgointogov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
placebosets <- lapply(dvs, matching) # This is one takes time
placebosets <- flatten(placebosets)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output <- lapply(placebosets, estimation)

results <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output)) {
  temp <- tibble(dv = output[[i]]$outcome, refinement = output[[i]]$refinement_method, 
                 coef = output[[i]]$summary$coefs,
                 low = output[[i]]$summary$bootstrap_low, high = output[[i]]$summary$bootstrap_high)
  results <- bind_rows(results, temp)
}

plot_function(data = results,
              title = "A12: Placebo treatment test (lead)",
              caption = "Notes: Analysis as in Figure 5 in main analysis, but we use the second lead of the treatment variable as a placebo.")

#################### A13: Placebo treatment random reassignment ##########

placebo_output <- vector(mode = "list", length = 1000)
for (k in 1:1000) {
  cmp$placebo_unif <- sample(x = c(0,1), size = nrow(cmp), replace = TRUE)
  matching <- function (y) {
    doanalysis <- function (x) {
      matches <- Sparse_PanelMatch(data = cmp, 
                                   time = "date", unit = "party", treatment = "placebo_unif", outcome = y,
                                   treatment_lags = 3, outcome_leads = 0, 
                                   time_window_in_months = 60, match_missing = TRUE,
                                   covs = c("pervote", "lag_sd_rile"), qoi = "att",
                                   refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
      output <- matches
      df1 <- data.table::as.data.table(output$summary)
      df1$ideological_change <- (df1$outcome - df1$lag_outcome)
      df1 <- df1[treatment == 0, `:=`(weight, (weight * -1))]
      output = data.frame(coefs = (sum(df1$ideological_change * df1$weight)/length(unique(df1$group))),
                      outcome = y,
                      refinement_method = x)
      return(output)
    }
    return(lapply(listmethods, doanalysis))
  }
  placebo_output[[k]] <- lapply(dvs, matching) 
}
Plac_Out <- rlang::squash(placebo_output)
Plac_Out %>% map_dfr(as_tibble, .name_repair = "universal") -> Plac_Out

Plac_Out %>%
  group_by(outcome, refinement_method) %>%
  summarise(bootstrap_coef = mean(coefs),
            bootstrap_low = quantile(coefs, probs = 0.05/2),
            bootstrap_high = quantile(coefs, probs = (1 - (0.05/2)))) -> results

results <- na.omit(results)
results$refinement_method <- gsub('CBPS.match', 'cbps.match', results$refinement_method)
results$refinement_method <- gsub('CBPS.weight', 'cbps.weight', results$refinement_method)
results$refinement_method <- gsub('none', 'exact only', results$refinement_method)
results$refinement_method <- factor(results$refinement_method, levels = c('mahalanobis','ps.match','ps.weight','cbps.match','cbps.weight','exact only'))
results$outcome <- gsub('per104', 'MilitPos', results$outcome)
results$outcome <- gsub('per105', 'MilitNeg', results$outcome)
results$outcome <- gsub('per203', 'ConstPos', results$outcome)
results$outcome <- gsub('per204', 'ConstNeg', results$outcome)
results$outcome <- gsub('per414', 'EconOrth', results$outcome)
results$outcome <- gsub('per701', 'Labour', results$outcome)
results$outcome <- factor(results$outcome, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))

ggplot(results, aes(x = bootstrap_coef, y = as.factor(refinement_method), colour = as.factor(refinement_method))) +
  geom_pointrange(aes(xmin = bootstrap_low, xmax = bootstrap_high), alpha = 0.2) +
  geom_pointrange(data = results[(results$bootstrap_low<0 & results$bootstrap_high<0) | (results$bootstrap_low>0 & results$bootstrap_high>0),], aes(xmin = bootstrap_low, xmax = bootstrap_high), alpha = 1) +
  xlab(NULL) + ylab(NULL) + 
  facet_grid(rows = vars(outcome), scales = 'free_y', space = 'free_y') +
  geom_vline(xintercept = 0) +
  scale_colour_viridis_d() + labs(title = "A13: Placebo treatment test (randomised)",
                                  caption = wrapper("Notes: Analysis as in Figure 5 in main analysis but coefficients use 1000 randomly generated binary vectors as the treatment variable. 0 and 1 are sampled with equal probability which closely matches the real treatment variable where the probabilities are 0.51 and 0.49.", 100)) +
  MyThemes::theme_base() +
  theme(legend.position = 'none', axis.ticks = element_blank())


#################### A14: placebo treatment reversed #########

matching <- function (y) {
  doanalysis <- function (x) {
    matches <- Sparse_PanelMatch(data = cmp, 
                                 time = "date", unit = "party", treatment = "wasingov", outcome = y,
                                 treatment_lags = 3, outcome_leads = 0, 
                                 time_window_in_months = 60, match_missing = TRUE,
                                 covs = c("pervote", "lag_sd_rile"), qoi = "atc",
                                 refinement_method = x, size_match = 5, use_diagonal_covmat = TRUE)
    return(matches)
  }
  return(lapply(listmethods, doanalysis))
}
MatchedSets_rev <- lapply(dvs, matching) # This is one takes time
MatchedSets_rev <- flatten(MatchedSets_rev)

estimation <- function (x) {
  estimates <- Sparse_PanelEstimate(data = x, n_iterations = 1000, alpha = 0.05)
  return(estimates)
}
output <- lapply(MatchedSets_rev, estimation) # This one runs slow

results <- tibble(dv = NA, refinement = NA, coef = NA, low = NA, high = NA)
for (i in 1:length(output)) {
  temp <- tibble(dv = output[[i]]$outcome, refinement = output[[i]]$refinement_method, 
                 coef = output[[i]]$summary$coefs,
                 low = output[[i]]$summary$bootstrap_low, high = output[[i]]$summary$bootstrap_high)
  results <- bind_rows(results, temp)
}

plot_function(data = results,
              title = "A14: Placebo treatment test (reversed)",
              caption = wrapper("Notes: Analysis as in Figure 5 in main analysis, but reversing the treatment condition (i.e. in this case the dummy variavle for being government does from 1 to 0)."), 100)


#################### A15: Variation in effect sizes by single party government ##########

output <- tibble(coef = numeric(), group = character(), singlepartygov = numeric(), dv = character())
for(i in c(1,13,25,31)){
  df1 <- MatchedSets[[i]]$summary
  dv <- MatchedSets[[i]]$outcome
  df1$ideological_change <- (df1$outcome - df1$lag_outcome)
  df1$weight[df1$treatment == 0] <- df1$weight[df1$treatment == 0]*-1
  groups <- unique(df1$group)
  did <- function(x) {
    df1 %>%
      filter(group == x) %>%
      summarise(coef = sum(ideological_change*weight)) %>%
      mutate(group = x,
             singlepartygov = df1$singlepartygov[df1$treatment == 1 & df1$group == x],
             dv = dv) %>%
      return()
  }
  testres <- lapply(groups, did)
  testres %>% map_dfr(as_tibble, .name_repair = "universal") -> coefs
  output <- bind_rows(output, coefs)
}
output$dv <- gsub('per104', 'MilitPos', output$dv)
output$dv <- gsub('per105', 'MilitNeg', output$dv)
output$dv <- gsub('per203', 'ConstPos', output$dv)
output$dv <- gsub('per204', 'ConstNeg', output$dv)
output$dv <- gsub('per414', 'EconOrth', output$dv)
output$dv <- gsub('per701', 'Labour', output$dv)
output$dv <- factor(output$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))
output$label <- as.factor(ifelse(output$singlepartygov == 1, "Single party government","Coalition"))
ggplot(output, aes(x = as.factor(label), group = as.factor(label), y = coef)) +
  geom_boxplot() + xlab(NULL) +
  ylab('Standardised coefficient') + 
  facet_wrap(facet = vars(dv), ncol = 1, strip.position = "right", scales = "free_y") +
  theme(legend.position = 'none', axis.ticks = element_blank())

output %>%
  group_by(dv) %>%
  do(model = lm(coef ~ as.factor(.$singlepartygov), data = .)) -> fitted_models # only two significant
for (i in 1:4) {
  print(fitted_models$dv[i])
  print(summary(fitted_models$model[[i]]))
}


#################### A16: Variation in effect sizes by whether party had prime minister ##########

output <- tibble(coef = numeric(), group = character(), prime_minister = numeric(), dv = character())
for(i in c(1,13,25,31)){
  df1 <- MatchedSets[[i]]$summary
  dv <- MatchedSets[[i]]$outcome
  df1$ideological_change <- (df1$outcome - df1$lag_outcome)
  df1$weight[df1$treatment == 0] <- df1$weight[df1$treatment == 0]*-1
  groups <- unique(df1$group)
  did <- function(x) {
    df1 %>%
      filter(group == x) %>%
      summarise(coef = sum(ideological_change*weight)) %>%
      mutate(group = x,
             prime_minister = df1$prime_minister[df1$treatment == 1 & df1$group == x],
             dv = dv) %>%
      return()
  }
  testres <- lapply(groups, did)
  testres %>% map_dfr(as_tibble, .name_repair = "universal") -> coefs
  output <- bind_rows(output, coefs)
}
output$dv <- gsub('per104', 'MilitPos', output$dv)
output$dv <- gsub('per105', 'MilitNeg', output$dv)
output$dv <- gsub('per203', 'ConstPos', output$dv)
output$dv <- gsub('per204', 'ConstNeg', output$dv)
output$dv <- gsub('per414', 'EconOrth', output$dv)
output$dv <- gsub('per701', 'Labour', output$dv)
output$dv <- factor(output$dv, levels = c('MilitPos','MilitNeg','ConstPos','ConstNeg','EconOrth','Labour'))
output$label <- as.factor(ifelse(output$prime_minister == 1, "Party of Prime Minister","Junior party"))
ggplot(output, aes(x = as.factor(label), group = as.factor(label), y = coef)) +
  geom_boxplot() + xlab(NULL) +
  ylab('Standardised coefficient') + 
  facet_wrap(facet = vars(dv), ncol = 1, strip.position = "right", scales = "free_y") +
  theme(legend.position = 'none', axis.ticks = element_blank())

output %>%
  group_by(dv) %>%
  do(model = lm(coef ~ as.factor(.$prime_minister), data = .)) -> fitted_models # only two significant
for (i in 1:4) {
  print(fitted_models$dv[i])
  print(summary(fitted_models$model[[i]]))
}

#################### A17: TWFE models #########

dvs <- c('per104', 'per105',
         'per203', 'per204',
         'per414',
         'per701')

cmp$year <- substr(cmp$date,1,4) # could add control variables but then milit_pos goes negative

# milit_pos: coefficient positive, non-significant 
summary(lm(per104 ~ factor(wasingov) + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]

# milit_neg: coefficient negative and significant
summary(lm(per105 ~ factor(wasingov)  + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]

# constitution_pos: positive non-significant
summary(lm(per203 ~ factor(wasingov)  + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]

# constitution_neg: positive non-significant so WRONG!!!!
summary(lm(per204 ~ factor(wasingov)  + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]

# economic_orthodoxy: positive and significant
summary(lm(per414 ~ factor(wasingov)  + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]

# labour_pos: negative, significant
summary(lm(per701 ~ factor(wasingov)  + factor(countryname) + factor(year), data = cmp))$coefficients[1:4,]
