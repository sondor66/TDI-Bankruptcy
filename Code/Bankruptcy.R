install.packages("haven")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("leaps")
install.packages("lattice")
install.packages("grid")
install.packages("hexbin")
install.packages("micromapST")
install.packages("tidyquant")
install.packages("ggalt")
install.packages("BBmisc")

#***************************************************************************************************#
# R version 3.5.2 (2019-07-25)
#***************************************************************************************************#

library(haven)
library(dplyr)
library(lubridate)
library(tidyverse)
library(leaps)
library(lattice)
library(grid)
library(hexbin)
library(micromapST)
library(tidyquant)
library(ggalt)
library(BBmisc)
#***************************************************************************************************#
# Package citations are at the end of the code
#***************************************************************************************************#

setwd("C:\\Users\\Arunsodo\\Downloads")

# Define variables that will be kept in the dataframe
v <- c(case_key = "CASEKEY",
       district = "DISTRICT",
       generation = "GEN",
       sequence = "SEQ",
       original_filing_date = "ORGFLDT",
       filing_date = "FILEDATE",
       original_filing_chapter = "ORGFLCHP",
       current_chapter = "CRNTCHP",
       nature_of_debt = "NTRDBT",
       fee_status = "FEESTS",
       type_case = "CASETYP",
       type_debtor = "DBTRTYP",
       nature_of_business = "NOB",
       estimated_creditors = "ECRDTRS",
       small_business_debtor = "SMLLBUS",
       total_assets = "TOTASSTS",
       real_property = "REALPROP",
       personal_property = "PERSPROP",
       total_liabilities = "TOTLBLTS",
       secured_claims = "SECURED",
       unsecured_priority_claims = "UNSECPR",
       unsecured_nonpriority_claims = "UNSECNPR",
       total_debt_discharged = "DSCHRGD",
       non_dischargeable_debt = "NDSCHRGD",
       total_debt = "TOTDBT",
       current_monthly_income = "CNTMNTHI",
       average_monthly_income = "AVGMNTHI",
       average_monthly_expenses = "AVGMNTHE",
       closing_date = "CLOSEDT",
       debtor1_final_disposition = "D1FDSP",
       chapter_11_percent_dividend = "C11DVDND")

# Read data
# Make sure you are using the read_sas function from the haven package

bank08 <- read_sas("bank08.sas7bdat", NULL)
bank09 <- read_sas("bank09.sas7bdat", NULL)
# dim(bank08) row:2319587 84

# Read in the s&p 500 data for 2008 and 2009
sp_500 <- read_csv("SP50008_09.csv")
sp_500$date <- as_date(as.character(sp_500$date, "%Y%m%d"))

# Combine the dataframes 2008/2009 together

bank08_09 <- bind_rows(list(bank08, bank09))
head(bank08_09,10)
dim(bank08_09)
# Select the columns based on variables defined above

bank08_09 <- dplyr::select(bank08_09, !!v) # to 31 columns

# Remove bank08, 09, 15, 16, 17 to save space
rm(bank08, bank09)

# Only select cases that have not changed chapter, and are chapter 7 or 13
# The reason for only selecting chapter 7 and 13 is they make up 98% of the data

bank08_09 <- filter(bank08_09, generation == 0 & original_filing_chapter == current_chapter &
                      current_chapter %in% c("7", "13") &
                      total_assets < 1000000000 & total_debt < 1000000000 & !is.na(total_debt))

bank08_09 <- filter(bank08_09, original_filing_date > as_date("2008-01-01"))

# Fill NA values for numerical columns with 0.0
bank08_09$total_assets[is.na(bank08_09$total_assets)] <- 0
bank08_09$real_property[is.na(bank08_09$real_property)] <- 0
bank08_09$personal_property[is.na(bank08_09$personal_property)] <- 0
bank08_09$total_liabilities[is.na(bank08_09$total_liabilities)] <- 0
bank08_09$secured_claims[is.na(bank08_09$secured_claims)] <- 0
bank08_09$unsecured_priority_claims[is.na(bank08_09$unsecured_priority_claims)] <- 0
bank08_09$unsecured_nonpriority_claims[is.na(bank08_09$unsecured_nonpriority_claims)] <- 0
bank08_09$total_debt_discharged[is.na(bank08_09$total_debt_discharged)] <- 0
bank08_09$non_dischargeable_debt[is.na(bank08_09$non_dischargeable_debt)] <- 0
bank08_09$current_monthly_income[is.na(bank08_09$current_monthly_income)] <- 0
bank08_09$average_monthly_income[is.na(bank08_09$average_monthly_income)] <- 0
bank08_09$average_monthly_expenses[is.na(bank08_09$average_monthly_expenses)] <- 0

# Convert district codes to state names
bank08_09 <- mutate(bank08_09, state = case_when(district == "00" ~ "Maine",
                                                 district == "01" ~ "Massachusetts",
                                                 district == "02" ~ "New Hampshire",
                                                 district == "03" ~ "Rhode Island",
                                                 district == "05" ~ "Connecticut",
                                                 district == "06" ~ "New York",
                                                 district == "07" ~ "New York",
                                                 district == "08" ~ "New York",
                                                 district == "09" ~ "New York",
                                                 district == "10" ~ "Vermont",
                                                 district == "11" ~ "Delaware",
                                                 district == "12" ~ "New Jersey",
                                                 district == "13" ~ "Pennsylvania",
                                                 district == "14" ~ "Pennsylvania",
                                                 district == "15" ~ "Pennsylvania",
                                                 district == "16" ~ "Maryland",
                                                 district == "17" ~ "North Carolina",
                                                 district == "18" ~ "North Carolina",
                                                 district == "19" ~ "North Carolina",
                                                 district == "20" ~ "South Carolina",
                                                 district == "22" ~ "Virginia",
                                                 district == "23" ~ "Virginia",
                                                 district == "24" ~ "West Virginia",
                                                 district == "25" ~ "West Virginia",
                                                 district == "26" ~ "Alabama",
                                                 district == "27" ~ "Alabama",
                                                 district == "28" ~ "Alabama",
                                                 district == "29" ~ "Florida",
                                                 district == "3A" ~ "Florida",
                                                 district == "3C" ~ "Florida",
                                                 district == "3E" ~ "Georgia",
                                                 district == "3G" ~ "Georgia",
                                                 district == "3J" ~ "Georgia",
                                                 district == "3L" ~ "Louisiana",
                                                 district == "3N" ~ "Louisiana",
                                                 district == "36" ~ "Louisiana",
                                                 district == "37" ~ "Mississippi",
                                                 district == "38" ~ "Mississippi",
                                                 district == "39" ~ "Texas",
                                                 district == "40" ~ "Texas",
                                                 district == "41" ~ "Texas",
                                                 district == "42" ~ "Texas",
                                                 district == "43" ~ "Kentucky",
                                                 district == "44" ~ "Kentucky",
                                                 district == "45" ~ "Michigan",
                                                 district == "46" ~ "Michigan",
                                                 district == "47" ~ "Ohio",
                                                 district == "48" ~ "Ohio",
                                                 district == "49" ~ "Tennessee",
                                                 district == "50" ~ "Tennessee",
                                                 district == "51" ~ "Tennessee",
                                                 district == "52" ~ "Illinois",
                                                 district == "53" ~ "Illinois",
                                                 district == "54" ~ "Illinois",
                                                 district == "55" ~ "Indiana",
                                                 district == "56" ~ "Indiana",
                                                 district == "57" ~ "Wisconsin",
                                                 district == "58" ~ "Wisconsin",
                                                 district == "60" ~ "Arkansas",
                                                 district == "61" ~ "Arkansas",
                                                 district == "62" ~ "Iowa",
                                                 district == "63" ~ "Iowa",
                                                 district == "64" ~ "Minnesota",
                                                 district == "65" ~ "Missouri",
                                                 district == "66" ~ "Missouri",
                                                 district == "67" ~ "Nebraska",
                                                 district == "68" ~ "North Dakota",
                                                 district == "69" ~ "South Dakota",
                                                 district == "7-" ~ "Alaska",
                                                 district == "70" ~ "Arizona",
                                                 district == "71" ~ "California",
                                                 district == "72" ~ "California",
                                                 district == "73" ~ "California",
                                                 district == "74" ~ "California",
                                                 district == "75" ~ "Hawaii",
                                                 district == "76" ~ "Idaho",
                                                 district == "77" ~ "Montana",
                                                 district == "78" ~ "Nevada",
                                                 district == "79" ~ "Oregon",
                                                 district == "80" ~ "Washington",
                                                 district == "81" ~ "Washington",
                                                 district == "82" ~ "Colorado",
                                                 district == "83" ~ "Kansas",
                                                 district == "84" ~ "New Mexico",
                                                 district == "85" ~ "Oklahoma",
                                                 district == "86" ~ "Oklahoma",
                                                 district == "87" ~ "Oklahoma",
                                                 district == "88" ~ "Utah",
                                                 district == "89" ~ "Wyoming",
                                                 district == "90" ~ "District of Columbia",
                                                 TRUE ~ "Out of Scope"))



# Remove all out of scope cases
bank08_09 <- filter(bank08_09, state != "Out of Scope")

# Create columns to hold normalized values of numerical columns
bank08_09 <- mutate(bank08_09, nrm_total_assets = round((bank08_09$total_assets - mean(bank08_09$total_assets)) / sd(bank08_09$total_assets), 2),
                    nrm_real_property = round((bank08_09$real_property - mean(bank08_09$real_property)) / sd(bank08_09$real_property), 2),
                    nrm_personal_property = round((bank08_09$personal_property - mean(bank08_09$personal_property)) / sd(bank08_09$personal_property), 2),
                    nrm_total_liabilities = round((bank08_09$total_liabilities - mean(bank08_09$total_liabilities)) / sd(bank08_09$total_liabilities), 2),
                    nrm_secured_claims = round((bank08_09$secured_claims - mean(bank08_09$secured_claims)) / sd(bank08_09$secured_claims), 2),
                    nrm_total_debt_discharged = round((bank08_09$total_debt_discharged - mean(bank08_09$total_debt_discharged)) / sd(bank08_09$total_debt_discharged), 2),
                    nrm_non_discharcheable_debt = round((bank08_09$non_dischargeable_debt - mean(bank08_09$non_dischargeable_debt)) / sd(bank08_09$non_dischargeable_debt), 2),
                    nrm_total_debt = round((bank08_09$total_debt - mean(bank08_09$total_debt)) / sd(bank08_09$total_debt), 2),
                    nrm_average_monthly_income = round((bank08_09$average_monthly_income - mean(bank08_09$average_monthly_income)) / sd(bank08_09$average_monthly_income), 2),
                    nrm_average_monthly_expenses = round((bank08_09$average_monthly_expenses - mean(bank08_09$average_monthly_expenses)) / sd(bank08_09$average_monthly_expenses), 2))


# Calculate log base 10 of numeric columns so they are easier to work with since there are extreme cases
bank08_09 <- mutate(bank08_09, log_total_assets = log10(total_assets),
                    log_real_property = log10(real_property),
                    log_personal_property = log10(personal_property),
                    log_total_liabilities = log10(total_liabilities),
                    log_secured_claims = log10(secured_claims),
                    log_total_debt_discharged = log10(total_debt_discharged),
                    log_non_discharcheable_debt = log10(non_dischargeable_debt),
                    log_total_debt = log10(total_debt),
                    log_average_monthly_income = log10(average_monthly_income),
                    log_average_monthly_expenses = log10(average_monthly_expenses))


# log of 0 is -infinity, so replace with 0
bank08_09$log_average_monthly_expenses[bank08_09$log_average_monthly_expenses < 0] <- 0
bank08_09$log_average_monthly_income[bank08_09$log_average_monthly_income < 0] <- 0
bank08_09$log_non_discharcheable_debt[bank08_09$log_non_discharcheable_debt < 0] <- 0
bank08_09$log_personal_property[bank08_09$log_personal_property < 0] <- 0
bank08_09$log_real_property[bank08_09$log_real_property < 0] <- 0
bank08_09$log_secured_claims[bank08_09$log_secured_claims < 0] <- 0
bank08_09$log_total_assets[bank08_09$log_total_assets < 0] <- 0
bank08_09$log_total_debt_discharged[bank08_09$log_total_debt_discharged < 0] <- 0
bank08_09$log_total_debt[bank08_09$log_total_debt < 0] <- 0
bank08_09$log_total_liabilities[bank08_09$log_total_liabilities < 0] <- 0

# Group everything by state for micromap

bank08_09_by_state <- group_by(bank08_09, state)

sum_by_state <- summarize(bank08_09_by_state,
                          m_total_debt = median(total_debt, na.rm = TRUE),
                          m_total_assets = median(total_assets, na.rm = TRUE),
                          m_real_property = median(real_property, na.rm = TRUE),
                          m_personal_property = median(personal_property, na.rm = TRUE),
                          m_total_liabilities = median(total_liabilities, na.rm = TRUE),
                          m_total_debt_discharged = median(total_debt_discharged, na.rm = TRUE),
                          m_non_dischargeable_debt = median(non_dischargeable_debt, na.rm = TRUE),
                          m_average_monthly_income = median(average_monthly_income, na.rm = TRUE),
                          m_average_monthly_expenses = median(average_monthly_expenses, na.rm = TRUE),
                          t_debt = sum(total_debt, na.rm = TRUE))


sum_by_state$diff_inc_exp <- sum_by_state$m_average_monthly_income - sum_by_state$m_average_monthly_expenses

# Normalize continuous variables 
nrm_sum_by_state <- normalize(sum_by_state)
# Create new column in normalized dataframe that identifies whether the variable is above or below the expected value
nrm_sum_by_state$above_below <- ifelse(nrm_sum_by_state$m_total_debt < 0, "below", "above")
# Sort the normalized dataframe by the variable
nrm_sum_by_state <- nrm_sum_by_state[order(nrm_sum_by_state$m_total_debt),]
# Factorize
nrm_sum_by_state$state <- factor(nrm_sum_by_state$state, levels = nrm_sum_by_state$state)



# Create the panel description for the micromap

panel_desc <- data.frame(type = c("map", "id", "dot", "dot", "dot"),
                         lab1 = rep("", 5), # repeat N in type
                         lab2 = c("", "", "Median Debt Discharged", "Median Income", "Median Expenses"),
                         lab3 = c("", "", "", "", ""),
                         col1 = c(NA, NA, 7, 9, 10))


# For some reason we had to coerce this to a dataframe or micromap package would error
sd <- as.data.frame(sum_by_state)

# Create micromap pdf
file_name <- "bk_micromap.pdf"
pdf(file=file_name, width=7.5, height=10)

micromapST(sd, panel_desc,
           rowNamesCol='state',
           rowNames='full',
           sortVar="m_total_debt_discharged", ascend=FALSE,
           title=c("2008 through 2009 Bankruptcy Filings"),
           ignoreNoMatches=TRUE)

dev.off()

# Make new column to hold just the year
bank08_09_by_state$year <- substring(bank08_09_by_state$original_filing_date,1,4)
bank08_09_state_by_year <- bank08_09_by_state %>% group_by(year, state) %>% mutate(count = n()) 

# Select only 2008 records
bank08_year <- filter(bank08_09_state_by_year, year == "2008")
# Select only 2009 records
bank09_year <- filter(bank08_09_state_by_year, year == "2009")
# Get the number of bankruptcies by state in 2008
bank_count_08 <- count(bank08_year,"state")
# Get the number of bankruptcies by state in 2009
bank_count_09 <- count(bank09_year,"state")

count_year <- merge(x=bank_count_08,y=bank_count_09,by="state",all.x=TRUE)

# Get the numerical difference between bankruptcy filings as a string in each state and make it a new column
count_year$diff <- sprintf("+%d", (as.integer(count_year$n.y)-as.integer(count_year$n.x)))

by_date <- plyr::count(bank08_09, c("original_filing_date"))

#bank08_09_by_state$state <- factor(bank08_09_by_state$state, levels=rev(unique(bank08_09_by_state$state)), ordered=TRUE)
# Make sure count_year is a tibble
count_year <- as_tibble(count_year)
# Get the numerical difference in bankruptcy filings by state as a number
count_year <- mutate(count_year, diff_num = n.y - n.x)
# Sort the count by the number of bankruptcy filings per state in 2008
count_year <- count_year[order(count_year$n.x),]

count_year$state <- factor(count_year$state, levels=as.character(count_year$state))

source("dumbbell_theme.R")
# Dumbbell plot
gg2 <- ggplot(count_year, aes(x=n.x, xend=n.y, y=state, group=state)) + 
  geom_point(mapping = aes(x = n.x, color = year.x)) +
  geom_point(mapping = aes(x = n.y, color = year.y)) +
  geom_dumbbell(color="#7cbdef", 
                size=1.5, 
                colour_x = "#252eb2",
                colour_xend = "#ed1212") + 
  scale_color_manual(name="Filing Year", values = c("#252eb2", "#ed1212")) + 
  labs(x="Number of Bankruptcies Filed", 
       y=NULL, 
       title="Total Bankruptcy Filings by State", 
       subtitle="Total Change: 2008 vs 2009") 

gg2 + dumbbell_theme

# Plot histogram of bankruptcy filings by date for 2008 and 2009
ggplot(data = bank08_09, mapping = aes(x = original_filing_date, fill = current_chapter)) + 
  geom_histogram(bins = 100)

# Plot stacked bar graph for different bankruptcy chapters and 
ggplot(data = bank08_09, mapping = aes(x = debtor1_final_disposition, fill = current_chapter)) +
  geom_bar(position = "fill") + coord_flip()

# Plot line charts for bankruptcy with S&P price overlay
source("line_theme.R")
by_date2 <- left_join(by_date, sp_500, by = c("original_filing_date" = "date"))
by_date2_70 <- by_date2
by_date2_70$PRC <- by_date2$PRC * 70
bd <- by_date2_70 %>% gather("freq", "PRC", key = "type", value = "value")
p <- ggplot(by_date2, aes(x = original_filing_date, y = freq)) +
  geom_line(data = bd[!is.na(bd$value),], aes(y = value, color=type, group=type), size=.75)
#p <- p + geom_line(data = by_date2[!is.na(by_date2$PRC),], aes(y = PRC * 70), color = "#ed1212", size=1)
p <- p + scale_y_continuous(sec.axis = sec_axis(~. / 66, name = "SPY Index Value")) +
         # citation for second axis: https://ggplot2.tidyverse.org/reference/sec_axis.html
         # Wickham, H., Chang, W., Henry, L., Pedersen, T., Takahashi, K., Wilke, C., & Woo, K. (n.d.). 
         # Specify a secondary axis. R, ggplot2. Retrieved from https://github.com/tidyverse/ggplot2/blob/master/R/axis-secondary.R
         scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") + 
         # citation for date breaks above: https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
         # joran. (2012). Formatting dates on X axis in ggplot2. R. Retrieved from https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
         labs(title="Bankruptcy Filings vs SPY Index Value",
              x="Date",
              y="Number of Bankruptcies Filed") +
  scale_color_manual(name="", labels=c("Filings", "SPY"), values = c("#252eb2", "#ed1212")) 
p + line_theme 

# Plot zoomed in line chart for bankruptcies
ggplot(data = by_date2, mapping = aes(x = original_filing_date, y = freq)) +
  geom_line(color = "#252eb2", size=.75) +
  coord_x_date(xlim = c("2009-01-01", "2009-05-30")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  # citation for date breaks above: https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
  # # joran. (2012). Formatting dates on X axis in ggplot2. R. Retrieved from https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
  labs(title="Cyclical Bankruptcy Filings",
       subtitle="January through June 2009",
       x="Date",
       y="Number of Bankruptcies Filed") + line_theme

# Normalized plot by state
# citation: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# Prabhakaran, S. (2017). Top 50 ggplot2 Visualizations - The Master List (With Full R Code). Retrieved from http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#top
ggplot(nrm_sum_by_state, aes(x = state, y = m_total_debt, label="Normalized Median Debt by State")) +
  geom_bar(stat = "identity", aes(fill = above_below), width = 0.5) +
  scale_fill_manual(name = "Median Debt",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#ed1212", "below"="#252eb2")) +
  labs(title="Normalized Median Debt by State") +
  coord_flip() +
  labs(title="Normalized Median Debt by State",
       y="Normalized Median Debt",
       x=NULL) + dumbbell_theme

citation(package = "haven")
# Hadley Wickham and Evan Miller (2018). haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files. R
# package version 1.1.2. https://CRAN.R-project.org/package=haven
citation(package = "dplyr")
# Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2018). dplyr: A Grammar of Data
# Manipulation. R package version 0.7.6. https://CRAN.R-project.org/package=dplyr
citation(package = "lubridate")
# Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of
# Statistical Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/.
citation(package = "tidyverse")
# Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1.
# https://CRAN.R-project.org/package=tidyverse
citation(package = "leaps")
# Thomas Lumley based on Fortran code by Alan Miller (2017). leaps: Regression Subset Selection. R
# package version 3.0. https://CRAN.R-project.org/package=leaps
citation(package = "lattice")
# Sarkar, Deepayan (2008) Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN
# 978-0-387-75968-5
citation(package = "grid")
# R Core Team (2018). R: A language and environment for statistical computing. R Foundation for
# Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
citation(package = "hexbin")
# Dan Carr, ported by Nicholas Lewin-Koh, Martin Maechler and contains copies of lattice functions
# written by Deepayan Sarkar (2018). hexbin: Hexagonal Binning Routines. R package version 1.27.2.
# https://CRAN.R-project.org/package=hexbin
citation(package = "micromapST")
# Daniel B. Carr and Linda Williams Pickle (2010), Visualizing Data Patterns with Micromaps
# 
# Linda Williams Pickle, James B. Pearson, Jr. and Daniel B. Carr (2014), micromapST: Exploring and
# Communicating Geospatial Patterns in U. S. State Data
citation(package = "tidyquant")
# Matt Dancho and Davis Vaughan (2018). tidyquant: Tidy Quantitative Financial Analysis. R package
# version 0.5.5. https://CRAN.R-project.org/package=tidyquant
citation(package = "ggalt")
# Bob Rudis, Ben Bolker and Jan Schulz (2017). ggalt: Extra Coordinate Systems, 'Geoms', Statistical
# Transformations, Scales and Fonts for 'ggplot2'. R package version 0.4.0.
# https://CRAN.R-project.org/package=ggalt
citation(package = "BBmisc")
# Bernd Bischl, Michel Lang, Jakob Bossek, Daniel Horn, Jakob Richter and Dirk Surmann (2017). BBmisc:
#   Miscellaneous Helper Functions for B. Bischl. R package version 1.11.
# https://CRAN.R-project.org/package=BBmisc

