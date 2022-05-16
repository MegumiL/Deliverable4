
#Parameters: Simulation/ Dashboard prototype

# Number of variables-columns: 7 

# Number of observations: 2,700

# MATRIX: 2,700 * 7

# -------

# 1) Partners : {1,2,.. 10} 					n=10

# 2) Years    : {2022, 2023, 2024,2025,2026}  n=5

# 3) IndicatorName : {GI, i11A, i11B, ...}   n= 54

# 4) IndicatorValue : { }						n= 2,700 = 54*10*5
# -------

# 5) ToCVariable :{Output1, Output2,. }

# 6) Target      : {Target1, ... }

# 7) SubTarget   : {Subtarget1, SubTarget2, ...}


#####################
# I) Setting up columns-variables
#####################

#####################
# Partner
#####################

Partner = rep(c("PartnerName1","PartnerName2","PartnerName3","PartnerName4","PartnerName5",

"PartnerName6","PartnerName7","PartnerName8","PartnerName9","PartnerName10"), each= 270) 


#####################
# Year
#####################

Year = rep(c("2022","2023","2024","2025","2026"),times=540) 


#####################
# IndicatorName
#####################

IndicatorName = rep(

				c("GI",

				  "i11A","i11B","i11C","i11D","i11E","i11F","i11G","i11H", 

				  "i12A","i12B","i12C", 

				  "i13A","i13B","i13C","i13D","i13E",

				  "i14A",

				  "i21A","i21B","i21C","i21D","i21E","i21F","i21G",

				  "i22A","i22B","i22C","i22D","i22E",

				  "i23A","i23B","i23C","i23D","i23E","i23F","i23G",

				  "i24A","i24B",

				  "i25A","i25B",

				  "i31A","i31B","i31C","i31D",

				  "i32A",

				  "i33A","i33B",

				  "i34A","i34B",

				  "i35A",

				  "IAW", "ILO", "IAD") , times = 50)


#####################
# IndicatorValue
#####################

IndicatorValue=c(round(runif(2700),2))

#####################
# II) Integrating columns into a df 
#####################

df <- data.frame(Partner, Year, IndicatorName,IndicatorValue)

#####################
# III) From df to tibble 
#####################

library(tidyverse)

df <- as_tibble(df)

#####################
# IV) Including conditional varaibles (using Figure 5 (Inform1): Link btw Indicators, Aflatoun's Strategic Goals & ToC)
#####################


#####################
SubTarget
#####################

df <- df  %>% mutate(SubTarget= 

           case_when(

           IndicatorName == c("i11A","i11B", "i11C", "i11D", "i11E","i11F") ~ "Subtarget1",

           TRUE ~ "SubTarget2")

           )


#####################
Target
#####################

df <-df %>% mutate(Target =

		  case_when( 

		  SubTarget == "Subtarget1" ~ "Target1",

		  TRUE ~ "SubTarget2")
		  
		  )

#####################
ToCVariable
#####################

df <- df %>% mutate (ToCVariable= 
	
			case_when(

			IndicatorName == "GI" ~ "GlobalOutcome",

			IndicatorName == c("i11A","i11B","i11C","i11D","i11E","i11F") ~ "OutputQualityEducationResourcesDeveloped",

			TRUE ~ "Output2")

			)


