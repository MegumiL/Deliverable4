#Parameters: Simulation/ Dashboard prototype

# Number of variables-columns: 7 

# Number of observations: 2,700

# MATRIX: 2,700 * 7

# -------

# 1) Partners : {1,2,.. 10} 					n=10

# 2) Years    : {2022, 2023, 2024,2025,2026}  n=5

# 3) IndicatorName : {GI, i11A, i11B, ...}   n= 59

# 4) IndicatorValue : { }						n= 2,950 = 59*10*5
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

				  "i12A","i12B","i12C","i12D",

				  "i13A","i13B","i13C","i13D","i13E",

				  "i14A","i14B","i14C","i14D","i14E",

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

				  "IAW", "ILO", "IAD") , times = 59)


#####################
# IndicatorValue
#####################

IndicatorValue=c(round(runif(2950),2))

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
# IV) Including conditional variables (using Figure 5 (Inform1): Link btw Indicators, Aflatoun's Strategic Goals & ToC)
#####################


#####################
SubTarget
#####################

df <- df  %>% mutate(SubTarget= 

           case_when(

           IndicatorName == c("i11A","i11B","i11C","i11D","i11E","i11F","i11G","i11H") ~ "A11",
	   
	   IndicatorName == c("i12A","i12B","i12C","i12D") ~ "A12",
          
	   IndicatorName == c("i13A","i13B","i13C","i13D","i13E") ~ "A13",

	   IndicatorName == c("i14A","i14B","i14C","i14D","i14E") ~ "A14",

	   IndicatorName == c("i21A","i21B","i21C","i21D","i21E","i21F","i21G") ~ "A21",

	   IndicatorName == c("i22A","i22B","i22C","i22D","i22E") ~ "A22",

	   IndicatorName == c("i23A","i23B","i23C","i23D","i23E","i23F","i23G") ~ "A23",

	   IndicatorName == c("i24A","i24B") ~ "A24",

	   IndicatorName == c("i25A","i25B") ~ "A25",

	   IndicatorName == c("i31A","i31B","i31C","i31D") ~ "A31",

	   IndicatorName == c("i32A") ~ "A32",

	   IndicatorName == c("i33A","i33B") ~ "A33",

	   IndicatorName == c("i34A","i34B") ~ "A34",

	   IndicatorName == c("i35A") ~ "A35")
			      
           )


#####################
Target
#####################

df <-df %>% mutate(Target =

		  case_when( 

		  SubTarget == c("A11","A12","A13","A14") ~ "O1",

		  SubTarget == c("A21","A22","A23","A24","A25") ~ "O2",
		  
		  SubTarget == c("A31","A32","A33","A34","A35") ~ "O3")
			  
		  )

#####################
ToCVariable
#####################

df <- df %>% mutate (ToCVariable= 
	
			case_when(

			IndicatorName == "GI" ~ "GlobalOutcome",
				
			IndicatorName == c("i21F","i21G","i23A","i23B","i23C","i23D","i23E","i23F","i23G","i24A") ~ "OutcomeVibrantPartnershipNetworksHaveGrown",

			IndicatorName == c("i11A","i11B","i11C","i11D","i11E","i11F","i11G","i11H") ~ "OutputQualityEducationResourcesDeveloped",

			IndicatorName == c("i12A","i12B","i12C","i12D","i13C","i13D","i21E","i22A","i22B","i22C","i22D","i22E","i25A","i25B") ~ "OutputStakeholdersCapacityBuilt",

			IndicatorName == c("i31A","i31B","i31C","i31D","i34A","i34B") ~ "OutputEvidenceIncreased",

			IndicatorName == c("i24B","IAW", "ILO", "IAD") ~ "OutputIncreasedawarenessThroughAdvocacy",
				
			IndicatorName == c("i13E","i14A") ~ "OutputM&EOngoing",
				
			IndicatorName == c("i21A","i21B","i21C","i21D","i32A","i33A","i33B","i35A") ~ "OutputBestPracticesShared",
				
			IndicatorName == c("i13A","i13B") ~ "Outputs1&2",
				
			IndicatorName == c("i14B","i14C") ~ "Outputs2&5",
				
			IndicatorName == c("i14D","i14E") ~ "Outputs1&5")
			
			)
