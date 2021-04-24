global root = "C:\Users\Kieu-Trang Nguyen\Dropbox\Clan Collusion and Constraints" //laptop


*** CLEAN & ASSIGN TERM START/END YEARS ****************************************

use "${root}\Data\PoliticianData v6.dta", clear
duplicates drop

//Create region IDs (8 regions & 3 regions)
gen Reg8ID = floor(ProID/100)
replace Reg8ID = . if Reg8ID == 0
gen Reg3ID = 1 if Reg8ID == 1 | Reg8ID == 2 | Reg8ID == 3
replace Reg3ID = 2 if Reg8ID == 4 | Reg8ID == 5 | Reg8ID == 6
replace Reg3ID = 3 if Reg8ID == 7 | Reg8ID == 8  

//Recode PolGroup
foreach i in 08 09 10 11 {
	replace PolGroup = "TW`i'" if PolGroup == "UVTW`i'"
}
foreach i in 00 04 09 12 {
	replace PolGroup = "CP`i'" if PolGroup == "TCHC`i'" & PolType == "CP"
	replace PolGroup = "UBND`i'" if PolGroup == "TCHC`i'" & PolType == "UBND"
}

//Assign PolYrBegin & PolYrEnd
gen PolYrBegin = .
gen PolYrEnd = .
replace PolYrBegin = 1997 if PolGroup == "TW08"
replace PolYrEnd   = 2001 if PolGroup == "TW08"
replace PolYrBegin = 2002 if PolGroup == "TW09" //same as Mandarin
replace PolYrEnd   = 2006 if PolGroup == "TW09" //same as Mandarin
replace PolYrBegin = 2007 if PolGroup == "TW10" //same as Mandarin
replace PolYrEnd   = 2011 if PolGroup == "TW10" //same as Mandarin
replace PolYrBegin = 2012 if PolGroup == "TW11" 
replace PolYrEnd   = 2016 if PolGroup == "TW11"
replace PolYrBegin = 1998 if PolGroup == "CP00" //same as Mandarin
replace PolYrEnd   = 2002 if PolGroup == "CP00" //same as Mandarin
replace PolYrBegin = 2003 if PolGroup == "CP04" //same as Mandarin
replace PolYrEnd   = 2007 if PolGroup == "CP04" //same as Mandarin
replace PolYrBegin = 2008 if PolGroup == "CP09" //same as Mandarin
replace PolYrEnd   = 2011 if PolGroup == "CP09" //same as Mandarin
replace PolYrBegin = 2012 if PolGroup == "CP12"
replace PolYrEnd   = 2016 if PolGroup == "CP12"
replace PolYrBegin = 2000 if PolGroup == "UBND00" //same as Mandarin
replace PolYrEnd   = 2003 if PolGroup == "UBND00" //same as Mandarin
replace PolYrBegin = 2004 if PolGroup == "UBND04" //same as Mandarin
replace PolYrEnd   = 2008 if PolGroup == "UBND04" //same as Mandarin
replace PolYrBegin = 2009 if PolGroup == "UBND09" //same as Mandarin
replace PolYrEnd   = 2012 if PolGroup == "UBND09" //same as Mandarin
replace PolYrBegin = 2013 if PolGroup == "UBND12"
replace PolYrEnd   = 2017 if PolGroup == "UBND12"
replace PolYrBegin = 2003 if PolGroup == "QH11" //same as Mandarin
replace PolYrEnd   = 2007 if PolGroup == "QH11" //same as Mandarin
replace PolYrBegin = 2008 if PolGroup == "QH12" //same as Mandarin
replace PolYrEnd   = 2011 if PolGroup == "QH12" //same as Mandarin
replace PolYrBegin = 2012 if PolGroup == "QH13"
replace PolYrEnd   = 2016 if PolGroup == "QH13"

//Create unique PosID
tostring PolID, gen(temp)
gen PosID = temp + PolGroup
egen PosIDno = group(PosID)
drop temp	
order ComID DisID ProID Reg8ID Reg3ID PolID PolGroup PolYrBegin PolYrEnd PolType PolRank PolDept ///
	PolProvince PolName PolDOB PolYOB CommuneName DistrictName ProvinceName PosID PosIDno


*** CREATE POWER VARIABLES & AGGREGATE DATA ************************************

//Create power variables in wide format (observation unit: PosID)
bysort PolID: egen PolYrEnter = min(PolYrBegin)
bysort PolID: egen PolYrEnter_TW = min(PolYrBegin) if PolType == "TW"
bysort PolID: egen PolYrEnter_TWa = min(PolYrBegin) if PolType == "TW" & PolRank == 1
bysort PolID: egen PolYrEnter_TWb = min(PolYrBegin) if PolType == "TW" & PolRank == 2
bysort PolID: egen PolYrEnter_CP = min(PolYrBegin) if PolType == "CP"
bysort PolID: egen PolYrEnter_UBND = min(PolYrBegin) if PolType == "UBND"
bysort PolID: egen PolYrEnter_QH = min(PolYrBegin) if PolType == "QH" & PolRank != 5 

gen minRank1996 = 10 
forval i = 0/19 {
	local yr = 1997 + `i'
	local preyr = `yr' - 1 
	gen temp = PolRank if PolYrBegin == `yr'
	replace temp = min(temp, minRank`preyr')
	bysort PolID: egen minRank`yr' = min(temp)
	drop temp 
	
	gen NewPower`i'      = PolYrBegin == `yr'
	gen NewPower_TW`i'   = PolYrBegin == `yr' & PolType == "TW"
	gen NewPower_TWa`i'  = PolYrBegin == `yr' & PolType == "TW" & PolRank == 1
	gen NewPower_TWb`i'  = PolYrBegin == `yr' & PolType == "TW" & PolRank == 2
	gen NewPower_CP`i'   = PolYrBegin == `yr' & PolType == "CP"
	gen NewPower_UBND`i' = PolYrBegin == `yr' & PolType == "UBND"
	gen NewPower_QH`i'   = PolYrBegin == `yr' & PolType == "QH" & PolRank != 5 
	
	gen NewPol`i'      = PolYrEnter == PolYrBegin & PolYrBegin == `yr'
	gen NewPol_TW`i'   = PolYrEnter_TW  == PolYrBegin & PolYrBegin == `yr' & PolType == "TW"
	gen NewPol_TWa`i'  = PolYrEnter_TWa == PolYrBegin & PolYrBegin == `yr' & PolType == "TW" & PolRank == 1
	gen NewPol_TWb`i'  = PolYrEnter_TWb == PolYrBegin & PolYrBegin == `yr' & PolType == "TW" & PolRank == 2
	gen NewPol_CP`i'   = PolYrEnter_CP  == PolYrBegin & PolYrBegin == `yr' & PolType == "CP"
	gen NewPol_UBND`i' = PolYrEnter_UBND == PolYrBegin & PolYrBegin == `yr' & PolType == "UBND"
	gen NewPol_QH`i'   = PolYrEnter_QH == PolYrBegin & PolYrBegin == `yr' & PolType == "QH" & PolRank != 5
	
	gen ImprovePower`i'      = PolRank < minRank`preyr' & PolYrBegin == `yr'
	gen ImprovePower_TW`i'   = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "TW"
	gen ImprovePower_TWa`i'  = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "TW" & PolRank == 1
	gen ImprovePower_TWb`i'  = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "TW" & PolRank == 2
	gen ImprovePower_CP`i'   = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "CP"
	gen ImprovePower_UBND`i' = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "UBND"
	gen ImprovePower_QH`i'   = PolRank < minRank`preyr' & PolYrBegin == `yr' & PolType == "QH" & PolRank != 5
	
	gen InPower`i'      = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd)
	gen InPower_TW`i'   = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "TW"
	gen InPower_TWa`i'  = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "TW" & PolRank == 1
	gen InPower_TWb`i'  = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "TW" & PolRank == 2
	gen InPower_CP`i'   = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "CP"
	gen InPower_UBND`i' = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "UBND"
	gen InPower_QH`i'   = (PolYrBegin <= `yr' &  `yr' <= PolYrEnd) & PolType == "QH" & PolRank != 5
	
	gen AfterPower`i'      = PolYrBegin <= `yr'
	gen AfterPower_TW`i'   = PolYrBegin <= `yr' & PolType == "TW"
	gen AfterPower_TWa`i'  = PolYrBegin <= `yr' & PolType == "TW" & PolRank == 1
	gen AfterPower_TWb`i'  = PolYrBegin <= `yr' & PolType == "TW" & PolRank == 2
	gen AfterPower_CP`i'   = PolYrBegin <= `yr' & PolType == "CP"
	gen AfterPower_UBND`i' = PolYrBegin <= `yr' & PolType == "UBND"
	gen AfterPower_QH`i'   = PolYrBegin <= `yr' & PolType == "QH" & PolRank != 5
	
	gen LeftPower`i' = PolYrEnd < `yr'
	gen LeftPower_TW`i' = PolYrEnd < `yr' & PolType == "TW"
	gen LeftPower_TWa`i' = PolYrEnd < `yr' & PolType == "TW" & PolRank == 1
	gen LeftPower_TWb`i' = PolYrEnd < `yr' & PolType == "TW" & PolRank == 2
	gen LeftPower_CP`i' = PolYrEnd < `yr' & PolType == "CP"
	gen LeftPower_UBND`i' = PolYrEnd < `yr' & PolType == "UBND"	
	gen LeftPower_QH`i' = PolYrEnd < `yr' & PolType == "QH" & PolRank != 5 
	
}
drop minRank*

//Reshape to long format (observation unit: PosID -> PosID x Year)
gen temp = _n
reshape long NewPower NewPower_TW NewPower_TWa NewPower_TWb NewPower_CP NewPower_UBND NewPower_QH ///
			 NewPol NewPol_TW NewPol_TWa NewPol_TWb NewPol_CP NewPol_UBND NewPol_QH ///
			 ImprovePower ImprovePower_TW ImprovePower_TWa ImprovePower_TWb ImprovePower_CP ImprovePower_UBND ImprovePower_QH ///
	         InPower InPower_TW InPower_TWa InPower_TWb InPower_CP InPower_UBND InPower_QH ///
			 AfterPower AfterPower_TW AfterPower_TWa AfterPower_TWb AfterPower_CP AfterPower_UBND AfterPower_QH ///
             LeftPower LeftPower_TW LeftPower_TWa LeftPower_TWb LeftPower_CP LeftPower_UBND LeftPower_QH, i(temp) j(Year)
replace Year = 1997 + Year
drop temp

//Create lagged variables
xtset PosIDno Year
foreach j in NewPower NewPol ImprovePower InPower AfterPower LeftPower {
	foreach k in "" _CP _QH _TW _TWa _TWb _UBND {
		gen D`j'`k' = D.`j'`k'
		gen L1`j'`k' = L1.`j'`k'
		gen L2`j'`k' = L2.`j'`k'
	}	
}

//Aggregate variables at district and province levels
foreach i in Dis Pro /*Reg8 Reg3*/ {
	foreach j in NewPower NewPol ImprovePower InPower AfterPower LeftPower {
		foreach k in "" _CP _QH _TW _TWa _TWb _UBND {
			bysort `i'ID Year: egen `i'_`j'`k' = sum(`j'`k')
			bysort `i'ID Year: egen `i'_D`j'`k' = sum(D`j'`k')
			bysort `i'ID Year: egen `i'_L1`j'`k' = sum(L1`j'`k')
			bysort `i'ID Year: egen `i'_L2`j'`k' = sum(L2`j'`k')
		}	
	}	
	egen tag`i'Year = tag(`i'ID Year)
}

save "${root}\Temp Data\PoliticianYearData.dta", replace


*** EXTRACT DISTRICT CONTROLS 1: VHLSS HOUSEHOLD 2002-12 ***********************

use "${root}\Data\VHLSS Household 2002-12.dta", clear
drop if Year == .

egen tag = tag(ComID Year)
bysort DisID Year: egen sumW = sum(weight)
bysort DisID Year: egen sumHHIncW = sum(HHInc * weight)
bysort DisID Year: egen sumHHExpW = sum(HHExp * weight)
bysort DisID Year: egen countCom = sum(tag)
bysort DisID Year: egen countComUrban = sum(tag) if ComUrban == 1  
drop tag

gen Dis_HHInc = sumHHIncW/sumW
gen Dis_HHExp = sumHHExpW/sumW
replace Dis_HHInc = . if Dis_HHInc == 0
replace Dis_HHExp = . if Dis_HHExp == 0 //HHExp for 2012 are all missing
ren countCom Dis_ComCovered
bysort DisID Year: egen Dis_ComUrban = mean(countComUrban/Dis_ComCovered)
replace Dis_ComUrban = 0 if Dis_ComUrban == .

label var Dis_HHInc 		"VHLSS wavg HHInc (th. dongs per HH per year)"
label var Dis_HHExp			"VHLSS wavg HHExp (th. dongs per HH per year)"
label var Dis_ComCovered	"VHLSS number of communes covered by HH surveys" 
label var Dis_ComUrban		"VHLSS share of urban communes" 
 
egen tag = tag(DisID Year)
keep if tag == 1
ren YearVHLSS Year
order DisID Year Dis_HH* Dis_Com*
keep DisID Year Dis*  
save "${root}\Temp Data\DIS VHLSS Household 2002-12.dta", replace

//Extrapolate for 2003, 2005, 2007, 2009, 2011
append using "${root}\Temp Data\DIS VHLSS Household 2002-12.dta"
replace Year = Year - 1 if _n > _N/2
foreach var of varlist Dis_HHInc-Dis_ComUrban {
	replace `var' = . if _n > _N/2
}
xtset DisID Year
gen missing = 1
foreach var of varlist Dis_HHInc-Dis_ComUrban {
	replace `var' = (L1.`var' + F1.`var')/2 if `var' == .
	replace missing = 0 if `var' != .
}
drop if missing
drop missing
save "${root}\Temp Data\DIS VHLSS Household 2002-12.dta", replace


*** EXTRACT DISTRICT CONTROLS 2: VHLSS COMMUNE 2002-12 *************************

use "${root}\Data\VHLSS Commune 2002-12.dta", clear

keep ProID DisID ComID YearVHLSS ComArea ComRemote ComPop ComHhDensity ComPoor ComPovHh ///
     ComHosLg ComPreSchool ComSchool_2 ComSchool_3 ComElectricity ComWaterWet_D ComWaterDry_D ///
	 ComPostOffice ComCulturalCenter ComRadioStation ComIrrigation ComMarket

replace ComHhDensity = (ComPop/ComArea)/4.3 if Year == 2002 //average household size is 4.3
gen ComPovHhShare = ComPovHh/(ComHhDensity * ComArea)
gen ComInfra = ComHosLg + ComPreSchool + ComSchool_2 + ComSchool_3 + ///
               ComElectricity + ComWaterWet_D + ComWaterDry_D + ComIrrigation + ComMarket + ///
		       ComPostOffice + ComCulturalCenter + ComRadioStation 
gen ComInfra_EH = ComHosLg + ComPreSchool + ComSchool_2 + ComSchool_3 
gen ComInfra_P = ComElectricity + ComWaterWet_D + ComWaterDry_D + ComIrrigation + ComMarket
gen ComInfra_C = ComPostOffice + ComCulturalCenter + ComRadioStation
gen ComSurveyed = 1

collapse (mean) ComArea ComRemote ComPop ComHhDensity ComPoor ComPovHhShare ComInfra* ///
         (count) ComSurveyed, by(DisID YearVHLSS)	 
ren YearVHLSS Year
order DisID Year ComSurveyed 

label var ComSurveyed 		"VHLSS number of communes surveyed"
label var ComArea			"VHLSS avg commune area (km2)"
label var ComRemote    		"VHLSS share of remote communes"
label var ComPop			"VHLSS avg commune population (person)"
label var ComHhDensity		"VHLSS avg commune hh density"
label var ComPoor   		"VHLSS share of poor communes"
label var ComPovHhShare     "VHLSS avg poverty hh/total hh"
label var ComInfra			"VHLSS avg total commune infrastructures"
label var ComInfra_EH		"VHLSS avg commune edu/health infrastructures"
label var ComInfra_P		"VHLSS avg commune productive infrastructures"
label var ComInfra_C		"VHLSS avg commune cultural infrastructures"
foreach var of varlist ComSurveyed-ComInfra_C {
	ren `var' Dis_`var'
}
save "${root}\Temp Data\DIS VHLSS Commune 2002-12.dta", replace

//Extrapolate for 2003, 2005, 2007, 2009, 2011
append using "${root}\Temp Data\DIS VHLSS Commune 2002-12.dta"
replace Year = Year - 1 if _n > _N/2
foreach var of varlist Dis_ComSurveyed-Dis_ComInfra_C {
	replace `var' = . if _n > _N/2
}
xtset DisID Year
gen missing = 1
foreach var of varlist Dis_ComSurveyed-Dis_ComInfra_C {
	replace `var' = (L1.`var' + F1.`var')/2 if `var' == .
	replace missing = 0 if `var' != .
}
drop if missing
drop missing
save "${root}\Temp Data\DIS VHLSS Commune 2002-12.dta", replace


*** EXTRACT DISTRICT CONTROLS 3: 671 QUAN HUYEN 2000-04*************************

use "${root}\Data\671QuanHuyen.dta", clear

keep province district disid distype year commune_total area_km2 population density ///
	 worker_pop cereal_ha cerealpercap_kg tuber_ha cane_ha peanut_ha soya_ha tobacco_ha ///
	 jute_ha sedge_ha cotton_ha sesame_ha tea_ha coffee_ha pepper_ha rubber_ha cashew_ha ///
	 forest_ha caredforest_ha buffalo cattle pig meat_ton fishery_ton establishment establishment_s ///
	 establishment_ns establishment_fi output94_mil output94_s_mil output94_ns_mil output94_fi_mil ///
	 output94_bil output94_s_bil output94_ns_bil output94_fi_bil investmentnom_mil investmentnom_bil ///
	 privatesectorworker_pop phonecoverage preschool_school gradeschool_school healthcare_establishment ///
	 electricity_com motorway_com primaryschool_com secondaryschool_com clinic_com revenue_mil expenditure_mil

replace cereal_ha = 0 if cereal_ha == .
replace cerealpercap_kg = 0 if cerealpercap_kg == . 
replace tuber_ha = 0 if tuber_ha == .
replace cane_ha = 0 if cane_ha == .
replace peanut_ha = 0 if peanut_ha == .
replace soya_ha = 0 if soya_ha == .
replace tobacco_ha = 0 if tobacco_ha == .
replace jute_ha = 0 if jute_ha == .
replace sedge_ha = 0 if sedge_ha == .
replace cotton_ha = 0 if cotton_ha == .
replace sesame_ha = 0 if sesame_ha == .
replace tea_ha = 0 if tea_ha == .
replace coffee_ha = 0 if coffee_ha == .
replace pepper_ha = 0 if pepper_ha == .
replace rubber_ha = 0 if rubber_ha == .
replace cashew_ha = 0 if cashew_ha == .
replace forest_ha = 0 if forest_ha == .
replace caredforest_ha = 0 if caredforest_ha == .
replace buffalo = 0 if buffalo == .
replace cattle = 0 if cattle == .
replace pig = 0 if pig == .
replace meat_ton = 0 if meat_ton == .
replace fishery_ton = 0 if fishery_ton == .

gen annual_ha = cane_ha + peanut_ha + soya_ha + tobacco_ha + jute_ha + sedge_ha + cotton_ha + sesame_ha
gen agriculture_ha = cereal_ha + tuber_ha + annual_ha
gen perennial_ha = tea_ha + coffee_ha + pepper_ha + rubber_ha + cashew_ha
replace forest_ha = forest_ha + caredforest_ha  
replace cattle = buffalo + cattle + pig

replace establishment = 0 if establishment == .
replace establishment_s = 0 if establishment_n == . 
replace establishment_ns = 0 if establishment_ns == .
replace establishment_fi = 0 if establishment_fi == .
replace output94_mil = 0 if output94_mil == .
replace output94_s_mil = 0 if output94_s_mil == .
replace output94_ns_mil = 0 if output94_ns_mil == .
replace output94_fi_mil = 0 if output94_fi_mil == .
replace output94_bil = 0 if output94_bil == .
replace output94_s_bil = 0 if output94_s_bil == .
replace output94_ns_bil = 0 if output94_ns_bil == .
replace output94_fi_bil = 0 if output94_fi_bil == .
replace investmentnom_mil = 0 if investmentnom_mil == .
replace investmentnom_bil = 0 if investmentnom_bil == .

gen output94 = output94_mil + output94_bil * 1000
gen output94_S = output94_s_mil + output94_s_bil * 1000
gen output94_NS = output94_ns_mil + output94_ns_bil * 1000
gen output94_FI = output94_fi_mil + output94_fi_bil * 1000
gen investmentnom = investmentnom_mil + investmentnom_bil * 1000
replace investmentnom = . if investmentnom == 0

gen tag = 0
replace tag = 1 if establishment != 0 
gen tag_s = 0
replace tag_s = 1 if establishment_s != 0  
gen tag_ns = 0
replace tag_ns = 1 if establishment_ns != 0
gen tag_fi = 0
replace tag_fi = 1 if establishment_fi != 0
gen tag_sum = tag_s + tag_ns + tag_fi
gen diff = establishment - establishment_s - establishment_ns - establishment_fi
replace establishment = . if tag == 0 & tag_sum == 0
replace establishment = establishment_s + establishment_ns + establishment_fi if tag == 0 & tag_sum > 0
replace establishment_s = . if tag_sum == 0
replace establishment_ns = . if tag_sum == 0
replace establishment_fi = . if tag_sum == 0
drop tag* diff

gen tag = 0
replace tag = 1 if output94 != 0 
gen tag_s = 0
replace tag_s = 1 if output94_S != 0  
gen tag_ns = 0
replace tag_ns = 1 if output94_NS != 0
gen tag_fi = 0
replace tag_fi = 1 if output94_FI != 0
gen tag_sum = tag_s + tag_ns + tag_fi
gen diff = output94 - output94_S - output94_NS - output94_FI
replace output94 = . if tag == 0 & tag_sum == 0
replace output94 = output94_S + output94_NS + output94_FI if tag == 0 & tag_sum > 0
replace output94_S = . if tag_sum == 0
replace output94_NS = . if tag_sum == 0
replace output94_FI = . if tag_sum == 0
drop tag* diff

ren disid 						DisID
ren year 						Year
ren commune_total 				commune				//unit
ren area_km2 					area				//square kilometer
ren worker_pop 					workingpop			//person
ren cerealpercap_kg 			cerealpc			//kilogram
ren establishment_s 			establishment_S		//unit
ren establishment_ns 			establishment_NS	//unit
ren establishment_fi			establishment_FI	//unit	
ren privatesectorworker_pop		workingpop_PS 		//person
ren preschool_school 			preschool			//unit
ren gradeschool_school 			gradeschool			//unit
ren healthcare_establishment	healthcare			//unit	
ren revenue_mil 				revenue				//million dongs
ren expenditure_mil 			expenditure			//million dongs	

gen logPopulation 				= log(population)
gen logWorkingpop 				= log(workingpop)
gen logOutput94 				= log(output94)
gen logOutput94_S 				= log(output94_S)
gen logOutput94_NS 				= log(output94_NS)
gen logOutput94_FI 				= log(output94_FI)
gen logInvestment 				= log(investmentnom)
gen logWorkingpop_PS 			= log(workingpop_PS)
gen logRevenue 					= log(revenue)
gen logExpenditure 				= log(expenditure)

drop cane_ha peanut_ha soya_ha tobacco_ha jute_ha sedge_ha cotton_ha sesame_ha tea_ha ///
     coffee_ha pepper_ha rubber_ha cashew_ha caredforest_ha buffalo pig output94_mil ///
	 output94_s_mil output94_ns_mil output94_fi_mil output94_bil output94_s_bil ///
     output94_ns_bil output94_fi_bil investmentnom_mil investmentnom_bil 
drop if DisID == .
order DisID Year

label var province 				"671 province name"
label var district 				"671 district name"
label var distype 				"671 district type: 1-quan 2-thanh pho 3-thi tran 4-huyen 5-huyen"
label var commune 				"671 number of communes in district"
label var area 					"671 area (km2)"
label var population 			"671 population (person)"
label var density 				"671 population density (person per km2)"
label var workingpop 			"671 working population (person)"
label var cereal_ha 			"671 planted area of cereal crops (ha)"
label var cerealpc 				"671 cereal crops per capital (kg)"
label var tuber_ha 				"671 planted area of tuber crops (ha)"
label var annual_ha 			"671 planted area of annual crops (ha)"
label var agriculture_ha 		"671 planted area of cereal, tuber, and annual crops (ha)"
label var perennial_ha 			"671 planted area of perennial crops (ha)"
label var forest_ha 			"671 forest area (ha)"
label var cattle 				"671 livestock (head)"
label var meat_ton 				"671 livestock output (ton)"
label var fishery_ton 			"671 fishery output (ton)"
label var establishment 		"671 establishments (unit)" 
label var establishment_S 		"671 state-owned establishments (unit)"
label var establishment_NS 		"671 non-state-owned establishments (unit)" 
label var establishment_FI 		"671 foreign-invested estabishments (unit)" 
label var output94 				"671 output value in 94 prices (mill. dongs)" 
label var output94_S 			"671 state-owned output value in 94 prices (mill. dongs)" 
label var output94_NS 			"671 non-state-owned output value in 94 prices (mill. dongs)" 
label var output94_FI 			"671 foreign-invested value in 94 prices (mill. dongs)"  
label var investmentnom 		"671 investment value in current prices (mill. dongs)" 
label var workingpop_PS 		"671 workers in private sectore (person)" 
label var phonecoverage 		"671 phone coverage (lines per 100 persons)" 
label var preschool 			"671 preschools (unit)"  
label var gradeschool 			"671 gradeschools (unit)" 
label var healthcare 			"671 healthcare establishments (unit)"
label var electricity_com 		"671 number of communes with electricity" 
label var motorway_com 			"671 number of communes with motorways"
label var primaryschool_com 	"671 number of communes with primary schools" 
label var secondaryschool_com	"671 number of communes with secondary schools" 
label var clinic_com 			"671 number of communes with clinics" 
label var revenue 				"671 budget revenue (mill. dongs)" 
label var expenditure 			"671 budget expenditure (mill.dongs)"
foreach var of varlist logPopulation-logExpenditure {
	label var `var' "671 log of corresponding value"
}
foreach var of varlist province-logExpenditure {
	ren `var' Dis_`var'
}

save "${root}\Temp Data\DIS 671QuanHuyen.dta", replace


*** EXTRACT PROVINCE CONTROLS 1: VHLSS HOUSEHOLD 2002-12 ***********************

use "${root}\Data\VHLSS Household 2002-12.dta", clear
drop if Year == .

egen tag = tag(ComID Year)
bysort ProID Year: egen sumW = sum(weight)
bysort ProID Year: egen sumHHIncW = sum(HHInc * weight)
bysort ProID Year: egen sumHHExpW = sum(HHExp * weight)
bysort ProID Year: egen countCom = sum(tag)
bysort ProID Year: egen countComUrban = sum(tag) if ComUrban == 1  
drop tag

gen Pro_HHInc = sumHHIncW/sumW
gen Pro_HHExp = sumHHExpW/sumW
replace Pro_HHInc = . if Pro_HHInc == 0
replace Pro_HHExp = . if Pro_HHExp == 0 //HHExp for 2012 are all missing
ren countCom Pro_ComCovered
bysort ProID Year: egen Pro_ComUrban = mean(countComUrban/Pro_ComCovered)
replace Pro_ComUrban = 0 if Pro_ComUrban == .

label var Pro_HHInc 		"VHLSS wavg HHInc"
label var Pro_HHExp			"VHLSS wavg HHExp"
label var Pro_ComCovered	"VHLSS number of communes covered by HH surveys" 
label var Pro_ComUrban		"VHLSS share of urban communes" 
 
egen tag = tag(ProID Year)
keep if tag == 1
ren YearVHLSS Year
order ProID Year Pro_HH* Pro_Com*
keep ProID Year Pro*  
save "${root}\Temp Data\PRO VHLSS Household 2002-12.dta", replace

//Extrapolate for 2003, 2005, 2007, 2009, 2011
append using "${root}\Temp Data\PRO VHLSS Household 2002-12.dta"
replace Year = Year - 1 if _n > _N/2
foreach var of varlist Pro_HHInc-Pro_ComUrban {
	replace `var' = . if _n > _N/2
}
xtset ProID Year
gen missing = 1
foreach var of varlist Pro_HHInc-Pro_ComUrban {
	replace `var' = (L1.`var' + F1.`var')/2 if `var' == .
	replace missing = 0 if `var' != .
}
drop if missing
drop missing
save "${root}\Temp Data\PRO VHLSS Household 2002-12.dta", replace


*** EXTRACT PROVINCE CONTROLS 2: GSO PROVINCIAL DATA****************************

use "${root}\Data\GSO ProvincialData.dta", clear
ren proid ProID
drop province

reshape long income_s healthcare graduate gradeschool preschool retail industry agriculture population /*
*/           budgetrev budgetshare budgetexp budgetadd, i(ProID) j(Year)

ren militaryland 		MilitaryLand 	//square kilometer
ren income_s 			Income_S 		//thousand dongs per month
ren healthcare 			Healthcare 		//unit
ren graduate 			Graduate 		//percent
ren gradeschool 		Gradeschool 	//unit
ren preschool 			Preschool 		//unit
ren retail 				Retail 			//billion dongs
ren industry 			Industry 		//billion dongs
ren agriculture 		Agriculture 	//billion dongs
ren population 			Population 		//thousand persons
ren budgetrev 			BudgetRev 		//million dongs
ren budgetshare 		BudgetShare 	//percent
ren budgetexp 			BudgetExp 		//million dongs
ren budgetadd 			BudgetAdd 		//million dongs

gen logRetail   		= log(Retail * 10^9)
gen logIndustry 		= log(Industry * 10^9)
gen logAgriculture 		= log(Agriculture * 10^9)
gen logPopulation		= log(Population * 10^3)
gen logBudgetRev 		= log(BudgetRev)
gen logBudgetExp 		= log(BudgetExp)
gen logBudgetAdd 		= log(BudgetAdd) 

label var MilitaryLand	"GSO special-use land area (km2)"
label var Income_S 		"GSO public-sector worker income (th. dongs per month)"
label var Healthcare 	"GSO healthcare establishments (unit)"
label var Graduate 		"GSO general-education graduates (percent)" 
label var Preschool 	"GSO preschool (unit)"
label var Gradeschool 	"GSO grade school (unit)"
label var Retail 		"GSO retail sales at current prices (bill. dongs)"
label var Industry 		"GSO industry output value at current prices (bill. dongs)"
label var Agriculture 	"GSO agriculture output value at current prices (bill. dongs)"
label var Population 	"GSO population (thousand persons)"
label var BudgetRev 	"GSO budget revenue (mill. dongs)"
label var BudgetShare 	"GSO share between central and local budget (percent)"
label var BudgetExp 	"GSO budget expenditure (mill. dongs)"
label var BudgetAdd 	"GSO addition from central budget (mill. dongs)"
foreach var of varlist logRetail-logBudgetAdd {
	label var `var' "GSO log of corresponding value"
}
foreach var of varlist MilitaryLand-logBudgetAdd {
	ren `var' Dis_`var'
}
keep if inrange(Year, 1997, 2016)
save "${root}\Temp Data\PRO GSO ProvincialData.dta", replace


*** MERGE DIFFERENT DATASETS TO CREATE WORKING DATA ****************************

//Create master district controls
use "${root}\Temp Data\DIS VHLSS Household 2002-12.dta", clear
merge 1:1 DisID Year using "${root}\Temp Data\DIS VHLSS Commune 2002-12.dta"
drop _m
merge 1:1 DisID Year using "${root}\Temp Data\DIS 671QuanHuyen.dta"
drop _m
save "${root}\Temp Data\DIS All controls.dta", replace

//Create master province controls
use "${root}\Temp Data\PRO VHLSS Household 2002-12.dta", clear
merge 1:1 ProID Year using "${root}\Temp Data\PRO GSO ProvincialData.dta"
drop _m
save "${root}\Temp Data\PRO All controls.dta", replace

//Create working data file at district level
use "${root}\Temp Data\DIS All controls.dta", clear
egen tag = tag(DisID)
keep if tag == 1
keep DisID
save "${root}\Temp Data\Working data.dta", replace
forval i=1/19 {
	append using "${root}\Temp Data\Working data.dta"
}
gen Year = 1997 + floor((_n-1)/(_N/20))
save "${root}\Temp Data\Working data.dta", replace

use "${root}\Temp Data\PoliticianYearData.dta", clear
keep if tagDisYear == 1
keep Year *ID *Name Dis* Pro*
drop Pos* Pol* Com*
drop if (DisID - floor(DisID/100)*100) == 0 //drop undentified districts
merge 1:1 DisID Year using "${root}\Temp Data\Working data.dta"
replace _m = (_m == 3)
ren _m Dis_everhaspol
foreach var of varlist Dis_NewPower-Pro_L2LeftPower_UBND {
	replace `var' = 0 if `var' == .
}
order DisID Year ProID-ProvinceName Dis_everhaspol
merge 1:1 DisID Year using "${root}\Temp Data\DIS All controls.dta"
drop _m
merge 1:1 DisID Year using "${root}\Temp Data\Pro All controls.dta"
drop _m
save "${root}\Temp Data\Working data.dta", replace






