#delimit;
cd "C:\data\NationalAssembly\LSQ";
clear;
set mem 500m;
use NA_January10, clear;


gen candidates_district=0;
replace candidates_district=1 if power_districtsize==4 | power_districtsize==6;
generate nominate=1 if  selfnominated==1;
replace nominate=2 if   centralnominated==1;
replace nominate=3 if   centralnominated==0 & selfnominated==0;
codebook nominate;
replace nominate=3 if nominate==.;

save NA_graphics, replace;

collapse percentage result candidates_district power_total, by(career);
generate id=_n;
sort id;
save NA_career, replace;

use NA_graphics, replace;
collapse percentage result candidates_district power_total, by(nominate);
generate id=18+nominate;
sort id;
save NA_nominate, replace;

merge id using NA_career.dta;
save NA_career2, replace;

lab var id "Primary Workplace";
label define id 1 "Central Party";
label define id 2 "Central Government", add;
label define id 3 "Local Party", add;
label define id 4 "Local Government", add;
label define id 5 "Central SOE", add;
label define id 6 "Local SOE", add;
label define id 7 "Private Company/Farmer", add;
label define id 8 "University/High School", add;
label define id 9 "Medicine", add;
label define id 10 "Mass Organization", add;
label define id 11 "Military", add;
label define id 12 "Business Association/VICOOPSME", add;
label define id 13 "Journalist", add;
label define id 14 "Lawyer", add;
label define id 15 "Cultural Institution", add;
label define id 16 "Not Working/Student", add;
label define id 17 "Religious Organization", add;
label define id 18 "Research Institute", add;
label define id 19 "Self Nominee", add;
label define id 20 "Central Nominee", add;
label define id 21 "Local Nominee", add;
label values id id;
pause;

replace result=result*100;

#delimit;
twoway (lfitci  result power_total if career<19) (scatter   result power_total if id<16, mlabel(id) mlabposition(3) mlabsize(vsmall) 
mlabcolor (black) msymbol(triangle) mcolor(black) msize(small)) (scatter   result power_total if id==16, mlabel(id) mlabposition(9) mlabsize(vsmall) 
mlabcolor (black) msymbol(triangle) mcolor(black) msize(small)) (scatter   result power_total if id>16& id<19, mlabel(id) mlabposition(9) mlabsize(vsmall) 
mlabcolor (black) msymbol(triangle) mcolor(black) msize(small)), xtitle("") 
ytitle("Percentage of Candidates Elected to NA", size(medlarge) margin(medsmall)) ylab(0(25)100) title("By Career", size(large)) legend(off);

graph save power_career.gph, replace;


twoway (lfit result power_total if career>=19) (scatter   result power_total if id>=19, mlabel(id) mlabposition(3) mlabsize(medsmall) 
mlabcolor (navy) msymbol(triangle) mcolor(navy) msize(medium)), xtitle("") 
ytitle("Percentage of Candidates Elected to NA", size(medlarge) margin(medsmall))legend(off) ylab(0(25)100) title("By Nomination Level", size(large));

graph save power_nom.gph, replace;

graph combine power_career.gph power_nom.gph, xcommon title("Competitiveness of District", size(medium) position(6)) ;

graph save power_final.gph, replace;


twoway (lfitci  result candidates_district if career<19) (scatter   result candidates_district if id<19, mlabel(id) mlabposition(3) mlabsize(tiny) 
mlabcolor (black) msymbol(diamond) mcolor(black) msize(vsmall)), xtitle (Percentage of Candidates in 5/3 Districts ) 
ytitle (Percentage of Candidates Elected to NA) legend(off) ylab(0(50)100);

graph save seats_career.gph, replace;


twoway (lfitci  result candidates_district if career>=19) (scatter   result candidates_district if id>=19, mlabel(id) mlabposition(3) mlabsize(tiny) 
mlabcolor (black) msymbol(diamond) mcolor(black) msize(vsmall)), xtitle (Percentage of Candidates in 5/3 Districts ) 
ytitle (Percentage of Candidates Elected to NA) legend(off) ylab(0(50)100);

graph save seats_nom.gph, replace;

graph combine power_career.gph power_nom.gph seats_career.gph seats_nom.gph, ycommon;




