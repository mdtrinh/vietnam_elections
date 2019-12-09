
cd "C:\data\NationalAssembly\LSQ"
use NA_November2010.dta, clear
set more off

drop b1 b2 b3  b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b73 b74 b75

drop if prov_id==.

set seed 08272007
by prov_id, sort: generate random=uniform()
sort  prov_id random
 
 
gsort -centralnom  random
generate centralnom_id=_n
tabstat  centralnominated if  centralnominated ==1, by(prov_id) stat(n)
list stt centralnominated random centralnom_id in 1/5

generate prov_id2=prov_id

/*Assign Central Nominees to a Province*/
replace prov_id2=1 if centralnom_id <=7
replace prov_id2=2 if centralnom_id >7 & centralnom_id <=11
replace prov_id2=3 if centralnom_id >11 & centralnom_id <=13
replace prov_id2=4 if centralnom_id >13 & centralnom_id <=21
replace prov_id2=5 if centralnom_id >21 & centralnom_id <=23
replace prov_id2=6 if centralnom_id >23 & centralnom_id <=29
replace prov_id2=7 if centralnom_id >29 & centralnom_id <=34
replace prov_id2=8 if centralnom_id >34 & centralnom_id <=36 
replace prov_id2=9 if centralnom_id >36 & centralnom_id <=38 
replace prov_id2=10 if centralnom_id >38 & centralnom_id <=40
replace prov_id2=11 if centralnom_id >40 & centralnom_id <=42  
replace prov_id2=12 if centralnom_id >42 & centralnom_id <=45  
replace prov_id2=13 if centralnom_id >45 & centralnom_id <=47  
replace prov_id2=14 if centralnom_id >47 & centralnom_id <=50  
replace prov_id2=15 if centralnom_id >50 & centralnom_id <=52  
replace prov_id2=16 if centralnom_id >52 & centralnom_id <=54
replace prov_id2=17 if centralnom_id >54 & centralnom_id <=57  
replace prov_id2=18 if centralnom_id >57 & centralnom_id <=61
replace prov_id2=19 if centralnom_id >61 & centralnom_id <=64
replace prov_id2=20 if centralnom_id >64 & centralnom_id <=66
replace prov_id2=21 if centralnom_id >66 & centralnom_id <=68
replace prov_id2=22 if centralnom_id >68 & centralnom_id <=71
replace prov_id2=23 if centralnom_id >71 & centralnom_id <=73 
replace prov_id2=24 if centralnom_id >73 & centralnom_id <=75
replace prov_id2=25 if centralnom_id >75 & centralnom_id <=77
replace prov_id2=26 if centralnom_id >77 & centralnom_id <=79
replace prov_id2=27 if centralnom_id >79 & centralnom_id <=81
replace prov_id2=28 if centralnom_id >81 & centralnom_id <=83
replace prov_id2=29 if centralnom_id >83 & centralnom_id <=85 



replace prov_id2=30 if centralnom_id >85 & centralnom_id <=88 
replace prov_id2=31 if centralnom_id >88 & centralnom_id <=90
replace prov_id2=32 if centralnom_id >90 & centralnom_id <=92
replace prov_id2=33 if centralnom_id >92 & centralnom_id <=94
replace prov_id2=34 if centralnom_id >94 & centralnom_id <=96
replace prov_id2=35 if centralnom_id >96 & centralnom_id <=98

replace prov_id2=37 if centralnom_id >98 & centralnom_id <=100
replace prov_id2=39 if centralnom_id >100 & centralnom_id <=104 


replace prov_id2=40 if centralnom_id >104 & centralnom_id <=106  
replace prov_id2=42 if centralnom_id >106 & centralnom_id <=110 
replace prov_id2=43 if centralnom_id >110 & centralnom_id <=113
replace prov_id2=45 if centralnom_id >113 & centralnom_id <=115
replace prov_id2=47 if centralnom_id >115 & centralnom_id <=118
replace prov_id2=48 if centralnom_id >118 & centralnom_id <=120


replace prov_id2=51 if centralnom_id >120 & centralnom_id <=123
replace prov_id2=53 if centralnom_id >123 & centralnom_id <=125
replace prov_id2=54 if centralnom_id >125 & centralnom_id <=127
replace prov_id2=55 if centralnom_id >127 & centralnom_id <=130
replace prov_id2=56 if centralnom_id >130 & centralnom_id <=132


replace prov_id2=57 if centralnom_id >132 & centralnom_id <=135

replace prov_id2=58 if centralnom_id >135 & centralnom_id <=137
replace prov_id2=59 if centralnom_id >137 & centralnom_id <=139

replace prov_id2=60 if centralnom_id >139 & centralnom_id <=141
replace prov_id2=62 if centralnom_id >141 & centralnom_id <=143
replace prov_id2=63 if centralnom_id >143 & centralnom_id <=145
replace prov_id2=64 if centralnom_id >145 & centralnom_id <=147
replace prov_id2=65 if centralnom_id >147 & centralnom_id <=149
replace prov_id2=66 if centralnom_id >149 & centralnom_id <=151
replace prov_id2=67 if centralnom_id >151 & centralnom_id <=153
replace prov_id2=68 if centralnom_id >153 & centralnom_id <=155
replace prov_id2=69 if centralnom_id >155 & centralnom_id <=157


replace prov_id2=70 if centralnom_id >157 & centralnom_id <=159
replace prov_id2=71 if centralnom_id >159 & centralnom_id <=161
replace prov_id2=72 if centralnom_id >161 & centralnom_id <=163
replace prov_id2=73 if centralnom_id >163 & centralnom_id <=165


by prov_id2, sort: egen number_candidates=count( stt)
tab number_candidates

 
 
by prov_id2, sort: generate random2=uniform()
sort  prov_id2 random2
by prov_id2, sort: generate random_id=_n


/*10 Person Electoral District*/
generate random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==10
replace random_district_id=(prov_id2*10)+2 if random_id>5 & number_candidates==10

/*11 Person Electoral District*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==11
replace random_district_id=(prov_id2*10)+2 if random_id>5 & number_candidates==11

/*13 Person Electoral Distrcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==13
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<10 & number_candidates==13
replace random_district_id=(prov_id2*10)+3 if random_id>=10 & number_candidates==13

/*14 Person Electoral Distrcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==14
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==14
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & number_candidates==14

/*15 Person Electoral Distrcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==15
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==15
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & number_candidates==15

/*18 Person Electoral Distrcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=6 & number_candidates==18
replace random_district_id=(prov_id2*10)+2 if random_id>6 & random_id<13 & number_candidates==18
replace random_district_id=(prov_id2*10)+3 if random_id>=13 & number_candidates==18


/*20 Person Electoral Distrcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==20
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==20
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & random_id<16 & number_candidates==20
replace random_district_id=(prov_id2*10)+4 if random_id>=16 & number_candidates==20


/*25 Person Electoral District*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==25
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==25
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & random_id<16 & number_candidates==25
replace random_district_id=(prov_id2*10)+4 if random_id>=16 & random_id<21 & number_candidates==25
replace random_district_id=(prov_id2*10)+5 if random_id>=21 & number_candidates==25


/*29 Person Electoral Disrtcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==29
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==29
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & random_id<16 & number_candidates==29
replace random_district_id=(prov_id2*10)+4 if random_id>=16 & random_id<21 & number_candidates==29
replace random_district_id=(prov_id2*10)+5 if random_id>=21 & random_id<26 & number_candidates==29
replace random_district_id=(prov_id2*10)+6 if random_id>=26 & number_candidates==29


/*37 Person Electoral Disrtcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==37
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==37
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & random_id<16 & number_candidates==37
replace random_district_id=(prov_id2*10)+4 if random_id>=16 & random_id<21 & number_candidates==37
replace random_district_id=(prov_id2*10)+5 if random_id>=21 & random_id<26 & number_candidates==37
replace random_district_id=(prov_id2*10)+6 if random_id>=26 & random_id<32 & number_candidates==37
replace random_district_id=(prov_id2*10)+7 if random_id>=32 & number_candidates==37


/*51 Person Electoral Disrtcit*/
replace random_district_id=(prov_id2*10)+1 if random_id<=5 & number_candidates==51
replace random_district_id=(prov_id2*10)+2 if random_id>5 & random_id<11 & number_candidates==51
replace random_district_id=(prov_id2*10)+3 if random_id>=11 & random_id<16 & number_candidates==51
replace random_district_id=(prov_id2*10)+4 if random_id>=16 & random_id<21 & number_candidates==51
replace random_district_id=(prov_id2*10)+5 if random_id>=21 & random_id<26 & number_candidates==51
replace random_district_id=(prov_id2*10)+6 if random_id>=26 & random_id<31 & number_candidates==51
replace random_district_id=(prov_id2*10)+7 if random_id>=31 & random_id<36 & number_candidates==51
replace random_district_id=(prov_id2*10)+8 if random_id>=36 & random_id<41 & number_candidates==51
replace random_district_id=(prov_id2*10)+9 if random_id>=41 & random_id<46 & number_candidates==51
replace random_district_id=(prov_id2*10)+10 if random_id>=46 & number_candidates==51


/*Check Coding*/
tab random_district_id
codebook random_district_id



/*Generate New Power-Ranking*/
#delimit;

/*Create Power Measures*/
pause;

by random_district_id, sort: egen NEWpower_nom= sum( centralnominated);
replace NEWpower_nom=NEWpower_nom-centralnominated;
lab var NEWpower_nom "Other Central Nominees in District";

by random_district_id, sort: egen NEWpower_bct= sum(politburo);
replace NEWpower_bct=NEWpower_bct-politburo;
lab var NEWpower_bct "Other Politburo Members in District";

replace centralcommittee=0 if id==182;
replace centralcommittee=0 if id==470;
replace centralcommittee=0 if id==340;

by random_district_id, sort: egen NEWpower_ccom= sum(centralcommittee);
replace NEWpower_ccom=NEWpower_ccom-centralcommittee;
lab var NEWpower_ccom "Other CCOM Members in District";

by random_district_id, sort: egen NEWpower_incum= sum(incumbency);
replace NEWpower_incum=NEWpower_incum-incumbency;
lab var NEWpower_incum "Other Incumbents in District";

by random_district_id, sort: egen NEWpower_bttu= sum(localpartysecretariat);
replace NEWpower_bttu=NEWpower_bttu-localpartysecretariat;
lab var NEWpower_bttu "Other Members of Party Secretariat in District";

by random_district_id, sort: egen NEWpower_pcom= sum(localpcom);

replace NEWpower_pcom=NEWpower_pcom-localpcom;
lab var NEWpower_pcom "Other Members of PCOM in District";

by random_district_id, sort: egen NEWpower_hdnd= sum(locallegislature);
replace NEWpower_hdnd=NEWpower_hdnd-locallegislature;
lab var NEWpower_hdnd "Other Members of People's Council in District";

by random_district_id, sort: egen NEWpower_partyyears= mean(years_party);
replace NEWpower_partyyears=years_party-NEWpower_partyyears;
lab var NEWpower_partyyears "Candidate in party longer than average competitor";
pause;



/*Added one to power rankings because some of the central members had a zero ranking already*/;
generate NEWpower_total=1+NEWpower_nom+NEWpower_bct+NEWpower_ccom+NEWpower_incum+NEWpower_bttu+NEWpower_pcom+NEWpower_hdnd;
pause;


pwcorr NEWpower_total power_total, star(5);
pwcorr NEWpower* power*, star(5);



/*************************************************GENERATE CANDIDATE SEAT RATIO******************************************************/
#delimit;
by  random_district_id, sort: egen NEWcandidates=count(stt);
tab NEWcandidates;

generate NEWeasydistrict=1 if  NEWcandidates==5;
replace NEWeasydistrict =0 if  NEWcandidates !=5;



/**********************************************RE-RUN SELECTION ANALYSIS***************************************************************/
#delimit;
xi3: dprobit NEWeasydistrict centralnominated polit, robust;
mfx;
outreg2 using NA_signallingNEW,  e(all) replace;

xi3: dprobit NEWeasydistrict centralnominated polit male minority religion degree age que, robust;
outreg2 using NA_signallingNEW,  e(all);

xi3: reg NEWeasydistrict centralnominated polit male minority religion degree age que i.prov_id2, robust;
outreg2 using NA_signallingNEW,  e(all);

xi3: dprobit NEWeasydistrict centralcom polit, robust;
outreg2 using NA_signallingNEW, e(all);

xi3: dprobit NEWeasydistrict centralcom polit male minority religion degree age que, robust;
outreg2 using NA_signallingNEW,  e(all);

xi3: reg NEWeasydistrict centralcom polit male minority religion degree age que i.prov_id2, robust;
outreg2 using NA_signallingNEW,  e(all) excel;

#delimit;

xi3: reg NEWpower_total centralnominated polit, robust;
outreg2 using NA_signallingNEW2, e(all) replace;

xi3: reg NEWpower_total centralnominated polit male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all);

xi3: reg NEWpower_total centralnominated polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all);

xi3: reg NEWpower_total centralnominated polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all) ci;

xi3: reg power_total centralnominated polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all) ci;

xi3: reg NEWpower_total centralcom polit, robust;
outreg2 using NA_signallingNEW2, e(all);

xi3: reg NEWpower_total centralcom polit male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all);

xi3: reg NEWpower_total centralcom polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all);

xi3: reg NEWpower_total centralcom polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all) ci;

xi3: reg power_total centralcom polit i.prov_id2 male minority religion degree age que, robust;
outreg2 using NA_signallingNEW2, e(all) excel ci;


ttest power_total == NEWpower_total if centralnominated==1, unpaired;
ttest power_total == NEWpower_total if centralcom==1, unpaired;
ttest power_total == NEWpower_total if politburo==1, unpaired;

#delimit;
ttest power_total, by(centralnominated);
ttest NEWpower_total, by(centralnominated);

ttest power_total, by(centralcom);
ttest NEWpower_total, by(centralcom);

ttest power_total, by(politburo);
ttest NEWpower_total, by(politburo);


/************************************************Election Results******************************************************************/
#delimit;
xi3: reg percentage NEWpower_total NEWeasydistrict male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business i.prov_id2, robust;
outreg2 using NA_signallingbaseNEW, e(all) replace ;

xi3: dprobit result NEWpower_total NEWeasydistrict male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business i.prov_id2, robust;
outreg2 using NA_signallingbaseNEW, e(all)  excel ;


#delimit;
estsimp probit result power_total;

foreach num of numlist 1(1)11 {;
setx  `num';
simqi;
};

drop b1 b2;


estsimp probit result NEWpower_total;

foreach num of numlist 1(1)11 {;
setx  `num';
simqi;
};

drop b1 b2;


save NA_November2010_v2.dta, replace

#delimit;
use "C:\data\NationalAssembly\LSQ\random.dta", clear;


twoway  (rcap lo hi power_total if random==0, lcolor(navy) lwidth(medthick)) 
(rcap lo hi power_total if random==1, lcolor(maroon) lwidth(medthick)) 
(scatter  mean  power_total if  random==0, mcolor(navy) msymbol(diamond) msize(medium)) 
(scatter  mean  power_total if  random==1, mcolor(maroon) msymbol(square) msize(medium)), 
xlab(1(1)11) xtitle("Competitiveness of District", size(medium) margin(medium)) 
ytitle("Probability of Victory", size(medium) margin(medium)) legend(rows(4) 
label(1 95% CI) label (2 95% CI) label(3 Actual Candidate Assignment) label(4 Random Candidate Assignment) size(vsmall) position(1) ring(0));

graph save random_assignment.gph, replace
