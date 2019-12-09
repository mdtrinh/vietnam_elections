#delimit;
cd "C:\Users\ejm5\Dropbox\HardDrive_Latitude_08142015\data\NationalAssembly\LSQ";
version 10;
clear;
set mem 500m;
use NA_January10, replace;
set more off;
log using NA_signallingJan10, replace;

drop candidates_district;
drop  b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b73 b74 b75;
drop prob;
drop s_trans;

gen candidates_district=1 if candidates_seat<2;
replace candidates_district=0 if candidates_seat>=2 & candidates_seat !=.;

replace birthyear=1967 if stt==2;
replace age=40 if stt==2;




#delimit;
xi: dprobit candidates_district centralnom polit, robust;
mfx;
outreg2 using NA_signalling,  e(all) replace;

xi: dprobit candidates_district centralnom polit male minority religion degree age que, robust;
outreg2 using NA_signalling,  e(all);

xi: reg candidates_district centralnom polit male minority religion degree age que i.prov_id, robust;
outreg2 using NA_signalling,  e(all);

xi: dprobit candidates_district centralcom polit, robust;
outreg2 using NA_signalling, e(all);

xi: dprobit candidates_district centralcom polit male minority religion degree age que, robust;
outreg2 using NA_signalling,  e(all);

xi: reg candidates_district centralcom polit male minority religion degree age que i.prov_id, robust;
outreg2 using NA_signalling,  e(all) excel;


#delimit;
xi: reg power_total centralnominated polit, robust;
outreg2 using NA_signalling2,  e(all) replace;
xi: reg power_total centralnominated polit male minority religion degree age que, robust;
outreg2 using NA_signalling2,  e(all);
xi: reg power_total centralnominated polit i.prov_id male minority religion degree age que, robust;
outreg2 using NA_signalling2,  e(all);
xi: reg power_total centralcom polit, robust;
outreg2 using NA_signalling2,  e(all);
xi: reg power_total centralcom polit i.male minority religion degree age que, robust;
outreg2 using NA_signalling2,  e(all);
xi: reg power_total centralcom polit i.prov_id male minority religion degree age que, robust;
outreg2 using NA_signalling2,  e(all) excel;




xi: estsimp poisson power_total centralcom polit i.prov_id male minority religion degree age que, robust;
setx centralcom 0 polit 0;
simqi;
setx centralcom 1 polit 0;
simqi;
setx centralcom 1 polit 1;
simqi;
drop b1-b72;

xi: estsimp poisson power_total centralcom polit localleg localpcom localpar i.prov_id male minority religion degree age que, robust;


count if centralcom==1 & polit==0 & localleg==0 & localpcom==0 & localpar==0;
setx centralcom 1 polit 0 localleg 0 localpcom 0 localpar 0;
simqi;

count if centralcom==1 & polit==1 & localleg==0 & localpcom==0 & localpar==0;
setx centralcom 1 polit 1 localleg 0 localpcom 0 localpar 0;
simqi;

count if centralcom==1 & polit==0 & localleg==1 & localpcom==0 & localpar==0;
setx centralcom 1 polit 0 localleg 1 localpcom 0 localpar 0;
simqi;

count if centralcom==1 & polit==0 & localleg==1 & localpcom==0 & localpar==1;
setx centralcom 1 polit 0 localleg 1 localpcom 0 localpar 1;
simqi;

count if centralcom==1 & polit==0 & localleg==1 & localpcom==1 & localpar==1;
setx centralcom 1 polit 0 localleg 1 localpcom 1 localpar 1;
simqi;

count if centralcom==1 & polit==0 & localleg==0 & localpcom==0 & localpar==1;
setx centralcom 1 polit 0 localleg 0 localpcom 0 localpar 1;
simqi;

count if centralcom==0 & polit==0 & localleg==0 & localpcom==0 & localpar==0;
setx centralcom 0 polit 0 localleg 0 localpcom 0 localpar 0;
simqi;

count if centralcom==0 & polit==0 & localleg==1 & localpcom==0 & localpar==0;
setx centralcom 0 polit 0 localleg 1 localpcom 0 localpar 0;
simqi;

count if centralcom==0 & polit==0 & localleg==1 & localpcom==1 & localpar==0;
setx centralcom 0 polit 0 localleg 1 localpcom 1 localpar 0;
simqi;

count if centralcom==0 & polit==0 & localleg==1 & localpcom==1 & localpar==1;
setx centralcom 0 polit 0 localleg 1 localpcom 1 localpar 1;
simqi;

count if centralcom==0 & polit==0 & localleg==0 & localpcom==1 & localpar==0;
setx centralcom 0 polit 0 localleg 0 localpcom 1 localpar 0;
simqi;

count if centralcom==0 & polit==0 & localleg==0 & localpcom==0 & localpar==1;
setx centralcom 0 polit 0 localleg 0 localpcom 0 localpar 1;
simqi;

count if centralcom==0 & polit==0 & localleg==1 & localpcom==0 & localpar==1;
setx centralcom 0 polit 0 localleg 1 localpcom 0 localpar 1;
simqi;

count if centralcom==0 & polit==0 & localleg==0 & localpcom==1 & localpar==1;
setx centralcom 0 polit 0 localleg 0 localpcom 1 localpar 1;
simqi;

drop b1-b72;


#delimit;
/*Baseline*/

dprobit result power_total candidates_district, robust;
outreg2 using NA_signallingbase, e(all) replace;

reg percentage power_total candidates_district, robust;
outreg2 using NA_signallingbase, e(all);

/*Demographic*/;

dprobit result power_total candidates_district male minority religion age degree que_districtsame, robust;
mfx;
outreg2 using NA_signallingbase, e(all);

reg percentage power_total candidates_district male minority religion age degree que_districtsame, robust;
outreg2 using NA_signallingbase, e(all);


/*Strength of Opposition*/;

xi: dprobit result power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business, robust;
outreg2 using NA_signallingbase, e(all);

xi: dprobit result power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature business i.pci_id, robust;
outreg2 using NA_signallingbase, e(all);

/*Transfers*/
xi: dprobit result power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature business south trans_rev ln_gdpcap secondary  if centralnom==1, robust cluster(pci_id);
outreg2 using NA_signallingbase, e(all);

 
reg percentage power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business, robust;
outreg2 using NA_signallingbase, e(all);

xi: reg percentage power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business i.prov_id, robust;
outreg2 using NA_signallingbase, e(all) ;

xi: reg percentage power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business politburo centralcomm centralnom i.prov_id, robust;
outreg2 using NA_signallingbase, e(all) ;

xi: reg percentage power_total candidates_district male minority religion age degree que_districtsame party  incumbencyterms locallegislature  business south trans_rev ln_gdpcap secondary  if centralnom==1, robust cluster(pci_id);
outreg2 using NA_signallingbase, e(all) excel;



#delimit;
/*Tranfers*/ 

xi: probit result candidates_district power_total party incumbency  male  religion age degree south trans_rev  if centralnom==1,  robust cluster(pci_id);
outreg2 using signalling_provincial, e(all) replace;


xi: probit result candidates_district power_total party incumbency  male  religion age degree south trans_rev    if centralnom==1, robust cluster (pci_id);
outreg2 using signalling_provincial, e(all);


xi: probit result candidates_district power_total party incumbency  male  religion age degree south trans_rev   ln_gdpcap secondary  if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);

xi: probit result candidates_district power_total party incumbency  business male  religion age degree south trans_rev  ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);

predict prob, p;
postgr3 trans_rev, by(south)  xtitle("") ytitle("") legend(rows(2) label(1 North) label(2 South) position(5) ring(0))  
clpattern (l "__" ".-") clwidth("vthick" "vthick") clcolor("maroon" "navy") title("Smoothed", size(vlarge))  
xlab(0(100)600) xline(100 200,  lwidth(medthin) lpattern(dash) lcolor(black));

graph save pred_prob2.gph, replace;


twoway   (scatter prob trans_rev if centralnom==1 & south==0  & trans_rev<400, msize(medlarge) msymbol(square) mcolor(maroon))    
(scatter prob trans_rev if centralnom==1 & south==1  & trans_rev<400, msize(medlarge) msymbol(diamond) mcolor(navy)), 
xtitle("") ytitle("Predicted Probability of Election", size(vlarge))
legend(rows(2) label(1 North) label(2 South) position(5) ring(0)) title("Individual Delegates", size(vlarge)) xlab(0(100)600);

graph save pred_prob1.gph, replace;

graph combine pred_prob1.gph pred_prob2.gph, xcommon title("Transfers as a Percentage of Local Revenue", size(large) position(6));

graph save pred_prob_comb.gph, replace;

xi: probit result candidates_district power_total party incumbency  business male  religion age degree i.south*trans_rev  ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);


xi: reg percentage candidates_district power_total party incumbency   male  religion age degree south trans_rev  if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);


xi: reg percentage  candidates_district power_total party incumbency   male  religion age degree south trans_rev   if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);


xi: reg percentage  candidates_district power_total party incumbency   male  religion age degree south trans_rev  ln_gdpcap secondary  if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all);

xi: reg percentage  candidates_district power_total party incumbency male  religion age degree south trans_rev  ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all) excel;

postgr3 trans_rev, by(south);

xi: reg percentage  candidates_district power_total party incumbency male  religion age degree i.south*trans_rev  ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using signalling_provincial, e(all) excel;

#delimit;
twoway   (scatter prob trans_rev if centralnom==1 & south==0  & trans_rev<400, msize(large) msymbol(diamond) mcolor(maroon))    
(scatter prob trans_rev if centralnom==1 & south==1  & trans_rev<400, msize(large) msymbol(square) mcolor(navy)), 
xtitle("Transfers as a Percentage of Local Revenue", size(large)) 
legend(rows(2) label(1 South) label(2 North) position(5) ring(0));

graph save pred_prob.gph, replace;

#delimit;
xi: dprobit candidates_district  male  religion degree age  south trans_rev ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using difficulty_provincial, e(all) replace;
xi: dprobit candidates_district  male religion degree age  south trans_rev ln_gdpcap secondary  g5_new tel_cap06 if centralcom==1, robust cluster(pci_id);
outreg2 using difficulty_provincial, e(all);
xi: reg power_total male religion degree age  south trans_rev ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust cluster(pci_id);
outreg2 using difficulty_provincial, e(all);
xi: reg power_total male  religion degree age  south trans_rev ln_gdpcap secondary  g5_new tel_cap06 if centralcom==1, robust cluster(pci_id);
outreg2 using difficulty_provincial, e(all) excel;



#delimit;
xi: estsimp probit result candidates_district power_total party incumbency  male  religion age degree i.south*trans_rev ln_gdpcap secondary  g5_new tel_cap06 if centralnom==1, robust;

foreach num of numlist 0(50)500 {;
setx  _Isouth_1 0  _Iso1Xtr 0  trans_rev `num';
simqi;
};

foreach num of numlist 0(50)500 {;
setx _Isouth_1 0  _Iso1Xtr  1*`num'  trans_rev `num';
simqi;
};

generate s_trans=south*trans_rev;

sum  percentage result power_total  candidates_district centralnom centralcom politburo male minority religion age degree 
que_districtsame party  incumbencyterms locallegislature  business south trans_rev s_trans ln_gdpcap secondary  g5_new tel_cap06 if stt!=.;


sum  percentage result power_total  candidates_district centralnom centralcom politburo male minority religion age degree
 que_districtsame party  incumbencyterms locallegislature  business south trans_rev s_trans ln_gdpcap secondary  
g5_new tel_cap06 if centralnom==1 & stt !=.;

pwcorr percentage result power_total  candidates_district centralnom centralcom politburo 
male minority religion age degree que_districtsame party  incumbencyterms locallegislature  
business south trans_rev s_trans ln_gdpcap secondary  g5_new tel_cap06 if stt!=., star(5); 

save NA_November2010, replace;
log close;
