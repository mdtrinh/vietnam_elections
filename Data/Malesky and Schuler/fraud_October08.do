/*Set-Up */
#delimit;
pause off;
clear;
pause;
set mem 15m;
pause;
set more off;
pause;
cd "C:\data\NationalAssembly\LSQ";
pause;
use NA_January10, clear;
pause;
save "fraud_October08",replace;
pause;
log using "October08", replace;

/*Creating final integer*/;
gen percentage_round=round(percentage,.01);
gen percentage_whole=percentage_round*100;
replace percentage_whole=round(percentage_whole);
gen percentage_divide=percentage_whole/10;
gen percentage_floor=floor(percentage_divide);
gen percentage_subtract=percentage_floor*10;
gen final_digit=percentage_whole-percentage_subtract;
replace final_digit=round(final_digit);
pause;

gen fin_int0=0;
gen fin_int1=0;
gen fin_int2=0;
gen fin_int3=0;
gen fin_int4=0;
gen fin_int5=0;
gen fin_int6=0;
gen fin_int7=0;
gen fin_int8=0;
gen fin_int9=0;

replace fin_int0=1 if final_digit==0;
replace fin_int1=1 if final_digit==1;
replace fin_int2=1 if final_digit==2;
replace fin_int3=1 if final_digit==3;
replace fin_int4=1 if final_digit==4;
replace fin_int5=1 if final_digit==5;
replace fin_int6=1 if final_digit==6;
replace fin_int7=1 if final_digit==7;
replace fin_int8=1 if final_digit==8;
replace fin_int9=1 if final_digit==9;

histogram final_digit, discrete yline(.08 .12);
pause;

histogram final_digit, discrete yline(.08 .12) by(centralnominated);
pause;

/*Generate Penultimate digit*/;
generate penultimate_divide=percentage_whole/100;
generate penultimate_floor=floor(penultimate_divide);
gen penultimate_subtract=penultimate_floor*100;
gen penultimate_finaltwo=percentage_whole-penultimate_subtract;
gen penultimate_finaltwox=penultimate_finaltwo-final_digit;
gen penultimate_final=penultimate_finaltwox/10;
gen penultimate_difference=abs(penultimate_final-final_digit);

gen penultimate_differencex=penultimate_difference;
replace penultimate_differencex=3 if penultimate_difference>=2;
histogram penultimate_differencex, discrete by(centralnominated);
pause;



#delimit;
/*Generate Confidence Intervals*/;
display .1-1.96*sqrt(.09/75);
display .1+1.96*sqrt(.09/75);
display .1-1.96*sqrt(.09/418);
display .1+1.96*sqrt(.09/418);


histogram final_digit if centralcom==0, fcolor(green) lcolor(forest_green)  lcolor(navy) discrete ytitle("Frequency of Final Digit", size(large) margin(small)) yline(.07 .13) ylabel(0(.025).2) xtitle("") xlabel(0(1)9) title(Non-Elite(n=418));
graph save finaldig_noccom, replace;
histogram final_digit if centralcom==1, fcolor(pink) lcolor(maroon) discrete ytitle("Frequency of Final Digit", size(large) margin(medlarge)) yline(.03 .17) ylabel(0(.025).2) xtitle("") xlabel(0(1)9) title(Central Comm.(n=75));
graph save finaldig_ccom, replace;
histogram final_digit,  fcolor(bluishgray) lcolor(navy) discrete ytitle("Frequency of Final Digit", size(large) margin(medsmall)) yline(.075 .125) ylabel(0(.025).2) xtitle("") xlabel(0(1)9) title(All(n=493));
graph save finaldig_all, replace;
gr combine finaldig_all.gph finaldig_noccom.gph finaldig_ccom.gph, col(3) title("Final Digit", size(large) position(6));
graph save VNfraud_combined, replace;




