*-------------------------------------------------------*
* PROGRAMMER:  		Michael Brumm  						*
*														*
* DATE CREATED: 	February 8, 2023	           	 	*
*                                                     	*
* FILE NAME:		Table 1.sas  						*
* 					  									*
*                                                     	*
* DESCRIPTION:      SAS code used to perform analyses 	*
* 					reported in Table 1.				*
* 														*
* CREDIT:												*
*                                                     	*
* INPUT DATA SETS: 	UPSIT Percentiles Data Cut.csv		*
*                                                     	*
* OUTPUT DATA SETS:   									* 
*					  									*
*-------------------------------------------------------*;

*import 'UPSIT Percentiles Data Cut.csv' data file and name the output SAS data set 'parsppmi';
PROC IMPORT OUT= WORK.PARSPPMI 
			DATAFILE= "I:\PPMI\Analysis\Analysis Calls\UPSIT Percentiles\Zenodo\Datasets\UPSIT Percentiles Data Cut.csv"
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

*use %INCLUDE statement to execute the 'SAS Macros.sas' program;
%include '<insert pathname>\SAS Macros.sas';

*group participants by sex;
data popn1;
	set parsppmi;
	if sex = 2 then group = 1; *combined females;
	if sex = 1 then group = 4; *combined males; 
run;

*duplicate 'popn1' table and and group participants by cohort and sex;
data popn2;
	set popn1;
	*females;
	if sex = 2 then do;
		if cohort = 1 then group = 2; *PARS females;
		if cohort = 2 then group = 3; *PPMI females;
	end;
	*males;
	if sex = 1 then do;
		if cohort = 1 then group = 5; *PARS males;
		if cohort = 2 then group = 6; *PPMI males;
	end;
run;

*combine the 'popn1' and 'popn2' tables;
data popn;
	set popn1 popn2;
run;

*compute descriptive statistics;
%cont(data = popn, var = age,   	 	dec = 1, dec2 = 1, num = 1); *Age;
%cat (data = popn, var = agecat,   	 				   	   num = 2); *Age category;
%cat (data = popn, var = race_white, 	 				   num = 3); *Race (White);
%cat (data = popn, var = ethnicity, 	dec = 1,   		   num = 4); *Ethnicity (Hispanic);
%cat (data = popn, var = famhxpd, 						   num = 5); *1st-degree relative(s) with PD;

*for PARS subgroups only, compute descriptive statistics for smoking status;
%cat (data = popn(where=(group IN (2 5))), var = smoking,  num = 6); *Smoking status;

*compute p values;
%ttest(data = popn, var = age,   		num = 1,  main = 1,    order = 0);
%chisq(data = popn, var = agecat, 		num = 2,  main = 1.5,  order = 0);
%chisq(data = popn, var = race_white, 	num = 3,  main = 3,    order = 1);
%chisq(data = popn, var = ethnicity, 	num = 4,  main = 4,    order = 1);
%chisq(data = popn, var = famhxpd, 		num = 5,  main = 5,    order = 1);

*combine output;
data combine1;
	merge stats4_: freq3_: ttest2_: chi2_: ;
	by main order;
run;

*row labels;
DATA labels;
INFILE DATALINES TRUNCOVER;
INPUT @1 main BEST4. @6 order BEST4. @10 name $100.;
DATALINES ;
1    0   ^{style[fontweight=bold]Age}, mean (SD)
1    1   Median (min, max)
1    2   Missing
1.5  0   ^{style[fontweight=bold]Age category}, n (%)
2 	 1 	 50-54
2 	 2 	 55-59
2 	 3 	 60-64
2 	 4 	 65-69
2 	 5 	 70-74
2 	 6 	 75-79
2 	 7	 ^{unicode '2265'x} 80
3    0   Other race(s)
3    1   ^{style[fontweight=bold]Race (White)}, n (%) ^{super a}
3    99  Missing
4    0   Not Hispanic or Latino
4    1   ^{style[fontweight=bold]Ethnicity (Hispanic)}, n (%)
4    99  Missing
5    0   No
5    1   ^{style[fontweight=bold]1st-degree relative(s) with PD (yes)}, n (%)
5    99  Missing
5.5  0   ^{style[fontweight=bold]Smoking status}, n (%) ^{super b}
6	 1	 Current smoker
6    2   Former smoker
6    3   Never smoker
6	 99  Missing
;
RUN;

*combine data/labels and reformat for final output;
data combine2;
	merge labels combine1;
	by main order;
	*Delete 'Missing' row is all values are zero;
	if name = 'Missing' AND _1 = '0' AND _2 = '0' AND _3 = '0' AND _4 = '0' AND _5 = '0' AND _6 = '0' then delete;
	*For age, remove decimal places from median (min, max) values;
	if main = 1 AND order = 1 then do;
		_1 = CAT(SUBSTR(_1, 1, 2), SUBSTR(_1, 5, 4), SUBSTR(_1, 11, 4), SUBSTR(_1, 17, 1));
		_2 = CAT(SUBSTR(_2, 1, 2), SUBSTR(_2, 5, 4), SUBSTR(_2, 11, 4), SUBSTR(_2, 17, 1));
		_3 = CAT(SUBSTR(_3, 1, 2), SUBSTR(_3, 5, 4), SUBSTR(_3, 11, 4), SUBSTR(_3, 17, 1));
		_4 = CAT(SUBSTR(_4, 1, 2), SUBSTR(_4, 5, 4), SUBSTR(_4, 11, 4), SUBSTR(_4, 17, 1));
		_5 = CAT(SUBSTR(_5, 1, 2), SUBSTR(_5, 5, 4), SUBSTR(_5, 11, 4), SUBSTR(_5, 17, 1));
		_6 = CAT(SUBSTR(_6, 1, 2), SUBSTR(_6, 5, 4), SUBSTR(_6, 11, 4), SUBSTR(_6, 17, 1));
	end;
	*For race, remove 'Other race(s)' row;
	if main = 3 AND name = 'Other race(s)' then delete; 
	*For ethnicity, remove 'Not Hispanic or Latino' row;
	if main = 4 AND name = 'Not Hispanic or Latino' then delete; 
	*For family history, remove 'No' row;
	if main = 5 AND name = 'No' then delete; 
	*For smoking status, insert '—' for any groups including PPMI participants (since collected from PARS participants only);
	if main = 5.5 then do; 
		pval_f = "—"; 
		pval_m = "—"; 
	end;
	if main = 6 then do; 
		_1 = "—"; *combined females;
		_3 = "—"; *PPMI females;
		_4 = "—"; *combined males;
		_6 = "—"; *PPMI males;
	end;
run;

*find group totals;
proc sql noprint;
	select count(ID)
	into: g1 - :g6
	from popn
	group by group;
quit;

*find frequencies for each race category;
proc sql noprint;
	select count(ID)
	into: r1 - :r6
	from parsppmi(where=(not missing(race)))
	group by race;
quit;

*output final table;
OPTIONS ORIENTATION=LANDSCAPE;
ODS RTF style=sasdocPrinter bodytitle startpage=never file="<insert pathname>\Table 1.rtf";

options nodate nonumber missing=' ';
ods noptitle ; title;
ods graphics / border=off;
ods escapechar='^'; 

	FOOTNOTE;
	TITLE BOLD H=8PT J=LEFT F='Arial' 'Table 1. Demographic Characteristics of PARS, PPMI, and Combined Cohorts';
	FOOTNOTE1  H=8PT J=LEFT F='Arial' 'Abbreviations: PARS = Parkinson Associated Risk Syndrome; PD = Parkinson’s disease; PPMI = Parkinson’s Progression Markers Initiative.';
	FOOTNOTE2  H=8PT J=LEFT F='Arial' '^{style[fontstyle=ITALIC]p} Values were found using chi-square and t-tests comparing PARS vs PPMI participants.';
	FOOTNOTE3  H=8PT J=LEFT F='Arial' '^{super a} Across PARS and PPMI, 13 individuals self-reported as American Indian/Alaskan Native, 51 as Asian, 115 as Black or African American, 4 as Native Hawaiian/Pacific Islander, and 17 as multiracial.';
	FOOTNOTE4  H=8PT J=LEFT F='Arial' '^{super b} Smoking status data collected from PARS participants only.';

	PROC REPORT DATA=combine2 SPANROWS SPLIT="/"  
		style(column)={font_size=8pt fontfamily=arial} 
		style(header)={font_size=8pt fontfamily=arial};
		COLUMN main order name 
			   ('^S={borderbottomcolor=black borderbottomwidth=2}Females by cohort' _1 _2 _3 pval_f)
				_empty
			   ('^S={borderbottomcolor=black borderbottomwidth=2}Males by cohort'   _4 _5 _6 pval_m); 
		DEFINE main  / DISPLAY NOPRINT;
		DEFINE order / DISPLAY NOPRINT;
		DEFINE name / 'Variable' ;

		COMPUTE name;
			IF FIND(name, "^{style[fontweight=bold]") EQ 0 THEN CALL DEFINE(_COL_, "style", "style=[paddingleft=3em]");
			ELSE 										        CALL DEFINE(_ROW_, "style", "style=[bordertopcolor=DarkSlateGrey]");
		ENDCOMP;

		DEFINE _1 / "Combined /^{style[fontsize=8pt fontweight=MEDIUM](N = &g1)}" 	CENTER;
		DEFINE _2 / "PARS     /^{style[fontsize=8pt fontweight=MEDIUM](N = &g2)}" 	CENTER;
		DEFINE _3 / "PPMI   /^{style[fontsize=8pt fontweight=MEDIUM](N = &g3)}"   	CENTER;
		DEFINE pval_f / "^{style[fontstyle=ITALIC]p} Value"   						CENTER;
		DEFINE _empty 		/ ' ' style(column)={cellwidth=0.5%}; 
		DEFINE _4 / "Combined /^{style[fontsize=8pt fontweight=MEDIUM](N = &g4)}" 	CENTER;
		DEFINE _5 / "PARS     /^{style[fontsize=8pt fontweight=MEDIUM](N = &g5)}" 	CENTER;
		DEFINE _6 / "PPMI   /^{style[fontsize=8pt fontweight=MEDIUM](N = &g6)}"   	CENTER;
		DEFINE pval_m / "^{style[fontstyle=ITALIC]p} Value"   						CENTER;
	RUN;

TITLE;
FOOTNOTE;

ODS RTF CLOSE;
