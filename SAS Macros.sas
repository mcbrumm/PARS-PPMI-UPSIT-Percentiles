*-------------------------------------------------------*
* PROGRAMMER:  		Michael Brumm  						*
*														*
* DATE CREATED: 	February 20, 2023	           	 	*
*                                                     	*
* FILE NAME:		SAS Macros.sas  					*
* 					  									*
*                                                     	*
* DESCRIPTION:      Defines SAS macros used for UPSIT 	*
* 					percentiles paper.					*
* 														*
* CREDIT:												*
*                                                     	*
* INPUT DATA SETS: 										*
*                                                     	*
* OUTPUT DATA SETS:   									* 
*					  									*
*-------------------------------------------------------*;

*define macro to derive descriptive summary for continuous variables;
%macro cont(data = , var = , dec = , dec2 = , num = );

	proc means data=&data;
		class group;
		var &var;
		output out=stats_&num mean = mean  stddev = std
							  median = median min = minimum  max = maximum
							  nmiss = missing  n = n;
	run;

	data stats2_&num (keep=main ave med miss group);
		set stats_&num (where=( not missing(group) ));

		if not missing(mean) then do;
			ave = strip(put(mean, 9.&dec.)) || " (" || compress(put(std, 9.&dec.))|| ")";
			med = strip(put(median, 9.&dec2.)) || " (" || compress(put(minimum, 9.&dec2.))|| ", " || compress(put(maximum, 9.&dec2.)) || ")";
		end;
		miss = strip(put(missing, 5.));

		main = &num;
	run;

	proc sort data=stats2_&num; by main group; run;

	proc transpose data=stats2_&num out=stats3_&num (drop=_name_);
		by main; id group;
		var ave med miss;
	run;

	data stats4_&num;
		set stats3_&num;
		order = _N_ - 1;
	run;

%mend cont;

*define macro to derive descriptive summary for categorical variables;
%macro cat(data = , var = , num = , dec = 0);

	proc freq data=&data;
		table group*&var;
		ods output CrossTabFreqs = freq_&num;
	run;

	proc freq data=&data;
		table group*&var / missing;
		ods output CrossTabFreqs = freq_m_&num;
	run;

	data freq2_&num (keep=group main order n_per);
		set freq_&num (where=(not missing(rowpercent)))
			freq_m_&num (where=(missing(&var) AND not missing(rowpercent)));

		if not missing(&var) then
			n_per = strip(put(frequency, 5.)) || ' (' || strip(put(rowpercent, 7.&dec.)) || '%)';
		else n_per = strip(put(frequency, 5.));

		main = &num;
		if not missing(&var) then order = &var;
		else if missing(&var) then order = 99;
				new = order*1;
		drop order;
		rename new=order;
	run;

	proc sort data=freq2_&num; by main order group; run;

	proc transpose data=freq2_&num out=freq3_&num (drop=_name_);
		by main order; id group;
		var n_per;
	run;

%mend cat;

*define macro to perform t-tests to compare continuous variables between PARS vs PPMI participants (separately by sex);
%MACRO ttest(data = , var = , num = , main = , order = 0);

	*females;
	proc ttest data=&data(where=(group IN (2 3))) plots=none;
		class group;
		var &var;
		ods output ttests = ttest1_f_&var
			       equality = equalvar_f_&var;
	run;

	data ttest_f_&var(keep=pval_f);
		merge ttest1_f_&var
			  equalvar_f_&var(keep=Variable ProbF);
		by Variable;
		if ProbF < 0.05 AND Variances = "Equal" then delete;
		if ProbF ge 0.05 AND Variances = "Unequal" then delete;
		pval_f = strip(put(probt, PVALUE6.4));	
	run;

	*males;
	proc ttest data=&data(where=(group IN (5 6))) plots=none;
		class group;
		var &var;
		ods output ttests = ttest1_m_&var
			       equality = equalvar_m_&var;
	run;

	data ttest_m_&var(keep=pval_m);
		merge ttest1_m_&var
			  equalvar_m_&var(keep=Variable ProbF);
		by Variable;
		if ProbF < 0.05 AND Variances = "Equal" then delete;
		if ProbF ge 0.05 AND Variances = "Unequal" then delete;
		pval_m = strip(put(probt, PVALUE6.4));	
	run;

	*merge;
	data ttest2_&num;
		merge ttest_f_&var
			  ttest_m_&var;
		main   = &main;
		order  = &order;
	run;

%MEND ttest;

*define macro to perform chi-squared tests to compare categorical variables between PARS vs PPMI participants (separately by sex);
%MACRO chisq(data = , var = , num = , main = , order = 0);

	*females;
	proc freq data=&data(where=(group IN (2 3)));
		table group*&var / chisq;
		ods output ChiSq = chi1_f_&num(rename=(Prob = Prob_F)
									   where=(Statistic = 'Chi-Square'));
	run;

	*males;
	proc freq data=&data(where=(group IN (5 6)));
		table group*&var / chisq;
		ods output ChiSq = chi1_m_&num(rename=(Prob = Prob_M)
									   where=(Statistic = 'Chi-Square'));
	run;
	
	*merge;
	data chi2_&num (keep=main order pval: );
		merge chi1_f_&num(keep=Prob_F)
			  chi1_m_&num(keep=Prob_M);
		main   = &main;
		order  = &order;
		pval_f = strip(put(Prob_F, PVALUE6.4));
		pval_m = strip(put(Prob_M, PVALUE6.4));
	run;

%MEND chisq;
