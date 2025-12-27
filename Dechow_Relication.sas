/***********************************
*  File name: Dechow_Replication
*  Date: 11/06/2025
*  Modification Date: 11/28/2025
/*  Description:
*   Replicates Dechow and Dichev (2002) accrual quality model.
*   Constructs firm-year level variables (CFO, \delta WC, earnings, accruals),
*   performs scaling by average total assets, applies winsorization,
*   and identifies firms with complete 3-year and 8-year consecutive CFO data
*   for pooled and firm-specific regressions.
*
/*  Input:Compustat North America data */

/***********************************/
dm 'log; clear;'; /* clean the log windows */
dm 'output; clear;'; /* clean the output */
dm "odsresults; clear;"; /* clean ODS Results windows */
options nomprint nomlogic nosymbolgen nodate nonumber;
/* 
nomprint:Do not display macro expansion content (keep it off when not using macros).
nomlogic:Do not display macro execution steps.
nosymbolgen:Do not print the macro variable parsing process.
nodate:The output does not display the run date.
nonumber: Page numbers are not displayed in the output.
*/
ods listing;/*Listing destination*/
proc datasets lib=work kill nolist;
quit;
/* Setup */
libname src "C:\Users\andyg\Desktop\QAccounting\SAS\SAS Project\Replication";
/*
proc import datafile="C:\Users\andyg\Desktop\QAccounting\SAS\SAS Project\Replication\Dechow.csv"
    out=src.comp_raw
    dbms=csv
    replace;
    getnames=yes;
    guessingrows=max;
run;
*/
/* check the database */
proc contents data=src.comp_raw; run;

proc print data=src.comp_raw(obs=10); run;

/*
  Step 1: prepare core variables from raw Compustat extract   
  In this part, keep original Compustat names (oancf, recch, ... , ib)    
  , and build variables: awc, earn, accr                 
  , then, and scale by firm-specific average assets (AT_t + AT_{t-1})/2 
 */

proc sort data=src.comp_raw(where=(2000 <= fyear <= 2024)) out=step1_prep;
  by gvkey fyear;
run;

data step1_prep;
  set step1_prep;
  if missing(fyear) then fyear = year(datadate);
  if not (2000 <= fyear <= 2024) then delete;
  by gvkey fyear;
  
  /* oancf  = Operating Activities – Net Cash Flow (SCF)         */
  /* recch  = Accounts Receivable –  Decrease (Increase) (302)   */
  /* invch  = Inventory     – Decrease (Increase) (303)          */
  /* apalch = AP & Accrued Liab – Inc/(Dec) (304)                */
  /* txach  = Income Taxes   – Accrued – Inc/(Dec) (305)        */
  /* uaoloch= Other Assets & Liab  – Net Change (307)            */
  /* ib     = Income before Extraordinary Items (item 123)        */
  /* at     = Total Assets                                        */

  /* KEEP ONLY firm-years with available {CFO, earnings, \delta AR, \delta INV, AT} */
  if nmiss(oancf, ib, recch, invch, at) > 0 then delete;
  if missing(apalch)  then apalch  = 0;
  if missing(txach)   then txach   = 0;
  if missing(uaoloch) then uaoloch = 0;

  awc = -(recch + invch + apalch + txach + uaoloch); /* \delta working capital  */
  earn  = oancf + awc;  /* Earnings before long-term accruals (Earn) = CFO + AWC */
  accr  = ib - oancf; 
  cfo   = oancf;
  if not missing(sic) then sic2 = floor(sic/100);
run;

  proc means data=step1_prep min max;
    var fyear;
  run;
  /* Scaling the variables */
  proc sort data=step1_prep out=step1_base nodupkey;
      by gvkey fyear;
  run;
   
/* Construct lagged total assets and scale all variables */

proc sort data=step1_base;
    by gvkey fyear;
run;

data step1_ready;
    set step1_base;
    by gvkey fyear;

    /* 1. Construct lagged total assets safely within firm  */ 
    retain prev_at;

    if first.gvkey then do;
        at_lag = .;
        prev_at = at;
    end;
    else do;
        at_lag = prev_at;
        prev_at = at;
    end;

    /*2. Compute average assets; drop if cannot be computed */    
    if nmiss(at, at_lag)=0 and at>0 and at_lag>0 then 
        at_avg = mean(at, at_lag);
    else delete;

    /*3. Scale all flow variables by average assets */
    awc  = awc  / at_avg;
    earn = earn / at_avg;
    accr = accr / at_avg;
    cfo  = cfo  / at_avg;

    /*keep gvkey fyear awc earn accr cfo at at_lag at_avg;*/
run;

/* Create CFO lag/lead aligned on fiscal-year axis   */
data step2_ready;
    set step1_ready;
run;

proc sort data=step2_ready out=step2_ready_u nodupkey;
    by gvkey fyear;
run;

/* Create CFO_{t-1} and CFO_{t+1} via self-join */
proc sql;
    create table step3 as
    select  
        a.gvkey,
        a.fyear,
        a.cfo,
        a.awc,

        /* CFO_{t-1} */
        b.cfo as cfo_lag,

        /* CFO_{t+1} */
        c.cfo as cfo_lead

    from step2_ready_u as a

    /* previous year: t-1 */
    left join step2_ready_u as b
        on a.gvkey = b.gvkey
       and a.fyear = b.fyear + 1

    /* next year: t+1 */
    left join step2_ready_u as c
        on a.gvkey = c.gvkey
       and a.fyear = c.fyear - 1

    order by a.gvkey, a.fyear;
quit;

/* Keep ONLY true 3-year contiguous firm-years (t-1, t, t+1) */
data pooled_ready3;
    set step3;
    if nmiss(cfo_lag, cfo, cfo_lead)=0;
run;



/* Identify firm-years belonging to any larger 8-year contiguous span */

proc sort data=pooled_ready3 out=pr3_u;
    by gvkey fyear;
run;

data firm_ready8_t;
    set pr3_u;
    by gvkey fyear;

    /* Retain previous year to check contiguity */
    retain prev_year run_length;

    if first.gvkey then do;
        prev_year = fyear;
        run_length = 1;
    end;
    else do;
        if fyear = prev_year + 1 then 
            run_length + 1;
        else
            run_length = 1;
        prev_year = fyear;
    end;

    if run_length >= 8 then flag_contig8 = 1;
    else flag_contig8 = 0;
run;

/* Now expand the "8-year end flags" */
data firm_ready8_t;
    set firm_ready8_t;
    by gvkey fyear;

    retain include;

    if first.gvkey then include = 0;

    /* If this year is flagged as end of =8-year run */
    if flag_contig8 = 1 then include = 8;

    /* If include > 0, we are still inside the 8-year span */
    if include > 0 then do;
        output;      /* keep this firm-year */
        include + (-1);
    end;
run;

/* list distinct firms with larger 8-year usable years */
proc sql;
    create table firm_ready8 as
    select gvkey, count(*) as n_t
    from firm_ready8_t
    group by gvkey
    having n_t >= 8
    order by gvkey;
quit;


/* Get percentiles for earn, awc, cfo */
proc univariate data=step1_ready noprint;
    where nmiss(earn, awc, cfo) = 0;
    var earn awc cfo;
    output out=pctl pctlpts=1 99 pctlpre=earn_p awc_p cfo_p;
run;

/* Winsorize variables at the 1st and 99th percentiles */
data step2_winsor;
    if _n_ = 1 then set pctl;   /* load percentile thresholds */
    set step1_ready;

    /* winsorize earn */
    if earn < earn_p1  then earn = earn_p1;
    else if earn > earn_p99 then earn = earn_p99;

    /* winsorize awc */
    if awc < awc_p1  then awc = awc_p1;
    else if awc > awc_p99 then awc = awc_p99;

    /* winsorize cfo */
    if cfo < cfo_p1  then cfo = cfo_p1;
    else if cfo > cfo_p99 then cfo = cfo_p99;
run;

/* Step 3: Merge winsorized data with contiguous-year filters */

/* Merge step2_winsor with 3-year contiguous sample */
proc sql;
    create table merged3 as
    select 
        a.*,
        lag_tbl.cfo as cfo_lag,
        lead_tbl.cfo as cfo_lead
    from step2_winsor as a
    
    inner join (select distinct gvkey, fyear from pooled_ready3) as filter
        on a.gvkey = filter.gvkey and a.fyear = filter.fyear
    
    left join step2_winsor as lag_tbl
        on a.gvkey = lag_tbl.gvkey and a.fyear = lag_tbl.fyear + 1
   
    left join step2_winsor as lead_tbl
        on a.gvkey = lead_tbl.gvkey and a.fyear = lead_tbl.fyear - 1;
quit;

/* Further keep only firms belonging to any 8-year contiguous span */
proc sql;
    create table final_regression_sample as
    select a.*
    from merged3 as a
    inner join firm_ready8 as f
        on a.gvkey = f.gvkey
    order by gvkey, fyear;
quit;

/* ============================================
   Residual Analysis and R-squared Reporting
   (Supplement to existing regression code)
   ============================================ */

/* ----------------------------------------
   1. Pooled Regression with Full Output
   ---------------------------------------- */
proc reg data=final_regression_sample;
    model awc = cfo_lag cfo cfo_lead;
    ods output FitStatistics=pooled_fit 
               ParameterEstimates=pooled_params;
    title "Pooled Regression Results";
run; quit;

proc print data=pooled_fit noobs; 
    title2 "R-squared Statistics";
run;

proc print data=pooled_params noobs;
    title2 "Coefficient Estimates";
run;

/* Extract pooled coefficients for comparison table */
proc reg data=final_regression_sample noprint outest=pooled_out;
    model awc = cfo_lag cfo cfo_lead;
run; quit;

proc sql;
    create table pooled_parms as
    select 
        "Pooled" as level length=20,
        Intercept     as beta0,
        cfo_lag       as beta1_cfo_lag,
        cfo           as beta2_cfo,
        cfo_lead      as beta3_cfo_lead
    from pooled_out
    where upcase(_TYPE_)='PARMS';
quit;

/* ----------------------------------------
   2. Firm-Level Regressions with R-squared
   ---------------------------------------- */
proc sort data=final_regression_sample out=final_reg_sorted;
    by gvkey fyear;
run;

proc reg data=final_reg_sorted noprint
         outest=beta_by_firm_full rsquare;
    by gvkey;
    model awc = cfo_lag cfo cfo_lead;
    output out=resid_by_firm r=eps p=fitted;
run; quit;

/* Extract coefficients and R-squared */
proc sql;
    create table firm_coef_rsq as
    select gvkey,
           Intercept as beta0,
           cfo_lag   as beta1,
           cfo       as beta2,
           cfo_lead  as beta3,
           _RSQ_     as r_squared
    from beta_by_firm_full
    where upcase(_TYPE_) = 'PARMS'
    order by gvkey;
quit;

/* ----------------------------------------
   3. Accrual Quality: Residual Std Dev
   ---------------------------------------- */
proc sql;
    create table aq_by_firm as
    select gvkey,
           count(eps) as n_obs,
           mean(eps)  as eps_mean,
           std(eps)   as aq_sigma
    from resid_by_firm
    group by gvkey;
quit;

/* ----------------------------------------
   4. Merge All Firm-Level Results
   ---------------------------------------- */
proc sql;
    create table firm_results as
    select a.gvkey,
           a.beta0,
           a.beta1,
           a.beta2,
           a.beta3,
           a.r_squared,
           b.n_obs,
           b.aq_sigma
    from firm_coef_rsq as a
    left join aq_by_firm as b 
        on a.gvkey = b.gvkey;
quit;

/* Firm-Level Summary for comparison table */
proc sql;
    create table firm_summary as
    select
        "Firm-Level" as level length=20,
        mean(beta0)       as beta0_mean,
        mean(beta1)       as beta1_mean,
        mean(beta2)       as beta2_mean,
        mean(beta3)       as beta3_mean,
        std(beta0)        as beta0_sd,
        std(beta1)        as beta1_sd,
        std(beta2)        as beta2_sd,
        std(beta3)        as beta3_sd,
        mean(aq_sigma)    as aq_mean,
        std(aq_sigma)     as aq_sd,
        mean(r_squared)   as r2_mean,
        median(r_squared) as r2_median,
        count(*)          as n_firms
    from firm_results;
quit;

/* ----------------------------------------
   5. Industry-Year Level Regressions
   ---------------------------------------- */
proc sort data=final_regression_sample;
    by sic2 fyear;
run;

proc reg data=final_regression_sample noprint
         outest=beta_by_indyear rsquare covout;
    by sic2 fyear;
    model awc = cfo_lag cfo cfo_lead;
run; quit;

proc sql;
    create table beta_indyear_parms as
    select sic2, fyear,
           Intercept,
           cfo_lag,
           cfo,
           cfo_lead,
           _RSQ_ as r_squared
    from beta_by_indyear
    where upcase(_TYPE_)='PARMS'
    order by sic2, fyear;
quit;

/* Industry-Year Summary for comparison table */
proc sql;
    create table industry_year_summary as
    select
        "Industry-Year" as level length=20,
        mean(Intercept)  as beta0_mean,
        mean(cfo_lag)    as beta1_mean,
        mean(cfo)        as beta2_mean,
        mean(cfo_lead)   as beta3_mean,
        std(Intercept)   as beta0_sd,
        std(cfo_lag)     as beta1_sd,
        std(cfo)         as beta2_sd,
        std(cfo_lead)    as beta3_sd,
        mean(r_squared)  as r2_mean,
        median(r_squared) as r2_median,
        count(*)         as n_indyear
    from beta_indyear_parms;
quit;

/* ----------------------------------------
   6. Combined Comparison Table (Table 3 Style)
   ---------------------------------------- */
data regression_comparison;
    set pooled_parms industry_year_summary firm_summary;
run;

proc print data=regression_comparison noobs label;
    title "Comparison of Regression Results: Pooled vs Industry-Year vs Firm-Level";
run;

/* ----------------------------------------
   7. Summary Statistics (Table 3 Style)
   ---------------------------------------- */

/* Panel A: Coefficient and R-squared Distribution */
proc means data=firm_results n mean std min p25 median p75 max;
    var beta0 beta1 beta2 beta3 r_squared;
    title "Table 3 Panel A: Firm-Specific Regression Coefficients";
run;

/* Accrual Quality Distribution */
proc means data=firm_results n mean std min p25 median p75 max;
    var aq_sigma;
    title "Accrual Quality (AQ) Distribution";
run;

/* Industry-Year R-squared Distribution */
proc means data=beta_indyear_parms n mean std min p25 median p75 max;
    var r_squared;
    title "Industry-Year Regression: R-squared Distribution";
run;

/* ----------------------------------------
   8. Residual Diagnostics
   ---------------------------------------- */

/* Residuals: Overall Distribution */
proc means data=resid_by_firm n mean std min p25 median p75 max;
    var eps;
    title "Residuals: Descriptive Statistics (All Firm-Years)";
run;

/* Check residual mean by firm (should be ~0) */
proc sql;
    select count(*) as n_firms_nonzero_mean
    from aq_by_firm
    where abs(eps_mean) > 0.001;
quit;

/* ----------------------------------------
   9. Comparison with Original Paper
   ---------------------------------------- */
proc sql;
    create table summary_comparison as
    select 
        "Firm-Level" as Specification,
        count(*)          as N_Firms,
        mean(beta0)       as Intercept_Mean,
        mean(beta1)       as b1_CFO_Lag_Mean,
        mean(beta2)       as b2_CFO_Mean,
        mean(beta3)       as b3_CFO_Lead_Mean,
        mean(r_squared)   as R2_Mean,
        median(r_squared) as R2_Median,
        mean(aq_sigma)    as AQ_Mean,
        median(aq_sigma)  as AQ_Median
    from firm_results;
quit;

proc print data=summary_comparison noobs label;
    title "Summary: Firm-Level Regressions (2000-2024)";
    title2 "Compare with Dechow & Dichev (2002) Table 3";
    format Intercept_Mean b1_CFO_Lag_Mean b2_CFO_Mean b3_CFO_Lead_Mean 
           R2_Mean R2_Median AQ_Mean AQ_Median 8.4;
run;

/* Reference values from original paper:
   Firm-Level: b1 = 0.17, b2 = -0.62, b3 = 0.09
               R2 Mean = 0.47, R2 Median = 0.55
               AQ Mean = 0.028, AQ Median = 0.020
   
   Industry:   b1 = 0.19, b2 = -0.51, b3 = 0.15
               R2 Mean = 0.34, R2 Median = 0.34
   
   Pooled:     b1 = 0.19, b2 = -0.51, b3 = 0.18
               R2 = 0.29
*/

/* ----------------------------------------
   10. AQ Quintile Analysis (Table 5 Style)
   ---------------------------------------- */

/* Create AQ quintiles */
proc rank data=firm_results out=firm_ranked groups=5;
    var aq_sigma;
    ranks aq_quintile;
run;

/* Merge quintile back to firm-year data */
proc sql;
    create table firmyear_with_aq as
    select a.*, 
           b.aq_sigma,
           b.aq_quintile + 1 as aq_quintile /* make it 1-5 instead of 0-4 */
    from resid_by_firm as a
    inner join firm_ranked as b
        on a.gvkey = b.gvkey;
quit;

/* Summary by AQ quintile */
proc sql;
    create table aq_quintile_summary as
    select aq_quintile,
           count(distinct gvkey) as n_firms,
           mean(aq_sigma) as aq_mean,
           mean(eps*eps) as mse  /* mean squared residual */
    from firmyear_with_aq
    group by aq_quintile
    order by aq_quintile;
quit;

proc print data=aq_quintile_summary noobs;
    title "Accrual Quality by Quintile";
    title2 "Quintile 1 = Best Quality, Quintile 5 = Worst Quality";
    format aq_mean mse 8.5;
run;

/* ----------------------------------------
   11. Sample Size Summary
   ---------------------------------------- */
proc sql;
    select "Final Sample" as Description,
           count(*) as N_FirmYears,
           count(distinct gvkey) as N_Firms,
           min(fyear) as Year_Min,
           max(fyear) as Year_Max
    from final_regression_sample;
quit;
