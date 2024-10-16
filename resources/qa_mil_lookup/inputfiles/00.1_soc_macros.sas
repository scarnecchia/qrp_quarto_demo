/*-------------------------------------------------------------------------------------*\
|  PROGRAM NAME: 00.1_soc_macros.sas.sas                                                |
|                                                                                       |
|  PACKAGE VERSION: 7.x.x                                                               |
|---------------------------------------------------------------------------------------|
|  PURPOSE:                                                                             |
|     The purpose of the program is to store macros used in the QA lookup file          |
|     generation program package                                                        |
|---------------------------------------------------------------------------------------|
|  PROGRAM INPUT:                                                                       |
|     see soc_qa_lookup_master.sas                                                      |
|                                                                                       |
|  PROGRAM OUTPUT:                                                                      |
|     see QA package Spec                                                               |
|---------------------------------------------------------------------------------------|
|  CONTACT:                                                                             |
|     Sentinel Coordinating Center                                                      |
|     info@sentinelsystem.org                                                           |
\*-------------------------------------------------------------------------------------*/

%macro import_xlsx (file=);
  %do i=1 %to &sheetct.;
    %let sheet=%scan(&sheetlist.,&i.,*);

    proc import out=work.&sheet. datafile="&file..xlsx" dbms=xlsx replace;
      sheet="&sheet.";
      getnames=yes;
    run;
    proc datasets memtype=data library=work nolist ;
      modify &sheet.;
      attrib _all_ label=' ';
    run; quit;
  %end;
%mend import_xlsx;

%macro test (result_type=, var1=, var2=);
  proc sql noprint;
    select varlength into :var1length
    from msoc.lkp_all_l1
    where lowcase(tabid)='lab' and lowcase(variable)=lowcase("&var1.")
    ;
    select varlength into :var2length
    from msoc.lkp_all_l1
    where lowcase(tabid)='lab' and lowcase(variable)=lowcase("&var2.")
    ;
    create table msoc.lkp_lab_l2_&result_type. 
      ( TestID char(4)
      , MS_Test_Name char(10)
      , Result_Type char(1)      
      , &var1. char(&var1length.)
      , &var2. char(&var2length.)
      )
    ; 
    select count(*)
         , countw(_&var1.,'|')
         , countw(_&var2.,'|')
         , testid
         , ms_test_name
         , result_type
         , _&var1.
         , _&var2.
    into   :nrows trimmed
         , :var1ct separated by ' '
         , :var2ct separated by ' '
         , :testidlist separated by '~'
         , :testlist separated by '~'
         , :rtlist separated by ' '
         , :_var1list separated by '~'
         , :_var2list separated by '~'
    from temp%unquote(&result_type.)
    ;
    insert into msoc.lkp_lab_l2_&result_type. 
          ( testid
          , ms_test_name
          , result_type
          , &var1.
          , &var2.
          )
  %do i=1 %to &nrows.;
    %let testid=%qscan(&testidlist.,&i.,~);
    %let test=%qscan(&testlist.,&i.,~);
    %let rt=%scan(&rtlist.,&i.);
    %let _1=%qscan(&_var1list.,&i.,~);
    %let _2=%qscan(&_var2list.,&i.,~);
    %let ct1=%scan(&var1ct.,&i.);
    %let ct2=%scan(&var2ct.,&i.);
    %do a=1 %to &ct1.;
      %let temp1=%qscan(&_1.,&a.,|);
      %do b=1 %to &ct2.;
        %let temp2=%qscan(&_2.,&b.,|);
        values
          ( "&testid."
          , "&test."
          , "&rt."
          , "&temp1"
          , "&temp2"
          )
      %end;
    %end;
  %end;
    ; 
%mend;

%macro macrovar (var=);
%global &var.;
proc sql noprint;
  select value into :&var.
  from macrovariables
  where macro_var="&var."
  ;
quit;
%mend;
