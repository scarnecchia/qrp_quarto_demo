/*-------------------------------------------------------------------------------------*\
|  PROGRAM NAME: 01.1_create_lookup_tables.sas                                          |
|---------------------------------------------------------------------------------------|
|  PURPOSE:                                                                             |
|     The purpose of the program is to generate lookup tables for the QA Package        |
|---------------------------------------------------------------------------------------|
|  PROGRAM INPUT:                                                                       |
|     see soc_qa_lookup_master.sas                                                      |
|                                                                                       |
|  PROGRAM OUTPUT:                                                                      |
|     see QA Package Spec                                                               |
|---------------------------------------------------------------------------------------|
|  CONTACT:                                                                             |
|     Sentinel Coordinating Center                                                      |
|     info@sentinelsystem.org                                                           |
\*-------------------------------------------------------------------------------------*/
proc datasets lib=work memtype=data kill nowarn nodetails nolist; run; quit;

%include "&infolder.\00.1_soc_macros.sas" / source2;
%include "&infolder.\macros\_expand_tab.sas" / source2;

/***************************************************************************************/
/* 1. Import JSON file and convert to SAS datasets                                     */
/***************************************************************************************/
filename qalookup "&infolder.\SCDM_lookup_table_values.json";
libname qajson JSON fileref=qalookup ordinalcount=none;

proc sql noprint;
  create table a as
  select *
  from dictionary.tables
  where libname="QAJSON"
  ;
  select memname, count(memname) into :sheetlist separated by '*', :sheetct trimmed
  from a
  where substr(memname,1,1) ne "_" and memname ne "ALLDATA"
  ;
  drop table a
  ;
quit;

/*-------------------------------------------------------------------------------------*/
/* 2. Create and output table MSOC.LKP_LAB_TEST                                        */
/*-------------------------------------------------------------------------------------*/
proc sql noprint;
  create table msoc.lkp_lab_test as
  select catx('-', testnum, result_type) as TestID length=4
       , TestNum
       , MS_Test_Name length=20
       , Result_Type
       , UnitReq
       , Characterized
  from qajson.lkp_lab_test
  ;
quit;

/*-------------------------------------------------------------------------------------*/
/* 3. Create and output table MSOC.LKP_ALL_L1                                          */
/*-------------------------------------------------------------------------------------*/
/* determine if code is needed for "Reference_Table" values, but do not expand list in lkp_all_l1)*/

%macro find_values;
  /** Validvaluetype = find_values **/
  proc sql noprint;
    select scan(_validvalue,1,".")
         , scan(_validvalue,2,".")
         , count(*)
    into :tablelist separated by "~"
       , :varlist separated by "~"
       , :fvct
    from qajson.lkp_all_l1
    where validvaluetype="find_values"
    ;
  quit;
  %do f=1 %to &fvct.;
    %let table=%scan(&tablelist.,&f.,~);
    %let variable=%scan(&varlist.,&f.,~);
    proc sql noprint;
      select distinct &variable. into :valuelist separated by '|'
      from qajson.&table.
      ;
      create table %bquote(tempfv&f.) as
      select *
           , "&valuelist." as ValidValue length=350 label=' '
      from qajson.lkp_all_l1
      where validvaluetype="find_values" and _validvalue="&table..&variable."
      ;
    quit;
  %end;

  data temp_fv;
    set tempfv:;
  run;

  proc datasets lib=work nolist nowarn nodetails;
    delete tempfv:;
  quit;

  /** Validvaluetype ne find_values **/
  proc sql noprint;
    create table temp_notfv as
    select *
         , _validvalue as ValidValue length=350
    from qajson.lkp_all_l1
    where validvaluetype ne "find_values"
    ;
  quit;

  data msoc.lkp_all_l1 (drop=_:);
    set temp_:;
    length FlagCondition $425.;
 /* Conditional Hardcode for MS_RESULT_C as a workaround for values that may contain
      ranges (e.g. "5|10 MG/DL" */
    if lowcase(variable)="ms_result_c" and find(validvalue,'|(range)','i') ge 1 then do;
      flagcondition=strip(variable)||" not in ("||compress("'"||tranwrd(tranwrd(validvalue,"|(range)", ' '),"|", "','")||"'")||")"||"
      and "||strip(variable)||" eq (compress("||strip(variable)||",'|', ))";
    end;
    else if validvaluetype in ("find_values", "list_values") then flagcondition=strip(variable)||" not in ("||compress("'"||tranwrd(validvalue,"|", "','")||"'")||")";
    else if validvaluetype in ("char_range") then flagcondition="input("||strip(variable)||",3.)"||" not in ("||compress(validvalue)||")";
    else if validvaluetype in ("num_range") then flagcondition=strip(variable)||" not in ("||compress(validvalue)||")";
    else if validvaluetype in ("value_case") then flagcondition=strip(variable)||" ne upcase("||strip(variable)||")";
    else if validvaluetype in ("no_spec_chars") then flagcondition=strip(variable)||" ne compress("||strip(variable)||",'._-','kda')";
    else if validvaluetype in ("numeric") then do;
      if (find(variable,'date','i') or find(variable,'enr_','i')) ge 1 then flagcondition=strip(variable)||" not "||strip(validvalue)||"'d";
      else flagcondition=strip(variable)||" not "||trim(validvalue);
    end;
    else if validvaluetype in ("special_missing") then flagcondition=strip(variable)||" lt 1 and "||strip(variable)||" not in ("||strip(validvalue)||")";
    else if validvaluetype in ("only") then do;
      if validvalue="numbers" then flagcondition="notdigit(trim("||variable||")) gt 0";
      else if validvalue="numbers|decimal" then flagcondition=strip(variable)||" ne compress("||strip(variable)||",'.','kd')";
      else if validvalue="alpha" then flagcondition="notalpha(strip("||variable||")) gt 0";
      else if validvalue="alphanumeric" then flagcondition="notalnum(strip("||strip(variable)||")) gt 0";
    end;
    else if validvaluetype in ("reference_table") then do;
      if upcase(variable) = "LOINC" then flagcondition="put("||strip(variable)||",$"||compress(tabid||varid)||"fmt.) ne ms_test_name";
      else flagcondition="put("||strip(variable)||",$"||compress(tabid||varid)||"fmt.) ne '1'";
    end;
  run;

/*If special missing characters are amongst your numeric data it is important to note that WHERE varname ne . will not*/
/*exclude the special characters and WHERE varname gt .z must be used instead.*/
/*WHERE varname is not missing or WHERE varname is not null can be used for both character and numeric data;*/
/*note that all missing data including special characters will be removed from the data set. */

/* Exclude any rows for retired variables */
  proc sort data=msoc.lkp_all_l1 (where=(scdmid ne . and tabid ne ' ' and varid ne ' ' and vartype ne ' ')) noduprec; /*remove blank rows */
    by scdmid tabid varid;
  run; quit;

  proc datasets lib=work nolist nowarn nodetails;
    delete temp:;
  quit;
%mend;
%find_values;

/* Assign macro variables */
data _null_;
  set qajson.macrovariables;
  if lowcase(macro_var) = 'modifier_n' then call symputx('modifier_n', strip(value));
  if lowcase(macro_var) = 'modifier_c' then call symputx('modifier_c', strip(value));
  if lowcase(macro_var) = 'ms_result_c' then call symputx('ms_result_c', strip(value));
run;

/*------------------------------------------------------------------------------------*/
/* 4. Create and output table MSOC.LKP_LAB_L2_N                                       */
/*------------------------------------------------------------------------------------*/

proc sql noprint;
  create table tempn as
  select b.TestID
       , a.*
       , "&modifier_n." as _modifier
  from qajson.lkp_lab_l2_n as a left join msoc.lkp_lab_test (keep=ms_test_name result_type testid) as b
    on upcase(a.ms_test_name)=upcase(b.ms_test_name) and a.result_type=b.result_type
  ;
quit;

%test (result_type=N, var1=MS_Result_Unit, var2=Modifier);

data msoc.lkp_lab_l2_n;
  set msoc.lkp_lab_l2_n;
  if ms_result_unit='.' then ms_result_unit=' ';
run;

/*-------------------------------------------------------------------------------------*/
/* 5. Create and output table MSOC.LKP_LAB_L2_C                                        */
/*-------------------------------------------------------------------------------------*/

proc sql noprint;
  create table tempC as
  select *
       , "&ms_result_c." as _ms_result_c
       , "&modifier_c." as _modifier
  from msoc.lkp_lab_test
  where result_type="C" and characterized="Y"
  ;
quit;
%test (result_type=C, var1=MS_Result_C, var2=Modifier);

proc datasets lib=work nolist nodetails;
  delete temp: _tempn;
run;
quit;

data msoc.lkp_lab_l2_nc;
  set msoc.lkp_lab_l2_:;
run;

proc datasets lib=msoc nolist nodetails;
  delete lkp_lab_l2_n lkp_lab_l2_c;
quit;

proc sort data=msoc.lkp_lab_l2_nc;
  by testid;
run;

%symdel modifier_n modifier_c ms_result_c / nowarn;

/*-------------------------------------------------------------------------------------*/
/* 6. Create and output table MSOC.LKP_LAB_L2                                          */
/*-------------------------------------------------------------------------------------*/
proc sql noprint;
  create table temp (drop=_ms_:) as
  select b.TestID
       , a.*
       , case when _ms_test_sub_category=' ' then '_'
         else _ms_test_sub_category
         end as MS_Test_Sub_Category length=6
  from qajson.lkp_lab_l2 as a left join msoc.lkp_lab_test (keep=ms_test_name result_type testid) as b
    on upcase(a.ms_test_name)=upcase(b.ms_test_name) and a.result_type=b.result_type
  ;
quit;

%macro lkp_lab_l2;
  proc sql noprint;
    create table msoc.lkp_lab_l2
      ( TestID char(4)
      , MS_Test_Name char(20)
      , Result_Type char(1)
      , MS_Test_Sub_Category char(6)
      , Fast_Ind char(1)
      , Specimen_Source char(7)
      )
    ;
    select count(*)
         , countw(_specimen_source,'|')
         , testid
         , ms_test_name
         , result_type
         , ms_test_sub_category
         , fast_ind
         , _specimen_source
    into   :nrows trimmed
         , :ssct separated by ' '
         , :testidlist separated by '~'
         , :testlist separated by '~'
         , :rtlist separated by ' '
         , :subcatlist separated by '~'
         , :fastlist separated by ' '
         , :_sslist separated by '~'
    from work.temp
    ;
    insert into msoc.lkp_lab_l2
          ( testid
          , ms_test_name
          , result_type
          , ms_test_sub_category
          , fast_ind
          , specimen_source
          )
  %do i=1 %to &nrows.;
    %let testid=%qscan(&testidlist.,&i.,~);
    %let test=%qscan(&testlist.,&i.,~);
    %let rt=%scan(&rtlist.,&i.);
    %let subcat=%qscan(&subcatlist.,&i.,~);
    %let fast=%scan(&fastlist.,&i.);
    %let _ss=%qscan(&_sslist.,&i.,~);
    %let sct=%scan(&ssct.,&i.);
    %do s=1 %to &sct.;
      %let ss=%qscan(&_ss.,&s.,|);
      values
          ( "&testid."
          , "&test."
          , "&rt."
          , "&subcat."
          , "&fast."
          , "&ss."
          )
    %end;
  %end;
    ;
    proc sql noprint;
      update msoc.lkp_lab_l2
      set ms_test_sub_category=
          (case when ms_test_sub_category ='_' then ' '
               else ms_test_sub_category
          end)
    ;
    drop table work.temp
    ;
  quit;
%mend;
%lkp_lab_l2;

/*-------------------------------------------------------------------------------------*/
/* 7. Create and output table MSOC.LKP_LAB_RESULT_RANGES                               */
/*-------------------------------------------------------------------------------------*/
proc sql noprint;
  create table msoc.lkp_lab_result_ranges as
  select MS_Test_Name length=20
       , MS_Test_Sub_Category
       , Fast_Ind
       , MS_Result_Unit
       , compress((ms_test_name||ms_test_sub_category||fast_ind||ms_result_unit),'/ ') as FMTNAME length=20
       , LABEL
       , START
       , END
       , SEXCL
       , EEXCL
       , HLO
       , TYPE
  from qajson.lkp_lab_result_ranges
  ;
quit;

proc datasets lib=work memtype=data nolist nowarn nodetails;
  delete lab_: _lab:;
run; quit;

/*-------------------------------------------------------------------------------------*/
/* 8. Create and output table MSOC.LKP_PVD_SPECIALTY                                  */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_pvd_specialty;
  set qajson.lkp_pvd_specialty;
run;

/*-------------------------------------------------------------------------------------*/
/* 9. Create and output table MSOC.LKP_ALL_L2                                          */
/*-------------------------------------------------------------------------------------*/
data lkp_all_l2 (keep=TabID variable: value CheckID AbortYN FlagCondition);
  length CheckID $3 Tabid $3 Value $10 Variable2 $21 abortYN $1;
  set msoc.lkp_all_l1;
  retain Variable;
  FlagType = ""; AbortYN="";
  if tabid in ('ENC','DIA','PRO') then do;
    if lowcase(obsreq)='some' then do;
      CheckID='223';
      Variable2='EncType';
      if lowcase(variable) ne 'ddate' then do;
        Value='AV'; FlagCondition="EncType='AV' and "||strip(variable)||" is not null"; output;
        Value='OA'; FlagCondition="EncType='OA' and "||strip(variable)||" is not null"; output;
        Value='IP'; FlagCondition="EncType='IP' and "||strip(variable)||" is null"; output;
        Value='IS'; FlagCondition="EncType='IS' and "||strip(variable)||" is null"; output;
      end;
      else if lowcase(variable) = 'ddate' then do;
        Value='AV'; FlagCondition="EncType='AV' and "||strip(variable)||" ne ."; output;
        Value='OA'; FlagCondition="EncType='OA' and "||strip(variable)||" ne ."; output;
        Value='IP'; FlagCondition="EncType='IP' and "||strip(variable)||" eq ."; output;
        Value='IS'; FlagCondition="EncType='IS' and "||strip(variable)||" eq ."; output;
      end;
    end;
  end;
  if tabid eq 'LAB' then do;
    array umvars (7) $20 ('Abn_ind' 'Fast_ind' 'Specimen_source' 'Result_Type' 'Pt_loc' 'stat' 'Modifier');
    array umvals (7) $3 ('UN' 'U' 'UNK' 'U' 'U' 'U' 'UN');
    do i = 1 to dim(umvars);
      if lowcase(variable) = lowcase(umvars(i)) then do;
        CheckID ='222'; AbortYN="Y";
        Variable2 = "MS_Test_Name";
        Value = "UNMAPPED";
        FlagCondition = cat(strip(variable2),"= '", strip(Value), "' and ", strip(umvars(i)), " ne '", strip(umvals(i)),"'"); output;
      end; /* end variable name condition */
    end; /* end i loop through variable list */
    if lowcase(variable) = "loinc" then do;
      CheckID ='222';
      AbortYN = "N";
      Variable2 = "MS_Test_Name";
      Value = "UNMAPPED";
      FlagCondition = cat(strip(variable2)," ne '", strip(Value),"' and not missing(",strip(variable),") and put(", strip(Variable), ",$lab07fmt.) ne ", strip(variable2)); output;
    end;
    if lowcase(variable) = "result_type" then do;
      CheckID ='222';
      AbortYN = "N";
      Variable2 = "MS_Test_Name";
      Value = "$lab03fmt.";
      FlagCondition = cat("put(", strip(Variable2), ",$lab03fmt.) ne 'NA' and put(", strip(Variable2), ",$lab03fmt.) ne ", strip(variable)); output;
    end;
  end; /* end condition: tabid= LAB */
  if lowcase(Variable)='dx_codetype' then do;
    CheckID='223';
    Variable2='ADate'; /* check ADate for encounters that are not IP/IS */
    Value='09'; FlagCondition=strip(variable)||"='09' and Adate ge '01oct2015'd"; output;
    Value='10'; FlagCondition=strip(variable)||"='10' and Adate lt '01oct2015'd"; output;
  end;
  if lowcase(Variable)='dx_codetype' then do;
    CheckID='223';
    Variable2='DDate'; /* check DDate for encounters that are IP/IS */
    Value='09'; FlagCondition=strip(variable)||"='09' and Ddate ge '01oct2015'd"; output;
    Value='10'; FlagCondition=strip(variable)||"='10' and Ddate lt '01oct2015'd"; output;
  end;
  if lowcase(Variable)='dx' then do;
    CheckID='223';
    Variable2='Dx_Codetype';
    Value='09'; FlagCondition="Dx_Codetype='09' and index((strip("||strip(variable)||")),'.')>0"; output;
    Value='10'; FlagCondition="Dx_Codetype='10' and index((strip("||strip(variable)||")),'.')>0"; output;
  end;
  if tabid eq 'PRO' then do;
    if lowcase(Variable)='px' then do; *all checks valid for PRO, only ICD9/ICD10 decimal checks valid for LAB;
      CheckID='223';
      Variable2='PX_Codetype';
      Value='09'; FlagCondition="Px_Codetype='09' and index((strip("||strip(variable)||")),'.')>0"; output;
      Value='10'; FlagCondition="Px_Codetype='10' and index((strip("||strip(variable)||")),'.')>0"; output;
      Value='CV'; FlagCondition="Px_Codetype='CV' and notdigit(strip("||strip(variable)||"))>0"; output;
      Value='C2'; FlagCondition="Px_Codetype='C2' and notdigit(substr(strip("||strip(variable)||"),1,4))>0"; output;
      Value='C3'; FlagCondition="Px_Codetype='C3' and notdigit(substr(strip("||strip(variable)||"),1,4))>0"; output;
      Value='C4'; FlagCondition="Px_Codetype='C4' and not (prxmatch('/\b\d{4}[AaMmUu]\b/',strip("||strip(variable)
                  ||"))) AND not (prxmatch('/\b\d{5}\b/',strip("||strip(variable)||")))"; output;
    end;
    if lowcase(Variable)='px_codetype' then do;
      CheckID='223';
      Variable2='ADate'; /* check ADate for encounters that are not IP/IS */
      Value='09'; FlagCondition=strip(variable)||"='09' and Adate ge '01oct2015'd"; output;
      Value='10'; FlagCondition=strip(variable)||"='10' and Adate lt '01oct2015'd"; output;
    end;
    if lowcase(Variable)='px_codetype' then do;
      CheckID='223';
      Variable2='DDate'; /* check DDate for encounters that are not IP/IS */
      Value='09'; FlagCondition=strip(variable)||"='09' and Ddate ge '01oct2015'd"; output;
      Value='10'; FlagCondition=strip(variable)||"='10' and Ddate lt '01oct2015'd"; output;
    end;
  end;
  if lowcase(Variable)='dx' then do;
    CheckID='228';
    Variable2='DX_Codetype';
    Value='09'; FlagCondition="Dx_Codetype='09' and (lengthc(strip("||strip(variable)||")) lt 3 OR lengthc(strip("||strip(variable)||")) gt 5)"; output;
    Value='10'; FlagCondition="Dx_Codetype='10' and (lengthc(strip("||strip(variable)||")) lt 3 OR lengthc(strip("||strip(variable)||")) gt 7)"; output;
  end;
  if lowcase(variable)='drg_type' then do;
    CheckID='229';
    Variable2='ADate';
    Value='1'; FlagCondition=strip(variable)||"='1' and Adate ge '01oct2007'd"; output;
    Value='2'; FlagCondition=strip(variable)||"='2' and Adate lt '01oct2007'd"; output;
  end;
  if tabid eq 'PRO' then do;
    if lowcase(Variable)='px' then do;
      CheckID='228';
      Variable2='PX_Codetype';
      Value='09'; FlagCondition="PX_Codetype='09' and (lengthc(strip("||strip(variable)||")) lt 3 OR lengthc(strip("||strip(variable)||")) gt 4)"; output;
      Value='10'; FlagCondition="PX_Codetype='10' and lengthc(strip("||strip(variable)||")) ne 7"; output;
      Value='CV'; FlagCondition="PX_Codetype='CV' and (lengthc(strip("||strip(variable)||")) lt 1 OR lengthc(strip("||strip(variable)||")) gt 3)"; output;
    end;
  end;
  /* NDC value checks 223 and 228 in PRE */
  if lowcase(Variable)='rx' and lowcase(tabid)='pre' then do;
    CheckID='223';
    Variable2='RX_Codetype';
    Value='ND'; FlagCondition="RX_Codetype='ND' and notdigit(strip("||strip(variable)||"))>0"; output;
    CheckID='228';
    Value='GP'; FlagCondition="RX_Codetype='GP' and lengthc(strip("||strip(variable)||")) ne 14"; output;
    Value='ND'; FlagCondition="RX_Codetype='ND' and (lengthc(strip("||strip(variable)||")) < 9 or lengthc(strip("||strip(variable)||")) >11)"; output;
  end;
  /* NDC value Checks 223 and 228 in DIS */
  if lowcase(Variable)='rx' and lowcase(tabid) ='dis' then do;
    CheckID='223';
    Variable2='RX_Codetype';
    Value='ND'; FlagCondition="RX_Codetype='ND' and notdigit(strip("||strip(variable)||"))>0"; output;
    CheckID='228';
    Value='ND'; FlagCondition="RX_Codetype='ND' and (lengthc(strip("||strip(variable)||")) < 9 or lengthc(strip("||strip(variable)||")) >11) "; output;
  end;
  if lowcase(Variable)='specialty_codetype' then do;
    CheckID='228';
    Variable2='Specialty';
    Value='2'; FlagCondition=strip(variable)||"='2' and lengthc(strip(Specialty)) ne 2"; output;
    Value='0'; FlagCondition=strip(variable)||"='0' and lengthc(strip(Specialty)) ne 10"; output;
  end;
  if lowcase(Variable)='facility_location' then do;
    CheckID='223';
    Variable2='FacilityID';
    Value='null'; FlagCondition="FacilityID ne .U and "||strip(variable)||" is null"; output;
    Value='not null'; FlagCondition="FacilityID = .U and "||strip(variable)||" is not null"; output;
  end;
  /** COD Death Code Checks */
  if lowcase(tabid) = 'cod' and lowcase(Variable) = 'cod' then do;
  /** CheckID223: Assess Form */
    CheckID='223';
    Variable2='Codetype';
    Value='09'; FlagCondition="CodeType='09' and index((strip("||strip(variable)||")),'.')>0"; output;
    Value='10'; FlagCondition="CodeType='10' and index((strip("||strip(variable)||")),'.')>0"; output;
    Value='NV'; FlagCondition="CodeType='NV' and not (prxmatch('/\b\d{4}[A-Za-z]\d{2}\b/',strip("||strip(variable) ||")))"; output;

  /** CheckID228: Assess Length */
    CheckID='228';
    Variable2='CodeType';
    Value='09'; FlagCondition="CodeType='09' and (lengthc(strip("||strip(variable)||")) lt 3 OR lengthc(strip("||strip(variable)||")) gt 5)"; output;
    Value='10'; FlagCondition="CodeType='10' and (lengthc(strip("||strip(variable)||")) lt 3 OR lengthc(strip("||strip(variable)||")) gt 7)"; output;
    Value='NV'; FlagCondition="CodeType='NV' and (lengthc(strip("||strip(variable)||")) ne 7)"; output;
  end;
run;

proc sql noprint;
  create table msoc.lkp_all_l2 as
  select CheckID
       , TabID
       , Variable as Variable1
       , Variable2
       , Value
       , AbortYN
       , FlagCondition
  from lkp_all_l2
  order by 1,2,3,4,5,6
  ;
  drop table lkp_all_l2
  ;
quit;

/*-------------------------------------------------------------------------------------*/
/* 10. Create and output table MSOC.LKP_ALL_MINMAX                                     */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_all_minmax;
  set qajson.lkp_all_minmax;
run;

/*-------------------------------------------------------------------------------------*/
/* 11. Create and output table MSOC.CONTROL_FLOW.CSV                                   */
/*-------------------------------------------------------------------------------------*/
data dplocal.control_flow;
  set qajson.control_flow;
run;

%ds2csv (data=dplocal.control_flow,
         runmode=b,
         csvfile=&msoc.\control_flow.csv );

/*-------------------------------------------------------------------------------------*/
/* 12. Create and output table MSOC.LKP_DEM_ZIP                                        */
/*-------------------------------------------------------------------------------------*/
%let month=%sysfunc(substr(&version.,1,3));
%let year=%sysfunc(substr(&version.,4,2));
%let quarter=%sysfunc(qtr("01&month.&year."d));
%let yearquarter=&year.q&quarter.;
%put &yearquarter;

proc cimport infile="&infolder./zipcode_&version..cpt" library=work;
run;

data temp_zip;
  set zip:;
run;

proc datasets lib=work memtype=data nolist nodetails nowarn;
  delete zip:;
run; quit;

proc sql noprint;
  create table msoc.lkp_dem_zip as
  select put(zip, z5.) as PostalCode
       , statecode
  from temp_zip (keep=zip statecode)
  order by 1,2
  ;
quit;

proc sort data=msoc.lkp_dem_zip nodupkey;
   by postalcode;
run;quit;

/*-------------------------------------------------------------------------------------*/
/* 13. Create and output table MSOC.LKP_ALL_SASLENGTH                                  */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_all_saslength;
  set qajson.lkp_all_saslength;
run;

/*SAS VarLength capacity format */
/*data _varlenf (drop = bytes capacity);*/
/*  length fmtname $8 type label $1 lstart end 8.;*/
/*  set msoc.lkp_all_saslength end=eof;*/
/*  retain fmtname "varlenf" type "n";*/
/*  label = put(bytes,1.);*/
/*  end = input(capacity,comma22.);*/
/*  lstart = lag(end);*/
/*  format lstart end comma22.;*/
/*  output;*/
/*  if eof then do;*/
/*    label = "U";*/
/*    hlo = 'O';*/
/*    output;*/
/*  end;*/
/*run; */

/*data varlenf (drop = lstart);*/
/*  set _varlenf;*/
/*  if _n_ = 1 then start = 1;*/
/*  else start = lstart + 1;*/
/*run; */

/*data msoc.lkp_l1_idlength;*/
/*  retain fmtname type label start end hlo;*/
/*  set varlenf;*/
/*run;*/

/*-------------------------------------------------------------------------------------*/
/* 14. Create and output table MSOC.LKP_PRE_<variable>                                 */
/*-------------------------------------------------------------------------------------*/
%macro lkp_pre_dose;
  %local varlist i;

  %let tabid=PRE;
  %let varidlist = 11 13 14;

  %let maxlength_var=0;
  %let maxlength_value=0;
  %let maxlength_descr=0;

  %do i=1 %to 3;
    %let varid=%scan(&varidlist.,&i.);

    proc sql noprint;
      select upcase(variable), varlength into :var trimmed, :length_value trimmed
      from msoc.lkp_all_l1 (where=(tabid="&tabid." and varid="&varid."))
      ;
      select length into :length_descr trimmed
      from dictionary.columns where LIBNAME="QAJSON" and memname="LKP_PRE_&var" and name="Description"
      ;
    quit;

    %let length_var=%sysfunc(lengthn(&var));

    %if &maxlength_value. lt &length_value. %then %do;
      %let maxlength_value=&length_value.;
    %end;
    %if &maxlength_var. lt &length_var. %then %do;
      %let maxlength_var=&length_var.;
    %end;
    %if &maxlength_descr. lt &length_descr. %then %do;
      %let maxlength_descr=&length_descr.;
    %end;
    data msoc.lkp_pre_&var.;
      retain Variable;
      length Value $&length_value. ;
      set qajson.lkp_pre_&var.;
    run;
  %end;

  data dplocal.lkp_pre_all;
    length Variable $&maxlength_var. Value $&maxlength_value. Description $&maxlength_descr.;
    set msoc.lkp_pre_: ;
  run;

%mend;
%lkp_pre_dose;

/*-------------------------------------------------------------------------------------*/
/* 15. Create and output table MSOC.LKP_PRM_TYPE                                       */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_prm_type;
  set qajson.lkp_prm_type;
run;

/*-------------------------------------------------------------------------------------*/
/* 16. Create and output table MSOC.LKP_ENC_L2                                         */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_enc_l2;
  set qajson.lkp_enc_l2;
run;

/*-------------------------------------------------------------------------------------*/
/* 17. Create and output table MSOC.LKP_RATE_THRESHOLD                                 */
/*-------------------------------------------------------------------------------------*/
proc sql noprint;
  create table msoc.lkp_rate_threshold as
  select CheckID
       , TableID
       , TestID
       , cats(TableID, CheckID , compress(TestID, '-'),'F')  as FMTNAME length=10
       , LABEL
       , START
       , END
       , SEXCL
       , EEXCL
       , HLO
       , TYPE
  from qajson.lkp_rate_threshold
  ;
quit;

/*-------------------------------------------------------------------------------------*/
/* 18. Create and output table MSOC.LKP_LAB_LOINC                                      */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_lab_loinc;
  set qajson.lkp_lab_loinc;
run;

/*-------------------------------------------------------------------------------------*/
/* 19. Create and output table MSOC.LKP_ALL_FLAGS                                      */
/*-------------------------------------------------------------------------------------*/

%macro expand_tables;
  proc sql noprint;
    select distinct tabid, count(distinct tabid) into :alllist separated by " ", :allct trimmed
    from msoc.lkp_all_l1
    ;
/* Lists for Table1=any and Table2=000 and _Varnum=01 (PatID) */
    select distinct tabid, count(distinct tabid) into :enrid_list separated by " ", :enrid_ct trimmed
    from msoc.lkp_all_l1
    where lowcase(variable)="patid"
    ;
/* Lists for Table1=any and Table2=ENR and _Varnum=01 (PatID) */
    select distinct tabid, count(distinct tabid) into :enrlist separated by " ", :enrct trimmed
    from msoc.lkp_all_l1
    where lowcase(variable)="patid" and tabid ne "ENR"
    ;
/* Lists for Table1=any and Table2=000 and _Varnum=02 (EncounterID) */
    select distinct tabid, count(distinct tabid), varid into :encid_list separated by " ", :encid_ct trimmed, :varidlist_enc separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="encounterid"
    ;
/* Lists for Table1=any and Table2=ENC and _Varnum=02 (EncounterID) */
    select distinct tabid, count(distinct tabid), varid into :enclist separated by " ", :encct trimmed, :any02list separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="encounterid" and tabid ne "ENC"
    ;
/* Lists for Table1=any and Table2=000 and _Varnum=03 (FacilityID) */
    select distinct tabid, count(distinct tabid), varid into :facid_list separated by " ", :facid_ct trimmed, :varidlist_fac separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="facilityid"
    ;
/* Lists for Table1=any and Table2=FAC and _Varnum=01 (FacilityID) */
    select distinct tabid, count(distinct tabid), varid into :faclist separated by " ", :facct trimmed, :any03list separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="facilityid" and tabid ne "FAC"
    ;
/* Lists for Table1=any and Table2=000 and _Varnum=01 (ProviderID) */
    select distinct tabid, count(distinct tabid), varid into :pvdid_list separated by " ", :pvdid_ct trimmed, :varidlist_pvd separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="providerid"
    ;
/* Lists for Table1=any and Table2=PVD and _Varnum=01 (ProviderID) */
    select distinct tabid, count(distinct tabid), varid into :pvdlist separated by " ", :pvdct trimmed, :any04list separated by " "
    from msoc.lkp_all_l1
    where lowcase(variable)="providerid" and tabid ne "PVD"
    ;
/* Lists for unique key variable(s) by table */
    select distinct tabid, count(distinct tabid) into :keylist separated by " ", :keyct trimmed
    from msoc.lkp_all_l1
    where keyvar="K"
    ;
 /* Lists for sort order variable(s) by table */
    select distinct tabid, count(distinct tabid) into :sortlist separated by " ", :sortct trimmed
    from msoc.lkp_all_l1 (where=(sortorder ne .))
    ;
  quit;

  data dplocal.master_key;
    set qajson.masterkey;
    where=checkid ne " ";
  run;

  proc sql noprint;
    create table _lkp_all_flags as
    select a.*, b.check_descr as CheckID_Description
    from qajson.lkp_all_flags (drop=temp) a left join dplocal.master_key (keep=checkid check_descr) b
    on a.checkid=b.checkid
    ;
  quit;

  data dplocal.check_key;
    length Level $1;
    set _lkp_all_flags;
    level=substr(CheckID,1,1);
/*    CheckID_Description=transtrn(tranwrd(CheckID_Description,'&amp;','&'),'0D0A'x,' ');*/
    if checkid ne ' ';
    if substr(_varnum1,1,2)="vj" then do;
  %do j=1 %to &sortct.;
    %let tabid=%scan(&sortlist.,&j.);
      Table1="&tabid.";
      output;
  %end;
    end;
    else if substr(_varnum1,1,2)="vk" then do;
  %do k=1 %to &keyct.;
    %let tabid=%scan(&keylist.,&k.);
        Table1="&tabid.";
        output;
  %end;
    end;
    else if table1="all" and table2="000" then do;
  %do t=1 %to &allct.;
    %let tabid=%scan(&alllist.,&t.);
      Table1="&tabid.";
      output;
  %end;
    end;
    else if table1="any" and table2="000" then do;
      if substr(_varnum1,1,2)="01" then do;
  %do a=1 %to &enrid_ct.;
    %let tabid=%scan(&enrid_list.,&a.);
        Table1="&tabid.";
        output;
  %end;
      end;
      else if substr(_varnum1,1,2)="02" then do;
  %do b=1 %to &encid_ct.;
    %let tabid=%scan(&encid_list.,&b.);
    %let varid=%scan(&varidlist_enc.,&b.);
        Table1="&tabid.";
        _Varnum1="&varid.";
        output;
  %end;
      end;
      else if substr(_varnum1,1,2)="03" then do;
  %do c=1 %to &facid_ct.;
    %let tabid=%scan(&facid_list.,&c.);
    %let varid=%scan(&varidlist_fac.,&c.);
        Table1="&tabid.";
        _Varnum1="&varid.";
        output;
  %end;
      end;
      else if substr(_varnum1,1,2)="04" then do;
  %do d=1 %to &pvdid_ct.;
    %let tabid=%scan(&pvdid_list.,&d.);
    %let varid=%scan(&varidlist_pvd.,&d.);
        Table1="&tabid.";
        _Varnum1="&varid.";
        output;
  %end;
      end;
    end;
    else if table1="any" and table2="ENR" then do;
  %do e=1 %to &enrct.;
    %let tabid=%scan(&enrlist.,&e.);
      Table1="&tabid.";
      output;
  %end;
    end;
    else if table1="any" and table2="ENC" then do;
  %do f=1 %to &encct.;
    %let tabid=%scan(&enclist.,&f.);
    %let varid=%scan(&any02list.,&f.);
      Table1="&tabid.";
      _Varnum1="&varid.";
      Varnum2="02";
      output;
  %end;
    end;
    else if table1="any" and table2="FAC" then do;
  %do g=1 %to &facct.;
    %let tabid=%scan(&faclist.,&g.);
    %let varid=%scan(&any03list.,&g.);
      Table1="&tabid.";
      _Varnum1="&varid.";
      Varnum2="01";
      output;
  %end;
    end;
    else if table1="any" and table2="PVD" then do;
  %do h=1 %to &pvdct.;
    %let tabid=%scan(&pvdlist.,&h.);
    %let varid=%scan(&any04list.,&h.);
      Table1="&tabid.";
      _Varnum1="&varid.";
      Varnum2="01";
      output;
  %end;
    end;
    else do; output; end;
  run;
%mend;
%expand_tables;

/* Call expand_tab to populate Table2 where Table2=ANY with actual TABIDs */
%macro expand_check201(table_values=, var_values=);

  %do i=1 %to %sysfunc(countw(&table_values.));
    %expand_tab(dsn=dplocal.check_key, checkval=201, table1val=%scan(&table_values.,&i.), table2val=any, keyvar=%scan(&var_values.,&i.));
  %end;

%mend expand_check201;

%expand_check201(table_values= ENR ENC FAC PVD, var_values = patid encounterid facilityid providerid);

%macro generate_formats;
/* Create formats to link VarID to name of Variable (e.g. 01 = Patid) */
%do i=1 %to 2;
  %if &i=1 %then %do;
    proc sql noprint;
      select distinct tabid into :table_list separated by " "
      from msoc.lkp_all_l1
      ;
    quit;
  /* Creates formats for converting VarID (e.g. 01) into Variable (e.g. PatID) for each tables */
    %let ds=lkp_all_l1;
    %let start=VarID;
    %let label=variable;
  %end;
  /* Create formats for converting the TestNum (e.g. 01) into MS_Test_Name (e.g. ALP) for LAB table only */
  %else %do;
    %let table_list=LAB;
    %let ds=msoc.lkp_lab_test;
    %let start=testnum;
    %let label=ms_test_name;
  %end;

  %do t=1 %to %sysfunc(countw(&table_list.));
    %let tabid=%scan(&table_list.,&t.);

    %if &i.=1 %then %do;
      proc sort data=msoc.&ds. out=&ds.;
        by tabid varid;
        where tabid="&tabid.";
      run;
    %end;

    data fmt_&tabid.;
      set &ds.(keep=&start. &label. rename=(&start.=start &label.=label)) end=last;
      retain fmtname "$&tabid._&label.";
      output;
      if last then do;
        fmtname="$&tabid._&label.";
        start='00';
        label ='NA';
        output;
      end;
    run;

    proc sort data=fmt_&tabid. nodupkey;
      by start;
    run;

    proc format cntlin=fmt_&tabid.;
    run;
  %end;
 /* Create formats for converting _Result_Type (e.g. 'c') into Result_Type (e.g. 'C') for LAB table only */
  %if &i.=2 %then %do;
    proc format;
      value $&tabid._result_type
      'c' = 'C'
      'n' = 'N'
      ;
    run;
  %end;

%end;
proc datasets lib=work memtype=data nowarn nolist nodetails;
  delete fmt_:;
quit;
%mend;
%generate_formats;

%macro generate_flags;

proc sql noprint;
  create table work.lkp_flags
    ( FlagType char(4)
    , AbortYN char(1)
    , TableID char(7)
    , Level char(1)
    , VarID char(2)
    , TestID char(4)
    , CheckID char(3)
    , Variable1 char(21)
    , Variable2 char(21)
    , Variable3 char(21)
    , Variable4 char(21)
    , MS_Test_Name char(20)
    , Result_type char(1)
    , Flag_Descr char(255)
    , Dataset char(50)
    , DatasetOutQA char(50)
    , DatasetOutComp char(50)
    , Lookup_Table char(20)
    , FlagYN char(1)
    )
  ;
  create table tempflag (drop=temp:) as
  select monotonic( ) as row
       , *
       , case when (table2=table1 or table2='000') then upcase(table1)
         else catx("-",upcase(table1),upcase(table2))
         end as table length=7
       , substr(temp1,1,2) as _varnum1 length=2
       , substr(temp2,1,2) as _testnum length=2
       , substr(temp3,1,1) as _result_type length=1
       , case when calculated _varnum1 contains 'v' then 'v0'
              else calculated _varnum1
         end as varpop length=2
       , case when calculated _testnum contains 't' then 't0'
              else calculated _testnum
         end as testpop length=2
       , case when calculated _result_type contains 'a' then '_'
         else upcase(calculated _result_type)
         end as resultpop length=1
  from dplocal.check_key (rename=(_varnum1=temp1 _testnum=temp2 _result_type=temp3))
  ;
  quit;

  %let fcount=&sqlobs.;

  %do f=1 %to &fcount.;

    proc sql noprint;
      select flagtype
           , checkid
           , level
           , table
           , table1
           , table2
           , _varnum1
           , varnum2
           , varnum3
           , varnum4
           , _testnum
           , _result_type
           , varpop
           , testpop
           , resultpop
           , datasetin
           , datasetoutqa
           , datasetoutcomp
           , lookup_table
           , flagyn
           , abortyn
      into :flagtype trimmed
         , :checkid trimmed
         , :level trimmed
         , :tableid trimmed
         , :table1 trimmed
         , :table2 trimmed
         , :varnum1 trimmed
         , :varnum2 trimmed
         , :varnum3 trimmed
         , :varnum4 trimmed
         , :_testnum trimmed
         , :_result_type trimmed
         , :varpop trimmed
         , :testpop trimmed
         , :resultpop trimmed
         , :ds trimmed
         , :dsout1 trimmed
         , :dsout2 trimmed
         , :lkp trimmed
         , :flagyn trimmed
         , :abortyn trimmed
      from tempflag
      where row=&f.
      ;
    quit;

      /* Populate variable 2-4 */
      %if &table2. ne 000 and &varnum2. ne 00 %then %let variable2=%sysfunc(putc(&varnum2.,&table2._variable.));
      %else %let variable2=%sysfunc(putc(&varnum2.,&table1._variable.));
      %let variable3=%sysfunc(putc(&varnum3.,&table1._variable.));
      %let variable4=%sysfunc(putc(&varnum4.,&table1._variable.));

      /* Redefine macros for expansion to default values */
      %let v_exclude = ; /* init to blank to utilize logic to include condition when applicable */
      %let varct=1;
      %let var1list=%sysfunc(putc(&varnum1.,&table1._variable.));
      %let varidlist=%bquote(&varnum1.);

      /*no variable list to expand*/
      %if &varpop. ne v0 %then %do;
        %let var1list=%sysfunc(putc(&varnum1.,&table1._variable.));
        %let varidlist=%bquote(&varnum1.);
      %end;

      /*(vj) Sort by Variables - no expansion required */
      %else %if &varnum1.=vj %then %do;
        %let var1list=%sysfunc(putc(00,&table1._variable.));
        %let varidlist=00;
      %end;

      /*(vk) Unique Key Variables - no expansion required */
      %else %if &varnum1.=vk %then %do;
        %let var1list=%sysfunc(putc(00,&table1._variable.));
        %let varidlist=00;
      %end;

      /* All other "v0" expanded list variables*/
      %else %do;

         /* create exclusion list for entries separately defined per checkid, table, and variable */
         proc sql noprint;
            select quote(strip(var1)) into :v_exclude separated by ','
            from tempflag
            where     checkid="&checkid."
                  and table1 = "&table1."
                  and varpop ne "v0";
         quit;

         /* Define conditions for selection expansion variables from lkp_all_l1 based on wildcard value */

            /* va= All variables in table */
            %if &varnum1.=va
                 %then %let where1=%str(variable ne "_");

            /* vl= Variables with defined length */
            %if &varnum1.=vl
                %then %let where1=%str(varlength ne 0);

            /* vp= Some missing/blank values allowed */
            %if &varnum1.=vp
                %then %let where1=%str(lowcase(obsreq)='some');

            /* vr= No missing/blanks values allowed */
            %if &varnum1.=vr
                %then %let where1=%str(lowcase(obsreq)='all');

            /* vw= Variables with defined valid values, excluding 121 flags that are warn instead of fail */
            %if &varnum1.=vw
                %then %let where1=%str(validvaluetype ne " " );

            /* vx= Character variables with length >1 and no valid value list, excluding lab characterization variables */
            %if &varnum1.=vx
                %then %let where1=%str(    vartype='C'
                                       and varlength >1
                                       and validvaluetype not in ('list_values', 'reference_table', 'find_values')
                                       and not(upcase(tabid)="LAB" and varid in ('20','24','25')) );

            /* vy= Site-specific num variables  */
            %if &varnum1.=vy
                %then %let where1=%str(vartype='N' and varlength=0);

            /* Select expanded variable list and load into varlist and varidlist parameters */
            proc sql noprint;
                select count(*), variable, varid into :varct, :var1list separated by " ", :varidlist separated by " "
                from msoc.lkp_all_l1
                where     tabid="&table1."
                      and &where1.
                      %if %length(&v_exclude.) > 0
                      %then %do;
                          and not(variable in (&v_exclude.))
                      %end;;
            quit;

      %end; /*end else do for all other "v0" expanded list variables*/

      %do v=1 %to &varct.;

        %let variable1=%scan(&var1list.,&v.);
        %let varid=%scan(&varidlist.,&v.);

        %if &testpop. ne t0 and &resultpop. ne %str(_) %then %do;
          %let testct=1;
          %let testnumlist=&_testnum.;
          %let testlist=%sysfunc(putc(&_testnum.,lab_ms_test_name.));
          %let restypelist=&_result_type.;
        %end;

        %else %if &_testnum.=tr %then %do;
          proc sql noprint;
            create table temp as
            select count(distinct result_type) as typect, testnum, ms_test_name, result_type
            from msoc.lkp_lab_test (where=(characterized='Y'))
            group by testnum, ms_test_name
            having calculated typect = 1
            ;
            select count(*), ms_test_name, testnum, result_type into :testct, :testlist separated by " ", :testnumlist separated by " ", :restypelist separated by " "
            from temp
          %if &resultpop. ne _ %then %do;
            where result_type="&resultpop."
          %end;
            ;
            drop table temp
            ;
          quit;
        %end; /* end tr do statement */

        %else %do;
          proc sql noprint;
            select count(*), ms_test_name, testnum, result_type into :testct, :testlist separated by " ", :testnumlist separated by " ", :restypelist separated by " "
            from msoc.lkp_lab_test
          %if &_testnum.=ta and &resultpop.=_ %then %let where2=%str( );
          %else %if &_testnum.=ta and &resultpop. ne _ %then %let where2=%str( (where=(result_type="&resultpop.")) );

          %else %if &_testnum.=tc %then %do;
            %if &resultpop=_ %then %let where2=%str( (where=(characterized='Y')) );
            %else %let where2=%str( (where=(characterized='Y' and result_type="&resultpop.")) );
          %end;

          %else %if &_testnum.=tm %then %do;
            %let where2=%str( (where=(result_type ne "U")) );
          %end;

          %else %if &_testnum.=tu %then %do;
            %if &resultpop=_ %then %let where2=%str( (where=(characterized='Y' and UnitReq='Y')) );
            %else %let where2=%str( (where=(characterized='Y' and UnitReq='Y' and result_type="&resultpop.")) );
          %end;
          %else %if &_testnum.=tw %then %do;
            %if &resultpop.=_ %then %let where2=%str( (where=(characterized='Y' and UnitReq='N')) );
            %else %let where2=%str( (where=(characterized='Y' and UnitReq='N' and result_type="&resultpop.")) );
          %end;
          %else %if &_testnum.=tz %then %do;
            %if &resultpop=_ %then %let where2=%str( (where=(characterized='N')) );
            %else %let where2=%str( (where=(characterized='N' and result_type="&resultpop.")) );
          %end;
            &where2.
            ;
          quit;
        %end; /* end else do - test number expansion */

        %do t=1 %to &testct.;

          %let test=%scan(&testlist.,&t.);
          %let testnum=%scan(&testnumlist.,&t.);
          %let result_type=%upcase(%scan(&restypelist.,&t.));

          %let ms_test_name=%sysfunc(putc(&testnum.,LAB_ms_test_name.));
          %let testid=&testnum.-&result_type;

          %if &table1.=LAB and &table2.=LAB %then %do k=1 %to 4;
            %let var=&&varnum&k.;
            %if %unquote(&var.=02) %then %let value&k.=%sysfunc(putc(&testnum.,&&table1._&&variable&k..));
            %if %unquote(&var.=03) %then %let value&k.=%sysfunc(putc(&result_type.,&&table1._&&variable&k..));
          %end;

          /* populate additional parameters to describe checks based on sort variables */
          %if &varnum1.=vj %then %do;
            proc sql noprint;
                select variable into :usortlist separated by ", "
                from msoc.lkp_all_l1 (where=(tabid="&table1." and sortorder ne .))
                order by sortorder;
            quit;
          %end;

          /* populate additional parameters to describe checks based on key variables */
          %if &varnum1.=vk %then %do;
            proc sql noprint;
              select variable into :ukeylist separated by ", "
              from msoc.lkp_all_l1 (where=(tabid="&table1." and keyvar="K"));
            quit;
         %end;

          /* Insert record for table, variable, and check into flags table */
          proc sql noprint;
             select %bquote(CheckID_description) into :checkdescr trimmed
             from tempflag
             where checkid="&checkid.";

          insert into work.lkp_flags
              ( flagtype
              , abortyn
              , level
              , tableid
              , varid
              , testid
              , checkid
              , variable1
              , variable2
              , variable3
              , variable4
              , ms_test_name
              , result_type
              , flag_descr
              , dataset
              , datasetoutqa
              , datasetoutcomp
              , lookup_table
              , flagyn
              )
          values
              ( "&flagtype."
              , "&abortyn."
              , "&level."
              , "&tableid."
              , "&varid."
              , "&testid."
              , "&checkid."
              , "&variable1."
              , "&variable2."
              , "&variable3."
              , "&variable4."
              , "&ms_test_name."
              , "&result_type."
              , "&checkdescr."
              , "&ds."
              , "&dsout1."
              , "&dsout2."
              , "&lkp."
              , "&flagyn."
              )
          ;
        quit;

        %end; /*end loop t*/
      %end; /*end loop v*/
  %end; /*end loop f*/

  /* Update lkp_flags to add flagID and format variables */
  proc sql noprint;
    update work.lkp_flags
    /* exclude LAB and VIT table from L3 expanded flags */
    set flagyn=case when level='3' and (index(TableID,"LAB") gt 0 or index(TableID,"VIT") gt 0) then 'X'
          else flagyn
        end
      , dataset=case when dataset='NA' then dataset
          else lowcase(dataset)
        end
      , datasetoutqa=case when datasetoutqa='NA' then datasetoutqa
          else lowcase(datasetoutqa)
        end
      , datasetoutcomp=case when datasetoutcomp='NA' then datasetoutcomp
          else lowcase(datasetoutcomp)
        end
      , lookup_table=case when lookup_table='NA' then lookup_table
          else lowcase(lookup_table)
        end
    ;
    create table work.lkp_flags1  as
    select catx('_',tableid,level,varid,testid,checkid) as FlagID length=21
         , *
    from work.lkp_flags
    order by level, tableid, checkid, varid, testid    ;
  quit;


  proc sql noprint;
    create table lkp_all_flags2 as
    select *
    from work.lkp_flags1 (drop=datasetout: where=(flagYN="Y"))
    order by level, tableid, checkid, varid, testid
    ;
    quit;

    data msoc.lkp_all_flags;
      set lkp_all_flags2;
      retain _all_;
      /* ensure 223 dia/pro icdtype ddate flags are not being read as cross-table checks */
      if checkid="223" and index(tableid,"-")>0 then tableid=substr(tableid,1,3);
    run;

  proc sql noprint;
    create table sortorder as
    select monotonic( ) as label
        ,  upcase(a.module) as start length=3
        , "$table_sortorder" as fmtname
    from (select module from dplocal.control_flow where upcase(cc_table) ne "X") as a
    ;
  quit;

  proc format cntlin=sortorder;
  run;

  proc sql noprint;
    create table master_all_flags as
    select *
         , put(substr(tableid,1,3),$table_sortorder.) as sort1
         , case when substr(tableid, 4,4) ne "-" then put(substr(tableid,1,3),$table_sortorder.)||'.'||put(substr(tableid,5,3),$table_sortorder.)
           else put(substr(tableid,1,3),$table_sortorder.)||'.0'
           end as sort2
    from work.lkp_flags1 (where=(flagYN ne "X"))
    order by sort1, level, varid, checkid, testid, sort2
    ;

    create table dplocal.master_all_flags as
    select *
         , monotonic( ) as SortOrder length=3
    from master_all_flags (drop=sort:)
    ;
  quit;

  ods noresults;
  title;
  footnote;
  ods listing close;
  ods html close;

  ods excel file="&dplocal.\master_all_flags_QAv&QAver._SCDMv&SCDMver._&sysdate9..xlsx"
    options(sheet_name="master_all_flags" frozen_headers = '1' flow='tables');
    proc print data=dplocal.master_all_flags noobs;
    var _all_ / style(data)={tagattr='type:String'};
    run;
  ods excel close;

%mend;
%generate_flags;


/* add a label to data sets with date and QA/SCDM versions  */
%macro label_ds (lib);
  %local ds;
  %let lib=%upcase(&lib.);
  %let QAver=%sysfunc(strip(&QAver.));
  %let SCDMver=%sysfunc(strip(&SCDMver.));

  proc sql noprint;
    select memname, count(memname) into :dslist separated by " ", :dsct trimmed
    from dictionary.members
    where libname="&lib." and memtype='DATA'
    ;
  quit;

  %do i=1 %to &dsct.;
    %let ds=%scan(&dslist.,&i.);

    proc datasets lib=&lib. nolist;
      modify &ds. (label="QA version &QAver., SCDM version &SCDMver., &sysdate9.");
    run; quit;

  %end;
%mend;
%label_ds (msoc);
%label_ds (dplocal);

/* Delete temporary datasets */
proc datasets lib=work kill nolist nodetails nowarn; run; quit;

proc datasets lib=dplocal nolist nodetails nowarn;
  delete control_flow;
run; quit;


*****  END OF PROGRAM  *****************************************************************;
