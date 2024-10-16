/*-------------------------------------------------------------------------------------*\
|  PROGRAM NAME: 01.1_create_mil_lookup_tables.sas                                      |
|---------------------------------------------------------------------------------------|
|  PURPOSE:                                                                             |
|     This program generates lookup tables for the most current QA MIL package          |
|     generation program package                                                        |
|---------------------------------------------------------------------------------------|
|  PROGRAM INPUT:                                                                       |
|     see soc_qa_mil_lookup_master.sas                                                      |
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

/***************************************************************************************/
/* 1. Import .xlsx lookup files from local package and convert to SAS datasets         */
/***************************************************************************************/
libname in xlsx "&infolder.\SCDM_lookup_table_values_MIL.xlsx";

proc sql noprint;
  create table a as
  select *
  from dictionary.tables
  where lowcase(libname)="in"
  ;
  select memname, count(memname) into :sheetlist separated by '*', :sheetct trimmed
  from a 
  where substr(memname,1,1) ne "_"
  ;
  drop table a
  ;
quit;

%import_xlsx (file=%str(&infolder.\SCDM_lookup_table_values_mil));

libname in clear;

%macro prime();
  %let prime = 2 3 5 7 11 13 17;
  data primeproduct;
    array x[7] (2 3 5 7 11 13 17);
    length p1-p7 8.;
    call missing(p1, p2, p3, p4, p5, p6, p7);
    n=dim(x);
    primeproduct = 1;
    %do k = 1 %to 7;
   	  ncomb=comb(n,&k.);
   	  do j=1 to ncomb;
        call allcomb(j, &k., of x[*]);
	    %do m = 1 %to &k.;
	  	  p&m. = x&m.;
		  primeproduct = primeproduct*p&m.;
	    %end;
	    output;
	    primeproduct = 1;
      end;
    %end;
  run;
  %global primes;
  proc sql noprint;
	 select primeproduct into: primes separated by ' '
	 from primeproduct;
  quit;
%mend prime;
%prime;
/*-------------------------------------------------------------------------------------*/
/* 2. Create and output table MSOC.LKP_ALL_L1                                          */
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
    from work.lkp_all_l1
    where validvaluetype="find_values"
    ;
  quit;
  %if %eval(&fvct. > 0) %then %do;
    %do f=1 %to &fvct.;
      %let table=%scan(&tablelist.,&f.,~);
      %let variable=%scan(&varlist.,&f.,~);
      proc sql noprint;
        select distinct &variable. into :varlist separated by '|' 
        from &table.
        ;
        create table %bquote(tempfv&f.) as
          select *
           , "&varlist." as ValidValue length=255 label=' '
        from work.lkp_all_l1
        where validvaluetype="find_values" and 
              _validvalue="&table..&variable."
        ;
      quit;
    %end; * end f loop;

    data temp_fv;
      set tempfv:;
    run;

    proc datasets lib=work nolist nowarn nodetails;
      delete tempfv:;
    quit;
  %end; *end condition fvct > 0;

  /** Validvaluetype ne find_values **/
  proc sql noprint;
    create table temp_notfv as
    select *
         , _validvalue as ValidValue length=255
    from work.lkp_all_l1
    where lowcase(validvaluetype) ne "find_values"
    ;
  quit;
   
  data msoc.lkp_all_l1 (drop=_:);
    set temp_:;
    length FlagCondition $600;
 /* Conditional Hardcode for MS_RESULT_C as a workaround for values that may contain 
      ranges (e.g. "5|10 MG/DL" */
    if lowcase(variable)="ms_result_c" and find(validvalue,'|(range)','i') ge 1 then do;
      flagcondition=strip(variable)||" not in ("||compress("'"||tranwrd(tranwrd(validvalue,"|(range)", ' '),"|", "','")||"'")||")"||"
      and "||strip(variable)||" eq (compress("||strip(variable)||",'|', ))";
    end;
    else if validvaluetype in ("find_values", "list_values") then do;
        if vartype = "C" then flagcondition=strip(variable)||" not in ("||compress("'"||tranwrd(validvalue,"|", "','")||"'")||")";
        else if vartype = "N" then flagcondition=strip(variable)||" not in ("||compress(tranwrd(validvalue,"|", ","))||")";
    end;
    else if validvaluetype in ("char_range") then flagcondition="input("||strip(variable)||",3.)"||" not in ("||compress(validvalue)||")";
    else if validvaluetype in ("num_range") then flagcondition=strip(variable)||" not in ("||compress(validvalue)||")";
    else if validvaluetype in ("value_case") then flagcondition=strip(variable)||" ne upcase("||strip(variable)||")";
    else if validvaluetype in ("no_spec_chars") then flagcondition=strip(variable)||" ne compress("||strip(variable)||",'._-','kda')";
    else if validvaluetype in ("numeric") then do;
      if (find(variable,'date','i') or find(variable,'enr_','i')) ge 1 then flagcondition=strip(variable)||" not "||strip(validvalue)||"'d";
      else if trim(lowcase(_ValidValue)) eq "product prime" then do;
	    flagcondition = strip(variable)||" not in (&primes.)";
	  end;
       else flagcondition=strip(variable)||" not "||trim(validvalue);
    end;
    else if validvaluetype in ("special_missing") then flagcondition=strip(variable)||" lt 1 and "||strip(variable)||" not in ("||strip(validvalue)||")";
    else if validvaluetype in ("only") then do;
      if validvalue="numbers" then flagcondition="notdigit(trim("||variable||")) gt 0";
      else if validvalue="numbers|decimal" then flagcondition=strip(variable)||" ne compress("||strip(variable)||",'.','kd')";
      else if validvalue="alpha" then flagcondition="notalpha(strip("||variable||")) gt 0";
      else if validvalue="alphanumeric" then flagcondition="notalnum(strip("||strip(variable)||")) gt 0";
    end;
    else if validvaluetype in ("reference_table") then flagcondition="put("||strip(variable)||",$"||compress(tabid||varid)||"fmt.) ne '1'";
  run;

/*If special missing characters are amongst your numeric data it is important to note that WHERE varname ne . will not*/
/*exclude the special characters and WHERE varname gt .z must be used instead.*/
/*WHERE varname is not missing or WHERE varname is not null can be used for both character and numeric data;*/
/*note that all missing data including special characters will be removed from the data set. */

/* Exclude any rows for retired variables */
  proc sort data=msoc.lkp_all_l1 (where=(not missing(scdmid) and 
                                  not missing(tabid)  and 
                                  not missing(varid)  and 
                                  not missing(vartype))) noduprec;
    by scdmid tabid varid;
  run; quit;

  proc datasets lib=work nolist nowarn nodetails;
    delete temp:;
  quit;
%mend;
%find_values;


/*-------------------------------------------------------------------------------------*/
/* 3. Create and output table DPLOCAL.CONTROL_FLOW and MSOC.CONTROL_FLOW.CSV          */
/*-------------------------------------------------------------------------------------*/
data dplocal.control_flow;
  set control_flow;
run;

%ds2csv (data=dplocal.control_flow, 
         runmode=b, 
         csvfile=&msoc.\control_flow.csv );

/*-------------------------------------------------------------------------------------*/
/* 4. Create and output table MSOC.LKP_ALL_SASLENGTH                                  */
/*-------------------------------------------------------------------------------------*/
data msoc.lkp_all_saslength;
  set lkp_all_saslength;
run;

* SAS VarLength capacity format;
data _varlenf (drop = bytes capacity);
  length fmtname $8 type label $1 lstart end 8.;
  set msoc.lkp_all_saslength end=eof;
  retain fmtname "varlenf" type "n";
  label = put(bytes,1.);
  end = input(capacity,comma22.);
  lstart = lag(end);
  format lstart end comma22.;
  output;
  if eof then do;
    label = "U";
    hlo = 'O';
    output;
  end;
run; 

data varlenf (drop = lstart);
  set _varlenf;
  if _n_ = 1 then start = 1;
  else start = lstart + 1;
run; 

data msoc.lkp_l1_idlength;
  retain fmtname type label start end hlo;
  set varlenf;
run;
 
/*-------------------------------------------------------------------------------------*/
/* 5. Create and output table MSOC.LKP_ALL_FLAGS                                      */
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
    set work.masterkey;
    where= not missing(checkid);
  run;

  proc sql noprint;
    create table _lkp_all_flags as
      select a.*
            ,b.check_descr as CheckID_Description
    from lkp_all_flags (drop=CheckID_Description temp) a 
      left join dplocal.master_key (keep=checkid check_descr) b
      on a.checkid=b.checkid
    ;
    drop table lkp_all_flags
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

%macro generate_formats;
/* Create formats to link VarID to name of Variable (e.g. 01 = Patid) */
  proc sql noprint;
    select distinct tabid into :table_list separated by " "
    from msoc.lkp_all_l1
    ;
  quit;
  /* Creates formats for converting VarID (e.g. 01) into Variable (e.g. PatID) for each tables */
  %let ds=lkp_all_l1;
  %let start=VarID;
  %let label=variable;
  %put &table_list;
  %do t=1 %to %sysfunc(countw(&table_list.));
    %let tabid=%scan(&table_list.,&t.);
  
    proc sort data=msoc.&ds. out=&ds.;
      by tabid varid;
      where tabid="&tabid.";
    run;
  
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
      , MS_Test_Name char(10)
      , Result_type char(1)
      , Flag_Descr char(255)
      , Dataset char(50)
      , DatasetOutQA char(50)
      , DatasetOutComp char(50)
      , Lookup_Table char(20)
      , FlagYN char(1)
      , _Linked char(2)
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

  proc sql noprint;
    select quote(strip(var1)) into :vw_exclude separated by ','
    from tempflag (where=(checkid='121' and lowcase(var1) ne 'na'))
    ;
  quit; 

  %do f=1 %to &fcount.;

    %* load input record to macro parms;
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
           ,_linked
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
         , :_linked trimmed
         , :abortyn trimmed
      from tempflag
      where row=&f.
      ;
    quit;

    %put checkID = &checkid;

    %* init to missing in order to not carry values across iterations;
    %do vN = 1 %to 4;
        %let variable&vN. = NA;
    %end; %* end VN do loop to assign variableN parm to NA;

    %* Populate variable 2-4 ;
    %if &table2. ne 000 and &varnum2. ne 00 %then 
        %let variable2=%sysfunc(putc(&varnum2.,&table2._variable.)); 
    %else %let variable2=%sysfunc(putc(&varnum2.,&table1._variable.)); 

    %if &varnum3. ne 00 and &varnum4. ne 00 and &table1. ne &table2. %then %do;
      %let variable3=%sysfunc(putc(&varnum3.,&table1._variable.));
      %let variable4=%sysfunc(putc(&varnum4.,&table2._variable.)); 
    %end;
    %else %if &varnum3. ne 00 | &varnum4. ne 00 %then %do;
      %let variable3=%sysfunc(putc(&varnum3.,&table1._variable.));
      %let variable4=%sysfunc(putc(&varnum4.,&table1._variable.));
    %end;
    /*no variable list to expand*/
    %if &varpop. ne v0 %then %do; 
      %let varct=1;
      %let var1list=%sysfunc(putc(&varnum1.,&table1._variable.));
      %let varidlist=%bquote(&varnum1.);
    %end;
     /*(vj) Sort by Variables*/
    %else %if &varnum1.=vj %then %do; 
      %let varct=1;
      %let var1list=%sysfunc(putc(00,&table1._variable.));
      %let varidlist=00;
    %end;
    /*(vk) Unique Key Variables*/
    %else %if &varnum1.=vk %then %do; 
      %let varct=1;
      %let var1list=%sysfunc(putc(00,&table1._variable.));
      %let varidlist=00;
    %end;

    /* All other "v0" expanded list variables*/
    %else %do;
      %let varct=1;
      %let var1list=%sysfunc(putc(&varnum1.,&table1._variable.));
      %let varidlist=%bquote(&varnum1.);

      %* assign where clause for selecting appropos records from lkp_all_l1;
        /* va= All variables in table */
        %if &varnum1.=va %then %let where1=%str((where=(variable ne "_")));  
        /* vc= Character variables */
        %else %if &varnum1.=vc %then %let where1=%str((where=(vartype='C'))); 
        /* vd= Date variables */      
        %else %if &varnum1.=vd %then %let where1=%str((where=(%sysfunc(index(&variable,date)) ge 1)));
        /* vl= Variables with defined length */
        %else %if &varnum1.=vl %then %let where1=%str((where=(varlength ne 0)));
        /* vn= Numeric variables */
        %else %if &varnum1.=vn %then %let where1=%str((where=(vartype='N')));
        /* vo= All missing/blank values allowed */
        %else %if &varnum1.=vo %then %let where1=%str((where=(lowcase(obsreq)='none')));
        /* vp= Some missing/blank values allowed */ 
        %else %if &varnum1.=vp %then %let where1=%str((where=(lowcase(obsreq)='some')));
        /* vr= No missing/blanks values allowed */
        %else %if &varnum1.=vr %then %let where1=%str((where=(lowcase(obsreq)='all')));
        /* vs= Character variables with length >1 and no valid value list */
        %else %if &varnum1.=vs %then %let where1=%str((where=(vartype='C' and varlength >1 and validvaluetype not in ('list_values', 'reference_table', 'find_values') )));
        /* vt= Time variables */  
        %else %if &varnum1.=vt %then %let where1=%str((where=(%sysfunc(index(&variable,time)) ge 1))); 
        /* vv= Variables with defined valid values */                  
        %else %if &varnum1.=vv %then %let where1=%str((where=(validvaluetype ne " "))); 
        /* vw= Variables with defined valid values, excluding 121 flags that are warn instead of fail */
        %else %if &varnum1.=vw %then %let where1=%str((where=(validvaluetype ne " " and not(variable in (&vw_exclude.)) ))); 
        /* vx= Character variables with length >1 and no valid value list, excluding lab characterization variables */
        %else %if &varnum1.=vx %then %let where1=%str((where=(vartype='C' and varlength >1 and validvaluetype not in ('list_values', 'reference_table', 'find_values') and not(upcase(tabid)="LAB" and varid in ('20','24','25')) )));
        /* vz= Site-specific num variables  */                  
        %else %if &varnum1.=vy %then %let where1=%str((where=(vartype='N' and varlength=0))); 
        /* vz= Site-specific char variables  */                  
        %else %if &varnum1.=vz %then %let where1=%str((where=(vartype='C' and varlength=0))); 


      proc sql noprint;
        select count(*), variable, varid into :varct, :var1list separated by " ", :varidlist separated by " "
        from msoc.lkp_all_l1
        &where1.
        where tabid="&table1."
        ; 
      quit;
    %end;   %*end do loop f=1 %to &fcount.;

    %do v=1 %to &varct.;
      %let variable1=%scan(&var1list.,&v.);
      %let varid=%scan(&varidlist.,&v.);  
 
      %if &varnum1.=vj %then %do;
        proc sql noprint;
          select variable into :usortlist separated by ", "
          from msoc.lkp_all_l1 (where=(tabid="&table1." and sortorder ne .))
          order by sortorder
          ;
        quit;   
      %end; 
        %else %if &varnum1.=vk %then %do;
          proc sql noprint;
            select variable into :ukeylist separated by ", "
            from msoc.lkp_all_l1 (where=(tabid="&table1." and keyvar="K"))
            ;
          quit;  
        %end;
 
      %if &testpop. ne t0 and &resultpop. ne %str(_) %then %do;
        %let testct=1;
        %let testnumlist=&_testnum.;
        %let testlist=%sysfunc(putc(&_testnum.,lab_ms_test_name.));
        %let restypelist=&_result_type.;
      %end;
        %else %if &_testnum.=tr %then %do;
          proc sql noprint;
            create table temp as
            select count(distinct result_type) as typect
              , testnum
              , ms_test_name
              , result_type
            from msoc.lkp_lab_test (where=(characterized='Y'))
            group by testnum, ms_test_name        
            having calculated typect = 1
            ;
            select count(*)
              , ms_test_name
              , testnum
              , result_type 
            into :testct    
              , :testlist separated by " "
              , :testnumlist separated by " "
              , :restypelist separated by " "
            from temp 
            %if &resultpop. ne _ %then %do;
              where result_type="&resultpop."
            %end;
            ;
            drop table temp
            ;
          quit;
        %end; /* end _testnum= tr condition */
          %else %do;
            proc sql noprint;
              select count(*)
                , ms_test_name
                , testnum
                , result_type 
              into :testct
                ,:testlist separated by " "
                ,:testnumlist separated by " "
                ,:restypelist separated by " "
              from msoc.lkp_lab_test
              %if &_testnum.=ta and &resultpop.=_ %then %let where2=%str( );
                %else %if &_testnum.=ta and &resultpop. ne _ %then 
                  %let where2=%str( (where=(result_type="&resultpop.")) );
                %else %if &_testnum.=tc %then %do;
                  %if &resultpop=_ %then %let where2=%str( (where=(characterized='Y')) );
                    %else %let where2=%str( (where=(characterized='Y' and result_type="&resultpop.")) );
                %end;    
                  %else %if &_testnum.=tu %then %do;
                    %if &resultpop=_ %then 
                      %let where2=%str( (where=(characterized='Y' and UnitReq='Y')) );
                      %else 
                        %let where2=%str( (where=(characterized='Y' and UnitReq='Y' AND
                          result_type="&resultpop.")) );
                  %end;
                    %else %if &_testnum.=tw %then %do;
                      %if &resultpop.=_ %then 
                        %let where2=%str( (where=(characterized='Y' and UnitReq='N')) );
                        %else %let where2=%str( (where=(characterized='Y' and UnitReq='N' AND 
                          result_type="&resultpop.")) );
                    %end;
                      %else %if &_testnum.=tz %then %do;
                        %if &resultpop=_ %then %let where2=%str( (where=(characterized='N')) );
                          %else %let where2=%str( (where=(characterized='N' and 
                            result_type="&resultpop.")) );
                      %end;
              &where2.
              ;
            quit;
          %end; * end else condition;

      %do t=1 %to &testct.;
        %let test=%scan(&testlist.,&t.);
        %let testnum=%scan(&testnumlist.,&t.);
        %let result_type=%scan(&restypelist.,&t.);

        %let ms_test_name=%sysfunc(putc(&testnum.,LAB_ms_test_name.));
        %let testid=&testnum.-&result_type;
            
        %if &table1.=LAB and &table2.=LAB %then %do k=1 %to 4;
          %let var=&&varnum&k.;
          %if %unquote(&var.=02) %then %let value&k.=%sysfunc(putc(&testnum.,&&table1._&&variable&k..));
          %if %unquote(&var.=03) %then %let value&k.=%sysfunc(putc(&result_type.,&&table1._&&variable&k..));
        %end;

        proc sql noprint;
          select %bquote(CheckID_description) into :checkdescr trimmed
          from tempflag 
          where checkid="&checkid."
          ;
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
              , _linked
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
              , "00"
              , "0"
              , "&checkdescr."
              , "&ds." 
              , "&dsout1."
              , "&dsout2."
              , "&lkp."
              , "&flagyn."
              , "&_linked."
              )
          ;
        quit;   
 
      %end; /*end loop t*/
    %end; /*end loop v*/
  %end; /*end loop f*/

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
    create table work.lkp_flags_2 as
    select catx('_',tableid,level,varid,testid,checkid) as FlagID length=21
         , *
    from work.lkp_flags
    ;
    drop table work.lkp_flags
    ;
  quit;

  /* if duplicate flags exist as flagtype warn and fail, remove fail row */
  proc sort data=work.lkp_flags_2;
    by flagid variable1 variable2 variable3 variable4 descending flagtype ;
  run;
  proc sort data=work.lkp_flags_2 out=work.lkp_flags nodupkey;
    by flagid variable1 variable2 variable3 variable4;
  run;

  proc sql noprint;
    drop table work.lkp_flags_2
    ;
    create table msoc.lkp_all_flags (drop = _flag_descr) as
    select *
        ,case when index(_flag_descr,'NA') > 0 then tranwrd(_flag_descr,', NA','') 
          else _flag_descr end as flag_descr
    from work.lkp_flags (drop=datasetout: where=(flagYN="Y") rename=(flag_descr=_flag_descr))
    order by level, tableid, checkid, varid, testid
    ;
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
    from work.lkp_flags (where=(flagYN ne "X"))
    order by sort1, level, varid, checkid, testid, sort2
    ;
    drop table work.lkp_flags
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

  ods excel file="&dplocal.\master_all_flags.xlsx" 
    options(sheet_name='flags' frozen_headers = '1' flow='tables');
    proc print data=dplocal.master_all_flags noobs;
    var _all_ / style(data)={tagattr='type:String' width=1000%};
    run;
  ods excel close;

  %let script_loc=%sysfunc(getoption(WORK))\enhance.vbs;
  %local open_workbook;
  %let open_workbook=&dplocal.\master_all_flags.xlsx;
  %let open_workbook=%sysfunc(tranwrd(&open_workbook,//,/));
  %let open_workbook=%sysfunc(tranwrd(&open_workbook,\\,\));
  %let open_workbook=%sysfunc(tranwrd(&open_workbook,/,\));
  %put &open_workbook;

  data _null_;
    file "&script_loc";
/* create the excel object */
    put "Set objExcel = CreateObject(""Excel.Application"")  ";
/* view the excel program and file, set to false to hide the whole process */
    put "objExcel.Visible = True ";
    put "objExcel.DisplayAlerts = False ";
/* open an excel file */
    put "Set objWorkbook =objExcel.Workbooks.Open(""&open_workbook"") ";
    put "objExcel.ScreenUpdating = False ";
/* rename worksheet to include QA and SCDM version #s and date */
    put "Set objWorksheet=objWorkbook.Worksheets(1)";
    put "objWorksheet.Name=""QAv&QAver. SCDMv&SCDMver. &sysdate9"" ";
/* autofit column size */
    put "objExcel.Columns(""A:Z"").AutoFit ";
    put "objExcel.ScreenUpdating = True ";
    put "objExcel.DisplayAlerts = True ";
/* save the existing excel file. use SaveAs to save it as something else */
    put "objWorkbook.Save ";
/* close the workbook */
    put "objWorkbook.Close ";
/* exit the excel program*/
   /* objExcel.Quit */
/* release objects */
    put "Set objExcel = Nothing" ;
    put "Set objWorkbook = Nothing" ;
  run;

  x "'&script_loc\'";

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
      modify &ds. (label="QA MIL version &QAver., SCDM version &SCDMver., &sysdate9.");
    run; quit;

  %end;
%mend;
%label_ds (msoc);
%label_ds (dplocal);

/* Delete temporary datasets */
proc datasets lib=work kill nolist nodetails nowarn; run; quit;


*****  END OF PROGRAM  *****************************************************************;
