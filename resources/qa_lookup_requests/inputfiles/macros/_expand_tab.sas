****************************************************************************************************
*                                           PROGRAM OVERVIEW
****************************************************************************************************
*
* PROGRAM: expand_tab.sas
* Created (mm/dd/yyyy): 06/12/2023
*
*--------------------------------------------------------------------------------------------------
* PURPOSE: The macro expands rows of the dplocal.check_key dataset based on the conditions set for
*          table1val, table2val, and checkval where either table1 or table2 is ANY or ALL.
*
* Program inputs:
*   - dsn: The name of the dataset you want to process. Currently utilizes dplocal.check_key.
*   - checkval: The CheckID number for which you are interested in expanding the ANY or ALL variables to check.
*   - table1val and table2val: SCDM Table values used for selecting rows for expansion. If either table1val or
*                              table2val is set to ANY or ALL, the other value should be a valid SCDM Table
*                              abbreviation.
*   - keyvar: The key variable used to filter unique 'tabid' from 'msoc.lkp_all_l1' table. Should
*             only be populated if table1val or table2val = ANY.
*
* Program outputs:
*   - An expanded version of the input dataset with new rows added based on the conditions and
*     the unique 'tabid' values fetched from 'msoc.lkp_all_l1' table.
*
*  PARAMETERS:
*   - tablist: The list of unique 'tabid' fetched from 'msoc.lkp_all_l1' table.
*   - tabct: The count of unique 'tabid' in 'tablist'.
*
*--------------------------------------------------------------------------------------------------
* CONTACT INFO:
*  Sentinel Coordinating Center
*  info@sentinelsystem.org
*
***************************************************************************************************;
%macro expand_tab (dsn=, checkval=, table1val=, table2val=, keyvar=);

  %put =====> MACRO CALLED: expand_tab ;
  %put =====> MACRO EXPAND TAB: &dsn., &checkval., &table1val., &table2val, &keyvar. ;
  %local tablist debug;

  /** Debug Mode-change to 1 to save a copy of the orginal dataset to file before modification */
  %let debug = 0;

  %if &debug. = 1 %then %do;
    %put DEBUG (Sentinel): Outputing original &dsn. as dplocal.debug_&dsn_&checkval._&table1val._&table2val.;
    data dplocal.debug_&dsn_&checkval._&table1val._&table2val.;
      set &dsn.;
    run;
  %end;
  /** End Debug */

  /** Create a list of TabIDs and save as a macro variable. These are used to update dsn. */
  proc sql noprint;
    select distinct tabid
    into :tablist separated by " "
    from msoc.lkp_all_l1
    where upcase(tabid) ne %upcase("&table1val.") and
          upcase(tabid) ne %upcase("&table2val.")
    %if
      %upcase(&table1val.) = ANY or
      %upcase(&table2val.) = ANY
    %then %do;
      and upcase(variable)=%upcase("&keyvar.");
    %end;
  quit;

  %put &=tablist;

  /** Update dsn to include a record per each tabid in tablist. */
  data &dsn. (drop=i);
    set &dsn.;
    if
      upcase(table1) = %upcase("&table1val.") and
      upcase(table2) = %upcase("&table2val.") and
      checkid="&checkval."
    then do i=1 to %sysfunc(countw(&tablist.));
      if upcase(table1) in ('ANY', 'ALL') then table1 = upcase(scan("&tablist.", i));
      else table2 = upcase(scan("&tablist.", i));
        output;
      end;
    else
      output;
  run;

  %put ******** END MACRO: expand_tab ******** ;
%mend expand_tab;