options nofmterr fullstimer linesize=99 pageno=1 pagesize=65;
options mlogic mprint symbolgen spool;

/*****************************************************************************************
*                                      SENTINEL PROGRAM
******************************************************************************************
* NAME: soc_qa_lookup_master.sas
*
* PURPOSE:  To create lookup tables for the SCDM QA data checking package version 6.x.x
*
* MAJOR STEPS:
*   1. If necessary, replace the SAS zipcode file (zipcode_[VERSION].cpt), located in the
*      'inputfiles' subdirectory of master package, with the most recent version.  SAS
*      issues a new version on a quarterly basis.
*   2. Enter macro variable inputs for ROOT (path to local copy of this package) and
*      VERSION (current version of the SAS zipcode file).  The format for VERSION is
*       '[Mmmyy]_v[x]' (e.g.,'Oct16_v9').
*
* KEY DEPENDENCIES/CONSTRAINTS/CAVEATS:
*
*---------------------------------------------------------------------
* CONTACT INFO:
*  Sentinel Coordinating Center
*  info@sentinel.org
*
*---------------------------------------------------------------------
* HISTORY:
*  Create date (mm/dd/yy): 02/19/2015
*  Last modified date (mm/dd/yy): 03/13/2024
*  Version: 4.0.0
*
********************************************************************/
/* Edit macro variable to point to the root directory              */
/*   Example:  %let in= U:\dev\test\qa_mil_lookup\;                */
%let root= ;
/*******************************************************************/;

*--------------------------------------------------------------------
* 0- Setup Environment
*-------------------------------------------------------------------/;
/*******************************************************************/
/* Edit macro variable for current version of the QA package       */
/**  Example: 3.0.0                                                */
%let QAver=4.0.0;
/*******************************************************************/
/* Edit macro variable for current version of the SCDM Model       */
/**  Example: 8.0.0                                                */
%let SCDMver=8.2.0;
/*******************************************************************/
*-------------------------------------------------------------------;

/*------------------------------------------------------------------*/
/* NOTE: This is standard SOC environment setup code -- Do Not Edit */
/*------------------------------------------------------------------*/
/* System options */
options obs=MAX ;
options msglevel=i ;
options mprint mprintnest ;
options errorcheck=strict ;
options merror serror ;
options dkricond=error dkrocond=error mergenoby=warn;
options dsoptions=nonote2err noquotelenmax ;
options reuse=no ;
options fullstimer ;
options validvarname=V7;
options fmterr;

/* Assign directory paths */
%let dplocal=&root.\dplocal;
%let infolder=&root.\inputfiles;
%let msoc=&root.\msoc;

/* Assign libnames */
libname infolder "&infolder.";
libname dplocal "&dplocal.";
libname msoc "&msoc.";

proc datasets lib=msoc kill nowarn nolist nodetails; run; quit;
proc datasets lib=dplocal kill nowarn nolist nodetails; run; quit;


*********************************************************************
*****                       BEGIN PROGRAM                        ****
*********************************************************************;
/*------------------------------------------------------------------*/
/* NOTE: Programmer preparing request includes/writes               */
/*       the rest of the program below                              */
/*------------------------------------------------------------------*/

%include "&INFOLDER.\01.1_create_mil_lookup_tables.sas" / source2 ;

/* End of master program */
