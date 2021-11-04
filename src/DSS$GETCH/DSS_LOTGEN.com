$!
$! 16-may-97 slk added doallc
$!----------------------------------------------------------------------------
$! COPYRIGHT 1990 GTECH CORPORATION.  ALL RIGHTS RESERVED.
$!
$! CONFIDENTIAL PROPRIETARY INFORMATION
$! This item is the property of GTECH Corporation, W. Greenwich, Rhode
$! Island, and contains confidential and trade secret information.  It
$! may not be transferred from the custody or control of GTECH except
$! as authorized in writing by an officer of GTECH.  Neither this item
$! nor the information it contains may be used, transferred,
$! reproduced, published or disclosed, in whole or in part, directly
$! or indirectly, except as expressly authorized by an officer of
$! GTECH pursuant to written agreement.
$!----------------------------------------------------------------------------
$!
$! Calling format:
$!
$!      LOTGEN[/wl] cg ["rem"]
$!
$! where:
$!      /wl     - warning level (optional qualifier)
$!                 /A - All (fatal + warning + informational messages displayed)
$!                 /W - Warning (fatal + warning messages displayed)
$!                 /F - Fatal (fatal messages only displayed) - DEFAULT

$!
$ if "''p2'" .eqs. ""
$ then
$   write sys$output "Obligatory parameters not specified"
$   exit
$ endif
$!
$ if "''p1'" .eqs. "*" then goto START
$ if "''p1'" .nes. "ACTIVE" .and. "''p1'" .nes. "LIVE" .and. -
     "''p1'" .nes. "TOOL"   .and. "''p1'" .nes. "JUNK"
$ then
$   write sys$output "Invalid group parameter."
$   stop
$ endif
$!
$ START:
$!
$ if "''guser'".eqs."" .or. "''guser'".eqs."???" .or. "''gxprojd'".eqs.""
$ then
$   write sys$output "Set development environment first"
$   stop
$ endif
$ if "''p2'" .eqs. ""
$ then
$   write sys$output "Obligatory remark not specified"
$   stop
$ endif
$!
$!   CHECK PASSWORD
$!
$ chkpass
$! Generate CHKCOM.DEF
$ IF F$SEARCH("GXCOM:GENDEF.COM").NES.""
$ THEN
$    write sys$output "Generating CHKCOM.DEF"
$    @GXCOM:GENDEF.COM
$ ENDIF
$!
$!
$!
$!   DETERMINE COMPILE-TIME PARAMETERS BASING ON CURRENT PROJECT PHASE
$!
$ open/read/share=read/error=phase_error phase_fil gxcom:setphase.nam
$ read/error=phase_error phase_fil phase
$ close/error=phase_error phase_fil
$ proj_phase=f$integer(phase)
$ if proj_phase .le. 6
$ then
$  bnds = "CHECK=BOUNDS"
$ else
$  bnds = "CHECK=NOBOUNDS"
$  wrng = "/WARNINGS=(NOUNUSED,NOUNCALLED,NOUSAGE)"
$ endif
$ if(f$extract(0,3,platform).nes."VAX")
$ then
$  algn="/ALIGN=(COMMON=(PACK,MULTILANGUAGE),RECORD=PACK)"
$  if "''p3'" .eqs. "A"
$  then
$    wl=""
$  else
$    if "''p3'" .eqs. "W"
$    then
$      wl=",NOINFORMATIONAL"
$    else
$      wl=",NOGEN"
$    endif
$  endif
$  if proj_phase .le. 6
$  then
$    wrng="/WARN=(ALIG,ARGUM,DECLA,GEN,GRANU,IGNO,TRUNC,UNCAL,UNINITI,USAGE)"
$  else
$    wrng="/WARN=(NOALIGN''wl')"
$  endif
$  calgn=algn
$ else
$  algn=""
$  calgn="none"
$ endif
$ plat=algn+wrng
$!
$ vms_chr_ver = "VMS " + f$getsyi("version")
$!
$ ftn_maj_ver = ftnversion/10
$ ftn_min_ver = ftnversion-ftn_maj_ver*10
$ ftn_chr_ver = "FORTRAN " + f$string(ftn_maj_ver) + "." + f$string(ftn_min_ver)
$!
$ write sys$output "This LOTGEN will be performed in the SYS$BATCH queue."
$ write sys$output "It will assume the following parameters:"
$ write sys$output " "
$ write sys$output "    Operating system : ",vms_chr_ver
$ write sys$output "    Compiler         : ",ftn_chr_ver
$ write sys$output "    Project name     : ",gxprojd
$ write sys$output "    Project phase #  : ",phase
$ write sys$output "    Boundary checks  : ",bnds
$ write sys$output "    Warning level    : ",wrng
$ write sys$output "    Alignment        : ",calgn
$ write sys$output "    INCLIB           : GXSRC"
$ write sys$output "    OBJLIB           : GXSRC"
$ write sys$output "    STDLIB           : GXSRC"
$ write sys$output "    TSKLIB           : GXTSK"
$ write sys$output "    IGSLIB           : IGSTSK"
$ write sys$output "    OLMLIB           : OLMTSK"
$ write sys$output " "
$ write sys$output "All .FOR, .LNK, .LSH, SIT_*.OBJ and NRM_*.OBJ files"
$ if "''p1'" .nes. "*"
$ then
$   domain = "''p1'_CODE GROUP"
$   write sys$output " "
$   write sys$output "              in ''domain' only"
$   write sys$output " "
$ else
$   domain = "ALL CODE"
$ endif
$ write sys$output "   will take part in the application generation."
$ write sys$output " "
$ inquire confirmed "Proceed with LOTGEN? [Y/N]"
$ if .not. confirmed then stop
$ write sys$output " "
$ write sys$output "Confirmed."
$ write sys$output "LOTGEN log file name: GXSRC:DSS_LOTGEN_BATCH.LOG"
$!
$!
$!   PRELOTGEN CLEANING
$!
$!
$ project = "GO''gxprojd'"
$!
$ SAV_DIR = F$DIRECTORY()
$ SET DEF GXSRC
$ DEL GXSRC:*.OBJ;*,*.LIS;*,*.DIA;*,*.MAP;*,*.OLB;*
$ DEL GXSHR:*.ESH;*
$ DEL GXTSK:*.EXE;*,*.ESH;*
$ DEL GXUTL:*.EXE;*
$ DEL IGSTSK:*.EXE;*
$ DEL OLMTSK:*.EXE;*
$!
$!   CREATE COMMANDS TO DO FORTRAN COMPILES, SHARED LINKS, LINKS
$!
$ IF F$SEARCH("GXSRC:*.FOR")     .NES. "" THEN @DSS$GTECH:DSS_DOALLFOR 'p1'
$ IF F$SEARCH("GXSRC:*.C")       .NES. "" THEN @DSS$GTECH:DSS_DOALLC   'p1'
$ IF F$SEARCH("GXSRC:SIT_*.FOR") .NES. "" THEN @DSS$GTECH:DSS_DOALLSIT 'p1'
$ IF F$SEARCH("GXSRC:NRM_*.FOR") .NES. "" THEN @DSS$GTECH:DSS_DOALLNRM 'p1'
$ IF F$SEARCH("GXSRC:*.LSH")     .NES. "" THEN @DSS$GTECH:DSS_DOALLLSH 'p1'
$ IF F$SEARCH("GXSRC:*.LNK")     .NES. "" THEN @DSS$GTECH:DSS_DOALLLNK 'p1'
$!
$! SITE DEPENDENT ACTIONS TAKEN HERE
$!
$ IF F$SEARCH("GXCOM:SITEGEN.COM") .NES. "" THEN @GXCOM:SITEGEN 'proj_phase'
$!
$!   LOTGEN IN SYS$BATCH QUEUE
$!
$ @DSS$GTECH:DSS_SUBBAT DSS$GTECH:DSS_LOTGEN_BATCH 'project' 'bnds' 'plat'
$!
$ @DSS$GTECH:DSS_LOG 13 "''domain'" "''p2'"
$!
$ SET DEF 'SAV_DIR'
$!
$ END:
$ exit
