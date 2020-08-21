$!
$! $Log:   GXPROJ:[ARCHIVES.GOLS]SUBRUN.COV  $
$! 
$!    Rev 1.0   25 Feb 1993 11:03:38   EBD
$! Initial revision.
$!
$!
$! V02 27-APR-91 MP   DISABLE MESSAGES ONLY FOR THE DURATION OF
$!		      THE 'DEFINE SYS$INPUT' COMMAND - OTHERWISE
$!		      NO ERRORS ARE REPORTED IF SUBPROCESS DID NOT
$!		      START.
$!
$! V01 11-APR-91 TKO  Initial Release
$!
$! This command file will change the working set quota to 40000
$! (current maximum in the UAF file).
$! It will then activate SUBRUN program to start a subtask
$! (or detached process if routine RUNTSK will change).
$! The routine then will change the working set quota to 4000.
$!
$! COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
$!
$! COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
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
$! COPYRITF.DEF-------------------------------------------------------
$!
$ SET NOON
$!
$ IF P1 .EQS. ""
$ THEN
$  WRITE SYS$OUTPUT "SPECIFY THE PROGRAM NAME TO BE STARTED"
$  STOP
$ ENDIF
$!
$! The program  DETRUN uses LIB$GET_FOREIGN...
$ DETRUN :== $GXTSK:DETRUN
$ PRGNAM = F$EDIT(P1,"COLLAPSE,UPCASE,UNCOMMENT")
$!
$ ON ERROR     THEN GOTO ALLDONE
$ ON CONTROL_Y THEN GOTO ALLDONE
$!
$ SET MESSAGE /NOFACIL/NOIDENT/NOSEVERITY/NOTEXT
$ DEFINE SYS$INPUT SYS$COMMAND			!(FOR LIB$FOREIGN)
$ SET MESSAGE /FACIL/IDENT/SEVERITY/TEXT
$!
$ DETRUN 'PRGNAM'
