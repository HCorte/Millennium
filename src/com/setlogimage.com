$!
$! $Log:   GXPROJ:[ARCHIVES.GOLS]SETLOGIMAGE.COV  $
$! 
$!    Rev 1.0   25 Feb 1993 11:02:42   EBD
$! Initial revision.
$!
$!
$! V01 11-APR-91 TKO  Initial Release
$!
$! This command file will define each shared image for a site to be named
$! as GXnnnnnn.ESH.   It requires that the file SHRIMG.NAM reside in GXCOM.
$! SHRIMG.NAM must contain the names of all shared images to be installed.
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
$ WRITE SYS$OUTPUT "GXCOM:SETLOGIMAGE: SETTING LOGICAL NAMES FOR SHARED IMAGES"
$!
$ @GXCOM:SETGXPROJD
$ TMP  = GXPROJD
$ TMP1 = F$TRNLNM("GXPROJ")
$ TMP2 = TMP1 + "[SHR]" + TMP
$!
$ OPEN INPUT GXCOM:SHRIMG.NAM
$!
$ LOOP:
$   READ/END_OF_FILE=ENDLOOP/ERROR=ENDLOOP INPUT NAME
$   IF NAME .EQS. "" THEN GOTO LOOP
$   FRST = F$EXTRACT(0,1,NAME)
$   IF FRST .EQS."$" THEN GOTO LOOP
$   IF FRST .EQS."!" THEN GOTO LOOP
$   XNAM = F$EDIT(NAME,"COLLAPSE,UPCASE,UNCOMMENT")
$!
$   LOGNAM = TMP2 + XNAM + ".ESH"
$   DEFINE/GROUP/TRANS=TERM/NOLOG GX'NAME' 'LOGNAM'
$!
$   GOTO LOOP
$!
$ ENDLOOP:
$!
$   CLOSE INPUT
$!
