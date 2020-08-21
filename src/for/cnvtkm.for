C  GXSRC:CNVTKM.FOR
C
C V03 23-MAR-2011 RXK Added DIR before and after conversion
C V02 25-FEB-2004 FRP EuroMillions: recalculate record according to MAXGAM=50.
C V01 12-MAY-1999 UXN INITIAL RELEASE.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	PROGRAM CNVTKM
	IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECTKM.DEF'
        INCLUDE 'INCLIB:RECTKM_OLD.DEF'
C
	INTEGER*4   YESNO
C
	INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   MAXREC_CNV   ! # of records to convert
C
        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE
C
	CALL COPYRITE
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR MKTMES.%%%.* /DATE/SIZE=ALL/EXCLU=MKTMES.EXE')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'FILE:MKTMES.NEW'
        OLD_FILE = 'FILE:MKTMES.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        MAXREC_CNV = 1
        FILE_SIZE  = MAXREC_CNV*TKMSEC/2
        TYPE*,IAM(),'Old file size > ', MAXREC_CNV*OLD_TKMSEC/2
        TYPE*,IAM(),'New file size > ', MAXREC_CNV*TKMSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL INPYESNO('Are you sure you want to convert MKTMES.FIL',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL NEWFIL(1,NEW_FILE,FILE_SIZE,.FALSE.,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,TKMSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,OLD_TKMSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C

        TYPE*,IAM(),'STARTING...'
C
	CALL READW(OLD_FDB,1,OLD_TKMREC,ST)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,1)
C
	CALL FASTSET(0, TKMREC, TKMLEN)
C
	CALL FASTMOV(OLD_TKMMES,TKMMES,TICKET_LENGTH*TICKET_ROWS*OLD_MAXGAM)
	CALL FASTMOV(OLD_TKMMES(1,1,OLD_MAXGAM+1),TKMMES(1,1,MAXGAM+1),
     *               PRM_NUMOPN+NUMCDU)
	CALL FASTMOV(OLD_TKMMLN,TKMMLN,OLD_MAXGAM)
	CALL FASTMOV(OLD_TKMMLN(OLD_MAXGAM+1),TKMMLN(MAXGAM+1),
     *               PRM_NUMOPN+NUMCDU)
	CALL FASTMOV(OLD_TKMMFL,TKMMFL,OLD_MAXGAM)
	CALL FASTMOV(OLD_TKMMFL(OLD_MAXGAM+1),TKMMFL(MAXGAM+1),
     *               PRM_NUMOPN+NUMCDU)
	CALL MOVBYT(OLD_TKMAFL,1,TKMAFL,1,NUMAGT)
	CALL FASTMOV(OLD_TKMMRV,TKMMRV,OLD_MAXGAM)
	CALL FASTMOV(OLD_TKMMRV(OLD_MAXGAM+1),TKMMRV(MAXGAM+1),
     *               PRM_NUMOPN+NUMCDU)
	TKMCDU = OLD_TKMCDU
C 
	CALL WRITEW(FDB,1,TKMREC,ST)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,1)
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:MKTMES.FIL to FILE:MKTMES.OLD'
        CALL LIB$RENAME_FILE('FILE:MKTMES.FIL','FILE:MKTMES.OLD')
C
        TYPE*,IAM(),'Renaming FILE:MKTMES.NEW to FILE:MKTMES.FIL'
        CALL LIB$RENAME_FILE('FILE:MKTMES.NEW','FILE:MKTMES.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR MKTMES.%%%.* /DATE/SIZE=ALL/EXCLU=MKTMES.EXE')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'MKTMES.FIL converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
C
	END

