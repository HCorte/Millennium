C  GXSRC:CNVDAF.FOR
C
C V05 14-FEB-2017 MTK/FRP MAXCDC from 6000 to 9999
C V04 23-MAR-2011 RXK Added DIR command before and after conversion
C V03 22-FEB-2011 FRP MAXGAM from 10 to 50
C V02 11-MAY-1999 UXN IMPROVED USERINTERFACE.
C V01 xx-xxx-xxxx xxx Initial release.
C
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
	PROGRAM CNVDAF
	IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
 	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:RECDAF_OLD.DEF'
C
	INTEGER*4  I4NAME(5),YESNO
	CHARACTER*20 CXNAME
	EQUIVALENCE (CXNAME,I4NAME)
C
	INTEGER*4   REC, REC_CNV,REC_INC
	INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   MAXREC_CNV   ! # of records to convert
        INTEGER*4   MAXREC_INC   ! # of records to increase
        INTEGER*4   I
C
        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
CC      INTEGER*4   SIZE,CHG/0/,START_CHK/3227/ !3227=Mo 01.03.2010
        INTEGER*4   SIZE,CHG/0/,START_CHK/5784/ !5784=We 01.03.2010
C
CC      INTEGER*4  OLD_MAXGAM
CC      PARAMETER (OLD_MAXGAM=10)
C
        INTEGER*4  MAXCDC
CC      PARAMETER (MAXCDC=6000)
        PARAMETER (MAXCDC=9999)
C
	CALL COPYRITE
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR DAF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'FILE:DAF.NEW'
        OLD_FILE = 'FILE:DAF.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
CC      MAXREC_CNV = 3995
        MAXREC_CNV = 6000
        MAXREC_INC = MAXCDC - MAXREC_CNV
        FILE_SIZE  = MAXCDC*DAFSEC/2
        TYPE*,IAM(),'Old file size = ', SIZE
        TYPE*,IAM(),'New file size = ', FILE_SIZE
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert  >',MAXREC_CNV
        TYPE*,IAM(),'Number of records to increase >',MAXREC_INC
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert DAF file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,DAFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,ODAFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
	DO REC=1,MAXREC_CNV
	    CALL READW(OLD_FDB,REC,ODAFREC,ST)   
	    IF(ST.EQ.144) GOTO 100
            IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
C
	    IF(MOD(REC,1000).EQ.0) TYPE*,IAM(),REC,' records converted...'
            CALL FASTSET(0, DAFREC, DAFLEN)
C
	    DAFSTS = ODAFSTS
            IF(REC.GT.START_CHK.AND.DAFSTS.LT.4) CHG=1
            IF(CHG.EQ.1.AND.DAFSTS.EQ.4) THEN
               DAFSTS = 2
               TYPE*,IAM(),'CDC',ODAFCDC,' DAY STATUS',ODAFSTS,' REPLACED WITH',
     *         DAFSTS
            ENDIF   
	    DAFCDC = ODAFCDC
	    DAFYER = ODAFYER
	    DAFJUL = ODAFJUL
	    DAFWEK = ODAFWEK
CC          CALL FASTMOV(ODAFTYP,DAFTYP,NUMTOT*NUMFIN*OMAXGAM)
CC          CALL FASTMOV(ODAFDRW,DAFDRW,OMAXGAM)
CC          CALL FASTMOV(ODAFHDR,DAFHDR,OMAXGAM)
CC          CALL FASTMOV(ODAFVAL,DAFVAL,OMAXGAM)
CC          CALL FASTMOV(ODAFSAL,DAFSAL,MAXDRW*OMAXGAM)
CC          CALL FASTMOV(ODAFDIS,DAFDIS,NUMTOT*OMAXGAM)
            CALL FASTMOV(ODAFTYP,DAFTYP,NUMTOT*NUMFIN*MAXGAM)
            CALL FASTMOV(ODAFDRW,DAFDRW,MAXGAM)
            CALL FASTMOV(ODAFHDR,DAFHDR,MAXGAM)
            CALL FASTMOV(ODAFVAL,DAFVAL,MAXGAM)
            CALL FASTMOV(ODAFSAL,DAFSAL,MAXDRW*MAXGAM)
	    CALL FASTMOV(ODAFDIS,DAFDIS,NUMTOT*MAXGAM)
	    CALL FASTMOV(ODAFCRS,DAFCRS,NUMCRS)
	    DAFIVAL = ODAFIVAL
	    DAFICLM = ODAFICLM
C 
	    CALL WRITEW(FDB,REC,DAFREC,ST)
            IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	END DO
C
100	CONTINUE
C
        REC_CNV = REC - 1
        TYPE*,IAM(),REC_CNV,' records converted in total.'
C
	REC = REC_CNV
        ST  = 0
	DO WHILE (ST.EQ.0)
	    REC = REC + 1
	    CALL READW(FDB,REC,DAFREC,ST)   
	    IF(ST.EQ.144) GOTO 200
            IF(ST.NE.0) CALL FILERR(I4NEW_FILE,2,ST,REC)
C
	    IF(MOD(REC,500).EQ.0) TYPE*,IAM(),REC,' records increased...'
            CALL FASTSET(0, DAFREC, DAFLEN)
C
	    DAFSTS = DSOPEN
C 
 	    CALL WRITEW(FDB,REC,DAFREC,ST)
            IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	END DO
C
200	CONTINUE
C
        REC_INC = REC - 1 - REC_CNV
        TYPE*,IAM(),REC_INC,' new records added in total.'
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:DAF.FIL to FILE:DAF.OLD'
        CALL LIB$RENAME_FILE('FILE:DAF.FIL','FILE:DAF.OLD')

        TYPE*,IAM(),'Renaming FILE:DAF.NEW to FILE:DAF.FIL'
        CALL LIB$RENAME_FILE('FILE:DAF.NEW','FILE:DAF.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR DAF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'DAF file converted successfully!'
        CALL GSTOP(GEXIT_SUCCESS)
C
	END
