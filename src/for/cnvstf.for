C CNVSTF.FOR
C
C V03 23-MAR-2011 RXK Added DIR before and after conversion
C V02 25-FEB-2004 FRP EuroMillions: recalculate record according to MAXGAM=50.
C V01 10-FEB-2000 OXK Initial revision for Vakio changes
C
C This program converts old Sports STATISTIC file STATS.FIL to new format.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	PROGRAM CNVSTF
	IMPLICIT NONE

        INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSTF.DEF'
	INCLUDE 'INCLIB:RECSTF_OLD.DEF'

        INTEGER*4   MAXGAM_OLD
        PARAMETER(MAXGAM_OLD=10)

        INTEGER*4   REC
        INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO
        INTEGER*4   RECS_TO_CNV          ! # OF RECORDS TO CONVERT
C
        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE

C Begin Code ------------------------------------------------

        CALL COPYRITE
C
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR STATS.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'FILE:STATS.NEW'
        OLD_FILE = 'FILE:STATS.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        RECS_TO_CNV = SIZE/(OSTFSEC/2)
        FILE_SIZE  = RECS_TO_CNV*STFSEC/2
        TYPE*,IAM(),'Old file size = ', RECS_TO_CNV*OSTFSEC/2
        TYPE*,IAM(),'New file size = ', RECS_TO_CNV*STFSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',RECS_TO_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert STATS.FIL file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,STFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,OSTFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
C       Read record from file

        REC=1
	DO WHILE(ST.EQ.0)
        CALL READW(OLD_FDB,REC,OSTFREC,ST)
        IF(ST.EQ.144) GOTO 100
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
C
	CALL FASTSET (0,STFREC,STFLEN)

C-----------------------------------------------

	
	CALL FASTMOV(OSTFLTO,STFLTO,NUMTOT*2*MAXBRD*(MAXBRD+1)*NUMLTO)
	CALL FASTMOV(OSTFGAM,STFGAM,NUMTOT*4*MAXGAM_OLD)
	CALL FASTMOV(OSTFKIK,STFKIK,2*MAXGAM_OLD)
	CALL FASTMOV(OSTFSPT,STFSPT,NUMTOT*2*MAXBRD*(MAXBRD+1)*NUMSPT)
	CALL FASTMOV(OSTFSPT_CUP,STFSPT_CUP,NUMSPT)
        CALL FASTMOV(OSTFSPT_TAB1,STFSPT_TAB1,SPGNBR*7*NUMSPT)
        CALL FASTMOV(OSTFSPT_TAB2,STFSPT_TAB2,SPGNBR*3*NUMSPT)
	CALL FASTMOV(OSTFTSL,STFTSL,NUMTOT*5*NUMTSL)

C-----------------------------------------------

C          Write record to file
C
           CALL WRITEW(FDB,REC,STFREC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	   REC=REC+1

	ENDDO
100     CONTINUE
C
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
	IF (REC.EQ.1) THEN
	    TYPE*,IAM(),'Unexpected EOF'
	    CALL GSTOP(GEXIT_FATAL)
	ELSE
	    TYPE*,IAM(),REC-1,' records converted'
	ENDIF
C
        TYPE*,IAM(),'Renaming FILE:STATS.FIL to FILE:STATS.OLD'
        CALL LIB$RENAME_FILE('FILE:STATS.FIL','FILE:STATS.OLD')

        TYPE*,IAM(),'Renaming FILE:STATS.NEW to FILE:STATS.FIL'
        CALL LIB$RENAME_FILE('FILE:STATS.NEW','FILE:STATS.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR STATS.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'Sports STATISTICS file converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
        END
