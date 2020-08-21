C CNVREP.FOR
C
C V01 10-FEB-2000 OXK Initial revision for Vakio changes
C
C This program converts old file REP.FIL to new format.
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

C=======OPTIONS /CHECK/EXT
	PROGRAM CNVREP
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECREP.DEF'
	INCLUDE 'INCLIB:RECREP_OLD.DEF'

        INTEGER*4   REC, LEN, OLD_LEN
        INTEGER*4   I, J, K, L, M
	INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO
        INTEGER*4   RECS_TO_CNV		  ! # OF RECORDS TO CONVERT
C
        INTEGER*4   FILE_SIZE		  ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE

C Begin Code ------------------------------------------------

        CALL COPYRITE
C
C
        NEW_FILE = 'FILE:REP.NEW'
        OLD_FILE = 'FILE:REP.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        RECS_TO_CNV = SIZE/(OREPSEC/2)
        FILE_SIZE  = RECS_TO_CNV*REPSEC/2
        TYPE*,IAM(),'Old file size = ', RECS_TO_CNV*OREPSEC/2
        TYPE*,IAM(),'New file size = ', RECS_TO_CNV*REPSEC/2
        TYPE*,IAM()
C        TYPE*,IAM(),'Number of draws to convert >',RECS_TO_CNV
C        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert file REP.FIL',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
	LEN = REPSEC*256
	OLD_LEN = OREPSEC*256
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENQX(3,NEW_FILE,0,0,0,ST)
        CALL IOQINIT(FDB,3,LEN)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENQX(4,OLD_FILE,0,0,0,ST)
        CALL IOQINIT(OLD_FDB,4,OLD_LEN)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
C       Read record from file

        REC=1
        CALL READQIO(OLD_FDB,REC,OREPREC,OLD_LEN,ST)
        IF(ST.EQ.144) GOTO 100
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
C
	CALL FASTSET (0,REPREC,REPSEC*64)

C---------------------------------

	REPUPD = OREPUPD
	REPPRG = OREPPRG
	REPBEG = OREPBEG
	REPEND = OREPEND

	DO I=1,MAXCMB
	    DO J=1,OGAMLEN
		DO K=1,SYSTYPS
		    DO L=1,QPTYPS
			DO M=1,MAXDAY
			    REPN1W (I,J,K,L,M) = OREPN1W (I,J,K,L,M)
			    REPN2W (I,J,K,L,M) = OREPN2W (I,J,K,L,M)
			    REPN3W (I,J,K,L,M) = OREPN3W (I,J,K,L,M)
			    REPN5W (I,J,K,L,M) = OREPN5W (I,J,K,L,M)
			    REPNAW (I,J,K,L,M) = OREPNAW (I,J,K,L,M)
			ENDDO
		    ENDDO
		ENDDO
	    ENDDO
	ENDDO

	DO I=1,OJOKLEN
	    DO J=1,MAXDAY
		REPT1W (I,J) = OREPT1W (I,J)
		REPT2W (I,J) = OREPT2W (I,J)
		REPT3W (I,J) = OREPT3W (I,J)
		REPT5W (I,J) = OREPT5W (I,J)
		REPTAW (I,J) = OREPTAW (I,J)
	    ENDDO
	ENDDO

C---------------------------------

C          Write record to file
C
        CALL WRITEQIO(FDB,REC,REPREC,LEN,ST)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	REC=REC+1

100     CONTINUE
C
        CALL CLOSEQFIL(FDB)
        CALL CLOSEQFIL(OLD_FDB)
	IF (REC.EQ.1) THEN
	    TYPE*,IAM(),'Unexpected EOF'
	    CALL GSTOP(GEXIT_FATAL)
	ELSE
	    TYPE*,IAM(),REC-1,' records converted'
	ENDIF
C
        TYPE*,IAM(),'Renaming FILE:REP.FIL to FILE:REP.OLD'
        CALL LIB$RENAME_FILE('FILE:REP.FIL','FILE:REP.OLD')

        TYPE*,IAM(),'Renaming FILE:REP.NEW to FILE:REP.FIL'
        CALL LIB$RENAME_FILE('FILE:REP.NEW','FILE:REP.FIL')
C
        TYPE*,IAM(),'REP.FIL converted succesfully'
        CALL GSTOP(GEXIT_SUCCESS)
        END
