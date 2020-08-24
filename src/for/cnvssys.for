C CNVSSYS.FOR
C
C V01 10-FEB-2000 OXK Initial revision for Vakio changes
C
C This program converts old Sports SYSTEMfile SPTSYS.FIL to new format.
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
	PROGRAM CNVSSYS
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
	INCLUDE 'INCLIB:RECSSF_OLD.DEF'

        INTEGER*4   REC, I4LEN, LASTDR, EXT
	INTEGER*4   LEN, OLD_LEN
        INTEGER*4   ST, I, J, K, AGT, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO
        INTEGER*4   RECS_TO_CNV		  ! # OF RECORDS TO CONVERT.
C
        INTEGER*4   FILE_SIZE		  ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE

	INTEGER*4   PTR,PTR2

C Begin Code ------------------------------------------------

        CALL COPYRITE
C
C
        NEW_FILE = 'FILE:SPTSYS.NEW'
        OLD_FILE = 'FILE:SPTSYS.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        RECS_TO_CNV = SIZE/(OSSFSEC/2)
        FILE_SIZE  = RECS_TO_CNV*SSFSEC/2
        TYPE*,IAM(),'Old file size = ', RECS_TO_CNV*OSSFSEC/2
        TYPE*,IAM(),'New file size = ', RECS_TO_CNV*SSFSEC/2
        TYPE*,IAM()
C        TYPE*,IAM(),'Number of recordss to convert >',RECS_TO_CNV
C        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert SPTSYS.FIL',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
	LEN = SSFSEC*256
	OLD_LEN = OSSFSEC*256

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
        CALL READQIO(OLD_FDB,REC,OSSFREC,OLD_LEN,ST)
        IF(ST.EQ.144) GOTO 100
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)
C
	CALL FASTSET (0,SSFREC,SSFLEN)

C-----------------------------------------------

	DO I=1,SPGSYS
	    SSFATR(I) = OSSFATR(I)
	    DO J=1,5
		SSFNUM(J,I) = OSSFNUM(J,I)
	    ENDDO
	ENDDO

C
C THE SIZE OF THE TABLE IS NOT CHANGED BECAUSE IT HAD ENOUGH FREE SPACE...
C
	CALL FASTSET(0,SSFPTR,SPGSYS)
	CALL FASTSET(0,SSFTAB,SFTABMAX/2)

	DO I=1, 260000
	    IF (I.LE.204248 .AND. OSSFTAB(I).LE.0)
     *	      TYPE*,I,'  ???',OSSFTAB(I),'   WRONG FILE ???'
	    IF (I.GT.204248 .AND. OSSFTAB(I).GT.0)
     *	      TYPE*,I,'  ???',OSSFTAB(I),'   WRONG FILE ???'
	ENDDO

	PTR=0
	PTR2=20
	DO I=1, 210000	! THE REST OF THE FILE IS FULL OF ZEROES, SO...
	    IF (OSSFTAB(I).GE.9) THEN
	        PTR=PTR+1
		SSFTAB(PTR) = OSSFTAB(I)
		PTR2=PTR2+1
		SSFPTR(PTR2) = PTR
	    ELSE
	        PTR=PTR+1
		SSFTAB(PTR) = OSSFTAB(I)
		IF ( MOD((I-OSSFPTR(PTR2)),13) .EQ. 0) THEN
		    PTR=PTR+1
		    SSFTAB(PTR) = 1
		ENDIF
	    ENDIF
	ENDDO

	SSFFPT = OSSFFPT
	SSFTST = OSSFTST
	SSFINT = OSSFINT

	DO I=0,15
	    DO J=0,15
		DO K=1,ONUMSPT
		    SSFFSF(I,J,K) = OSSFFSF(I,J,K)
		ENDDO
	    ENDDO
	ENDDO

	DO I=1,SPGSYS
	    SSFGAR(I) = OSSFGAR(I)
	ENDDO

C-----------------------------------------------

C          Write record to file
C
        CALL WRITEQIO(FDB,REC,SSFREC,LEN,ST)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	REC=REC+1

100     CONTINUE
C
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
        TYPE*,IAM(),'Renaming FILE:SPTSYS.FIL to FILE:SPTSYS.OLD'
        CALL LIB$RENAME_FILE('FILE:SPTSYS.FIL','FILE:SPTSYS.OLD')

        TYPE*,IAM(),'Renaming FILE:SPTSYS.NEW to FILE:SPTSYS.FIL'
        CALL LIB$RENAME_FILE('FILE:SPTSYS.NEW','FILE:SPTSYS.FIL')
C
        TYPE*,IAM(),'Vakio SYSTEMS file converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
        END
