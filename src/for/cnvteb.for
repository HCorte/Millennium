C  GXSRC:CNVTEB.FOR
C
C V01 12-MAY-1999 UXN INITIAL RELEASE
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
	PROGRAM CNVTEB
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECTEB.DEF'
        INCLUDE 'INCLIB:RECTEB_OLD.DEF'
C
	INTEGER*4  I4NAME(5),YESNO
	CHARACTER*20 CXNAME
	EQUIVALENCE (CXNAME,I4NAME)
C
	INTEGER*4   FIRST_CDC, LEN, I4LEN, REC
	INTEGER*4   ST, I, J, K, AGT, PREV_TER, OLD_FDB(7), FDB(7)
        CHARACTER*1    BLANK     /'20'X/
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
        NEW_FILE = 'FILE:TEB.NEW'
        OLD_FILE = 'FILE:TEB.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        MAXREC_CNV = SIZE/(OTEBSEC/2)
C        FILE_SIZE  = MAXREC_CNV*TEBSEC/2
	FILE_SIZE = 6000*TEBSEC/2	    ! ALLOCATE FOR 6000 CDC'S.
        TYPE*,IAM(),'Old file size = ', MAXREC_CNV*OTEBSEC/2
        TYPE*,IAM(),'New file size = ', FILE_SIZE
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert TEB.FIL file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,TEBSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,OTEBSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C

        TYPE*,IAM(),'STARTING...'
C
	REC = 0
        ST  = 0

	DO WHILE (ST.EQ.0)
	    REC = REC + 1
	    CALL READW(OLD_FDB,REC,OTEBREC,ST)   
	    IF(ST.EQ.144) GOTO 100
            IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,REC)

	    IF(MOD(REC,500).EQ.0) TYPE*,IAM(),REC,' records converted...'
            CALL FASTSET(0, TEBREC, TEBLEN)
C
	    TEBCDC = OTEBCDC
	    CALL FASTMOV(OTEBTYP,TEBTYP,AGAMLEN*OMAXGAM)	    
C 
C          Write record to file
C
	   CALL WRITEW(FDB,REC,TEBREC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,REC)
	END DO
100	CONTINUE
C
C
        TYPE*,IAM(),REC-1,' records converted in total.'
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:TEB.FIL to FILE:TEB.OLD'
        CALL LIB$RENAME_FILE('FILE:TEB.FIL','FILE:TEB.OLD')

        TYPE*,IAM(),'Renaming FILE:TEB.NEW to FILE:TEB.FIL'
        CALL LIB$RENAME_FILE('FILE:TEB.NEW','FILE:TEB.FIL')
C
        TYPE*,IAM(),'TEB.FIL converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
C
	END
