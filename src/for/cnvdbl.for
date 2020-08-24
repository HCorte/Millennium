C
C V01 24-MAY-1999 UXN INITIAL RELEASE.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
        PROGRAM CNVDBL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
C
        INTEGER*4   DRAW, LEN, I4LEN, LASTDR, EXT
        INTEGER*4   ST, I, J, K, AGT, OLD_FDB(7), FDB(7)
        CHARACTER*20 CXNAME,CXNAME2
        INTEGER*4    I4NAME(5),I4NAME2(5),YESNO
        EQUIVALENCE  (I4NAME,CXNAME)
        EQUIVALENCE  (I4NAME2,CXNAME2)
	LOGICAL     FLAG
C
	CHARACTER*4 EXTENSION(2,2) /'.FIL','.OLD','.NEW','.FIL'/	
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
        NEW_FILE = 'FILE:DB1F.NEW'
        OLD_FILE = 'FILE:DB1F.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
	SIZE = (SIZE/10)*10
        MAXREC_CNV = SIZE/(DDBSEC/2)
        FILE_SIZE  = MAXREC_CNV*DDBSEC/2
        TYPE*,IAM(),'Old file size = ', MAXREC_CNV*DDBSEC/2
        TYPE*,IAM(),'New file size = ', MAXREC_CNV*DDBSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert DB%F.FIL ',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C Create new DB%F.NEW
C	
	DO I=1,NUMDBL
	   WRITE(CXNAME,900) I,'.NEW'
           CALL CRTFIL(I4NAME,FILE_SIZE,ST)
           IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
	ENDDO
C
C Convert all files.
C
	DO I=1,NUMDBL
	   WRITE(CXNAME,900)  I,'.FIL'
	   WRITE(CXNAME2,900) I,'.NEW'
	   CALL CONVERT(I4NAME,I4NAME2)
	ENDDO
C
C Rename *.FIL to *.OLD and *.NEW to *.FIL
C
	DO J=1,2
	  DO I=1,NUMDBL
	     WRITE(CXNAME,900)   I,EXTENSION(1,J)
	     WRITE(OLD_FILE,900) I,EXTENSION(2,J)
	     WRITE(6,910) IAM(),CXNAME,OLD_FILE
	     CALL LIB$RENAME_FILE(CXNAME,OLD_FILE)
	  ENDDO
	ENDDO
	CALL GSTOP(GEXIT_SUCCESS)
900     FORMAT('FILE:DB',I1,'F',A4,T20)
910	FORMAT(1X,A,'Renaming ',A,' to ',A)
	END
C
C SUBROUTINE TO CONVERT GAME FILES...
C	   
	SUBROUTINE  CONVERT(INP_FILE,OUT_FILE)
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
C
	INTEGER*4    INP_FILE(5),OUT_FILE(5)
	INTEGER*4    ST,K,I, REC,J
	INTEGER*4    INP_FDB(7)
	INTEGER*4    OUT_FDB(7)
	INTEGER*4    TMP1(2,MAXDBLTI)
	INTEGER*4    TMP2(2,MAXDBLTI)
	INTEGER*4    TOT_WIN
C
	WRITE(6,900) IAM(),(INP_FILE(K),K=1,5)
C
C OPEN INPUT FILE
C
        CALL OPENW(3,INP_FILE,0,0,0,ST)
        CALL IOINIT(INP_FDB,3,DDBSEC*256)
        IF(ST.NE.0) CALL FILERR(INP_FILE,1,ST,0)
C
C OPEN OUTPUT FILE
C	
        CALL OPENW(4,OUT_FILE,0,0,0,ST)
        CALL IOINIT(OUT_FDB,4,DDBSEC*256)
        IF(ST.NE.0) CALL FILERR(OUT_FILE,1,ST,0)
C
C FILE CONVERSION
C
	REC = 0
	DO WHILE(.TRUE.)
	   REC = REC + 1
	   CALL READW(INP_FDB,REC,DDBREC,ST)
	   IF(ST.EQ.144) GOTO 10 ! END-OF-FILE
	   IF(ST.NE.0) CALL FILERR(INP_FILE,2,ST,REC)
C	   
	   CALL FASTMOV(DDBWIN,TMP1,2*MAXDBLTI)
	   CALL FASTMOV(DDBWIN,TMP2,2*MAXDBLTI)
	   CALL FASTSET(0,DDBWIN,2*MAXDBLTI)
	   CALL FASTSET(0,DDBHLD,2*MAXDBLTI)
C
	   IF(TMP1(2,1).EQ.0) THEN
	      DO I=1,MAXDBLTI
		 TMP1(2,I) = TMP1(1,I)	
		 TMP2(2,I) = TMP2(1,I)	
	      ENDDO
	   ENDIF

	   TOT_WIN = 0
	   DO 110 I=1,MAXDBLTI
	      IF(TMP1(1,I).LE.0) GOTO 110
	      DO 100 J=1,MAXDBLTI
		 IF(TMP1(1,J).LE.0) GOTO 100
		 IF(TMP1(1,I).NE.TMP1(2,J)) THEN
		    TOT_WIN = TOT_WIN + 1
		    DDBWIN(1,TOT_WIN) = TMP1(1,I)
		    DDBWIN(2,TOT_WIN) = TMP1(2,J)
		    DDBHLD(1,TOT_WIN) = TMP2(1,I)
		    DDBHLD(2,TOT_WIN) = TMP2(2,J)
		 ENDIF
100	      CONTINUE
110	   CONTINUE
	   DDBCMB = TOT_WIN
	   IF(DDBCMB.GT.1) WRITE(6,910) IAM(),DDBDRW,DDBCMB
C
	   CALL WRITEW(OUT_FDB,REC,DDBREC,ST)
	   IF(ST.NE.0) CALL FILERR(OUT_FILE,3,ST,REC)
	ENDDO
10	CONTINUE
	TYPE*,IAM(),REC-1,' records converted in total.'	
	CALL CLOSEFIL(INP_FDB)
	CALL CLOSEFIL(OUT_FDB)
C
900	FORMAT(1X,A,'Starting to convert ',5A4)
910	FORMAT(1x,A,'Draw # ',I4,' has ', I2,' winning combinations.')
	END
