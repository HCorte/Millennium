C GUI_015.FOR
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
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_015(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUISTR.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 GIND, CNT

        INTEGER*8  LPAD(KIGDIV)
        INTEGER*8  LVAL(KIGDIV)
        INTEGER*8  LWIN(KIGDIV)
	INTEGER*8  KSAL(KIGENT)
 	REAL*8	   POOL
	INTEGER*8  TOTWON(2),TOTPAD(2)
C
        INTEGER*4  FRAMT
	EXTERNAL   FRAMT
C
        INTEGER*4  GNUM
        INTEGER*4  ST,STS
	INTEGER*4  DRAW,I,J,K,YEAR,WEK
	INTEGER*8  LTOT
	CHARACTER  FIX(KIGDIV)

        CHARACTER DNAME(7,3,KIGDIV)
        CHARACTER*7 CDNAME(3,KIGDIV)
        EQUIVALENCE (CDNAME,DNAME)
	
	CHARACTER*26 DISP_DIV(KIGDIV)
	INTEGER*8    I4TOI8
	EXTERNAL     I4TOI8
C
C  GET CDC
C
	CALL GUI_GETPARAMS(OUTBUF, ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	GIND = GUI_ARGVAL(1)
	DRAW = GUI_ARGVAL(2)

	IF(GIND.LT.1.OR.GIND.GT.NUMKIK) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	GNUM = GTNTAB(TKIK,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	IF(DRAW.LT.1) DRAW = DAYDRW(GNUM)	
	IF(DRAW.EQ.0) DRAW = DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
5       CONTINUE
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	    CALL GAMLOG(TKIK,GIND,DKKREC,KIKSTS)
	    GOTO 100
	ENDIF
C
	CALL READW(GAMFDB(1,GNUM),DRAW,DKKREC,ST)
	IF(ST.NE.0) THEN
	    CALL OPS('Failed to read '//CGFNAMES(GNUM),ST,DRAW)
	    RET_CODE = 11
	    RETURN
	ENDIF
C
100	CONTINUE
C
	LTOT = 0
	DO I = 1, KIGENT
	  KSAL(I) = 0
	  DO J=1,MAXGAM
	    LTOT = LTOT + I4TOI8(DKKSAL(I,J))
	    KSAL(I) = KSAL(I) + I4TOI8(DKKSAL(I,J))
	  ENDDO
        END DO

        POOL=LTOT*CALPER(DKKSPR)
        DO I=1,DKKDIV
           IF(DKKPER(I).NE.0) THEN
            LVAL(I)=IDNINT(POOL*CALPER(DKKPER(I)))
            LVAL(I)=LVAL(I)+I4TOI8(DKKPOL(1,I))        !ADD POOL CARRIED OVER
            FIX(I)=' '
           ELSE
            LVAL(I) = I4TOI8(DKKSHV(I))                !FIX PRIZES
            FIX(I)='F'
           ENDIF	 
           LWIN(I)=0
           LPAD(I)=0
        END DO
C
        IF(DKKSTS.GE.GAMDON.OR.DKKSPR.EQ.0) THEN
            DO I = 1, DKKDIV
                LVAL(I) = I4TOI8(DKKSHV(I))
                LWIN(I) = I4TOI8(DKKSHR(I))*LVAL(I)
                LPAD(I) = I4TOI8(FRAMT(MAXFRC(GNUM),DKKPAD(I),LVAL(I)))
            END DO
        ENDIF
C
	CALL GUI_CHKSTS_RESCOM(GIND,TKIK,DRAW,STS)
	STS = MAX(STS,DKKSTS)
C
C BUILD GUI MESSAGE
C
	CALL GUIARG_INIT()
C
C RESULT SET 1
C
	NUM_COLS = 8
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
        CALL FIGWEK(DKKESD,WEK,YEAR)
C
	CALL GUIARG_CHAR(OUTBUF,GLNAMES(1,GNUM),16)
	CALL GUIARG_INT2(OUTBUF,DKKDRW)
	CALL GUIARG_INT2(OUTBUF,WEK)
	CALL GUIARG_INT2(OUTBUF,YEAR)

	CALL GUIARG_BYTE(OUTBUF,STS)
	CALL GUIARG_MONYI8(OUTBUF,KSAL(1))	
	CALL GUIARG_MONYI8(OUTBUF,I4TOI8(DKKPOL(1,1)))	
	TYPE *,LTOT
	CALL GUIARG_MONYI8(OUTBUF,LTOT)	
C
C RESULT SET 2
C
	NUM_COLS = 2
	NUM_ROWS = DKKDIV
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

        DO I = 1, KIGDIV
            WRITE (CDNAME(1,I),800)
            WRITE (CDNAME(2,I),800)
            WRITE (CDNAME(3,I),800)
        END DO

        DO 170 I = 1, DKKDIV  
	    CNT = 0
	    DO J = 1, 3
	       IF(DKKMAT(J,I).NE.0) CNT = CNT + 1
	    ENDDO
            DO J = 1, CNT
                WRITE (CDNAME(J,I),801) DKKMAT(J,I)
                DO K=1,7
                    IF(DNAME(K,J,I).EQ.'0'.OR.DNAME(K,J,I).EQ.'1')
     *                DNAME(K,J,I)=' '
                ENDDO
            ENDDO
	    IF(CNT.EQ.1) THEN
	       WRITE(DISP_DIV(I),804) (CDNAME(K,I),K=1,1),FIX(I)
	    ELSEIF(CNT.EQ.2) THEN
	       WRITE(DISP_DIV(I),803) (CDNAME(K,I),K=1,2),FIX(I)
	    ELSE
	       WRITE(DISP_DIV(I),804) (CDNAME(K,I),K=1,3),FIX(I)
	    ENDIF
170     CONTINUE
	
	DO I=1,DKKDIV
 	   CALL GUIARG_CHAR(OUTBUF,%REF(DISP_DIV(I)),26)
       	   CALL GUIARG_MONYI8(OUTBUF,LVAL(I))
	ENDDO	
C
C RESULT SET 3
C
	NUM_COLS = 2
        NUM_ROWS = (DKKDAT(CURDRW) - DKKBSD) + 1    	
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	DO I=1, NUM_ROWS
          CALL GUIARG_DATE(OUTBUF,DKKDAT(CURDRW)-I+1)
          CALL GUIARG_MONYI8(OUTBUF,KSAL(2+I))
	ENDDO
C
	IF(DKKSTS.LT.GAMENV) GOTO 9000
C
C RESULT SET 4
C
	NUM_COLS = 1
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

        CALL GUIARG_INT4(OUTBUF,DKKWIN)

	IF(DKKSTS.LT.GAMDON) GOTO 9000
C
	TOTWON(1) = 0
	TOTWON(2) = 0
	TOTPAD(1) = 0
	TOTPAD(2) = 0
        DO I=1,DKKDIV
            TOTWON(1) = TOTWON(1) + DKKSHR(I)
            TOTWON(2) = TOTWON(2) + LWIN(I)
            TOTPAD(1) = TOTPAD(1) + DKKPAD(I)
            TOTPAD(2) = TOTPAD(2) + LPAD(I)
        END DO
C
C RESULT SET 5
C
	NUM_COLS = 4
	NUM_ROWS = DKKDIV
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	DO I=1,DKKDIV
 	   CALL GUIARG_INT4(OUTBUF,DKKSHR(I))
	   CALL GUIARG_MONYI8(OUTBUF,LWIN(I))
	   CALL GUIARG_INT4(OUTBUF,DKKPAD(I))
	   CALL GUIARG_MONYI8(OUTBUF,LPAD(I))
	ENDDO
C	
C RESULT SET 6
C
	NUM_COLS = 4
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_INT4(OUTBUF,TOTWON(1))
	CALL GUIARG_MONYI8(OUTBUF,TOTWON(2))
	CALL GUIARG_INT4(OUTBUF,TOTPAD(1))
	CALL GUIARG_MONYI8(OUTBUF,TOTPAD(2))
C
9000	CONTINUE
	CALL GUIARG_SET_MESLEN(MES_LEN)
	RETURN
C
	RETURN
C
800     FORMAT(7X)
801     FORMAT(I7.7)
802     FORMAT('( ',A7,'/',A7,'/',A7,' )',A1)
803     FORMAT('( ',A7,'/',A7,' )',A1)
804     FORMAT('( ',A7,' )',A1)
	END
C
C Function to return an Integer 8 from Integer 4 using their 32 bits as positive number
C To avoid overflows
C
C=======OPTIONS /CHECK=NOOVERFLOW
	INTEGER*8 FUNCTION I4TOI8(AMOUNT)
	IMPLICIT NONE
    	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INTEGER*4 AMOUNT 
C
        INTEGER*4 TEMP,LMASK,HMASK
        INTEGER*8 BIG,SBIT
        DATA LMASK/Z7FFFFFFF/
        DATA HMASK/Z80000000/
        DATA SBIT/2147483648/
                    
                 
        IF (IAND(AMOUNT,HMASK) .NE. 0) THEN
          TEMP=IAND(AMOUNT,LMASK)
          BIG=SBIT+TEMP
        ELSE
          BIG=AMOUNT
        END IF

        I4TOI8 = BIG        
        RETURN
C        
        END
