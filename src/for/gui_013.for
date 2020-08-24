C GUI_013.FOR
C
C V02 13-JAN-2011 FJG Lotto2 Batch: Include new divisions
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
	SUBROUTINE GUI_013(OUTBUF,MES_LEN,RET_CODE)
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
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 GIND
        INTEGER*4  LPAD(LTGDIV)
        INTEGER*4  LVAL(LTGDIV)
        INTEGER*4  LWIN(LTGDIV)
        INTEGER*4  BPAD(LTGDIV)
        INTEGER*4  BVAL(LTGDIV)
        INTEGER*4  BWIN(LTGDIV)
	REAL*8	   POOL
	INTEGER*4  TOTWON(2),TOTPAD(2)
C
        INTEGER*4  TLEN
	PARAMETER (TLEN=NUMTOT*(NUMFIN+1))
        INTEGER*4  FRAMT
	EXTERNAL   FRAMT
C
        INTEGER*4  GNUM,BNUM,LNUM
        INTEGER*4  ST,DIV,STS
	INTEGER*4  DRAW,I,J,K,YEAR,WEK,LTOT

        CHARACTER*8 DNAME(LTGDIV)          
        CHARACTER*1 PLUS(2)
!=======V02=====================================================================
        DATA PLUS/' ','+'/
	INTEGER*4   MATCH(2,LTGDIV)
	CHARACTER*1 LUCK(2)                 !
	DATA LUCK/'L',' '/        
!=======V02=====================================================================	
C
C  GET CDC
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	GIND = GUI_ARGVAL(1)
	DRAW = GUI_ARGVAL(2)

	IF(GIND.LT.1.OR.GIND.GT.NUMLTO) THEN
	  RET_CODE = 11
	  RETURN
	ENDIF

	GNUM = GTNTAB(TLTO,GIND)
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
	    CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
	    GOTO 100
	ENDIF
C
	CALL READW(GAMFDB(1,GNUM),DRAW,DLTREC,ST)
	IF(ST.NE.0) THEN
	    CALL OPS('Failed to read '//CGFNAMES(GNUM),ST,DRAW)
	    RET_CODE = 11
	    RETURN
	ENDIF
C
100	CONTINUE
C
	LTOT = 0
	DO I = 1, LTGENT
	    LTOT = LTOT + DLTSAL(I)
        END DO
        POOL=LTOT*CALPER(DLTSPR)
        DO I=1,DLTDIV
            LVAL(I)=IDNINT(POOL*CALPER(DLTPER(I)))
            LVAL(I)=LVAL(I)+DLTPOL(I)        !ADD POOL CARRIED OVER
            LWIN(I)=0
            LPAD(I)=0
        END DO
C
        IF(DLTSTS.GE.GAMDON.OR.DLTSPR.EQ.0) THEN
            DO I = 1, DLTDIV
                LVAL(I) = DLTSHV(I,1)
                LWIN(I) = DLTSHR(I,1)*LVAL(I)
                LPAD(I) = FRAMT(MAXFRC(GNUM),DLTPAD(I,1),LVAL(I))
                IF(DLTBDR.GT.0) THEN
                   BVAL(I) = DLTSHV(I,2)
                   BWIN(I) = DLTSHR(I,2)*BVAL(I)
                   BPAD(I) = FRAMT(MAXFRC(GNUM),DLTPAD(I,2),BVAL(I))
                ENDIF
            END DO
        ENDIF
C
	CALL GUI_CHKSTS_RESCOM(GIND,TLTO,DRAW,STS)
	STS = MAX(STS,DLTSTS)
C
C SEND DATA TO GUI
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 8
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C

        CALL FIGWEK(DLTESD,WEK,YEAR)

	CALL GUIARG_CHAR(OUTBUF,GLNAMES(1,GNUM),16)
	CALL GUIARG_INT2(OUTBUF,DLTDRW)
	CALL GUIARG_INT2(OUTBUF,WEK)
	CALL GUIARG_INT2(OUTBUF,YEAR)

	CALL GUIARG_BYTE(OUTBUF,STS)
	CALL GUIARG_MONY(OUTBUF,DLTSAL(1))
	CALL GUIARG_MONY(OUTBUF,DLTPOL(1))	
	CALL GUIARG_MONY(OUTBUF,LTOT)	
C
C RESULT SET 2
C
	NUM_COLS = 2
	NUM_ROWS = DLTDIV
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

        DO I=1,LTGDIV
            DNAME(I)=' - '
        END DO
	
        DO 180 K=1,LTGBET
            DO 170 I=1,DLTBET(K)
                DO 170 J=1,2
                    IF(DLTWTB(I,J,K).NE.0) THEN
                        DIV=DLTWTB(I,J,K)
                        IF(DLTLFL.EQ.0) THEN
                          IF(DNAME(DIV).EQ.' - ') WRITE (DNAME(DIV),800) I,PLUS(J), DLTBET(K)
                        ELSE
                          MATCH(1,DIV) = I
                          MATCH(2,DIV) = DLTBET(K)                             
                        ENDIF
                    ENDIF
170          CONTINUE
180     CONTINUE
!=======V19 FJG=================================================================
        IF(DLTLFL.NE.0) THEN	           
          DO I=1,LTGDIV          
            DO K=1,2
              IF(DLTLNC(K,I).NE.0) THEN
                DIV = DLTLNC(K,I)                
                WRITE (DNAME(DIV),800) MATCH(1,I),LUCK(K), MATCH(2,I)
              ENDIF
            ENDDO
          ENDDO
          DIV = DLTLDV
          WRITE(DNAME(DIV),8001)          
        ENDIF
!=======V19 FJG=================================================================
	DO I=1,DLTDIV
 	   CALL GUIARG_CHAR(OUTBUF,%REF(DNAME(I)),8)
       	   CALL GUIARG_MONY(OUTBUF,LVAL(I))
	ENDDO	
C
C RESULT SET 3
C
	NUM_COLS = 2
	NUM_ROWS = (DLTDAT(CURDRW) - DLTBSD) + 1        
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
	
	DO I=1, NUM_ROWS
          CALL GUIARG_DATE(OUTBUF,DLTDAT(CURDRW)-I+1)
          CALL GUIARG_MONY(OUTBUF,DLTSAL(2+I))
	ENDDO
C
	IF(DLTSTS.LT.GAMENV) GOTO 9000
C
C RESULT SET 4
C
	NUM_COLS = 3
	NUM_ROWS = DLTNUM
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	DO I=1,NUM_ROWS
	   IF(I.LE.LTGBON) THEN
	      BNUM = DLTBNM(I,1)
	   ELSE
	      BNUM = 0
	   ENDIF
	   IF(DLTLFL.NE.0.AND.I.EQ.1) THEN
	      LNUM = DLTLNM(1)
	   ELSE
	      LNUM = 0
	   ENDIF
	   CALL GUIARG_BYTE(OUTBUF,DLTWIN(I,1))
	   CALL GUIARG_BYTE(OUTBUF,BNUM)
           CALL GUIARG_BYTE(OUTBUF,LNUM)	   
	ENDDO
C
	IF(DLTSTS.LT.GAMDON) GOTO 9000
C
	TOTWON(1) = 0
	TOTWON(2) = 0
	TOTPAD(1) = 0
	TOTPAD(2) = 0
        DO I=1,DLTDIV
            TOTWON(1) = TOTWON(1) + DLTSHR(I,1)
            TOTWON(2) = TOTWON(2) + LWIN(I)
            TOTPAD(1) = TOTPAD(1) + DLTPAD(I,1)
            TOTPAD(2) = TOTPAD(2) + LPAD(I)
        END DO
C
C RESULT SET 5
C
	NUM_COLS = 4
	NUM_ROWS = DLTDIV
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	DO I=1,DLTDIV
 	   CALL GUIARG_INT4(OUTBUF,DLTSHR(I,1))
	   CALL GUIARG_MONY(OUTBUF,LWIN(I))
	   CALL GUIARG_INT4(OUTBUF,DLTPAD(I,1))
	   CALL GUIARG_MONY(OUTBUF,LPAD(I))
	ENDDO
C	
C RESULT SET 6
C
	NUM_COLS = 4
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_INT4(OUTBUF,TOTWON(1))
	CALL GUIARG_MONY(OUTBUF,TOTWON(2))
	CALL GUIARG_INT4(OUTBUF,TOTPAD(1))
	CALL GUIARG_MONY(OUTBUF,TOTPAD(2))
C
9000	CONTINUE
	CALL GUIARG_SET_MESLEN(MES_LEN)
	RETURN
C
800     FORMAT(I1,A1,'/',I1,' ')
8001	FORMAT('LUCK ')
	END
