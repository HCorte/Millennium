C
C SUBROUTINE XXXNUM
C
C V02 24-MAY-1999 UXN OUTPUT LUN CHANGED TO 6
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XXXNUM(PRMFLG, STRING,NUM,LOW,HIGH,EXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C
	LOGICAL	PRMFLG
C
C
	CHARACTER STRING*(*),CBUF(20)
	CHARACTER CXINBUF*(20) !1 CHARACTER -> 1 Byte maximo de 20 caracteres
	INTEGER*4 INLEN
	INTEGER*4 INBUF(5), EXT, ST, FLG, NUM, SIGN, I !INBUF têm o tamanho de 5 ou seja
	!1 Byte -> 255 decimal ASCII the extended version ( uses 255 decimal -> Byte) 
	INTEGER*4 DIG, HIGH, LOW, POS
C
	EQUIVALENCE(INBUF,CBUF,CXINBUF)
C
C
C
C Skip spaces in the beginning of the prompt.
C
        INLEN = LEN(STRING)
        POS   = 1
        DO 5 I=1,INLEN
           IF(STRING(I:I).EQ.' ') THEN
              POS = POS + 1
           ELSE
              GOTO 6
           ENDIF
5       CONTINUE
6       CONTINUE
C
C READ LU
C
10	CONTINUE
	EXT=0
	ST=0
	IF(PRMFLG)THEN
	  CALL PRMTEXT(STRING(POS:), CXINBUF, INLEN)
	ELSE
	  CALL WIMG(6,STRING(POS:))
	  READ (5,900) INBUF
	ENDIF
C
C CHECK FOR EXIT
C
	IF (CBUF(1).EQ.'E' .OR. CBUF(1).EQ.'e') THEN
	   EXT=-1
	   GOTO 8888
	ENDIF
C
C CHECK FOR PRINT
C
	IF (CBUF(1).EQ.'P' .OR. CBUF(1).EQ.'p') THEN
	   EXT=-2
	   GOTO 8888
	ENDIF
C
C CHECK FOR MORE
C
	IF (CBUF(1).EQ.'M' .OR. CBUF(1).EQ.'m') THEN
	   EXT=-3
	   GOTO 8888
	ENDIF
C
C CHECK FOR ALL
C
	IF (CBUF(1).EQ.'A' .OR. CBUF(1).EQ.'a') THEN
	   EXT=-4
	   GOTO 8888
	ENDIF
C
C CHECK FOR CANCEL
C
	IF (CBUF(1).EQ.'C' .OR. CBUF(1).EQ.'c') THEN
	   EXT=-5
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'X'
C
	IF (CBUF(1).EQ.'X' .OR. CBUF(1).EQ.'x') THEN
	   EXT=-6
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'B'
C
	IF (CBUF(1).EQ.'B' .OR. CBUF(1).EQ.'b') THEN
	   EXT=-7
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'N'
C
	IF (CBUF(1).EQ.'N' .OR. CBUF(1).EQ.'n') THEN
	   EXT=-8
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'H'
C
	IF(CBUF(1).EQ.'H'.OR.CBUF(1).EQ.'h') THEN
	   EXT=-9
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'T'
C
	IF(CBUF(1).EQ.'T'.OR.CBUF(1).EQ.'t') THEN
	   EXT=-10
	   GOTO 8888
	ENDIF
C
C CHECK FOR '?'
C
	IF(CBUF(1).EQ.'?') THEN
	   EXT=-11
	   GOTO 8888
	ENDIF
C
C CHECK FOR 'Q'
C
	IF(CBUF(1).EQ.'Q'.OR.CBUF(1).EQ.'q') THEN
	   EXT=-12
	   GOTO 8888
	ENDIF
C
C *V04*   CHECK FOR '$'
C
	IF(CBUF(1).EQ.'$') THEN		    !V04
	  ST=IMONY(INBUF,2,5,BETUNIT)	    !V04
	  IF(ST.GE.0) NUM=ST		    !V04
	ELSE
C
C CONVERT TO NUMERIC DATA
C
	  FLG=0
	  NUM=0
	  SIGN=1
	  DO 20 I=1,10
	    IF (I.EQ.1 .AND. CBUF(I).EQ.'-') THEN
	      SIGN=-1
	      GOTO 20
	    ENDIF
	    IF (CBUF(I).EQ.' ') THEN
	      GOTO 20
	    ENDIF
	    CALL ASCBIN(INBUF,I,1,DIG,ST)
	    IF (ST.LT.0) THEN
	      GOTO 30
	    ENDIF
	    FLG=1
	    NUM=NUM*10+DIG !move uma posição para a esquerda e adiciona o digito acabado de ler
20	  CONTINUE
	  NUM=NUM*SIGN
	ENDIF
C
C CHECK IF INPUT IS VALID
C
	IF (ST.LT.0 .OR. FLG.EQ.0) THEN
	   GOTO 30
	ENDIF
	IF (NUM.LE.HIGH .AND. NUM.GE.LOW) THEN
	   GOTO 8888
	ENDIF
	IF (HIGH.GE.2100000000) THEN
	   GOTO 8888
	ENDIF
C
C LIMIT EXCEEDED
C
	WRITE(6,901) IAM(),LOW,HIGH
	GOTO 10
C
C INVALID INPUT
C
30	CONTINUE
	WRITE(6,902) IAM(),LOW,HIGH
	GOTO 10
8888	CONTINUE
	RETURN
C
900	FORMAT(5A4)
901	FORMAT(1X,A,' Input value invalid.   Does not fall within interval',
     !	         I10,' to ',I10)
902	FORMAT(1X,A,' The number should fall within interval',
     !	         I10,' to ',I10)
C
	END
