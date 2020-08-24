C
C PROGRAM ENALIB
C $Log:   GXAFXT:[GOLS]ENALIB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:03:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:12:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - enalib.for **
C
C ENALIB.FOR
C
C V01 13-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM ENALIB
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:POLCOM.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
C
	INTEGER*4 CBUF(CDLEN), OPT, EXT, I, NUM, POOL, TYP, GIND,K
	INTEGER*4 GNUM, NUMPOL, POOLNBR, OFFSET, MESOFF, FLAG, ST
	INTEGER*4 MAXNUM
	INTEGER*4 LIB(2)/'WARN','LIAB'/
C
C GET OPTION
C
100	CONTINUE
	WRITE(5,900)
	CALL INPNUM('Enter option [E-Exit] ',OPT,1,2,EXT)
	IF(EXT.NE.0) GOTO 8000
	GOTO(1000,2000) OPT
C
C SHOW VALUE OF PMESS
C
1000	CONTINUE
	WRITE(5,901)
	DO 1100 I=1,100
	   IF(PMESS(I).NE.0) THEN
	     NUM  = (PMESS(I)/10000)
	     POOL = (PMESS(I)-NUM*10000)/100
	     TYP  = (PMESS(I)-NUM*10000-POOL*100)/10
	     GIND = (PMESS(I)-NUM*10000-POOL*100-TYP*10)
	     GNUM = GTNTAB(TNBR,GIND)
	     IF(GIND.EQ.NB3TYP) THEN
	       WRITE(5,902) NUM,P3NAMES(POOL),LIB(TYP),
     *			    (GLNAMES(K,GNUM),K=1,4)
	     ELSE
	       WRITE(5,906) NUM,P4NAMES(POOL),LIB(TYP),
     *			    (GLNAMES(K,GNUM),K=1,4)
	     ENDIF
	   ENDIF
1100	CONTINUE
	GOTO 100
C
C GET GAME INDEX
C
2000	CONTINUE
	DO 2100 GIND=1,MAXIND
	   GNUM=GTNTAB(TNBR,GIND)
	   IF(GNUM.LT.1) GOTO 2100
	   WRITE(5,903) GIND,(GLNAMES(K,GNUM),K=1,4)
2100	CONTINUE
	CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.NE.0) GOTO 100
	IF(GTNTAB(TNBR,GIND).LT.1) GOTO 2000
C
C GET POOL
C
	NUMPOL=NUMPL3
	IF(GIND.EQ.NB4TYP) NUMPOL=NUMPL4
	DO 2200 I=1,NUMPOL
	   IF(GIND.EQ.NB3TYP) THEN
	     WRITE(5,904) I,P3NAMES(I)
	     MAXNUM=999
	   ELSE
	     WRITE(5,904) I,P4NAMES(I)
	     MAXNUM=9999
	   ENDIF
2200	CONTINUE
	CALL INPNUM('Enter pool       ',POOL,1,NUMPOL,EXT)
	IF(EXT.NE.0) GOTO 100
C
C GET MESSAGE TYPE
C
	DO 2300 I=1,2
	   WRITE(5,905) I,LIB(I)
2300	CONTINUE
	CALL INPNUM('Enter type       ',TYP,1,2,EXT)
	IF(EXT.NE.0) GOTO 100
C
C GET NUMBER
C
	CALL INPNUM('Enter number     ',NUM,0,MAXNUM,EXT)
	IF(EXT.NE.0) GOTO 100
	POOLNBR = NUM*10000+POOL*100+TYP*10+GIND
C
C FIND A FREE OFFSET IN PMESS
C
	OFFSET=0
	MESOFF=0
	DO 2400 I=1,100
	   IF(PMESS(I).EQ.0) OFFSET=I
	   IF(PMESS(I).EQ.POOLNBR) MESOFF=I
2400	CONTINUE
C
	IF(MESOFF.NE.0) THEN
	  TYPE *,'This liability message is currently off'
	  CALL WIMG(5,'Do you to turn it back on ')
	  CALL YESNO(FLAG)
	  IF(FLAG.EQ.1) THEN
	    POOLNBR=0
	    OFFSET = MESOFF
	  ELSE
	    GOTO 100
	  ENDIF
	ELSE
	  IF(OFFSET.EQ.0) THEN
	    TYPE *,'Maximum number of liability messages turned off'
	    GOTO 100
	  ELSE
	    TYPE *,'This liability message is currently on'
	    CALL WIMG(5,'Do you to turn it off ')
	    CALL YESNO(FLAG)
	    IF(FLAG.NE.1) GOTO 100
	  ENDIF
	ENDIF
C
C SET UP COMMAND BUFFER
C
	CALL FASTSET(0,CBUF,CDLEN)
	CBUF(1)=6
	CBUF(2)=POOLNBR
	CBUF(3)=TCNBR
	CBUF(6)='SYS '
	CBUF(8)=OFFSET
	CBUF(9)=GIND
10	CONTINUE
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	   TYPE*,'Queue command error > ',ST,' continue to retry'
	   CALL GPAUSE
	   GOTO 10
	ENDIF
	GOTO 100
C
C
C
8000	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT SECTION
C
900	FORMAT(1X,'1. Display liablity message turned off',/,
     *         1X,'2. Change the status of a liability message',/)
901	FORMAT(1X,'NUMBER',1X,'POOL',1X,'TYPE',1X,'INDEX',/)
902	FORMAT(4X,I3.3,1X,A4,1X,A4,1X,4A4)
903	FORMAT(1X,I2,3X,4A4)
904	FORMAT(1X,I2,3X,A4)
905	FORMAT(1X,I2,3X,A4)
906	FORMAT(3X,I4.4,1X,A4,1X,A4,1X,4A4)
	END
