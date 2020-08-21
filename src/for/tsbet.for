C
C SUBROUTINE TSBET
C
C V05 01-FEB-2000 UXN TNFRAC ADDED.
C V04 05-AUG-1993 GXA Adjusted 1X2 again.
C V03 05-AUG-1993 GXA Changed 1X2 to 12X.
C V02 26-JUL-1993 SXH Changed 123 to 1X2
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C SUBROUTINE TO BUILD BET IMAGE FOR TOTO SELECT TRANSACTION
C
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
C
	SUBROUTINE TSBET(TRABUF,BIMAGE)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4   STAT(0:3)		    !Row status
	INTEGER*4   LIN			    !Screen line number
	INTEGER*4   I			    !LOOP VARIABLES

	CHARACTER*3 POOL(0:3)		    !Pool marks
        CHARACTER*56 BIMAGE(12)
C
	DATA	    POOL/'   ','1--','--2','-X-'/
	DATA	    STAT/'    ','won ','can ','lost'/
	INTEGER*4   AMT
C
C
	LIN=1
	DO 10 I=0,TRABUF(TWTSEL1)-1,3
	IF(I.EQ.0) THEN
	  AMT = TRABUF(TWTAMT1)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	  WRITE(BIMAGE(LIN),901) 
     *     CMONY(AMT,7,BETUNIT),
     *    TRABUF(TWTROW1+I*TWTBLEN),
     *	  POOL(TRABUF(TWTPOL1+I*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS1+I*TWTBLEN)),
     *    TRABUF(TWTROW1+(I+1)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL1+(I+1)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS1+(I+1)*TWTBLEN)),
     *    TRABUF(TWTROW1+(I+2)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL1+(I+2)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS1+(I+2)*TWTBLEN))
	ELSE
            WRITE(BIMAGE(LIN),902) TRABUF(TWTROW1+I*TWTBLEN),
     *      POOL(TRABUF(TWTPOL1+I*TWTBLEN)),
     *      STAT(TRABUF(TWTSTS1+I*TWTBLEN)),
     *      TRABUF(TWTROW1+(I+1)*TWTBLEN),
     *      POOL(TRABUF(TWTPOL1+(I+1)*TWTBLEN)),
     *      STAT(TRABUF(TWTSTS1+(I+1)*TWTBLEN)),
     *      TRABUF(TWTROW1+(I+2)*TWTBLEN),
     *      POOL(TRABUF(TWTPOL1+(I+2)*TWTBLEN)),
     *      STAT(TRABUF(TWTSTS1+(I+2)*TWTBLEN))
	ENDIF
	LIN=LIN+1
10	CONTINUE
	IF(TRABUF(TWNBET).EQ.1) RETURN
C
C
        LIN=LIN+1
        DO 20 I=0,TRABUF(TWTSEL2)-1,3
	IF(I.EQ.0) THEN
	  AMT = TRABUF(TWTAMT2)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
          WRITE(BIMAGE(LIN),901) 
     *    CMONY(AMT,7,BETUNIT),
     *    TRABUF(TWTROW2+I*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+I*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+I*TWTBLEN)),
     *    TRABUF(TWTROW2+(I+1)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+(I+1)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+(I+1)*TWTBLEN)),
     *    TRABUF(TWTROW2+(I+2)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+(I+2)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+(I+2)*TWTBLEN))
	ELSE
          WRITE(BIMAGE(LIN),902) TRABUF(TWTROW2+I*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+I*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+I*TWTBLEN)),
     *    TRABUF(TWTROW2+(I+1)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+(I+1)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+(I+1)*TWTBLEN)),
     *    TRABUF(TWTROW2+(I+2)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL2+(I+2)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS2+(I+2)*TWTBLEN))
	ENDIF
        LIN=LIN+1
20      CONTINUE
        IF(TRABUF(TWNBET).EQ.2) RETURN
C
C
	LIN=LIN+1
        DO 30 I=0,TRABUF(TWTSEL3)-1,3
	IF(I.EQ.0) THEN
	  AMT = TRABUF(TWTAMT3)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
          WRITE(BIMAGE(LIN),901) 
     *    CMONY(AMT,7,BETUNIT),
     *    TRABUF(TWTROW3+I*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+I*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+I*TWTBLEN)),
     *    TRABUF(TWTROW3+(I+1)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+(I+1)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+(I+1)*TWTBLEN)),
     *    TRABUF(TWTROW3+(I+2)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+(I+2)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+(I+2)*TWTBLEN))
	ELSE
          WRITE(BIMAGE(LIN),902) TRABUF(TWTROW3+I*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+I*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+I*TWTBLEN)),
     *    TRABUF(TWTROW3+(I+1)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+(I+1)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+(I+1)*TWTBLEN)),
     *    TRABUF(TWTROW3+(I+2)*TWTBLEN),
     *    POOL(TRABUF(TWTPOL3+(I+2)*TWTBLEN)),
     *    STAT(TRABUF(TWTSTS3+(I+2)*TWTBLEN))
	ENDIF
        LIN=LIN+1
30      CONTINUE
        RETURN
C
C
901	FORMAT(1X,A7,3(3X,I2.0,1X,A3,1X,A4))
902     FORMAT(8X,3(3X,I2,1X,A3,1X,A4))
	END
