C
C SUBROUTINE STTSK
C
C V03 11-Nov-1999 RXK Changed for ALPHA (UXN)
C V02 21-Jan-1993 DAB Initial Release Based on Netherlands Bible, 12/92, and
C                     Comm 1/93 update DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ** Source - nrm_sttsk.for **
C
C VAX_STTSK.FOR
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE STTSK (TNAME, TSKSTS, STATUS)
	IMPLICIT  NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
        INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	BYTE      TNAME(8)
	INTEGER*4 TSKSTS(2)
	INTEGER*4 STATUS
C
	INTEGER*4	I4NAME(4)
	CHARACTER*15	PRCNAMEXP
	EQUIVALENCE	(I4NAME,PRCNAMEXP)
C
	INTEGER*4	CPUBUF, BUFLEN
C
	INTEGER*4	I4ITEM(4)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)
C
	INTEGER*4	ST, I, PREFIX_LEN
	CHARACTER	NAMC
	BYTE		NAMB
C
	LOGICAL		FIRST_TIME/.TRUE./
	INTEGER*4	PRCNAMEXP_LEN
C
C       FOR MULTIPLE GAMING SYSTEMS ON ONE COMPUTER EACH PROCESS SHOULD
C       HAVE A PREFIX IDENTIFYING WICH GAMING ENVIRONMENT THE PROCESS
C       BELONGS
C
        IF (FIRST_TIME) THEN
            FIRST_TIME = .FALSE.
            CALL GETPRFX(I4NAME, PREFIX_LEN) !so I4NAME will have the following PREFIX's INIGS,COMIGS,OUTIGS -> XXXX_COMIGS
        ENDIF
C
C	LENGTH IS MODIFIED LATER, SO INITIALIZE IT FIRST
C
C
        DO 30 I=1,8
           NAMB = TNAME(I)
           NAMC = CHAR(NAMB)
           IF (NAMB .EQ. 0 .OR. NAMC.EQ.' ') THEN
              GOTO 40
           ENDIF
           PRCNAMEXP((PREFIX_LEN+I):(PREFIX_LEN+I))=NAMC
30      CONTINUE
C
40      CONTINUE
        I = I - 1
C
	PRCNAMEXP_LEN  = PREFIX_LEN+I
C
	I2ITEM(1) = 4
	I2ITEM(2) = JPI$_CPUTIM
	I4ITEM(2) = %LOC(CPUBUF)
	I4ITEM(3) = %LOC(BUFLEN)
	I4ITEM(4) = 0		    !TO TERMINATE LIST
C
	ST = SYS$GETJPIW( ,,PRCNAMEXP(1:PRCNAMEXP_LEN),I4ITEM,,,)
	IF(.NOT.ST)THEN
D	    TYPE *, IAM(), 'PROCESS ', PRCNAMEXP(1:PRCNAMEXP_LEN), ' NOT FOUND'
	    STATUS = 4
	    RETURN
	ENDIF
C
D	TYPE *,IAM(), 'TASK ', PRCNAMEXP, ' USED ', CPUBUF/100, ' SECONDS'
	STATUS = 0
	RETURN
	END
