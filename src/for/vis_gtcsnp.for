C
C SUBROUTINE GTCSNP
C
C GTCSNP.FOR
C
C V01 07-DEC-1999 OXK Initial Release
C
CDESC:
C	GTC SNAPSHOT FOR VISION
C	Displaying ticket status in files could be added later.
C				    (TMF - VLF - TCF)
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE GTCSNP(CDC,INTSER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
C ARGUMENTS
C
	INTEGER*4  CDC		    ! IN CDC-date
	INTEGER*4  INTSER	    ! IN internal serial# requested
C
C LOCAL VARIABLES
C
        INTEGER*4    EXTSER
        INTEGER*4    JULDAY, JULYEAR
        INTEGER*4    CHKDGT

        INTEGER*2    DBUF(LDATE_LEN)

C
C-----------------------------------------------------------
C BEGIN CODE; INITIALIZATIONS ETC.
C
	SMODE=.TRUE.

      	CALL OUTGEN(CDC,INTSER,EXTSER,CHKDGT)

      	DBUF(VCDC) = CDC
      	CALL LCDATE(DBUF)
	JULDAY = DBUF(VJUL)
	JULYEAR= DBUF(VYEAR2)

	WRITE(CLIN1,9000)
 	IF ((CDC.GT.0).AND.(INTSER.GT.0).AND.
     *      (EXTSER.GE.0).AND.(CHKDGT.GE.0)) THEN
	    WRITE(CLIN3,9010)CDC, JULDAY, JULYEAR
	    WRITE(CLIN5,9020)INTSER
	    WRITE(CLIN7,9030)JULDAY, EXTSER, CHKDGT
	ENDIF

	WRITE(CLIN23,9005)

	RETURN

C     ==================== FORMAT STATEMENTS ===================
C
9000	FORMAT('Getchk (GTC) snapshot ')
9005    FORMAT('Enter /CDC internal#  OR   /CDC @external-chk')
9010	FORMAT('The date used: CDC =',I5,'   Julian = ',I3.3,'/',I4.2) 
9020 	FORMAT('Internal serial number: ',I9.8)
9030 	FORMAT('External serial number: ',I3.3,' - ',I9.8,' - ',I3.3)
C
	END
