C
C SUBROUTINE CDUREQ
C $Log:   GXAFIP:[GOLS]CDUREQ.FOV  $
C  
C     Rev 1.0   31 Jan 1997 15:16:12   RXK
C  Initial revision.
C  
C CDUREQ.FOR
C
C SUBROUTINE TO PROCESS CUSTOMER DISPLAY UNIT MESSAGE REQUEST
C
C CALLING SEQUENCE:
C      CALL CDUREQ(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CDUREQ(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	INTEGER*4 I4TEMP, TEMP
	BYTE      MESTAB(*),I1TEMP(4)
	EQUIVALENCE(I4TEMP,I1TEMP)
 
	INTEGER*4 MYCHKSUM, CHKLEN, IND, ERRTYP
        INTEGER*4 CDU_LINE_LENGTH 
        INTEGER*4 CDU_ROWS
        INTEGER*4 I, J
	INTEGER*2 OUTLEN
	DATA ERRTYP/Z90/
C
C 
        CDU_LINE_LENGTH=20
        CDU_ROWS=2
C
C CDU TEXT WILL BE SENT TO ALL AGENTS 
C BUILD OUTPUT MESSAGE BACK TO TERMINAL
C
        IND = 5
        I4TEMP=TKTCDU
	MESTAB(IND+0) = I1TEMP(1)
	MESTAB(IND+1) = I1TEMP(2)
	IND = IND + 2
C
        DO I=1,NUMCDU
           DO J=1,CDU_ROWS
 	      CALL MOVBYT(TKTMES(1,J,MAXGAM+PRM_NUMOPN+I),1,
     *                    MESTAB,IND,CDU_LINE_LENGTH)
              IND=IND+CDU_LINE_LENGTH
           ENDDO
        ENDDO
C
C TERMINATOR
C
	MESTAB(IND) = '00'X
	IND = IND + 1
	OUTLEN = IND - 1
C
C CALCULATE CHECKSUM AND RETURN
C
9000	CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN = OUTLEN - 1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
	RETURN
	END
