C
C SUBROUTINE NMOV2TOI2
C $Log:   GXAFXT:[GOLS]NMOV2TOI2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:12:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nbuffmove.for **
C
C VAX_BUFFMOVE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C BUFFMOVE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C MODIFIED FOR VAX ****
C
C
C     BUFFMOVE.FTN
C     _____________
C
C
C    CALL NMOV4TOI4(DATA,BUFFER,BYTE_OFFSET) ; MOVE FROM BUFFER TO I4
C    CALL NMOV2TOI4(DATA,BUFFER,BYTE_OFFSET) ;MOVE 2 FROM BUFFER TO I4
C    CALL NMOV2TOI2(DATA,BUFFER,BYTE_OFFSET) ; MOVE FROM BUFFER TO I2
C    CALL NI2TOBUF2(DATA,BUFFER,BYTE_OFFSET) ;MOVE FROM I2 TO BUFFER
C    CALL NI4TOBUF2(DATA,BUFFER,BYTE_OFFSET) ;MOVE 2 FROM I4 TO BUFFER
C    CALL NI4TOBUF4(DATA,BUFFER,BYTE_OFFSET) ;MOVE FROM I4 TO BUFFER
C
C     BUFFER         - BUFFER WITH DATA, OR TO BE FILLED
C     BYTE_OFFSET    - BYTE OFFSET, STARTING WITH BYTE 0
C     DATA           - DATA TO BE MOVED (FROM OR TO)
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
	SUBROUTINE NMOV2TOI2(DATA,BUFFER,BYTE_OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4 BYTE_OFFSET
	BYTE	BUFFER(*)
	BYTE    DATA(2)
C
	DATA(1) = BUFFER(BYTE_OFFSET+1)
	DATA(2) = BUFFER(BYTE_OFFSET+2)
C
	RETURN
	END
