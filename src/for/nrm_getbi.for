C
C SUBROUTINE GETBI
C $Log:   GXAFXT:[GOLS]GETBI.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:24:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getbi.for **
C
C GETBI.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO CACULATE BLOCK, INDEX, AND
C OFFSET INTO LOG BUFFER GIVEN RECORD SIZE
C AND SERIAL AND TRANSACTION SERIAL NUMBER.
C
C CALLING SEQUENCE:
C     CALL GETBI(SERIAL,BLOCK,INDEX,OFFSET)
C INPUT
C     SERIAL - TRANSACTION SERIAL NUMBER
C
C OUTPUT
C     BLOCK  - DISK FILE BLOCK NUMBER
C     INDEX  - INDEX INTO DISK BLOCK
C     OFFSET - OFFSET INTO DISK BLOCK
C
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
	SUBROUTINE GETBI(SERIAL,BLOCK,INDEX,OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 SER, OFFSET, INDEX, BLOCK, SERIAL
C
C CACULATE BLOCK, INDEX, AND OFFSET
C
	SER=MOD(SERIAL,SYSOFF)
	BLOCK=((SER-1)/LBLK)+1
	INDEX=MOD((SER-1),LBLK)+1
	OFFSET=((INDEX-1)*LREC)+LHDR+1
	RETURN
	END
