C
C $Log:   GXAFXT:[GOLS]TMFSWI.FOV  
C  
C     Rev 1.0   17 Apr 1996 15:35:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   22 Oct 1993 15:25:48   GXA
C  Initial revision.
C
C TMFSWI.FTN
C
C
C SUBROUTINE TO UPDATE MEMORY IN THE EVENT OF RESTART FROM
C CHECKPOINT, THUS UF A PRIMARY DISK FAILURE OCCURS THE SYSTEM
C WILL RECOVER, AND NOW IF FORCED TO RESTART FROM CHECKPOINT
C THE SYSTEM WILL BE CONSISTANT.
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TMFSWI
	IMPLICIT NONE 
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4 ST
C
C GET SYSTEM CONFIGURATION
C
	CALL GETSCONF(SCFREC,ST)
C
	CALL FASTMOV(SCFSFN(1,PTMF),SFNAMES(1,PTMF),5)
	CALL FASTMOV(SCFSFN(1,BTMF),SFNAMES(1,BTMF),5)
	P(DISKSW) = SCFPAR(DISKSW)
C
C
	RETURN
	END

