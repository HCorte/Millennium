C V02 10-OCT-2012 FRP USE AAAACCC FORMAT INSTEAD OF YYYYWW FORMAT
C V01 23-FEB-2002 ANG INITIAL RELEASE FOR PORTUGAL 
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
        SUBROUTINE GETWEEK(CLINE,DAT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INTEGER*4  CLINE(20),AUXLINE(20)
        INTEGER*4  I,DAT,L,K

        CHARACTER*80 COMMAND

        EQUIVALENCE(AUXLINE,COMMAND)

	CALL FASTMOV(CLINE(1),AUXLINE(1),20)

       DAT=0
       DO K=1,80
          IF (COMMAND(K:K).EQ.'/') THEN
             DO I=K+1,80
               IF (COMMAND(I:I).EQ.'-') THEN
                   DAT = CTOI(COMMAND(K+1:I-1),L)
                   DAT = DAT*1000
                   DAT = DAT + CTOI(COMMAND(I+1:I+3),L)
                   RETURN
               ENDIF
             ENDDO
          ENDIF
       ENDDO

       RETURN

       END
