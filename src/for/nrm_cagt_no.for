C CAGT_NO.FOR
C
C V01 20-JAN-01 CS  INITIAL RELEASE FOR PORTUGAL
C
C THIS IS A CHARACTER FUNCTION
C
C Calling sequence:
C
C     STRING=CAGT_NO(AGTNO)
C     STRING=IAGT_NO(IAGTNO)
C
C Input parametrs:
C
C     AGTNO(7)   Char*1        !THE CHARACTER STRING OF THE CONTIGUOUS AGENT
C                              !NUMBER (WITHOUT THE '.')
C     IAGTNO     INTEGER
C
C Output parameters:
C
C     CAGT_NO    Char*8        !THE AGT NUMBER TO BE DISPLAYED ON REPORTS
C     IAGT_NO    Char*8
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	CHARACTER FUNCTION CAGT_NO*8(AGTNO)
	IMPLICIT NONE
C
        INTEGER       I, K
        CHARACTER     AGTNO(*),DASH
        CHARACTER*8   CTEMP 
        DATA          DASH/'-'/
C
C BUILD THE AGENT NUMBER WITH EMBEDDED DASHES
C
        K = 0
        DO I = 1, SIZEOF(CTEMP)
          IF(I.EQ.3)THEN    ! POSITION THE DASH
            CTEMP(I:I) = DASH
	  ELSE
            K=K+1
            CTEMP(I:I) = AGTNO(K)
          ENDIF
	ENDDO
C
C RETURN THE AGENT NUMBER STRING WITH MASK
C
        CAGT_NO = CTEMP
C
	RETURN
        END
C
C****************************************************************
C
C	FUNCTION THAT RECEIVE THE AGENT NUMBER AS AN INTEGER
C
C****************************************************************
C
	CHARACTER FUNCTION IAGT_NO*8(IAGTNO)
	IMPLICIT  NONE
C
C ROUTINE PARAMETERS
C
	INTEGER*4	IAGTNO
C
	CHARACTER*7	CTEMP
C
	CHARACTER*8	CAGT_NO
	EXTERNAL	CAGT_NO
C
	WRITE(CTEMP,'(I7.7)') IAGTNO
	IAGT_NO = CAGT_NO(CTEMP)
	RETURN
	END
