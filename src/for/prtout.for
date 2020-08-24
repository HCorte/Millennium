C PRTOUT.FOR
C
C V07 24-JUL-2000 UXN Ported for Finland code.
C V06 06-JAN-1998 EVB  Add comm type
C V05 07-MAY-1997 EVB  Print buffer number
C V04 16-AUG-1996 EVB  Check I/O status at opening file
C V03 11-NOV-1994 EVB  Write to GTECH$DEBUG
C V02 28-OCT-1994 EVB  Clean code
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C THIS SUBROUTINE WILL PRINT THE CONTENTS OF
C A PROCOM BUFFER (IN HEX).
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
      SUBROUTINE PRTOUT(BUF)
      IMPLICIT NONE

      INCLUDE '(LIB$ROUTINES)'
      INCLUDE '($FORIOSDEF)'
      INCLUDE 'INCLIB:SYSDEFINE.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PROCOM.DEF'

*     Arguments: 

      INTEGER * 4  BUF       ! INPUT: Buffer number 

*     Locals: 

      INTEGER * 4  J, K      ! Loop counters
      INTEGER * 4  ST        ! I/O status
      INTEGER * 4  WST
      INTEGER * 4  LU        ! Logical unit


*     Implementation: 

        ST = LIB$GET_LUN(LU)

        ST = FOR$IOS_OPEFAI
        DO WHILE(ST.EQ.FOR$IOS_OPEFAI)
           OPEN (UNIT        = LU, 
     .        FILE        = 'GTECH$DEBUG', 
     .        ACCESS      = 'APPEND', 
     .        STATUS      = 'UNKNOWN', 
     .        SHARED,
     .        IOSTAT      = ST)    
	   CALL XWAIT(50,1,WST)
        ENDDO
	Write (LU, *) IAM()

        Write (LU, '(''   Buffer     = '', I5   )') BUF
        Write (LU, '(''   Comm       = '', X, I4)') HPRO(PRCSRC, BUF)
        Write (LU, '(''   TrCode     = '', I5   )') HPRO(TRCODE  , BUF)
        Write (LU, '(''   Term Nr    = '', I5   )') HPRO(TERNUM  , BUF)

        Write (LU, '(''   Line Nr    = '', I5)') HPRO(LINENO  , BUF)

        Write (LU, '(''   Dest.      = '', I5)') HPRO(X2X_DEST, BUF)
        Write (LU, '(''   Message    = '', I5)') HPRO(MSGNUM  , BUF)
        Write (LU, '(''   Length     = '', I5)') HPRO(INPLEN  , BUF)

        DO K=0, HPRO(INPLEN,BUF)-1, 20
          Write (LU, '(3X, I3, '':'', 20(X, Z2.2))') 
     .          K+1, (BPRO(BINPTAB+J, BUF), J=K, 
     .                MIN(K+19, HPRO(INPLEN, BUF)-1))
        ENDDO 

        Write (LU, *)

        CLOSE (LU)

	ST = LIB$FREE_LUN(LU)

	RETURN
	END
