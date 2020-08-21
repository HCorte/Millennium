C
C SUBROUTINE GETFLG
C $Log:   GXAFXT:[GOLS]GETFLG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   02 Aug 1993 12:36:58   SXH
C  Swap bytes in BITS before testing bits
C  
C     Rev 1.3   30 Jul 1993 18:21:52   SXH
C  CORRECTLY SPELL BSTRNG
C  
C     Rev 1.2   30 Jul 1993 17:59:16   SXH
C  CALL TSTBIT_BSRTNRG
C  
C     Rev 1.1   21 Jul 1993 16:05:34   GXA
C  Changed ordering of bits. Bit 1 is the leftmost bit.
C  
C     Rev 1.0   21 Jan 1993 16:25:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - getflg.for **
C
C GETFLG.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-OCT-89 LOU R. INITIAL RELEASE FOR FINLAND
C
C SUBROUTINE TO CONVERT BITMAP OF FLAGS TO ARRAY OF FLAGS.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE GETFLG(BITS,TABLE,CNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

        ! arguments
        INTEGER*4  BITS                  ! QP array
	INTEGER*4  TABLE(32)             ! output table to show board QPs
        INTEGER*4  CNT                   ! no of boards

        ! variables
	INTEGER*4  I                     ! counter
        INTEGER*4  TEST                  ! local I4 which holds swapped bits
        INTEGER*4  EQBITS                ! I4 equivalenced with argument BITS

        BYTE       BBITS(4)              ! byte array equivalenced with EQBITS
        BYTE       BTEST(4)              ! byte array equivalenced with TEST


        EQUIVALENCE (EQBITS,BBITS)
        EQUIVALENCE (TEST,BTEST)

CCC for reasons I don't fully understand it is necessary to swap the
CCC bytes in TRABUF(TWQPF) (BITS) in order to test if the boards are QP 
CCC (bit set) or not QP (bit clear)

C        WRITE(5,100)BITS
C100     FORMAT(1X,'BITS = ',Z8.8)

        ! copy value of BITS into local EQBITS (and thus into BBITS)
        EQBITS = BITS      

        ! initialise test I4
        TEST = 0

        ! swap bytes in BITS for testing 
        BTEST(2) = BBITS(1)
        BTEST(1) = BBITS(2)

        ! test to see which bits are set in TEST (BITS)
	DO I=1,CNT
	    TABLE(I)=0
            IF (TSTBIT_BSTRNG(TEST, I-1)) TABLE(I)=1

        END DO

	RETURN

	END
