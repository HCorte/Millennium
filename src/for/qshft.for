C
C SUBROUTINE QSHFT
C $Log:   GXAFXT:[GOLS]QSHFT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:24:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - atoh.for;1 **
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C QSHFT.FOR  (Future: replace by X2QSHFT, add to LNK files)
C
C Shift bits of a quadword specified as two integers.
C Follows ISHFT conventions: "+" for left shift
C
C Calling sequence:
C
C     CALL QSHFT(HI_WD,LO_WD,SHIFT)
C
C Input parameters:
C
C     HI_WD       Int*4       Most  Significant word
C     LO_WD       Int*4       Least Significant word
C     SHIFT       Int*4       Number of bits to shift
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE QSHFT(HI_WD,LO_WD,SHIFT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C       
        INTEGER*4  HI_WD,LO_WD       ! High-,low-fullwords
        INTEGER*4  SHIFT             ! bits to shift (signed)
C
        INTEGER*4  I,SI              ! bit-shift number and direction
        INTEGER*4  CARRY,K           ! work bit-field
C
      IF (SHIFT.EQ.0) RETURN         ! Silly NOP
C
      I =IABS(SHIFT)
      IF (SHIFT.GT.0)THEN
          SI=1
      ELSE
          SI=0
      ENDIF
C
      IF(I.GT.64) THEN
         HI_WD=0
         LO_WD=0
      ELSEIF (I.GE.32) THEN       
         IF(SI.GT.0) THEN
           HI_WD=ISHFT(LO_WD,I-32)
           LO_WD=0
         ELSE
           LO_WD=ISHFT(HI_WD,32-I)
           HI_WD=0
         ENDIF
      ELSE
         K=32-I
         IF(SI.GT.0) THEN
           CARRY= ISHFT(LO_WD,-K)
           LO_WD= ISHFT(LO_WD,I)
           HI_WD= IOR(ISHFT(HI_WD,I),CARRY)
         ELSE
           CARRY= ISHFT(HI_WD,K)
           LO_WD= IOR(ISHFT(LO_WD,-I),CARRY)
           HI_WD= ISHFT(HI_WD,-I)
         ENDIF
      ENDIF
C
      RETURN
      END
