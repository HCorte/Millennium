C
C SUBROUTINE X2QSHFT.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2QSHFT.FOV                                  $
C  $Date::   17 Apr 1996 16:27:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 00-DEC-92  MF Pass Qwd [I*2(2)] as a single arg
C V01 00-NOV-92  MF Original
C
C Shift bits of a quadword specified as two integers.
C Follows ISHFT conventions: "+" for left shift
C
C Calling sequence:
C
C     CALL X2QSHFT(QVAL,SHIFT)
C
C Input parameters:
C
C     QVAL        Int*4(2)    Quad-word
C     SHIFT       Int*4       Number of bits to shift
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE X2QSHFT(QVAL,SHIFT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C       
        INTEGER*4  QVAL(2)           ! Quad-word
        INTEGER*4  SHIFT             ! Number of bits to shift (signed)
C
        INTEGER*4  I,SI              ! bit-shift number and direction
        INTEGER*4  CARRY,K           ! work bit-field
C
        INTEGER*4   LO_WD,HI_WD            
        PARAMETER  (HI_WD=1,LO_WD=2) ! hi/lo order

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
         QVAL(HI_WD)=0
         QVAL(LO_WD)=0
      ELSEIF (I.GE.32) THEN       
         IF(SI.GT.0) THEN
           QVAL(HI_WD)=ISHFT(QVAL(LO_WD),I-32)
           QVAL(LO_WD)=0
         ELSE
           QVAL(LO_WD)=ISHFT(QVAL(HI_WD),32-I)
           QVAL(HI_WD)=0
         ENDIF
      ELSE
         K=32-I
         IF(SI.GT.0) THEN
           CARRY= ISHFT(QVAL(LO_WD),-K)
           QVAL(LO_WD)= ISHFT(QVAL(LO_WD),I)
           QVAL(HI_WD)= IOR(ISHFT(QVAL(HI_WD),I),CARRY)
         ELSE
           CARRY= ISHFT(QVAL(HI_WD),K)
           QVAL(LO_WD)= IOR(ISHFT(QVAL(LO_WD),-I),CARRY)
           QVAL(HI_WD)= ISHFT(QVAL(HI_WD),-I)
         ENDIF
      ENDIF
C
      RETURN
      END

