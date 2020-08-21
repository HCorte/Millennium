C
C SUBROUTINE NHTOA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NHTOA.FOV                                    $
C  $Date::   17 Apr 1996 14:12:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - atoh.for;1 **
C
C
C ===============================================================
C NHTOA.FTN
C
C NOTE: CONVERTS  BCD ADDRESS IN NETWORK  FORMAT TO ASCII FORMAT
C       NETWORK FORMAT IS ALWAYS LEFT JUSTIFIED. HOST IS RIGHT JUSTIFIED.
C       ON DEC:
C 
C
C Calling sequence:
C
C     CALL NHTOA(ASCSTR,START,LENGTH,INTSTR,ERR)
C
C Input parameters:
C
C     INTSTR      Int*4(2)    Binary Coded Decimal string 
C     START       Int*4       Starting BCD digit (2 digits per byte)
C     NUMCHR      Int*4       Number of BCD digits to convert
C
C Output parameters:
C
C     ASCSTR      Char*1(*)   ASC string of converted BCD
C     ERR         Int*4       Return error
C                             -1 = invalid string
C                             -2 = invalid length
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE NHTOA(ASCSTR,START,NUMCHR,INTSTR,ERR)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        CHARACTER   ASCSTR(*)*1             !ASCII string
        INTEGER*4   START                   !Start position into string
        INTEGER*4   NUMCHR                  !Length to convert
        INTEGER*4   LENGTH                  !Length to convert (Work Variable)
        INTEGER*4   INTSTR(2)               !Input BCD value
        INTEGER*4   QVAL(2)                 !Work BCD value
        INTEGER*4   ERR                     !Conversion error
C
        INTEGER*4   LO_WD,HI_WD            
        PARAMETER  (LO_WD=2,HI_WD=1)       ! A hint for Concurrent
C
C       INITIALIZE VARIABLES.
C
        ERR=0
C
C       VALIDATE LENGTH 
C
        IF(NUMCHR.GT.16 .OR. NUMCHR.LE.0) THEN
          ERR=-2
          GOTO 8000
        ENDIF
C
        LENGTH = (NUMCHR+1)/2              !Number of bytes
C
C       PLACE IN FORMAT THAT IS KNOWN TO THE HOST (DEC)
C
        IF(LENGTH .LE. 4) THEN
          CALL MOVBYTN(INTSTR(HI_WD),1,QVAL(HI_WD),1,LENGTH)
        ELSE
          CALL MOVBYTN(INTSTR(HI_WD),1,QVAL(HI_WD),1,4)
          CALL MOVBYTN(INTSTR(LO_WD),1,QVAL(LO_WD),1,LENGTH-4)
        ENDIF
        CALL X2QSHFT(QVAL,-(64-NUMCHR*4))    ! right-justify
C
C       TRANSLATE HEX FORMAT TO ASCII
C
        CALL HTOA(ASCSTR,START,NUMCHR,QVAL,ERR)
C
C       PROGRAM EXIT.
C
8000    CONTINUE
        RETURN
        END
