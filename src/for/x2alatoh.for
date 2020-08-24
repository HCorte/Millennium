C
C SUBROUTINE ATOH
C $Log:   GXAFXT:[GOLS]X2ALATOH.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:07:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:17:48   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.0   26 Jun 1994 14:48:28   HXK
C  Initial revision.
C  
C     Rev 1.2   31 Mar 1994 14:12:16   GPR
C  Change Copyright date
C  
C     Rev 1.1   10 Jan 1994 12:22:56   RXD
C  X2X baseline update for U.k. Gsat,async,dial,x.28.
C  
C     Rev 1.1   22 Apr 1993  8:40:00   DAS
C  Changed values of hi_wd / lo_wd.
C  
C     Rev 1.0   21 Jan 1993 15:40:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - atoh.for;1 **
C
C ATOH
C
C V02 08-JUN-94 SCD COPY UK'S CORRECTLY WORKING SUBROUTINE ATOH TO
C                   A NEW FILE NAME TO FIX X2ALLSTN SNAPSHOT'S ADDRESS 
C                   SORT (FINLAND'S RFSS 158)
C
C V01 17-NOV-92  MF Initial (Rewritten ASCBCD merged with ASCHEX)
C
C
C Routines to convert between an ASCII string and its HEX 
C representation in two Fullwords. Note that the first string character
C becomes the Most Significant (BCD) Nibble.
C 
C  ASCSTR[1:length]              INTSTR[2]          INTSTR[2]
C  ---------------------         -----------------  ----------------
C  |x| | | | |z| | | | |   <=>   |0 0|0 0|x  |   |  |   |   |  |  z|
C  ---------------------         --.---.---.---.--  --.---.---.--.--
C  <--- (16 max)    -->                 high          low        
C
C NOTE:  This source file includes ATOH, HTOA and  NHTOA 
C        NHTOA accepts "left-justified" quadword input.
C        X2QSHFT implements "quadword" shift of two I*4
C
C
C The ATOH subroutine will convert a string from ASCII to HEX
C
C Calling sequence:
C
C     CALL ATOH(ASCSTR,START,LENGTH,INTSTR,ERR)
C
C Input parameters:
C
C     ASCSTR      Char*1(*)   String to be converted
C     START       Int*4       Start position of ASCSTR
C     LENGTH      Int*4       Length of string to convert
C
C Output parameters:
C
C     INTSTR      Int*4(2)    Binary Coded Decimal string
C     ERR         Int*4       Return error
C                             -1 = invalid string
C                             -2 = string too long to convert
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
	SUBROUTINE X2ALATOH(ASCSTR,START,LENGTH,INTSTR,ERR)   !V02
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*1 ASCSTR(*)               !ASCII string
	INTEGER*4   INTSTR(2)               !Output BCD value
	INTEGER*4   START                   !Start position into string
	INTEGER*4   LENGTH                  !Length to convert
	INTEGER*4   ERR                     !Conversion error
	INTEGER*4   VALUE                   !Work variables
	INTEGER*4   I,K                     !Work variables
C
        INTEGER*4   LO_WD,HI_WD            
        PARAMETER  (LO_WD=2,HI_WD=1)       
C
       IF(LENGTH.LT.0 .OR. LENGTH.GT.16) THEN
          ERR=-2
          GO TO 8000
       ENDIF  
C
C INITIALIZE VARIABLES.
C
	INTSTR(1)=0
	INTSTR(2)=0
	ERR=0
C
	DO 60 I=START,START+LENGTH-1
          VALUE = ICHAR(ASCSTR(I)) 
          IF     (VALUE.GE.ICHAR('0') .AND. VALUE.LE.ICHAR('9')) THEN
            VALUE = VALUE - ICHAR('0')
          ELSEIF (VALUE.GE.ICHAR('A') .AND. VALUE.LE.ICHAR('F')) THEN
            VALUE = VALUE - ICHAR('A') + 10
          ELSEIF (VALUE.GE.ICHAR('a') .AND. VALUE.LE.ICHAR('f')) THEN
            VALUE = VALUE - ICHAR('a') + 10
	  ELSE 
	    ERR=-1
	    GOTO 8000
	  ENDIF
C
          K=START+LENGTH-I
          IF (K .GT. 8) THEN
            INTSTR(HI_WD) =  INTSTR(HI_WD) + ISHFT(VALUE,(K-8-1)*4)
          ELSE
            INTSTR(LO_WD) =  INTSTR(LO_WD) + ISHFT(VALUE,(K-1)*4)
          ENDIF
60      CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
