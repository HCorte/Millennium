C
C SUBROUTINE WIMG
C $Log:   GXAFXT:[GOLS]WIMG.FOV  $
C
C V02 11-FEB-22 SCML Wrapper for command procedure invocation use unit=6
C                    instead of unit=5 that is for READ/SYS$INPUT purposes
C  
C     Rev 1.0   17 Apr 1996 15:59:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:05:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_wimg.for **
C
C VAX_WIMG.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C WIMG.FOR
C
C V01 09-JUL-90 TKO  RELEASED FOR VAX
C
C This will output a string without a carriage return
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
        SUBROUTINE WIMG(LUN, STRING)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C        INCLUDE '(LIB$ROUTINES)'

C
        INTEGER*4   LUN
        CHARACTER   STRING*(*)
C Validate if symbol flag exists if it does then
C subroutine is being called from script scope wrapper that invokes dynamicaly
C a group of executables    
C        INTEGER*4   SYMBOL_STATUS
C        CHARACTER*10 SYMBOL_VALUE

C        IF(LIB$GET_SYMBOL('script_origin',SYMBOL_VALUE)) THEN
C          WRITE(6, 1000) IAM(),STRING
C        ELSE
          WRITE(LUN, 1001) IAM(),STRING
C        ENDIF 

C1000    FORMAT(' ',A,A,' >')        
1001    FORMAT(' ',A,A,' >',$)
C
        RETURN
        END
