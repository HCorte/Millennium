C SUBROUTINE BINGBMAP.FOR 
C
C $Log:   GXAFXT:[GOLS]BINGBMAP.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:16:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   27 Oct 1994 17:05:18   HXK
C  Initial revision.
C  
C THIS SUBROUTINE WILL GENERATE BITMAPS SHOWING THE WINNING BITMAP AFTER EACH
C NUMBER IS DRAWN.
C
C I.E.	  WINNING NUMBERS: 5 2 7 - - - 75 3
C
C	BITMAP      1  2  3  4  5  6  7  - - - - 74 75
C	  1	    0  0  0  0  1  0  0           0  0
C	  2         0  1  0  0  1  0  0           0  0
C         3         0  1  0  0  1  0  1           0  0
C         :
C         :
C        74         1  1     1  1  1  1           1  1
C        75         1  1     1  1  1  1           1  1
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
	SUBROUTINE BINGBMAP(GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
C
	INTEGER*4 BNG_WRD
	PARAMETER(BNG_WRD = (BGONBR/32)+1)
C
	INTEGER*4 NUM, GIND      !Winning number Loop Variable
C
	CALL FASTSET(0,MATCHTAB,BNG_WRD*BGONBR)
C
	DO NUM = 1,BGONBR
	    CALL BSET(MATCHTAB(1,NUM),LBNWIN(NUM,GIND))
	    IF(NUM.LT.BGONBR) 
     *	       CALL FASTMOV(MATCHTAB(1,NUM),MATCHTAB(1,NUM+1),BNG_WRD)
	ENDDO
C
	RETURN
	END
