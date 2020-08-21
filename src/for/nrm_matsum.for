C
C SUBROUTINE MATSUM
C $Log:   GXAFXT:[GOLS]MATSUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:00:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:58:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_matsum.for **
C
C MATSUM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C *** MATSUM.FTN ***
C B01.00 10-OCT-85 TKO  INITIAL RELEASE
C
C THIS ROUTINE WILL FACILITATE ADDITION OF ARRAY VALUES
C
C CALL AS FOLLOWS:
C
C CALL MATSUM(TOTAL,ARRAY,NUMENT,INC)
C
C INPUT:
C      ARRAY - ARRAY OF ENTRIES, ANY NUMBER OF INTEGER*4S
C      NUMENT- # OF ENTRIES TO SUM
C      INC   - INCREMENT TO ADD FOR EACH VALUE
C
C OUTPUT:
C      TOTAL - TOTAL OF ALL AMOUNTS
C
C
C FOR EXAMPLE, TO ADD THE SALES COUNT FOR ALL GAMES IN AGTCOM, CALL
C AS FOLLOWS:
C
C     CALL MATSUM(TOTCNT,AGTTAB(G1SCNT,TER),NUMGAM,G2SCNT-G1SCNT)
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
	SUBROUTINE MATSUM(TOTAL,ARRAY,NUMENT,INC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 TOTAL
	INTEGER*4 ARRAY(*)
	INTEGER*4 NUMENT
	INTEGER*4 INC, K, OFF
C
C
	TOTAL=0
	OFF=1
	DO 1100 K=1,NUMENT
	  TOTAL=TOTAL+ARRAY(OFF)
	  OFF=OFF+INC
1100	CONTINUE
C
	RETURN
	END
