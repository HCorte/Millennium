C
C Subroutine to find duplicated non-zero numbers. 
C
C Output - ST = 1, if duplicates found, otherwise 0
C
C V01 20-AUG-1997 UXN Initial release.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE BINGO_CHKDUP(ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
C
	INTEGER*4   ST
C
	INTEGER*4   WINNING_NUMBERS(BGONBR),I
C
C Initialize variables
C   
	CALL FASTSET(0,WINNING_NUMBERS,BGONBR)
C
	DO I=1,BGONBR
	  IF(DBNWIN(I).GT.0) THEN
	    IF(WINNING_NUMBERS(DBNWIN(I)).NE.0) THEN
	      ST = 1
	      RETURN
	    ELSE
	      WINNING_NUMBERS(DBNWIN(I)) = 1
	    ENDIF
	  ENDIF
	ENDDO
C
	ST = 0
	END	
