C
C Subroutine to count the numbers in LOTTO reduced system bet.
C
C 03-SEP-97 UXN Initial release.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE REDUCED_SYSTEM_COUNTER(IND,SYSNR,COUNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
C
	INTEGER*4   SYSNR,IND,COUNT
	INTEGER*4   POINTER,I,OFFSET
	INTEGER*4   BOARD(LMXMARK)
	STATIC	    BOARD
C
C INITIALIZE 
C
	ENTRY REDUCED_SYSTEM_INIT(SYSNR)
	POINTER = LSYS_PTR(SYSNR)
C
	CALL FASTSET(0,BOARD,LMXMARK)
	DO I=1,LSYS_NUMBET(SYSNR)
	    DO OFFSET=1,LMXMARK
	      IF(TSBIT(LSYS_TAB(POINTER+(I-1)*2+1),OFFSET-1)) THEN
		BOARD(OFFSET) = BOARD(OFFSET) + 1
	      ENDIF
	    ENDDO
	 ENDDO
	 RETURN
C
C ENTRY TO SET HOW MANY COMBINATIONS ARE WITH GIVEN INDEX.
C
	ENTRY REDUCED_SYSTEM(IND,COUNT)
	COUNT = BOARD(IND)
	RETURN	
	END
