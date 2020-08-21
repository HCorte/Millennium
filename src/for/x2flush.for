C
C SUBROUTINE X2FLUSH
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FLUSH.FOV                                  $
C  $Date::   17 Apr 1996 16:17:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xfile.for;1 **
C
C V03 05-DEC-94 SCD Integrate UK changes into X2X Baseline
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
	SUBROUTINE X2FLUSH
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
	INCLUDE 'INCLIB:X2FLOCAL.DEF'				!V03
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 STATUS, BLOCK
C
	BLOCK=X2FBUF(X2FBNUM)
	IF(X2FBUF(X2FBSTA).EQ.X2FSDTY) THEN
C***     CALL OPS("**** X2X FLUSH ****",P(X2XIDX),BLOCK)
	   CALL WRITEW(X2PBLKW,BLOCK,X2FBUF(X2FBDTA),STATUS)
	   IF(STATUS.NE.0) THEN
	      CALL OPS('**** X2X FILE WRITE ERROR ****',X2FWLUN,STATUS)
	      RETURN
	   ENDIF
	ENDIF
	X2FBUF(X2FBSTA)=X2FSCLN
	RETURN
	END
