C
C SUBROUTINE X2FWRITE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FWRITE.FOV                                 $
C  $Date::   17 Apr 1996 16:18:16                                         $
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
C V02 JWE 16-FEB-94 JWE Change DMPHEX parameters
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
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
	SUBROUTINE X2FWRITE(BUFFER,INDEX,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
	INCLUDE 'INCLIB:X2FLOCAL.DEF'				!V02
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 BUFFER(*), DUMMY, X2FHIBW, OFFSET, RECORD
	INTEGER*4 BLOCK, STATUS, INDEX
C
	CALL X2BLKREC(INDEX,BLOCK,RECORD)
C
100	CONTINUE
	IF(BLOCK.EQ.X2FBUF(X2FBNUM)) THEN
	   OFFSET=(RECORD-1)*X2FRSIZ+1
D	   TYPE*,'COPY: IDX BLK REC P()',INDEX,BLOCK,RECORD,P(X2XIDX)
	   CALL FASTMOV(BUFFER,X2FBUF(OFFSET),X2FRSIZ)
D	   TYPE*,'WRIITEN TO OFFSET (SIZE) ',OFFSET,X2FRSIZ
D	   CALL DMPHEX(X2FBUF(OFFSET),X2FRSIZ*4)
	   STATUS=0
	   X2FBUF(X2FBSTA)=X2FSDTY
	   RETURN
	ENDIF
C
	IF(X2FBUF(X2FBSTA).EQ.X2FSDTY) THEN
	   CALL WRITEW(X2PBLKW,X2FBUF(X2FBNUM),X2FBUF(X2FBDTA),STATUS)
	   IF(STATUS.NE.0) THEN
	      CALL OPS('**** X2X FILE READ ERROR ****',X2FRLUN,STATUS)
	      RETURN
	   ENDIF
	ENDIF
C
	IF(P(X2XIDX).GT.0) THEN
	   CALL X2BLKREC(P(X2XIDX),X2FHIBW,DUMMY)
	ELSE
	   X2FHIBW=999999999
	ENDIF
C
C
	IF(BLOCK.LE.X2FHIBW) THEN
	   CALL READW(X2PBLKW,BLOCK,X2FBUF(X2FBDTA),STATUS)
	   IF(STATUS.NE.0) THEN
	      CALL OPS('**** X2X FILE READ ERROR ****',X2FRLUN,STATUS)
	      RETURN
	   ENDIF
D      TYPE*,'READ: BLK HIB IDX P() ',BLOCK,X2FHIBW,INDEX,P(X2XIDX)	!V02
	ELSE
	   CALL FASTSET(0,X2FBUF,X2FRSIZ*X2FBSIZ+2) !2=HDR SIZE
D      TYPE*,'INIT: BLK HIB IDX P() ',BLOCK,X2FHIBW,INDEX,P(X2XIDX)	!V02
	ENDIF
C
	X2FBUF(X2FBSTA)=X2FSCLN
	X2FBUF(X2FBNUM)=BLOCK
C
	GOTO 100
	END
