C
C SUBROUTINE X2FREAD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FREAD.FOV                                  $
C  $Date::   17 Apr 1996 16:17:52                                         $
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
	SUBROUTINE X2FREAD(BUFFER,INDEX,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
	INCLUDE 'INCLIB:X2FLOCAL.DEF'				!V02
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 BUFFER(*), OFFSET, RECORD, BLOCK, STATUS, INDEX
	INTEGER*4 TMPBUF(X2FRSIZ*2)
	INTEGER*4 INDEX2, OFF2
C
	IF(INDEX.GT.P(X2XIDX).AND.X2XPRO_ROUND_ROBIN.EQ.0) THEN
	   STATUS=-1
	   RETURN
	ENDIF
C
	CALL X2BLKREC(INDEX,BLOCK,RECORD)

D	TYPE *,'INDEX,BLOCK,RECORD,X2FBUF(X2FBNUM)'
D	TYPE *,'X2FBUF (-1:0) ',X2FBUF(-1),X2FBUF(0),X2FBNUM
D	TYPE *,INDEX,BLOCK,RECORD,X2FBUF(X2FBNUM)

	IF(BLOCK.EQ.X2FBUF(X2FBNUM)) THEN
	   OFFSET=(RECORD-1)*X2FRSIZ+1    !WS 10/18/90
C***     OFFSET=(RECORD-1)*X2FRSIZ
	   CALL FASTMOV(X2FBUF(OFFSET),BUFFER,X2FRSIZ)
	   STATUS=0
	   RETURN
	ENDIF
C
 	INDEX2=INDEX-(INDEX/2)
        CALL READW(X2PBLKR,INDEX2,TMPBUF,STATUS)
        IF(STATUS.NE.0) THEN
C...           CALL OPS('**** X2X FILE READ ERROR ****',X2FRLUN,STATUS)
           RETURN
        ENDIF
        OFF2=MOD(INDEX-1,2)+1
        OFF2=(OFF2-1)*X2FRSIZ+1

D	TYPE *,'INDEX2,OFF2',INDEX2,OFF2

        CALL FASTMOV(TMPBUF(OFF2),BUFFER,X2FRSIZ)
C
	RETURN
	END
