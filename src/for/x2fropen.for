C
C SUBROUTINE X2FROPEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FROPEN.FOV                                 $
C  $Date::   17 Apr 1996 16:18:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2xfile.for;1 **
C
C X2XFILE.FOR
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C X2X FILE ACCESS ROUTINES
C LOG RECORDS ARE 1 SECTOR LONG (64 FW, 256 B)
C
C CALL X2FROPEN
C CALL X2FRCLOSE
C CALL X2FREAD(BUFFER,INDEX,STATUS) - SAME AS ABOVE
C CALL X2BLKREC(INDEX,BLOCK,RECORD) - INT*4 INDEX (RECORD NUMBER)
C                                   - INT*4 BLOBK IT'S IN
C                                   - INT*4 RECORD NUM IN BLK
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
	SUBROUTINE X2FROPEN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
	INCLUDE 'INCLIB:X2FLOCAL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4  STATUS, NOCHECK0
C
	COMMON /NOCHECK0/ NOCHECK0
C
	NOCHECK0=-1
C
	CALL OPENW(X2FRLUN,SFNAMES(1,X2F),4,0,0,STATUS)
	IF(STATUS.NE.0) THEN
	   CALL OPS('**** X2X.FIL open error R **** ',X2F,STATUS)
C**      TYPE 900,(SFNAMES(I,X2F),I=1,5)
	ENDIF
C
C INIT TO READ IN ONE RECORD BLOCKS
C
	CALL IOINIT(X2PBLKR,X2FRLUN,2*256)
C
	RETURN
	END
