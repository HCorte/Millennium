C
C FUNCTION CHKVAL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CHKVAL.FOV                                   $
C  $Date::   17 Apr 1996 12:34:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkval.for **
C
C CHKVAL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 24-OCT-89 MBK ORIGINAL RELEASE
C
C VERIFY VALUE TO FALL INTO A RANGE
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
	INTEGER FUNCTION CHKVAL(VALUE,LOWLIM,UPLIM,TEXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 DISVAL, BCHAR1, UPLIM, LOWLIM, VALUE
C
	CHARACTER *10  TEXT
C
	CHARACTER *10  BCHAR10(4)
	CHARACTER *40  BCHAR40
	EQUIVALENCE (BCHAR1,BCHAR10,BCHAR40)
	DATA BCHAR10 /'**** ILLEG','AL VALUE -','          ',
     *	              '      ****'/
C
	CHKVAL=0
	DISVAL=0
	IF(VALUE.LT.LOWLIM) THEN
	   DISVAL=LOWLIM
	   CHKVAL=-1
	ELSEIF(VALUE.GT.UPLIM) THEN
	   DISVAL=UPLIM
	   CHKVAL=-1
	ENDIF
C
	IF(CHKVAL.NE.0) THEN
	   BCHAR10(3)=TEXT
	   CALL OPS(BCHAR40,DISVAL,VALUE)
	ENDIF
C
	RETURN
	END
