C
C SUBROUTINE TMFRLN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]TMFRLN.FOV                                   $
C  $Date::   17 Apr 1996 15:35:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tmfrln.for **
C
C TMFRLN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C
C     TMFRLN(RECORD,LENGTH)
C     GET LENGTH OF TMF RECORD
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
	SUBROUTINE TMFRLN(LBUF,LENGTH)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INTEGER*4 LBUF(LMUREC), TYPE, LENGTH, CNT
C
	CNT=1
	TYPE=ISHFT(LBUF(LREC),-24)
	TYPE=IAND(TYPE,7)
	IF(TYPE.EQ.LONE) THEN
 	  TYPE=ISHFT(LBUF(LREC*2),-24)
	  TYPE=IAND(TYPE,7)
	  IF(TYPE.EQ.LEND) CNT=2
	  IF(TYPE.EQ.LTWO) CNT=3
	ENDIF
	LENGTH=LREC*CNT
	RETURN
	END
