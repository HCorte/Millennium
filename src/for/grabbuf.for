C
C SUBROUTINE GRABBUF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GRABBUF.FOV                                  $
C  $Date::   17 Apr 1996 13:27:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C     SUBROUTINE GRABBUF(BUF,WAY,ST);  GET BUFFER
C     OUT - BUF - BUFFER #
C
C           ST  - SAME AS RTL AND RBL
C
C      IN  -  CURRENT WAY
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
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
	SUBROUTINE GRABBUF(BUF,WAY,ST)
	IMPLICIT NONE
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 ADR/0/, ST, WAY, BUF
C
	CALL RTL(BUF,NETFREE(1,WAY),ST)
	IF (ST.EQ.2.AND.(NETCMDFRZ.EQ.0.OR.WAY.NE.RECOVWAY)) RETURN
	IF (ST.NE.2) THEN
C**	   CALL TONR(BUF,ADR)
	ELSE
	   CALL RTL(BUF,RECOVQUE,ST)
	   IF (ST.EQ.2) RETURN
	ENDIF
C
	IF (BUF.LE.0.OR.BUF.GT.NETNUM) THEN
	  TYPE 900,IAM(),CHAR(7),ADR,BUF
900	  FORMAT(1X,2A,'invalid buffer ',Z8,' nr ',I4)
C
	  ST=2
	  RETURN
	ENDIF
	CALL SETTAB(0,NETBUF(NCNLEN+1,BUF),HDRSIZ-NCNLEN)
	NETBUF(PPORG,BUF)=NODEID
	NETBUF(NEXT,BUF)=HDRSIZ+1
	NETBUF(WAYNR,BUF)=WAY
C
D	TYPE *,'buffer grabbed ',BUF,' way ',WAY
	RETURN
	END
