C
C FUNCTION GIVADR.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GIVADR.FOV                                   $
C  $Date::   17 Apr 1996 13:25:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 25-APR-89 MBK BASED ON NETSUB2
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
	INTEGER FUNCTION GIVADR(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INTEGER*4 BASE, BUF
	DATA BASE/0/
C
	IF (BASE.EQ.0) THEN
	   CALL GETADR(LANBUF,BASE)
	ENDIF
	GIVADR=BASE+(BUF-1)*LANBTOT     !START OF THE BUFFER
C                                     ;DAVE'S DRV WANTS DATA ADR
C**X     TYPE 900,BASE,GIVADR,LANBTOT,BUF
C**X900  FORMAT(1X,2(1X,Z8),2I10)
	RETURN
	END
