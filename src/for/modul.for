C
C SUBROUTINE MODUL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MODUL.FOV                                    $
C  $Date::   17 Apr 1996 14:03:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - modul.for **
C
C MODUL.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     SUBROUTINE TO FIND OUT IF THERE EXISTS A LOGICAL
C     UNIT OPEN AND DEVICE TO A GIVEN SYSTEM
C     ALSO SYS HAS TO HAVE GOOD NETSTAT AND NETROU SET
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
	SUBROUTINE MODUL(SYS,WAY,RET)
C
C     RET=  0  DEV & LUN EXIST AND GOOD MODES
C     RET=  1  ONE OF THEM DOESN'T EXIST OR BAD MODES
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 UNIT, UNEX, DEVEX, RET, WAY, SYS
C
	DEVEX=0
	UNEX=0
C
D	TYPE*,'OPNNET NETDEV ',OPNNET,NETDEV
C
	DO 10 UNIT=1,NETLUN
C
	IF(OPNNET(UNIT,WAY).NE.SYS) GOTO 11
C
	UNEX=1
C
11	IF(NETDEV(1,UNIT).EQ.0) GOTO 10
C
	DEVEX=1
C
10	CONTINUE
C
D	TYPE*,'NETSTAT NETROUT ',NETSTAT,NETROUT
C
	IF(   NETROUT(SYS,WAY).LT.ROUIDLE.OR.
     *	   DEVEX.EQ.0.OR.
     *	   UNEX.EQ.0)     THEN
C
	   RET=1
C
	ELSE
C
	   RET=0
C
	ENDIF
C
	RETURN
	END
