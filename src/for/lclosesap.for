C
C SUBROUTINE LCLOSESAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LCLOSESAP.FOV                                $
C  $Date::   17 Apr 1996 13:48:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lclosesap.for;1 **
C
C LCLOSESAP.FOR
C
C V01 29-NOV-90 XXX RELEASED FOR VAX
C
C      INT*4 SAP    - IN
C      INT*4 REPLY - OUT
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
	SUBROUTINE LCLOSESAP(SAP,REPLY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:NMADEF.DEF'
	INCLUDE '($SSDEF)'
        INCLUDE '($IODEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 STATUS, REPLY, SAP, LAN, I
	INTEGER*4 IOSTAT
C
	RECORD /LN_IOSSTRUCT/    LOCAL_IOSB	!IO STATUS BLOCK
	INTEGER*4 IFUNC
C
C CHECK SAP
C
	REPLY=-1
	IF(SAP.GE.1.AND.SAP.LE.MAXSAP) THEN
C
C STOP THE ETHERNET DEVICE.
C
	   DO 100 LAN=1,MAXLAN
	     IF(LUNLAN(SAP,LAN).NE.0) THEN
	       IFUNC=IO$_SETMODE .OR. IO$M_CTRL .OR. IO$M_SHUTDOWN
	       IOSTAT=SYS$QIOW(,%VAL(LUNLAN(SAP,LAN)),
     *                          %VAL(IFUNC),
     *                          %REF(LOCAL_IOSB),,,,
     *                          ,,,,)
	       IF(.NOT.IOSTAT) THEN
	         CALL LIB$SIGNAL(%VAL(IOSTAT))
	       ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	         CALL LIB$SIGNAL(%VAL(LOCAL_IOSB.STAT))
	       ENDIF
	       CALL SYS$DASSGN(LUNLAN(SAP,LAN))
	       CALL SYS$DASSGN(LUNLAN(SAP,LAN))
C***	       LANOPN(SAP,LAN)=LSCLO
	       CALL OPS('CONNECTION CLOSED FOR LAN/SAP',LAN,SAP)
	     ENDIF
100	   CONTINUE
	   REPLY=0
	ELSE
	   CALL OPS('**** INVALID SAP ****',SAP,0)
	ENDIF
	RETURN
	END
