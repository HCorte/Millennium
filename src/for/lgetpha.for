C
C SUBROUTINE LGETPHA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LGETPHA.FOV                                  $
C  $Date::   17 Apr 1996 13:49:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lgetpha.for;1 **
C
C LGETPHA.FOR
C
C V02 22-NOV-95 WJK REMOVE FOUND BECAUSE IT IS NOT USED
C V01 06-MAY-91 MRM RELEASED FOR VAX
C
C GET THE ETHERNET ADDRESS FROM THE CONTROLLER.
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
	SUBROUTINE LGETPHA(LAN,SAP,REPLY)
	IMPLICIT NONE
C
	INCLUDE '($SSDEF)'
	INCLUDE '($IODEF)'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:NMADEF.DEF'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	RECORD /LN_IOSSTRUCT/    LOCAL_IOSB	!IO STATUS BLOCK
C
	INTEGER*4       DESCRIP_P2(2)		!DESCRIPTOR OF P2 PARAMETERS
	CHARACTER*5	ETHDEV		        !ETHERNET DEVICE NAME
	INTEGER*4	SAP,LAN			!SAP/LAN ID
	INTEGER*4	REPLY,I			!REPLY CODE
	INTEGER*4	IOSTAT, IFUNC		!STATUS/FUNC CODE
	BYTE            DEVCHAR(4000)		!CHAR BUFFER
	INTEGER*4	PARAM			!DEVICE PARAMETER
	INTEGER*4	POS			!BUFFER POSITION
CV02	LOGICAL		FOUND			!ADDRESS FOUND
	INTEGER*4	LEN			!LENGTH OF STRING
C
	DESCRIP_P2(1)=4000			!LENGTH OF PARAMETERS
	DESCRIP_P2(2)=%LOC(DEVCHAR)		!POINTER TO PARAMETERS
C
C READ THE CHARACTERISTICS OF THE ETHERNET DEVICE.
C
	IFUNC=IO$_SENSECHAR .OR. IO$M_CTRL
	IOSTAT=SYS$QIOW(,%VAL(LUNLAN(SAP,LAN)),
     *                   %VAL(IFUNC),
     *                   %REF(LOCAL_IOSB),,,,
     *                   %REF(DESCRIP_P2),,,,)
	IF(.NOT.IOSTAT) THEN
	   CALL LIB$SIGNAL(%VAL(IOSTAT))
	   GOTO 1000
	ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	   CALL LIB$SIGNAL(%VAL(LOCAL_IOSB.STAT))
	   GOTO 1000
	ENDIF
C
C SCAN THROUGH THE RETURNED INFORMATION FOR THE
C CONTROLLER ADDRESS.  NOTE: THERE ARE TWO WAYS THAT
C THE CONTROLLER WILL RETURN INFORMATION: LONGWORD FORMAT
C OR STRING FORMAT. SEE IO USERS MANUAL FOR DESCRIPTION.
C
	POS=1
100	CONTINUE
CV02	IF(.NOT.FOUND .AND. POS.LT.100) THEN
	IF(POS.LT.100) THEN					! V02
	  CALL NMOV2TOI4(PARAM,DEVCHAR,POS-1)
	  POS=POS+2
C
C CHECK THE BIT WHICH INDICATES WHETHER LONGWORD FORMAT
C OF STRING.  IF LONGWORD, INCREMENT POSITION POINTER AND SKIP.
C
	  IF(IAND(PARAM,'1000'X).EQ.0) THEN
	    POS=POS+4
C
C STRING DATA, CHECK PARAMETER TO SEE IF IT IS FOR THE
C PHYSICAL CONTROLLER ADDRESS.
C
	  ELSE
	    PARAM=IAND(PARAM,'0FFF'X)
	    CALL NMOV2TOI4(LEN,DEVCHAR,POS-1)
	    POS=POS+2
C
C STORE CONTROLLER ADDRESS INTO MEMORY.
C
	    IF(PARAM.EQ.NMA$C_PCLI_PHA) THEN
	      DO 200 I=1,LEN
	        LANHOME(I,LAN)=DEVCHAR(POS)
	        POS=POS+1
200	      CONTINUE
	    ELSE
	      POS=POS+LEN
	    ENDIF
	  ENDIF
	  GOTO 100
	ENDIF  
C
C PROGRAM EXIT.
C
1000	CONTINUE
	RETURN
	END
