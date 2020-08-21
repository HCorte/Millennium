C
C *** SUBROUTINE MSCWRITEW ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCWRITEW.FOV                                $
C  $Date::   17 Apr 1996 14:07:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - mscwritew.for ***
C
C V01 25-JAN-90 RRB VAX INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	ISSUE WRITE TO THE TELENEX MATRIX SWITCH CONTROLLER.
C
C Calling Sequence:
C		CALL MSCWRITEW(MSCWRITEBUF, LEN, ST)
C
C Input:	MSCWRITEBUF - DATA TO WRITE
C		LEN         - LENGTH OF DATA
C
C Output:	ST          - STATUS OF THE OPERATION
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCWRITEW(MSCWRITEBUF,LEN,ST)
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
	BYTE          MSCWRITEBUF(*)
	INTEGER*4     LEN
	INTEGER*4     ST
C
	INTEGER*4     WRITEFUNCOD               !WRITE FUNCTION CODE
	PARAMETER    (WRITEFUNCOD= IO$_WRITEVBLK + IO$M_CANCTRLO +
     *                             IO$M_NOFORMAT + IO$M_BREAKTHRU)
C
C INITIATE WRITE
C
D	TYPE*,IAM(),'LAT CHANNEL ',LAT_CHANNEL,' BUFFER LENGTH ',LEN
	ST = SYS$QIOW ( , %VAL(LAT_CHANNEL), %VAL(WRITEFUNCOD),
     *                   MSC_WRITEIOSB,,,
     *                   %REF(MSCWRITEBUF), %VAL(LEN),,
     *                   %VAL(0),,)
	IF(ST.NE.SS$_NORMAL) THEN
	   CALL LIB$SIGNAL(%VAL(ST))
	ENDIF
C
        IF(MSC_WRITEIOSB.STAT.NE.SS$_NORMAL) THEN
           IF(MSC_WRITEIOSB.STAT.EQ.SS$_HANGUP) THEN
              CALL OPS('SERVER CONNECTION LOST ',0,0)
              MSC_CONNECT_FLAG = MSC_CLOSED
           ELSE IF(MSC_WRITEIOSB.STAT.EQ.SS$_ABORT) THEN
              MSC_CONNECT_FLAG = MSC_CLOSED
 	   ELSE
	      CALL LIB$SIGNAL(%VAL(MSC_WRITEIOSB.STAT))
           ENDIF
           ST = MSC_WRITEIOSB.STAT
        ENDIF
C
	RETURN
	END
