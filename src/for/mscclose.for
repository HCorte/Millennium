C
C *** SUBROUTINE MSCCLOSE ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCCLOSE.FOV                                 $
C  $Date::   17 Apr 1996 14:05:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_latsubs.for ***
C
C V01 06-JUN-91 RRB  INITIAL VAX RELEASE FOR MARYLAND
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
C	SUBROUTINE THAT IS USED TO CLOSE THE CONNECTION TO A DEC SERVER,
C	VIA 'LAT' PROTOCOL, FOR USE WITH THE TELENEX MATRIX SWITCH CONTROL
C	SUBSYSTEM. ONCE THE CONNECTION IS OPEN, REGULAR QIO OR QIOW CAN BE
C	USED TO READ AND WRITE TO THE DEVICE.
C
C	MSCCLOSE - CLOSES THE CONNECTION TO THE DEC SERVER.
C	           DE-ASSIGNS THE CHANNEL.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCCLOSE(LAT_CHANNEL, ST)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
C DEFINE STRUCTURES TO MAINTAIN IO STATUS BLOCKS AND BUFFER
C NUMBERS.
C
	STRUCTURE /LT_IOSSTRUCT/
          INTEGER*2 STAT                          !VMS STATUS
          INTEGER*2 XSIZE                         !TRANSFER SIZE
          INTEGER*4 PARM                          !PARAMETER/BUFFER ADDRESS
        END STRUCTURE
C
	RECORD /LT_IOSSTRUCT/    LOCAL_IOSB	!IO STATUS BLOCK
C
        INTEGER*4    DISCON_FUNCOD
        PARAMETER   (DISCON_FUNCOD = IO$_TTY_PORT +
     *                               IO$M_LT_DISCON)
C
	INTEGER*4	LAT_CHANNEL
	INTEGER*4	ST
C
C
C
D	TYPE *,'DISCONNECTING THE TERMINAL SERVER'
        ST = SYS$ QIOW (,%VAL(LAT_CHANNEL),
     *		        %VAL(DISCON_FUNCOD),
     *                  LOCAL_IOSB,,,,,,,,)
C
	IF(ST.NE.SS$_NORMAL) GOTO 10000
C
	IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	   ST=LOCAL_IOSB.STAT
	   GOTO 10000
	ENDIF
C
C DEASSIGN THE CHANNEL
C
D	TYPE *,'DEASSIGNING THE CHANNEL'
        ST = SYS$CANCEL(%VAL(LAT_CHANNEL))
        ST = SYS$DASSGN(%VAL(LAT_CHANNEL))
C
10000	CONTINUE
	LAT_CHANNEL = 0
	IF(ST.EQ.SS$_NORMAL) ST = 0
	RETURN
	END
