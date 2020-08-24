C
C *** SUBROUTINE MSCOPEN ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSCOPEN.FOV                                  $
C  $Date::   17 Apr 1996 14:06:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_latsubs.for ***
C
C V02 07-MAR-95 SCD  INCREASE SIZE OF LAT_NAME FROM 5 TO 8 CHARACTERS (DC)
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
C	SUBROUTINE THAT IS USED TO OPEN THE CONNECTION TO A DEC SERVER,
C	VIA 'LAT' PROTOCOL, FOR USE WITH THE TELENEX MATRIX SWITCH CONTROL
C	SUBSYSTEM. ONCE THE CONNECTION IS OPEN, REGULAR QIO OR QIOW CAN BE
C	USED TO READ AND WRITE TO THE DEVICE.
C
C	MSCOPEN  - ASSIGNS A CHANNEL TO A LOGICAL LAT PORT ON THE VAX.
C	           OPENS A CONNECTION TO A PORT ON THE DEC SERVER.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCOPEN(LAT_NAME, LAT_CHANNEL, ST)
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
        INTEGER*4    CONNECT_FUNCOD
        PARAMETER   (CONNECT_FUNCOD = IO$_TTY_PORT +
     *                                IO$M_LT_CONNECT)
C
	CHARACTER*8	LAT_NAME		  ! V02
	INTEGER*4	LAT_CHANNEL
	INTEGER*4	ST
C
C ASSIGN A CHANNEL TO THE TERMINAL SERVER
C
D	TYPE *,'ATTACHING TO A CHANNEL'
        ST = SYS$ASSIGN(LAT_NAME,LAT_CHANNEL,,)
        IF(.NOT.ST) GOTO 10000
C
C TRY TO CONNECT TO THE TERMINAL SERVER
C
C
D	TYPE *,'CONNECTING TO TERMINAL SERVER'
        ST = SYS$ QIOW (,%VAL(LAT_CHANNEL),
     *		         %VAL(CONNECT_FUNCOD),
     *                   LOCAL_IOSB,,,,,,,,)
C
	IF(ST.NE.SS$_NORMAL) GOTO 10000
C
	IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
           ST = LOCAL_IOSB.STAT
	ELSE
	   ST = 0
	ENDIF
C
10000	CONTINUE
	RETURN
	END
