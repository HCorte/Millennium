C
C SUBROUTINE LAT_OPEN
C $Log:   GXAFXT:[GOLS]LAT_OPEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:48:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:49:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - latsubs.for **
C
C LATSUBS.FOR
C
C V01 15-JAN-91 KWP  INITIAL RELEASE
C
C
C SET OF TWO SUBROUTINES THAT ARE USED TO OPEN AND CLOSE CONNECTIONS
C TO A DEC SERVER VIA 'LAT' PROTOCOL. ONCE THE CONNECTION IS OPEN
C REGULAR QIO OR QIOW CAN BE USED TO READ AND WRITE TO THE DEVICE.
C
C
C LAT_OPEN    ASSIGNS A CHANNEL TO A LOGICAL LAT PORT ON THE VAX.
C	      OPENS A CONNECTION TO A PORT ON THE DEC SERVER.
C
C LAT_CLOSE   CLOSES THE CONNECTION TO THE DEC SERVER.
C	      DE-ASSIGNS THE CHANNEL.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LAT_OPEN(LAT_NAME, LAT_CHANNEL, ST)
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
	CHARACTER*5	LAT_NAME
	INTEGER*4	LAT_CHANNEL
	INTEGER*4	ST
C
C ASSIGN A CHANNEL TO THE TERMINAL SERVER
C
D	TYPE *,'ATTACHING TO A CHANNEL'
        ST = SYS$ASSIGN(LAT_NAME,LAT_CHANNEL,,)
        IF(.NOT.ST) THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 10000
	ENDIF
C
C TRY TO CONNECT TO THE TERMINAL SERVER
C
C
D	TYPE *,'CONNECTING TO TERMINAL SERVER'
        ST = SYS$ QIOW (,%VAL(LAT_CHANNEL),
     *		         %VAL(CONNECT_FUNCOD),
     *                   LOCAL_IOSB,,,,,,,,)
	IF(ST.NE.SS$_NORMAL) THEN
	   CALL LIB$SIGNAL(%VAL(ST))
	ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	   CALL LIB$SIGNAL(%VAL(LOCAL_IOSB.STAT))
	   ST=LOCAL_IOSB.STAT
	ENDIF
C
10000	CONTINUE
	RETURN
	END
