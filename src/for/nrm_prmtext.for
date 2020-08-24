C PRMTEXT.FOR
C
C V06 08-FEB-2000 UXN Alpha changes.
C V05 13-JAN-1999 GLS INITIAL RELEASE FOR FINLAND (FROM POLAND)
C V04 08-JUL-1996 SLK CHANGED RE-PROMPTING PERIOD TO 5 MINUTES
C V03 08-MAY-1996 GLS ADDED AUTO RE-PROMPTING + DOUBLE WIDTH PRINTING 
C V02 20-SEP-1991 KWP CHANGES RE MIKE PINDRIK
C V01 02-APR-1991 TKO INITIAL RELEASE
C
C This program is used to output a message and wait for a CHARACTER STRING
C response.
C It will do the following:
C
C 1)	If the parent process is in DCL mode, it will create a mailbox and
C	wait for mail containing the string.
C
C 2)	If the parent process is not in DCL mode, it will simply prompt
C	for the character string.
C
C Calling sequence:
C
C	CALL PRMTEXT(OUTPUTSTRING, INPUTSTRING, INPUTLEN)
C
C INPUT:
C	OUTPUTSTRING	This is the text string to output as a prompt.  It
C			must be a character string whose length can be
C			determined by the LEN(X) function.
C
C OUTPUT:
C	INPUTSTRING	This is the string as typed by the user.  It must be
C			a character string whose length can be determined by
C			the LEN(X) function (which will be the maximum length
C			of input).
C	INPUTLEN	This is the actual length of the input string.
C			(INPUTSTRING will be padded with blanks at the end
C		        to fill it up).  Note that in local prompt mode, if
C			the user types return only, INPUTLEN will be set to
C			ZERO and INPUTSTRING will contain all blanks.
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	SUBROUTINE PRMTEXT(OUTPUTSTRING, INPUTSTRING, INPUTLEN)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
        INCLUDE     '($SSDEF)'                                       !V03
C
	CHARACTER   OUTPUTSTRING*(*)
	CHARACTER   INPUTSTRING* (*)
	INTEGER*4   INPUTLEN
C
	STRUCTURE   /JPISTRUC/
	    INTEGER*2	BUFLEN
	    INTEGER*2	ITMCOD
	    INTEGER*4	BUFADR
	    INTEGER*4	LENADR
	END STRUCTURE
C
	RECORD	    /JPISTRUC/ ITEMLIST(10)
C
	INTEGER*4   MASTER_PID
	CHARACTER   MASTER_NAME*60
	INTEGER*4   I4OWN(15)
	EQUIVALENCE (I4OWN,MASTER_NAME)
	VOLATILE    MASTER_NAME, I4OWN
C
	INTEGER*4   RETURN_LEN
C
	INTEGER*4   I4PREFIXLEN
	INTEGER*4   I4PREFIX
	CHARACTER   CXPREFIX*4
	EQUIVALENCE (I4PREFIX,CXPREFIX)
C
	INTEGER*4   NAMLEN
C
	CHARACTER*8 CURTIME
C
	INTEGER*4   MBXCHANNEL
	INTEGER*4   ST, J
        BYTE        BSTRING(8)/27,91,54,119,27,91,119,20/          !V03
        CHARACTER*4 CSTRING(2)                                     !V03
        EQUIVALENCE (BSTRING,CSTRING)                              !V03
        INTEGER*4       MES_BUF(33)                                !V03
        CHARACTER*132   MES_CBUF                                   !V03
        EQUIVALENCE     (MES_BUF, MES_CBUF)                        !V03
        LOGICAL     FIRST_TIME/.TRUE./                             !V03
        CHARACTER   BELL/Z07/                                      !V03
        INTEGER*4   BLANK                                          !V03
        DATA        BLANK/'    '/                                  !V03
	CHARACTER*8 PROC_NAME
C
C
C
C Get the PID of my MASTER
C
	ITEMLIST(1).BUFLEN = 4
	ITEMLIST(1).ITMCOD = JPI$_MASTER_PID
	ITEMLIST(1).BUFADR = %LOC(MASTER_PID)
	ITEMLIST(1).LENADR = %LOC(RETURN_LEN)
C
	ITEMLIST(2).BUFLEN = 0			!TO TERMINATE LIST
	ITEMLIST(2).ITMCOD = 0			!TO TERMINATE LIST
C
	ST = SYS$GETJPIW( ,%VAL(0),,ITEMLIST(1),,,)
	IF(.NOT.ST)THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 2000				!NORMAL PROMPT
	ENDIF
C
C If this is not a subprocess, just do normal pause prompt.
C
	IF(MASTER_PID.EQ.0)GOTO 2000
C
C Now get the image name of my MASTER process.  If it does not exist or if
C image name is blank, then the MASTER must be DCL so use MAILBOX prompt.
C
	ITEMLIST(1).BUFLEN = 60
	ITEMLIST(1).ITMCOD = JPI$_IMAGNAME
	ITEMLIST(1).BUFADR = %LOC(MASTER_NAME)
	ITEMLIST(1).LENADR = %LOC(RETURN_LEN)
C
	ITEMLIST(2).BUFLEN = 0			!TO TERMINATE LIST
C
	ST = SYS$GETJPIW( ,MASTER_PID,,ITEMLIST(1),,,)
	IF(.NOT.ST)GOTO 3000
C
C If SBRUN ran this, treat as though it is not there
C
	ST = INDEX(MASTER_NAME,']SBRUN.')
	IF(ST.NE.0)GOTO 3000
C
C If master process is running TELL, it will go away in a bit so treat
C as though it was not there
C
	ST = INDEX(MASTER_NAME,']TELL.')
	IF(ST.NE.0)GOTO 3000
C
	IF(MASTER_NAME(1:1).EQ.CHAR(0))GOTO 3000
	GOTO 2000
C
C Come here to perform a normal pause
C
2000	CONTINUE
	CALL INPTEXT(OUTPUTSTRING, INPUTSTRING, INPUTLEN)
	GOTO 9000
C
C Come here to perform a mailbox pause
C First, create a mailbox then wait for the answer
C
3000	CONTINUE
	CALL GETNAM(%REF(PROC_NAME))
C
	CALL GETPRFX( I4PREFIX, I4PREFIXLEN )
	NAMLEN=INDEX(PROC_NAME,' ')
	IF(NAMLEN.EQ.0)NAMLEN=9
	CALL MBXCREATE(0, CXPREFIX(1:I4PREFIXLEN)//
     *                 PROC_NAME(1:NAMLEN-1), MBXCHANNEL, ST )
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'CANNOT CREATE MAILBOX'
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 2000
	ENDIF
C
C*V03...
        FIRST_TIME=.TRUE.
3100    CONTINUE         
	CALL TIME(CURTIME)
C
        CALL FASTSET(BLANK, MES_BUF,33)
        IF(FIRST_TIME) THEN
          FIRST_TIME=.FALSE.
        ELSE
          TYPE *,BELL
        ENDIF
	WRITE(MES_CBUF,3301)CSTRING(1),CURTIME,' **TELL** ',
     $                      PROC_NAME,CSTRING(2),' ',OUTPUTSTRING
3301	FORMAT(7A)
C
        CALL WRITEBRK(MES_CBUF)
C
	CALL MBXREAD(MBXCHANNEL, INPUTSTRING, INPUTLEN, ST)
        IF(ST.EQ.SS$_ENDOFFILE) THEN
          DO 3400 J = 1, 300                    ! 300 TIMES !V04
            CALL XWAIT(1,2,ST)                  ! 1 SECOND
            CALL MBXREAD(MBXCHANNEL, INPUTSTRING, INPUTLEN, ST)
            IF(ST.NE.SS$_ENDOFFILE) GOTO 3500
3400      CONTINUE
          GOTO 3100                             ! ASK AGAIN
        ENDIF
3500    CONTINUE
C...V03
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'Error reading mailbox'
	  CALL LIB$SIGNAL(%VAL(ST))
	  CALL MBXDASSGN(MBXCHANNEL, ST)
	  GOTO 2000
	ENDIF
C
	CALL MBXDASSGN(MBXCHANNEL, ST)
C
C
9000	CONTINUE
	IF(INPUTLEN.LT.LEN(INPUTSTRING))THEN
	  INPUTSTRING(INPUTLEN+1: ) = ' '
	ENDIF
C
	RETURN
	END
