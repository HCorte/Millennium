C MESMNT.FOR
C 
C v06 21-jun-2000 OXK CLEANUP W/ WARNING=ALL
C v05 21-FEB-1996 wsm X2X Upgrade: Added TVNEWS, news msg length 158 in Finland
C V04 14-SEP-1995 DAS LEIPZIG VERSION FOR BACKGROUND DOWNLOADS
C V03 30-DEC-1994 GPR MODIFIED TO SAVE MSGBLKNUM MESSAGES INSTEAD OF 256
C V02 09-JUL-1994 GPR MODIFIED TO USE PX2XFILES:SCF.FIL
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM MESMNT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
C
C  VARIABLE DECLARATION
C
        INTEGER*4 TARGET, INSTRT, OUTSTRT, LINENUM
        INTEGER*4 MSGLEN, INANYR, INLEN, CURSOR, MSGNUM
        INTEGER*4 GTMSGNUM, J, INANYL, MSGSCAN
	CHARACTER*(*)NPERMES1,NPERMES2  !ERROR MESSAGES.
	CHARACTER*(*) MESSAGENM,ISEMPTY,EXISTS,DELREP
	PARAMETER(NPERMES1='ERROR: MESSAGE NUMBER (1-200) REQUIRED.')
	PARAMETER(NPERMES2='ERROR: MESSAGE NUMBER (1-200) OR '//
     +	'''A(LL)'' REQUIRED.')
	PARAMETER(MESSAGENM='MESSAGE # ',ISEMPTY=' IS EMPTY.')
	PARAMETER(EXISTS=' ALREADY EXISTS.')
	PARAMETER(DELREP='DELETE AND REPLACE?(Y/OTHER)')
	CHARACTER CMDLINE*20,INPUT*62
	EQUIVALENCE(CMDLINE,INPUT)  !SAVE SPACE.
	INTEGER*4 CMDNDX,CMDNUM  !CMDNDX IS INDEX INTO CMDLINE,
C
C CMDNUM IS COMMAND NUMBER (ZERO TO (NUMBER OF COMMANDS-1),-1 IF
C UNIDENTIFIED) RETURNED BY EXTERNAL FUNCTION MSGSCAN.
C
	CHARACTER MSGBUF*1000,MSGBUFA(1000)
	EQUIVALENCE(MSGBUF,MSGBUFA)
	INTEGER*4 YESNO_FLAG, DLL_MODIFIED
C
C====================================================================
C
        CALL COPYRITE
C
	CALL SNIF_AND_WRKSET
C
10	TYPE *,'THERE ARE ',I4MSGTAB(MFRCNT,MSGCTL),
     +	' OVERFLOWS AVAILABLE.'
	TYPE *,'ENTER COMMAND[INIT,ENter,DIsplay,DElete,Save,'//
     +	'EDit,Load,Move,DLL,Quit]:'
	READ(5,2)CMDLINE  !READ USER COMMAND-PARAMETER INPUT.
2	FORMAT(A)
	CMDNUM=MSGSCAN(CMDLINE,CMDNDX)  !THIS IS AN EXTERNAL CAL/MACRO
C FUNCTION WHICH TAKES AS INPUT A STRING AND AN I*4 INDEX, SKIPS
C LEADING BLANKS IN THE STRING AND THEN PERFORMS AN SVC2 MNEMSCAN
C WITH IT AGAINST A MNEMONIC TABLE DEFINED IN THE FUNCTION. THE
C FUNCTION RETURNS AN I*4 VALUE AS DEFINED ABOVE FOR VARIABLE
C CMDNUM. CMDNDX IS SET TO POINT TO THE FIRST CHARACTER AFTER THE
C MNEMONIC.
	IF(CMDNDX.GT.0.AND.CMDNDX.LT.21) THEN
	  J=INANYL(CMDLINE(CMDNDX:),' ')  !FIND NONBLANK.
	ELSE
	  J=0
	ENDIF
	IF(J.GT.0)THEN
	  CMDNDX=CMDNDX+J-1    !POINT TO PARAMETER.
	ELSE
	  CMDNDX=0             !NO PARAMETER.
	ENDIF
	GOTO(50,100,200,300,400,500,600,700,800,900)CMDNUM+1
	TYPE *,CMDLINE,'UNRECOGNIZED. RETRY:'
	GOTO 10
C====================================================================
C THIS SECTION OF CODE INTIIALIZES THE COMMON FOR TEST PURPOSES:
C
50	TYPE *,'CLEAR AND INITIALIZE MESSAGE COMMON?(Y/OTHER)'
	READ(5,2)CMDLINE
	IF(CMDLINE(1:1).NE.'Y')GOTO 10! NO:EXIT.
	DO 52 J=1,MSGS  !CLEAR ALL MESSAGES.
	I4MSGTAB(SEGLEN,J)=0  !SET LENGTH TO ZERO.
52	I4MSGTAB(MSGLNK,J)=0  !CLEAR LINK.
	I4MSGTAB(MFRLNK,MSGCTL)=MSGS+1   !FIRST FREE SEGMENT.
	DO 54 J=MSGS+1,MSGSIZ-1   !INITIALIZE FREE LIST.
54	 I4MSGTAB(MSGLNK,J)=J+1
	 I4MSGTAB(MFRCNT,MSGCTL)=MSGCTL-MSGS-1  !NUMBER OF OVERFLOWS.
	GOTO 10
C ===================================================================
C ENTER NEW MESSAGE:
C
100	IF(CMDNDX.LE.0)THEN
	  TYPE *,NPERMES1  !MESSAGE NUMBER REQUIRED.
	  GOTO 10  !RETRY.
	ENDIF
	MSGNUM=GTMSGNUM(CMDLINE(CMDNDX:))  !GET MESSAGE NUMBER.
	IF (MSGNUM.EQ.MNEWS .OR. MSGNUM.EQ.TVNEWS) THEN
	  TYPE *,' NEWS MESSAGE CAN ONLY BE EDITED SORRY'
	  GOTO 10  !RETRY
	ENDIF
	IF(MSGNUM)103,10,105   !VECTOR ON MSGNUM AS FLAG.
103	MSGNUM=-MSGNUM  !MSGNUM WAS NEGATIVE: MAKE POSITIVE.
	GOTO 107  !SLOT IS FREE: ENTER NEW MESSAGE.
105	TYPE *,MESSAGENM,MSGNUM,EXISTS,DELREP !ALREADY EXISTS.
	READ(5,2)CMDLINE  !DELETE AND REPLACE?(Y/OTHER).
	IF(CMDLINE(1:1).NE.'Y')GOTO 10     !EXIT IF NOT YES.
107	CURSOR=1
	TYPE *,'INPUT BUFFER IN 60 CHAR SEGMENTS, NONBLANK '//
     +	'IN COL. 60 OR 61 TO CONTINUE:'
110	WRITE(5,109)
109	FORMAT(' >',60('-'),'C<')
	READ(5,2)INPUT         !FORMAT(C).
	IF(INPUT(61:61).NE.' ')THEN
	  INLEN=60
	ELSE
	  INLEN=INANYR(INPUT,' ')  !FIND END OF INPUT
	ENDIF
	MSGBUF(CURSOR:CURSOR+INLEN-1)=INPUT
	CURSOR=CURSOR+INLEN  !POINT TO NEXT CHAR.
	IF(INLEN.EQ.60)GOTO 110  !CONTINUE INPUTTING.
	MSGLEN=CURSOR-1  !LAST CHAR WRITTEN.
	CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(:MSGLEN))
	GOTO 10  !RETURN.
C===================================================================
C DISPLAY MESSAGE:
C
200	IF(CMDNDX.LE.0)THEN
	  TYPE *,NPERMES2  !MESSAGE NUMBER OR 'A(LL)' REQUIRED.
	  GOTO 10
	ENDIF
	IF(CMDLINE(CMDNDX:CMDNDX).EQ.'A')THEN   !TYPE ALL MESSAGES.
	  DO 206 MSGNUM=1,MSGS
	  IF(I4MSGTAB(SEGLEN,MSGNUM).EQ.0)GOTO 206
	  TYPE *,MESSAGENM,MSGNUM,' :'
	  CALL TYPEMSG(MSGNUM,MSGBUF,MSGLEN)
206	  CONTINUE
	ELSE                   !TYPE SINGLE MESSAGE.
	  MSGNUM=GTMSGNUM(CMDLINE(CMDNDX:))
	  IF(MSGNUM)210,10,220  !VECTOR ON MSGNUM AS FLAG.
210	  TYPE *,MESSAGENM,-MSGNUM,ISEMPTY
	  GOTO 10 !EXIT: SLOT IS EMPTY.
220	  CALL TYPEMSG(MSGNUM,MSGBUF,MSGLEN)
	ENDIF
	GOTO 10
C====================================================================
C DELETE MESSAGE:
C
300	IF(CMDNDX.LE.0)THEN
	  TYPE *,NPERMES1  !MESSAGE NUMBER REQUIRED.
	  GOTO 10
	ENDIF
	MSGNUM=GTMSGNUM(CMDLINE(CMDNDX:))
	IF(MSGNUM.LE.0)GOTO 10  !MESSAGE NONEXISTENT.
	IF (MSGNUM.EQ.MNEWS .OR. MSGNUM.EQ.TVNEWS) THEN
	  TYPE *,' NEWS MESSAGE CAN ONLY BE EDITED SORRY'
	  GOTO 10  !RETRY
	ENDIF
	CALL FLXDELETE(MSGNUM) !DELETE MESSAGE, FREE ITS SPACE.
	GOTO 10
C===================================================================
C SAVE ALL MESSAGES
C
400	CONTINUE
	CALL SAVESMF(1)
	GOTO 10
C===================================================================
C EDIT MESSAGE:
C
500	IF(CMDNDX.LE.0)THEN
	  TYPE *,NPERMES1  !MESSAGE NUMBER REQUIRED.
	  GOTO 10
	ENDIF
	MSGNUM=GTMSGNUM(CMDLINE(CMDNDX:))
C
C IF THIS IS A NEWS MESSAGE OR A SPECIAL MESSAGE
C USE THE EZEDIT ROUTINE . RATHER THAN THIS
C GARBAGE EDITOR.
C
	IF (MSGNUM.EQ.MNEWS .OR. MSGNUM.EQ.TVNEWS) THEN
	  WRITE (MSGBUF(1:158),10013)
10013	  FORMAT(158(' '))
	  CALL READMSG(MSGCHR,MSGNUM,MSGBUF,MSGLEN)
	  CALL EZEDIT(MSGBUF,MSGNUM)
	  CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(1:158))
	  GOTO 10             !EXIT ONCE COMPLETED
	ENDIF
	IF (MSGNUM.EQ.GVTNEWS) THEN
	  WRITE (MSGBUF(1:120),10014)
10014	  FORMAT(120(' '))
	  CALL READMSG(MSGCHR,MSGNUM,MSGBUF,MSGLEN)
	  CALL EZEDIT(MSGBUF,MSGNUM)
	  CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(1:120))
	  GOTO 10             !EXIT ONCE COMPLETED
	ENDIF
C
C
	IF(MSGNUM)510,10,520  !VECTOR ON MESSAGE AS FLAG.
510	MSGLEN=0  !ZERO MESSAGE LENGTH.
	MSGNUM=-MSGNUM  !MAKE MESSAGE NUMBER POSITIVE.
515	CALL TYPEBUF(MSGBUF,MSGLEN)
	GOTO 550
520	CALL TYPEMSG(MSGNUM,MSGBUF,MSGLEN)
550	IF(MSGLEN.LE.60)THEN
	  CURSOR=1
	ELSE
	  TYPE *,'STARTING LINE NUMBER:'
	  READ(5,557,ERR=10)LINENUM  !FORMAT(I).
557	  FORMAT(I)
	  IF(LINENUM.LT.1!LINENUM.GT.MSGLEN/60+1
     +	  .OR.LINENUM.GT.MSGLEN/60.AND.MOD(MSGLEN,60).LT.1)THEN
	    TYPE *,'NONEXISTANT/ILLEGAL LINE.'
	    GOTO 10 !EXIT.
	  ENDIF
	  CURSOR=(LINENUM-1)*60+1  !SET CURSOR TO FIRST CHAR OF LINE.
	ENDIF
	TYPE *,'EDIT COMMANDS ARE A,I,R,D,Q (APPEND,INSERT,'//
     +	'REPLACE,DELETE, AND QUIT). ENTER '//
     +	'COMMAND CHAR IN COL BEFORE STARTING COL, STRING, AND '//
     +	'ANY CHAR FOR TERMINATOR (APPEND IS POSITION-INDEPENDENT):'
	WRITE(5,558)
558	FORMAT(' >C',60('-'),'D<')
	WRITE(5,559)MSGBUF(CURSOR:MIN(MSGLEN,CURSOR+59))
559	FORMAT('  >',A)
	READ(5,2)INPUT         !FORMAT(C).
	OUTSTRT=INANYL(INPUT,' ')  !FIND STARTING COL. FOR OUTPUT
	IF(OUTSTRT.LT.1)GOTO 515  !NO INPUT: RETRY.
	IF(INPUT(OUTSTRT:OUTSTRT).EQ.'Q')GOTO 10 !EXIT.
	INSTRT=OUTSTRT+1
	INLEN=INANYR(INPUT,' ')-INSTRT  !CALCULATE LENGTH.
	IF(INLEN.LT.1)GOTO 515  !INVALID INPUT: RETRY.
	CURSOR=CURSOR+OUTSTRT-1
	GOTO(565,570,580,590)INDEX('AIRD',INPUT(OUTSTRT:OUTSTRT))
	TYPE *,'ERROR: ''',INPUT(OUTSTRT:OUTSTRT),
     +	''' UNRECOGNIZABLE COMMAND.'
	GOTO 10  !EXIT.
C APPEND: *********************************************************
565	CURSOR=MSGLEN+1  !SET CURSOR, LET 'INSERT' DO THE REST.
	GOTO 570 !(GOTO FOR SAFETY) 'INSERT' DOES THE REST.
C INSERT: *******************************************************
570	IF(MSGLEN+INLEN.GT.LEN(MSGBUF))THEN
	  TYPE *,'ERROR: OVERFLOW. REJECTED.'
	  GOTO 10
	ENDIF
	DO 574 J=MSGLEN+INLEN,CURSOR+INLEN,-1
574	MSGBUFA(J)=MSGBUFA(J-INLEN)
	MSGBUF(:CURSOR+INLEN-1)=MSGBUF(:CURSOR-1)//
     +	INPUT(INSTRT:OUTSTRT+INLEN)
	MSGLEN=MSGLEN+INLEN   !OLD LENGTH PLUS INSERT.
	CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(:MSGLEN))
	GOTO 515               !CONTINUE EDITING.
C REPLACE: *******************************************************
580	IF(CURSOR+INLEN-1.GT.LEN(MSGBUF))THEN
	  TYPE *,'ERROR: OVERFLOW. REJECTED.'
	  GOTO 10
	ENDIF
	MSGLEN=MAX(MSGLEN,CURSOR+INLEN-1)
	MSGBUF(CURSOR:CURSOR+INLEN-1)=INPUT(INSTRT:OUTSTRT+INLEN)
	CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(:MSGLEN))
	GOTO 515     !CONTINUE EDITING.
C DELETE: **********************************************************
590	MSGLEN=MAX(MSGLEN-INLEN,0)
	MSGBUF(:MSGLEN)=MSGBUF(:CURSOR-1)//MSGBUF(CURSOR+INLEN:)
	CALL WRITMSG(MSGCHR,MSGNUM,MSGBUF(:MSGLEN))
	GOTO 515  !CONTINUE EDITING.
C===================================================================
C LOAD ALL MESSAGES FROM SMF.FIL
C
C
600	CONTINUE
	CALL LOADSMF(1)
	GOTO 10
C====================================================================
C MOVE MESSAGE TO ANOTHER SLOT:
C
700	IF(CMDNDX.LE.0)THEN
	  TYPE *,NPERMES1  !ERROR: MESSAGE NUMBER REQUIRED.
	  GOTO 10
	ENDIF
	MSGNUM=GTMSGNUM(CMDLINE(CMDNDX:))
	IF(MSGNUM) 710,10,720  !VECTOR ON MSGNUM AS FLAG.
710	TYPE *,MESSAGENM,MSGNUM,ISEMPTY
	GOTO 10
720	TYPE *,'MOVE TO:'
	READ(5,2)CMDLINE  !READ TARGET MESSAGE NUMBER.
	TARGET=GTMSGNUM(CMDLINE)
	IF(TARGET) 725,10,730
725	TARGET=-TARGET !MAKE TARGET POSITIVE.
	GOTO 740
730	TYPE *,MESSAGENM,TARGET,EXISTS,DELREP
	READ(5,2)CMDLINE
	IF(CMDLINE(1:1).NE.'Y')GOTO 10 !NO: EXIT.
740	MSGCHR(TARGET)=MSGCHR(MSGNUM)  !COPY BASE SEGMENT.
	I4MSGTAB(MSGLNK,MSGNUM)=0  !CLEAR OLD BASE LINK.
	I4MSGTAB(SEGLEN,MSGNUM)=0  !ZERO OLD LENGTH.
	GOTO 10
800	CONTINUE
	CALL SPCDLL(DLL_MODIFIED)
	IF (DLL_MODIFIED.NE.0) THEN
	    TYPE *,IAM(),
     *       'Broadcast flag changes did not take effect in the file'
	    CALL WIMG(5,'Do you want to save to SMF file ?')
	    CALL YESNO(YESNO_FLAG)
	    IF (YESNO_FLAG.EQ.1) THEN
		CALL SAVESMF(2)
	    ENDIF
	ENDIF
	GOTO 10	
900	CALL GSTOP(GEXIT_SUCCESS)
	END
C====================================================================
C====================================================================
C EXTRACT MESSAGE NUMBER, TEST FOR LEGALITY:
C
	INTEGER*4 FUNCTION GTMSGNUM(CMDLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4 J
C
	CHARACTER*(*) CMDLINE  !SHOULD CONTAIN MESSAGE NUMBER
C
	GTMSGNUM=CTOI(CMDLINE,J)  !CONVERT MESSAGE NUMBER.
	IF(GTMSGNUM.LT.1.OR.GTMSGNUM.GT.MSGS)THEN
	TYPE *,CMDLINE(:J),' IS NOT A LEGAL MESSAGE NUMBER (1-200).'
	  GTMSGNUM=0  !ERROR FLAG.
	  RETURN
	ENDIF
	IF(I4MSGTAB(SEGLEN,GTMSGNUM).LE.0)
     +	  GTMSGNUM=-GTMSGNUM  !NEGATE GTMSGNUM AS 'FREE' FLAG.
	RETURN
	END
C====================================================================
C READ MESSAGE INTO BUFFER, DISPLAY MESSAGE BUFFER:
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE TYPEMSG(MSGNUM, MSGBUF, MSGLEN)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4	I, J,
     *			LASTSEG,
     *			NUMSEGS,
     *			CURSOR,
     *			MSGLEN,
     *			MSGNUM
C
	CHARACTER*(*)	MSGBUF
C
	CALL READMSG(MSGCHR, MSGNUM, MSGBUF, MSGLEN)
C
	ENTRY TYPEBUF(MSGBUF, MSGLEN)	!JUST TYPE BUFFER.
C
	CURSOR  = 1
	NUMSEGS = MSGLEN / 32
	LASTSEG = MOD(MSGLEN, 32)
C
	TYPE *, 'MESSAGE NUMBER = ', MSGNUM
	TYPE *, 'MESSAGE LENGTH = ', MSGLEN
	TYPE *, 'OFF'
C
	DO 10 J = 1, NUMSEGS		! DISPLAY BUFFER IN 32-CHAR SEGS.
	  WRITE(5, 19) (J-1)*32, (MSGBUF(I:I), I = CURSOR, CURSOR + 31)
19	  FORMAT(' ', Z3.2, 3X, 8(X, 4Z2.2))
	  CURSOR = CURSOR + 32
10	CONTINUE
C
	IF (LASTSEG .GT. 0) WRITE(5, 19) NUMSEGS*32,
     +	(MSGBUF(I:I), I = CURSOR, CURSOR + LASTSEG - 1)
C
	RETURN
	END
C===================================================================
C REMOVE FREE SEGMENT FROM FREE LIST AND RETURN ITS NUMBER AS
C FUNCTION VALUE, 0 IF NO FREE SEGMENTS LEFT:
C
	INTEGER*4 FUNCTION FREESEG(DUMMY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
        INTEGER*4 DUMMY
C
	FREESEG=I4MSGTAB(MFRLNK,MSGCTL)  !OBTAIN FREE SEGMENT.
	IF(FREESEG.GT.0)THEN
	  I4MSGTAB(MFRLNK,MSGCTL)=I4MSGTAB(MFRLNK,FREESEG) !ADJUST LIST.
	  I4MSGTAB(MFRCNT,MSGCTL)=I4MSGTAB(MFRCNT,MSGCTL)-1 !INC. COUNT.
	ENDIF
	RETURN
	END
