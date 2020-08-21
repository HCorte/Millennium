C
C PROGRAM X2XGBL_DUMP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XGBL_DUMP.FOV                              $
C  $Date::   17 Apr 1996 16:42:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  GXSRC:X2XGBL_DUMP.FOR
C
C V03 25-JAN-96 DAS ADDED IOSB TO SYSTEM CALL 
C V02 17-JUL-95 SCD ADD NODE NAME, DATE AND TIME TO HEADER INFORMATION
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This program will dump the X2XGBL.FIL in ascii format.
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
	PROGRAM X2XGBL_DUMP
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
        INCLUDE 'INCLIB:X2XGBL.DEF'
C
        INTEGER*4   REC		    !Record to display
        INTEGER*4   ST	            !Status/Exit
        CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE GLOBAL PARAMETERS FILE.
C
        CALL OPENX(1,X2FILNAM(XGBL),4,0,0,ST)
        CALL IOINIT(X2XGBL_FDB,1,X2XGBL_SECT*256)
        IF(ST.NE.0) THEN
          CALL OS32ER(6,X2FILNAM(XGBL),'OPENX',ST,0)
          CALL GPAUSE
        ENDIF
C
C READ THE SPECIFIED OUTPUT RECORD.
C
	REC=1
	CALL READW(X2XGBL_FDB,REC,X2XGBL_REC,ST)
	IF(ST.NE.0) THEN
	  CALL OS32ER(6,X2FILNAM(XGBL),'READW',ST,1)
	  CALL GPAUSE
	ENDIF
C
        CALL X2GBL_DISPLAY
C
C PROGRAM EXIT.
C
8000    CONTINUE
        CALL CLOSEFILE(X2XGBL_FDB)
	END


C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2GBL_DISPLAY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XGBL.DEF'
        INCLUDE '($SYSSRVNAM)'		    !V02
        INCLUDE '($SYIDEF)'                 !V02
C
	INTEGER*4   CNT, J, OFFSET, I, LINES
	INTEGER*4   INFO_LEN	    !V02 - LENGTH OF SYSTEM INFO STRING
	INTEGER*4   DATE_LEN	    !V02 - LENGTH OF DATE STRING
	INTEGER*4   TIME_LEN	    !V02 - LENGTH OF TIME STRING
	INTEGER*4   ISTART	    !V02 - STARTING OFFSET VARIABLE
	INTEGER*4   IEND	    !V02 - ENDING OFFSET VARIABLE
        INTEGER*4   NODE_NAME_LEN   !V02
        INTEGER*4   STATUS          !V02
        INTEGER*2   IOSB(4)

        CHARACTER*255 NODE_NAME     !V02
	CHARACTER*32 SYSTEM_INFO    !V02
	CHARACTER*9 CURRENT_DATE    !V02
	CHARACTER*8 CURRENT_TIME    !V02

        STRUCTURE   /SYISTRUC/      !V02
            INTEGER*2   BUFLEN      !V02
            INTEGER*2   ITMCOD      !V02
            INTEGER*4   BUFADR      !V02
            INTEGER*4   LENADR      !V02
        END STRUCTURE               !V02

        RECORD      /SYISTRUC/ ITEMLIST(2)  !V02

C	Start of V02 Changes
C
C       Setup item list (itmlst) for Get Systen Info (SYS$GETSYIW) call
C
        ITEMLIST(1).BUFLEN = LEN(NODE_NAME)
        ITEMLIST(1).ITMCOD = SYI$_NODENAME
        ITEMLIST(1).BUFADR = %LOC(NODE_NAME)
        ITEMLIST(1).LENADR = %LOC(NODE_NAME_LEN)
C
        ITEMLIST(2).BUFLEN = 0                  !TO TERMINATE ITEM LIST

C
C       Get the NODE name, current date and current time
C
        STATUS = SYS$GETSYIW(,,,ITEMLIST(1),IOSB,,)

	CALL GCDATE (CURRENT_DATE)		!RETURNS DD-MMM-YY
	CALL TIME (CURRENT_TIME)		!RETURNS HH:MM:SS
	DATE_LEN = LEN(CURRENT_DATE)		!GET DATE AND TIME 
	TIME_LEN = LEN(CURRENT_TIME)		!STRING LENGTHS

C	Fit as much of the system information as possible into the SYSTEM_INFO
C	string.  Even if there is no room for anything else, at least include
C	the node name.  If we have enough room, then fit date and time.  If
C	we don't have enough space, then exclude the seconds from the time and
C	exclude year from the date.  As a last resort, assume that node name
C	and date without year will always fit.

	INFO_LEN = LEN(SYSTEM_INFO)		!GET LENGTH OF SYSTEM_INFO STRING

	DO IEND = 1,INFO_LEN			!BLANK OUT STRING
	   SYSTEM_INFO(IEND:IEND) = ' '
	END DO

	IF (NODE_NAME_LEN .GE. INFO_LEN) THEN	!ONLY HAVE ROOM FOR NAME
	    ISTART = 1
	    IEND = INFO_LEN
	    SYSTEM_INFO(ISTART:IEND) = NODE_NAME(ISTART:IEND)
	ELSE
	    IF (NODE_NAME_LEN + DATE_LEN + TIME_LEN .LE. INFO_LEN) THEN
	        ISTART = 1			!ALL INFORMATION WILL FIT
	        IEND = NODE_NAME_LEN
	        SYSTEM_INFO(ISTART:IEND) = NODE_NAME(1:NODE_NAME_LEN)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + DATE_LEN - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_DATE(1:DATE_LEN)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + TIME_LEN - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_TIME(1:TIME_LEN)
	    ELSEIF (NODE_NAME_LEN + DATE_LEN + TIME_LEN-3 .LE. INFO_LEN) THEN
	        ISTART = 1			!ALL INFORMATION EXCEPT FOR
	        IEND = NODE_NAME_LEN		!":SS" FROM TIME WILL FIT
	        SYSTEM_INFO(ISTART:IEND) = NODE_NAME(1:NODE_NAME_LEN)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + DATE_LEN - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_DATE(1:DATE_LEN)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + (TIME_LEN - 3) - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_TIME(1:TIME_LEN-3)
	    ELSEIF (NODE_NAME_LEN + DATE_LEN-3 + TIME_LEN-3 .LE. INFO_LEN) THEN
	        ISTART = 1			!ALL INFORMATION EXCEPT FOR
	        IEND = NODE_NAME_LEN		!":SS" FROM TIME and "-YY" FROM
						!DATE WILL FIT
	        SYSTEM_INFO(ISTART:IEND) = NODE_NAME(1:NODE_NAME_LEN)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + (DATE_LEN - 3) - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_DATE(1:DATE_LEN-3)
	        ISTART = IEND + 2		!SKIP PAST BLANK
	        IEND = ISTART + (TIME_LEN - 3) - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_TIME(1:TIME_LEN-3)
	    ELSE				!JUST USE NODE NAME AND DATE
	        ISTART = 1			!WITHOUT "-YY"
	        IEND = NODE_NAME_LEN
	        SYSTEM_INFO(ISTART:IEND) = NODE_NAME(1:NODE_NAME_LEN)
	        ISTART = MIN(NODE_NAME_LEN+2,NODE_NAME_LEN+DATE_LEN-3)
	        IEND = ISTART + (DATE_LEN - 3) - 1
	        SYSTEM_INFO(ISTART:IEND) = CURRENT_DATE(1:IEND-ISTART+1)
	    ENDIF

	ENDIF

C	Right justify the SYSTEM_INFO string ONLY if there are trailing blanks,
C	i.e. IEND<INFO_LEN.  At this point, IEND is the position of the last 
C	non-blank character in SYSTEM_INFO.
	IF (IEND .LT. INFO_LEN) THEN
	    DO I = IEND,1,-1
	       SYSTEM_INFO(INFO_LEN-IEND+I:INFO_LEN-IEND+I) = SYSTEM_INFO(I:I) 
	       SYSTEM_INFO(I:I) = ' '
	    END DO
	ENDIF

C	End of V02 Changes

C
C CLEAR THE SCREEN.
C
	WRITE(6,9050) SYSTEM_INFO		!V02 - ADD SYSTEM INFO TO HEADER
C
C DISPLAY THE CURRENT CONFIGURATION AND ENSURE THE USER WANTS
C TO SAVE THE RECORD.
C
	LINES=X2XGBL_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  WRITE(6,9010) (J,X2XGBL_FIELD(J),
     *	                   X2XGBL_REC(X2XGBL_INDEX(J)),
     *	                 J=OFFSET,OFFSET+1)
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XGBL_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  OFFSET=(I-1)*2+1
	  WRITE(6,9010) OFFSET,X2XGBL_FIELD(OFFSET),
     *	                X2XGBL_REC(X2XGBL_INDEX(OFFSET))
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C =================== Format Statements =====================
C
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9050	FORMAT(T10,'Global Network Configuration',T40,A<INFO_LEN>/)	!V02
	END
