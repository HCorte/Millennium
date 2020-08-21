C
C *** PROGRAM X2XBRO_DUMP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XBRO_DUMP.FOV                              $
C  $Date::   17 Apr 1996 16:41:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V03 25-JAN-96 DAS ADDED IOSB TO SYSTEM CALL
C V02 17-JUL-95 SCD ADD NODE NAME, DATE AND TIME TO HEADER INFORMATION.
C		    MOVE WRITING OF HEADER INFORMATION FROM MAIN PROGRAM
C		    TO SUBROUTINE X2XBRO_DISPLAY.
C V01 13-DEC-94 GPR RELEASED FOR UK
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
C       Utility routine to dump X2XBRO file in ASCII.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM X2XBRO_DUMP
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
        INCLUDE 'INCLIB:X2XBRO.DEF'
C
        INTEGER*4   REC		    !Record to display
        INTEGER*4   ST	            !Status/Exit
        CHARACTER   X2FILNAM*20     !File name function
        LOGICAL     DONE            !Stop displaying?
C
C OPEN THE BROADCAST PARAMETERS FILE.
C
        CALL OPENX(1,X2FILNAM(XBRO),4,0,0,ST)
        CALL IOINIT(X2XBRO_FDB,1,X2XBRO_SECT*256)
        IF(ST.NE.0) THEN
          CALL OS32ER(6,X2FILNAM(XBRO),'OPENX',ST,0)
          CALL GPAUSE
        ENDIF
C
C
C READ THE SPECIFIED OUTPUT RECORD.
C
        DONE = .FALSE.
        REC = 1
        DO WHILE ((REC.LE.X2X_NETWORK_PORTS).AND.
     *            (.NOT. DONE))
	  CALL READW(X2XBRO_FDB,REC,X2XBRO_REC,ST)
	  IF(ST.NE.0.AND.ST.NE.144) THEN
	    CALL OS32ER(6,X2FILNAM(XBRO),'READW',ST,1)
	    CALL GPAUSE
	  ELSEIF (X2XBRO_REC(1).NE.0) THEN
              CALL X2XBRO_DISPLAY
              REC = REC + 1
          ELSE
              REC = REC + 1
	  ENDIF
	ENDDO
C
C PROGRAM EXIT.
C
8000    CONTINUE
        CALL CLOSEFILE(X2XBRO_FDB)
C
C =================== Format Statements =====================
C
	END

C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2XBRO_DISPLAY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
        INCLUDE '($SYSSRVNAM)'		    !V02
        INCLUDE '($SYIDEF)'                 !V02
C
	INTEGER*4   CNT, J, OFFSET, I, LINES
	INTEGER*4   INFO_LEN	    !V02 - LENGTH OF SYSTEM INFO STRING
	INTEGER*4   DATE_LEN	    !V02 - LENGTH OF DATE STRING
	INTEGER*4   TIME_LEN	    !V02 - LENGTH OF TIME STRING
	INTEGER*4   ISTART	    !V02 - STARTING OFFSET VARIABLE
	INTEGER*4   IEND	    !V02 - ENDING OFFSET VARIABLE
        INTEGER*4   REC_PER_PAGE    !V02 - NUMBER OF X2XBRO RECORDS TO PUT 
        PARAMETER  (REC_PER_PAGE=3) !V02 - ON ONE PAGE
        INTEGER*4   REC_COUNT       !V02 - RECORD COUNTER TO KEEP TRACK OF
                                    !V02 - HOW MANY RECORDS HAVE BEEN WRITTEN
        INTEGER*4   NODE_NAME_LEN   !V02
        INTEGER*4   STATUS          !V02
        INTEGER*2   IOSB(4)

        CHARACTER*255 NODE_NAME     !V02
	CHARACTER*30 SYSTEM_INFO    !V02
	CHARACTER*9 CURRENT_DATE    !V02
	CHARACTER*8 CURRENT_TIME    !V02

        STRUCTURE   /SYISTRUC/      !V02
            INTEGER*2   BUFLEN      !V02
            INTEGER*2   ITMCOD      !V02
            INTEGER*4   BUFADR      !V02
            INTEGER*4   LENADR      !V02
        END STRUCTURE               !V02

        RECORD      /SYISTRUC/ ITEMLIST(2)  !V02

	DATA REC_COUNT /0/	            !V02 - INIT RECORD COUNTER AND
	COMMON /X2XBRO_DISPLAY_BLOCK/ REC_COUNT        !V02 - KEEP VALUE 
                                            !V02 - ACROSS SUBROUTINE CALLS

C
C CLEAR THE SCREEN.
C
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

C	Write 'REC_PER_PAGE' records on 1 page.  If we are writing the first 
C	record, then don't go to the top of the next page to write the header.
	REC_COUNT = REC_COUNT + 1		!INCREMENT COUNTER
        IF (MOD(REC_COUNT,REC_PER_PAGE).EQ.1) THEN	!WRITE HEADER INFO
            IF (REC_COUNT .GT. 1) THEN		!GO TO TOP OF NEXT PAGE
	        WRITE(6,9050) SYSTEM_INFO
	    ELSE				!THIS IS THE 1ST PAGE, DON'T
	        WRITE(6,9051) SYSTEM_INFO	!GO TO TOP OF THE NEXT PAGE
	    ENDIF
	ENDIF

C	End of V02 Changes
C
C DISPLAY THE CURRENT CONFIGURATION AND ENSURE THE USER WANTS
C TO SAVE THE RECORD.
C
	LINES=X2XBRO_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  WRITE(6,9010) (J,X2XBRO_FIELD(J),
     *	                   X2XBRO_REC(X2XBRO_INDEX(J)),
     *	                 J=OFFSET,OFFSET+1)
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XBRO_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  OFFSET=(I-1)*2+1
	  WRITE(6,9010) OFFSET,X2XBRO_FIELD(OFFSET),
     *	                X2XBRO_REC(X2XBRO_INDEX(OFFSET))
	ENDIF

C PRINT A BLANK LINE BETWEEN EACH BRO RECORD ON A PAGE - V02
	WRITE (6,*)					!V02
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C =================== Format Statements =====================
C
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9050	FORMAT('1',T10,'Broadcast Network Configuration',T42,A<INFO_LEN>/) !V02
9051	FORMAT(T10,'Broadcast Network Configuration',T42,A<INFO_LEN>/)	   !V02
	END
