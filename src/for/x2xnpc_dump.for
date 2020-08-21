C
C *** PROGRAM X2XNPC_DUMP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XNPC_DUMP.FOV                              $
C  $Date::   17 Apr 1996 16:43:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V03 25-JAN-96 DAS ADDED IOSB TO SYSTEM CALL
C V02 17-JUL-95 SCD ADD NODE NAME, DATE AND TIME TO HEADER INFORMATION.
C		    MOVE WRITING OF HEADER INFORMATION FROM MAIN PROGRAM
C		    TO SUBROUTINE X2XNPC_DISPLAY.
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
C       Utility routine to dump X2XNPC file in ASCII.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2XNPC_DUMP
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
        INCLUDE 'INCLIB:X2XNPC.DEF'
C
        INTEGER*4   ST		    !Status/Exit
        INTEGER*4   REC		    !Record to display
        CHARACTER   X2FILNAM*20     !File name function
	LOGICAL	    DONE	    !Stop displaying?
C
C OPEN THE NETWORK PARAMETERS FILE.
C
        CALL OPENX(1,X2FILNAM(XNPC),4,0,0,ST)
        CALL IOINIT(X2XNPC_FDB,1,X2XNPC_SECT*256)
        IF(ST.NE.0) THEN
          CALL OS32ER(6,X2FILNAM(XNPC),'OPENX',ST,0)
          CALL GPAUSE
        ENDIF
C
C
C READ THE CURRENT CONFIGURATION.
C
	DONE = .FALSE.
	REC = 1
	DO WHILE ((REC.LE.X2X_NETWORK_PORTS).AND.
     *		  (.NOT. DONE))

           CALL READW(X2XNPC_FDB,REC,X2XNPC_REC,ST)
C
C DISPLAY THE CURRENT CONFIGURATION
C	   
           IF (ST.NE.0.AND.ST.NE.144) THEN
              CALL OS32ER(6,X2FILNAM(XNPC),'READW',ST,0)
              CALL GPAUSE
	      DONE = .TRUE.
	   ELSEIF (X2XNPC_REC(1).NE.0) THEN	
	      CALL X2XNPC_DISPLAY
	      REC = REC + 1
	   ELSE
	      REC = REC + 1
	   ENDIF
C
	ENDDO
C
C PROGRAM EXIT.
C
8000    CONTINUE
        CALL CLOSEFILE(X2XNPC_FDB)
C
C =================== Format Statements =====================
C

	END
C
C *** SUBROUTINE X2XNPC_DISPLAY ***
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C       Routine to display X2XNPC record in ASCII
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2XNPC_DISPLAY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
        INCLUDE 'INCLIB:X2VIS.DEF'
        INCLUDE '($SYSSRVNAM)'		    !V02
        INCLUDE '($SYIDEF)'                 !V02
C
	INTEGER*4   CNT, J, OFFSET, I, LINES, ST
	INTEGER*4   INFO_LEN	    !V02 - LENGTH OF SYSTEM INFO STRING
	INTEGER*4   DATE_LEN	    !V02 - LENGTH OF DATE STRING
	INTEGER*4   TIME_LEN	    !V02 - LENGTH OF TIME STRING
	INTEGER*4   ISTART	    !V02 - STARTING OFFSET VARIABLE
	INTEGER*4   IEND	    !V02 - ENDING OFFSET VARIABLE
        INTEGER*4   REC_PER_PAGE    !V02 - NUMBER OF X2XNPC RECORDS TO PUT 
        PARAMETER  (REC_PER_PAGE=6) !V02 - ON ONE PAGE
        INTEGER*4   REC_COUNT       !V02 - RECORD COUNTER TO KEEP TRACK OF
                                    !V02 - HOW MANY RECORDS HAVE BEEN WRITTEN
        INTEGER*4   NODE_NAME_LEN   !V02
        INTEGER*4   STATUS          !V02
        INTEGER*2   IOSB(4)

        CHARACTER*255 NODE_NAME     !V02
	CHARACTER*35 SYSTEM_INFO    !V02
	CHARACTER*9 CURRENT_DATE    !V02
	CHARACTER*8 CURRENT_TIME    !V02

        STRUCTURE   /SYISTRUC/      !V02
            INTEGER*2   BUFLEN      !V02
            INTEGER*2   ITMCOD      !V02
            INTEGER*4   BUFADR      !V02
            INTEGER*4   LENADR      !V02
        END STRUCTURE               !V02

        RECORD      /SYISTRUC/ ITEMLIST(2)  !V02
        CHARACTER   CHRSTR(20)          !ASCII for BCD address
        CHARACTER   CHRSTR2(20)         !ASCII for BCD address
	INTEGER*4   ADDRESS_INDEX
	PARAMETER   (ADDRESS_INDEX=7)
	INTEGER*4   HUNT_INDEX
	PARAMETER   (HUNT_INDEX=11)

	DATA REC_COUNT /0/	            !V02 - INIT RECORD COUNTER AND
	COMMON /X2XNPC_DISPLAY_BLOCK/ REC_COUNT        !V02 - KEEP VALUE 
                                            !V02 - ACROSS SUBROUTINE CALLS
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

C DISPLAY THE CURRENT CONFIGURATION
C
	CALL HTOA(CHRSTR,1,X2XNPC_ADDLEN,X2XNPC_ADDRES,ST)
	CALL HTOA(CHRSTR2,1,X2XNPC_ADDLEN,X2XNPC_HUNTADR,ST)
C	
	LINES=X2XNPC_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  IF (.NOT.((OFFSET.EQ.ADDRESS_INDEX).OR.
     *		    (OFFSET.EQ.HUNT_INDEX))) THEN
	    WRITE(6,9010) (J,X2XNPC_FIELD(J),
     *	                   X2XNPC_REC(X2XNPC_INDEX(J)),
     *	                   J=OFFSET,OFFSET+1)
	  ELSEIF (OFFSET.EQ.ADDRESS_INDEX) THEN
	    WRITE(6,9020) OFFSET,X2XNPC_FIELD(OFFSET),
     *	                  (CHRSTR(J),J=MAX0(X2XNPC_ADDLEN-DADRL+1,1),
     *			   X2XNPC_ADDLEN),OFFSET+1,X2XNPC_FIELD(OFFSET+1),
     *			  X2XNPC_REC(X2XNPC_INDEX(OFFSET+1))
	  ELSEIF (OFFSET.EQ.HUNT_INDEX) THEN
	    WRITE(6,9020) OFFSET,X2XNPC_FIELD(OFFSET),
     *	                  (CHRSTR2(J),J=MAX0(X2XNPC_ADDLEN-DADRL+1,1),
     *			   X2XNPC_ADDLEN),OFFSET+1,X2XNPC_FIELD(OFFSET+1),
     *			  X2XNPC_REC(X2XNPC_INDEX(OFFSET+1))
	  ENDIF
C	    
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XNPC_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  OFFSET=(I-1)*2+1
	  WRITE(6,9010) OFFSET,X2XNPC_FIELD(OFFSET),
     *	                X2XNPC_REC(X2XNPC_INDEX(OFFSET))
	ENDIF

	WRITE (6,*)
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C =================== Format Statements =====================
C
9010	FORMAT(T10,2(I2.2,'.',1X,A15,3X,I10,2X))
9020	FORMAT(T10,I2.2,'.',1X,A15,<DADRL-MIN0(DADRL,X2XNPC_ADDLEN)+1>X,
     *	<MIN0(DADRL,X2XNPC_ADDLEN)>A1,2X,I2.2,'.',1X,A15,3X,I10,2X)
9050	FORMAT('1',T10,'Network Port Configuration',T41,A<INFO_LEN>/)	   !V02
9051	FORMAT(T10,'Network Port Configuration',T41,A<INFO_LEN>/)	   !V02
	END
