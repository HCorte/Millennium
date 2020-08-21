C
C *** PROGRAM X2XTTN_DUMP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XTTN_DUMP.FOV                              $
C  $Date::   17 Apr 1996 16:45:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V03 26-JAN-96 DAS ADDED IOSB TO SYSTEM CALL
C V02 05-JUL-95 SCD ADD NODE NAME, DATE AND TIME TO HEADER INFORMATION AND
C		    PUT EACH TITAN RECORD ON A SEPARATE PAGE.  REMOVE
C		    "NETWORK PORT CONFIGURATION" HEADER
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
C       Utility routine to dump X2XSTCL file in ASCII.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2XTTN_DUMP
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
        INCLUDE 'INCLIB:X2XTTN.DEF'
C
        INTEGER*4   ST		    !Status/Exit
        INTEGER*4   REC		    !Record to display
        CHARACTER   X2FILNAM*20     !File name function
	LOGICAL	    DONE	    !Stop displaying?
C
C OPEN THE NETWORK PARAMETERS FILE.
C
        CALL OPENX(1,X2FILNAM(XTTN),4,0,0,ST)
        CALL IOINIT(X2XTTN_FDB,1,X2XTTN_SECT*256)
        IF(ST.NE.0) THEN
          CALL OS32ER(6,X2FILNAM(XTTN),'OPENX',ST,0)
          CALL GPAUSE
        ENDIF
C
C
C READ THE CURRENT CONFIGURATION.
C
	DONE = .FALSE.
	REC = 1
	DO WHILE ((REC.LE.X2XC_CLASSES).AND.
     *		  (.NOT. DONE))

           CALL READW(X2XTTN_FDB,REC,X2XTTN_REC,ST)
C
C DISPLAY THE CURRENT CONFIGURATION
C	   
           IF (ST.NE.0.AND.ST.NE.144) THEN
              CALL OS32ER(6,X2FILNAM(XTTN),'READW',ST,0)
              CALL GPAUSE
	      DONE = .TRUE.
	   ELSEIF (X2XTTN_REC(1).NE.0) THEN
	      CALL X2XTTN_DISPLAY
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
        CALL CLOSEFILE(X2XTTN_FDB)
C
C =================== Format Statements =====================
C

	END
C
C *** SUBROUTINE X2XTTN_DISPLAY ***
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C       Routine to display X2XTTN record in ASCII
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2XTTN_DISPLAY
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
        INCLUDE '($SYSSRVNAM)'		    !V02
        INCLUDE '($SYIDEF)'                 !V02
C
	INTEGER*2   DATBUF(12)              !Date buffer
	!INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   ENDOFF, BEGOFF
	INTEGER*4   INFO_LEN		    !V02 - LENGTH OF SYSTEM INFO STRING
	INTEGER*4   DATE_LEN		    !V02 - LENGTH OF DATE STRING
	INTEGER*4   TIME_LEN		    !V02 - LENGTH OF TIME STRING
	INTEGER*4   ISTART		    !V02 - STARTING OFFSET VARIABLE
	INTEGER*4   IEND		    !V02 - ENDING OFFSET VARIABLE
        INTEGER*4   NODE_NAME_LEN           !V02
        INTEGER*4   STATUS                  !V02
        INTEGER*2   IOSB(4)

	LOGICAL     UPDATE                  !Field update flag
	LOGICAL     PRTFLG                  !Print flag
	LOGICAL     FIRST_RECORD            !First output record flag - V02
                                            !only true for first record written

        CHARACTER*255 NODE_NAME             !V02
	CHARACTER*46 SYSTEM_INFO            !V02
	CHARACTER*9 CURRENT_DATE            !V02
	CHARACTER*8 CURRENT_TIME            !V02

        STRUCTURE   /SYISTRUC/              !V02
            INTEGER*2   BUFLEN              !V02
            INTEGER*2   ITMCOD              !V02
            INTEGER*4   BUFADR              !V02
            INTEGER*4   LENADR              !V02
        END STRUCTURE                       !V02

        RECORD      /SYISTRUC/ ITEMLIST(2)  !V02

	DATA FIRST_RECORD /.TRUE./          !V02
	COMMON /SAVE_FLAG/ FIRST_RECORD     !V02

C
C CLEAR SCREEN AND DISPLAY TITLE.
C
        ST = 0
	UPDATE=.FALSE.
	PRTFLG=.FALSE.
	CALL FASTMOV(X2XTTN_REC,DMYBUF,X2XTTN_SECT*64)

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
	IF (.NOT. FIRST_RECORD) THEN		!SKIP TO TOP OF NEXT PAGE
	    WRITE(6,9001) SYSTEM_INFO
	ELSE					!DON'T SKIP THE FIRST PAGE -
	    WRITE(6,9000) SYSTEM_INFO		!FIRST RECORD ONLY
	    FIRST_RECORD = .FALSE.
	ENDIF

C	End of V02 Changes

C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C DISPLAY ALL GLOBAL INFORMATION PRINTING TWO VARIABLES
C PER LINE.
C
50	CONTINUE
	BEGOFF = 1
	ENDOFF = X2XTTN_ENTRIES
	DO 100 I=BEGOFF,ENDOFF,2
     	  WRITE(6,9010) I,X2XTTN_FIELD(I),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I)),
     *	              I+1,X2XTTN_FIELD(I+1),
     *	                  X2XTTN_REC(X2XTTN_INDEX(I+1))
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=0
	IF(ENDOFF.EQ.X2XTTN_ENTRIES) CNT=MOD(X2XTTN_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
     	   WRITE(6,9010) J,X2XTTN_FIELD(J),
     *	                  X2XTTN_REC(X2XTTN_INDEX(J))
	ENDIF
C
C
C     ================== Format Statements =====================
C
9000    FORMAT(T11,'TITAN Parameters',T28,A<INFO_LEN>/)		!V02
9001    FORMAT('1',T11,'TITAN Parameters',T28,A<INFO_LEN>/)	!V02
9010	FORMAT(T10,2(I3.2,'.',1X,A15,1X,I10,2X))
C
	END

