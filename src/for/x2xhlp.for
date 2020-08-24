C
C SUBROUTINE X2XHLP
C $Log:   GXAFXT:[GOLS]X2XHLP.FOV  $
C  
C     Rev 1.1   19 Apr 1996 10:33:38   HXK
C  Merge of updates from Finland (Rita, Wojtek, Siew Mun)
C  
C     Rev 1.2   18 Apr 1996 16:17:42   HXK
C  Merge of LIVE code from Finland (RXK,WXW)
C  
C     Rev 1.2   16 Apr 1996 14:36:22   RXK
C  Different changes for HELP
C  
C     Rev 1.0   21 Jan 1993 18:37:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xhlp.for;1 **
C
C X2XHLP.FOR
C
C VO2 18-DEC-91 DAS USE FILES LOCATED IN GXHLP..
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display the help text files for the
C input menu.  The user will have the option of sequencely
C going through all help, or entering a page number to display.
C
C Calling sequence:
C
C     CALL X2XHLP(HLPNAM)
C
C Input parameters:
C
C     HLPNAM  Char*(*)    Name of help file
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
	SUBROUTINE X2XHLP(HLPNAM,FOUND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   OSERR               !OS error code
	INTEGER*4   PAGE,OLDPAGE        !Page to display
	INTEGER*4   LINES               !Lines per page
	INTEGER*4   I,K              !Work variables
	CHARACTER   HLPNAM*(*)          !Help file to display
	CHARACTER   FILNAM*60           !File to open
	CHARACTER   PROMPT*60           !Output prompt
	CHARACTER   OPT*3               !Input option
	CHARACTER   TEXT*80             !Help text line
	LOGICAL     MORE                !More help flag
	LOGICAL     FIRST               !First line already read flag
	LOGICAL     FOUND               !Help file found
C
C REMOVE ANY /2 FROM THE FILE NAME
C
	FILNAM=' '
        FILNAM(1:6)='GXHLP:'
	DO 50 I=1,LEN(HLPNAM)
	  IF(HLPNAM(I:I).EQ.'/' .OR. HLPNAM(I:I).EQ.' ') GOTO 51
	  FILNAM(I+6:I+6)=HLPNAM(I:I)
50	CONTINUE
        FILNAM(I+2:I+5)='.HLP'
        GOTO 52
51      CONTINUE
        FILNAM(I+6:I+9)='.HLP'
C
C ATTEMPT TO OPEN THE HELP FILE.
C
52	CONTINUE
	OPEN(UNIT=10,FILE=FILNAM,  STATUS='OLD',
     *	    RECL=80,  IOSTAT=OSERR, READONLY, SHARED)
	IF(OSERR.NE.0) THEN
          IF(OSERR.EQ.29 .OR. OSERR.EQ.30) THEN
             FOUND=.FALSE. 
             RETURN
             ENDIF  
	  CALL OS32ER(5,FILNAM,'OPEN',OSERR,0)
	  CALL GPAUSE
	ENDIF
        FOUND=.TRUE. 
	PAGE=1
C
C DISPLAY THE NEXT PAGE IN THE HELP FILE.
C
	FIRST=.TRUE.
200	CONTINUE
	MORE=.FALSE.
	LINES=0
	DO 100 I=(PAGE-1)*22+1,(PAGE-1)*22+22
C
C READ A LINE FROM THE HELP FILE,
C AND DISPLAY IT TO THE SCREEN.
C
C DON'T READ FIRST LINE OF PAGE
C IF IT ALREADY HAS BEEN READ INTO TEXT BUFFER
C
	  IF(FIRST) THEN
	    READ(10,9070,IOSTAT=OSERR,END=1000) TEXT
	    IF(OSERR.NE.0) THEN
	      CALL OS32ER(5,HLPNAM,'READ',OSERR,I)
	      CALL GPAUSE
	    ENDIF
	  ENDIF
	  FIRST=.TRUE.
C
C DISPLAY THE HELP TEXT.
C
	  IF(LINES.EQ.0) CALL CLRSCR(5)
CCC	  WRITE(5,*) TEXT(1:80)
	  TYPE *, TEXT
	  LINES=LINES+1
100	CONTINUE
C
C READ NEXT LINE TO DETERMINE IF MORE HELP EXISTS.
C
	MORE=.FALSE.
	READ(10,9070,IOSTAT=OSERR,END=1000) TEXT
	IF(OSERR.NE.0) THEN
	  CALL OS32ER(5,HLPNAM,'READ',OSERR,I)
	  CALL GPAUSE
	ELSE
	  MORE=.TRUE.
	  FIRST=.FALSE.	    !DON'T READ THE FIRST LINE OF NEXT PAGE
	ENDIF
C
C BUILD THE APPROPRIATE OUTPUT PROMPT.
C
1000	CONTINUE
	IF(MORE) THEN
	  WRITE (PROMPT,9000)
	ELSE
	  WRITE (PROMPT,9010)
	ENDIF
C
C ASK USER WHAT THEY WANT TO DO.
C
250	CONTINUE
	CALL WIMG(5,PROMPT)
	READ(5,9020) OPT
	IF(MORE .AND. (OPT(1:1).EQ.'M'.OR.OPT(1:1).EQ.'m')) THEN
	  PAGE=PAGE+1
	  GOTO 200
        ELSE IF(OPT(1:1).EQ.'P'.OR.OPT(1:1).EQ.'p') THEN
          OLDPAGE=PAGE
          PAGE=CTOI(OPT(2:3),K)
          IF(PAGE.EQ.0) THEN
            IF(MORE) THEN
              PAGE=OLDPAGE+1
              GOTO 200
            ELSE
              WRITE(5,9030)
              GOTO 250
            ENDIF
          ELSE
            REWIND 10
            IF(PAGE.EQ.1) GOTO 200
            DO K=1,(PAGE-1)*22
	       READ(10,9070,IOSTAT=OSERR,END=1000) TEXT
	       IF(OSERR.NE.0) THEN
	         CALL OS32ER(5,HLPNAM,'READ',OSERR,K)
	         CALL GPAUSE
	       ENDIF
            ENDDO
          ENDIF
          GOTO 200
	ELSE IF(OPT(1:1).EQ.'E'.OR.OPT(1:1).EQ.'e') THEN
	  GOTO 8000
	ELSE
	  WRITE(5,9040)
	  GOTO 250
	ENDIF
C
C PROGRAM EXIT
C
8000	CONTINUE
	CALL USRCLOS1(10)
	RETURN
C
C     ==================== Format Statements ===================
C
9000    FORMAT(15(' '),'Enter M for more, E to exit, or P# for page ')
9010    FORMAT(15(' '),'Enter E to exit, or P# for page             ')
9020	FORMAT(A2)
9030    FORMAT(15(' '),'Invalid page number entered ')
9040	FORMAT(15(' '),'Invalid option input ')
9050    FORMAT(15(' '),'Sorry, no help exists for page ',I3)
9060	FORMAT(15(' '),'Sorry, help for ',A18,' does not exist')
9070	FORMAT(A80)
	END
