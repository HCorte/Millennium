C
C SUBROUTINE X2TERDEL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TERDEL.FOV                                 $
C  $Date::   17 Apr 1996 16:38:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2terdel.for;1 **
C
C X2TERDEL.FOR
C
C V03 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate 
C		    UK changes into X2X Baseline
C V02 15-FEB-94 GPR Changed write format to use I5
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will mark a record in the Terminal Configuration file
C as deleted.  This is done by negating the record index number.
C
C Calling sequence:
C
C     CALL X2TERDEL(REC)
C
C Input parameters:
C
C     REC     Int*4   Record to be modified
C
C Output parameters:
C
C     NONE
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2TERDEL(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XTER.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   LINES,OFFSET            !Line printing
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   ANS                     !Input response
	LOGICAL     UPDATE                  !Field update flag
	CHARACTER   X2FILNAM*20
	CHARACTER   PROMPT*50               !Input prompt
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	UPDATE=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XTER_REC,DMYBUF,X2XTER_SECT*64)
	WRITE(5,9000)
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
	LINES=X2XTER_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
C
C NOTE: OFFSET 3 IS HARDCODED TO ALLOW FIELD 4 (DROP)
C       TO BE CORRECTLY DISPLAYED.
C
	  IF(OFFSET.EQ.3) THEN
	    WRITE(5,9040) 3,X2XTER_FIELD(3),X2XTER_REC(3),
     *	                  4,X2XTER_FIELD(4),X2XTER_DROP
	  ELSE
	    J=OFFSET
	    WRITE(5,9010) J,X2XTER_FIELD(J),
     *	                    X2XTER_REC(X2XTER_INDEX(J)),
     *	                J+1,X2XTER_FIELD(J+1),
     *	                    X2XTER_REC(X2XTER_INDEX(J+1))
	  ENDIF
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XTER_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
	  WRITE(5,9010) J,X2XTER_FIELD(J),
     *	                  X2XTER_REC(X2XTER_INDEX(J))
	ENDIF
C
C ENSURE OPERATOR WISHES TO DELETE THIS RECORD.
C
	WRITE(5,*)
	WRITE(5,*)
	WRITE (PROMPT,9070)
	CALL WIMG(5,PROMPT)
	CALL YESNO(ANS)
	IF(ANS.EQ.-9) THEN
	  CALL X2XHLP('X2TERDEL.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  GOTO 50
	ELSE IF(ANS.LT.0) THEN
	  GOTO 8000
	ENDIF
C
C RECORD TO BE DELETED.
C
	IF(ANS.EQ.1) THEN
	  X2XTER_UPDATE=DATBUF(VCDC)
	  X2XTER_REC(1)=0
CV03	  CALL BSET(X2XTER_BITMAP,1)
	  CALL X2BSET(X2XTER_BITMAP,0,XTER,REC)				!V03
	  UPDATE=.TRUE.
	ENDIF
C
C IF A RECORD IS TO BE DELETED UPDATE THE FILE.
C
	IF(UPDATE) THEN
	  CALL WRITEW(X2XTER_FDB,REC,X2XTER_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTER),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9080) REC
	  CALL X2CHKMOD(XTER,1)
	ELSE
	  CALL WRITEW(X2XTER_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XTER),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9082) REC
	ENDIF
	CALL XWAIT(2,2,ST)
C
C PROGRAM EXIT
C
8000	CONTINUE
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T24,'Delete Terminal Configuration',//)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9040	FORMAT(T10,I2.2,'.',1X,A15,1X,I10,2X,
     *	           I2.2,'.',1X,A15,1X,20A)
9070	FORMAT(20(' '),'Delete this record [Y/N] ')
9080	FORMAT(20(' '),'Record ',I5,' has been deleted ')		! V02
9082	FORMAT(20(' '),'Record ',I5,' has not been deleted ')		! V02
	END
