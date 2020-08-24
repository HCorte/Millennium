C
C SUBROUTINE X2BRODEL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BRODEL.FOV                                 $
C  $Date::   17 Apr 1996 16:10:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2brodel.for;1 **
C
C X2BRODEL.FOR
C 
C V03 08-DEC-95	JAC Removed  CALL X2XHLP('X2BRODEL.HLP') block 
C                   because X2BRODEL.HLP does not exist and YESNO(ANS)
C                   does not return an "H" or equivalent
C V02 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate
C		    UK changes into X2X Baseline
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This program will mark a record in the relay application file
C as deleted.  This is done by negating the record index number.
C
C Calling sequence:
C
C     CALL X2BRODEL(REC)
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
	SUBROUTINE X2BRODEL(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   LINES,OFFSET            !Line printing
	INTEGER*4   ST,I,J,CNT              !Work variables
	INTEGER*4   ANS                     !Input response
	LOGICAL     UPDATE                  !Field update flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   X2FILNAM*20             !File name function
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	UPDATE=.FALSE.
	CALL CLRSCR(5)
	CALL FASTMOV(X2XBRO_REC,DMYBUF,X2XBRO_SECT*64)
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
	LINES=X2XBRO_ENTRIES/2
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
	  J=OFFSET
	  WRITE(5,9010) J,X2XBRO_FIELD(J),
     *	                  X2XBRO_REC(X2XBRO_INDEX(J)),
     *	              J+1,X2XBRO_FIELD(J+1),
     *	                  X2XBRO_REC(X2XBRO_INDEX(J+1))
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XBRO_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
	  WRITE(5,9010) J,X2XBRO_FIELD(J),
     *	                  X2XBRO_REC(X2XBRO_INDEX(J))
	ENDIF
C
C ENSURE OPERATOR WISHES TO DELETE THIS RECORD.
C
	WRITE(5,*)
	WRITE(5,*)
	WRITE (PROMPT,9070)
	CALL WIMG(5,PROMPT)
	CALL YESNO(ANS)
C
C RECORD TO BE DELETED.
C
	IF(ANS.EQ.1) THEN
	  X2XBRO_UPDATE=DATBUF(VCDC)
	  X2XBRO_REC(1)=0
CV02	  CALL BSET(X2XBRO_BITMAP,1)
	  CALL X2BSET(X2XBRO_BITMAP,0,XBRO,REC)				!V02
	  UPDATE=.TRUE.
	ENDIF
C
C IF A RECORD IS TO BE DELETED UPDATE THE FILE.
C
	IF(UPDATE) THEN
	  CALL WRITEW(X2XBRO_FDB,REC,X2XBRO_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9080) REC
	  CALL X2CHKMOD(XBRO,1)
	ELSE
	  CALL WRITEW(X2XBRO_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9082) REC
	ENDIF
	CALL XWAIT(2,2,ST)
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T26,'Delete a Relay Application',//)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9070	FORMAT(20(' '),'Delete this record [Y/N] ')
9080	FORMAT(20(' '),'Record ',I4,' has been deleted ')
9082	FORMAT(20(' '),'Record ',I4,' has not been deleted ')
	END
