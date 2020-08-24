C
C SUBROUTINE X2STNDEL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNDEL.FOV                                 $
C  $Date::   17 Apr 1996 16:36:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2stndel.for **
C
C X2STNDEL.FOR
C
C V03 21-OCT-94 GPR SET THE FIRST BIT OF THE BITMAP NOT THE LAST - Integrate 
C		    UK changes into X2X Baseline
C V02 18-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program will mark a record in the Station file
C as deleted.  This is done by negating the record index number.
C
C Calling sequence:
C
C     CALL X2STNDEL(REC)
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
	SUBROUTINE X2STNDEL(REC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
	INTEGER*2   DATBUF(12)              !Date buffer
	INTEGER*4   REC                     !Record being modified
	INTEGER*4   DMYBUF(128)             !Blank buffer
	INTEGER*4   SYSDATE(3)              !System date
	INTEGER*4   LINES,OFFSET            !Line printing
	INTEGER*4   ST,I,J,CNT,K            !Work variables
	INTEGER*4   ERR                     !Program exit/error
	INTEGER*4   ANS                     !Input response
	LOGICAL     UPDATE                  !Field update flag
	CHARACTER   PROMPT*50               !Input prompt
	CHARACTER   CHRSTR(20)*1            !Output address
	CHARACTER   X2FILNAM*20             !File name function
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	CALL FASTMOV(X2XSTN_REC,DMYBUF,X2XSTN_SECT*64)
	WRITE(5,9000)
	UPDATE=.FALSE.
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
	LINES=MIN0(15,X2XSTN_ENTRIES/2)
	DO 100 I=1,LINES
	  OFFSET=(I-1)*2+1
C
C NOTE: OFFSET 3 IS HARDCODED TO ALLOW FIELD 3 (ADDRESS)
C       TO BE CORRECTLY DISPLAYED.
C
	  IF(OFFSET.EQ.3) THEN
	    CALL HTOA(CHRSTR,1,X2XSTN_ADDLEN,X2XSTN_ADDRES,ERR)
	    DO 102 J=X2XSTN_ADDLEN+1,20
	      CHRSTR(J)=' '
102	    CONTINUE
	    J=OFFSET
	    WRITE(5,9040) J,X2XSTN_FIELD(J),(CHRSTR(K),K=1,16),
     *	                J+1,X2XSTN_FIELD(J+1),
     *	                    X2XSTN_REC(X2XSTN_INDEX(J+1))
C
C NOTE: OFFSET 45 IS HARDCODED TO ALLOW FIELD 45 (Extended verifcation
C       Sequence number) TO BE CORRECTLY DISPLAYED.
C
          ELSE IF(I+1.EQ.45) THEN
            CALL HTOA(CHRSTR,1,X2XSTN_EVSN_LEN,X2XSTN_EVSN,ERR)
            DO 202 J=X2XSTN_EVSN_LEN+1,20
              CHRSTR(J)=' '
202         CONTINUE
            J=OFFSET
            WRITE(5,9050) J,X2XSTN_FIELD(J),
     *                  X2XSTN_REC(X2XSTN_INDEX(J)),
     *                  J+1,X2XSTN_FIELD(J+1),(CHRSTR(K),K=1,16)
	  ELSE
	    J=OFFSET
	    WRITE(5,9010) J,X2XSTN_FIELD(J),
     *	                    X2XSTN_REC(X2XSTN_INDEX(J)),
     *	                J+1,X2XSTN_FIELD(J+1),
     *	                    X2XSTN_REC(X2XSTN_INDEX(J+1))
	  ENDIF
100	CONTINUE
C
C PRINT REMAINING FIELDS, IF ANY.
C
	CNT=MOD(X2XSTN_ENTRIES,2)
	IF(CNT.NE.0) THEN
	  J=(I-1)*2+1
	  WRITE(5,9010) J,X2XSTN_FIELD(J),
     *	                  X2XSTN_REC(X2XSTN_INDEX(J))
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
	  CALL X2XHLP('X2STNDEL.HLP')
	  CALL CLRSCR(5)
	  WRITE(5,9000)
	  GOTO 50
	ENDIF
C
C RECORD TO BE DELETED.
C
	IF(ANS.EQ.1) THEN
	  X2XSTN_UPDATE=DATBUF(VCDC)
	  CALL FASTSET(0,X2XSTN_REC,X2XSTN_SECT*64)
CV03	  CALL BSET(X2XSTN_BITMAP,1)
	  CALL X2BSET(X2XSTN_BITMAP,0,XSTN,REC)				!V03
	  UPDATE=.TRUE.
	ENDIF
C
C IF A RECORD IS TO BE DELETED UPDATE THE FILE.
C
C
	IF(UPDATE) THEN
	  CALL WRITEW(X2XSTN_FDB,REC,X2XSTN_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9080) REC
	  CALL X2CHKMOD(XSTN,1)
	ELSE
	  CALL WRITEW(X2XSTN_FDB,REC,DMYBUF,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XSTN),'WRITEW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  WRITE(5,9082) REC
	ENDIF
	CALL XWAIT(2,2,ST)
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT(/,T26,'GTECH Distributed Network',/,
     *	         T28,'Delete Station Record',/)
9010	FORMAT(T10,2(I2.2,'.',1X,A15,1X,I10,2X))
9040	FORMAT(T10,I2.2,'.',1X,A10,1X,16A,1X,
     *	           I2.2,'.',1X,A15,1X,I10)
9050    FORMAT(T10,I2.2,'.',1X,A15,1X,I10,1X,
     *             I2.2,'.',1X,A13,1X,16A)
9070	FORMAT(20(' '),'Delete this record [Y/N] ')
9080	FORMAT(20(' '),'Record ',I5,' has been deleted ')		! V02
9082	FORMAT(20(' '),'Record ',I5,' has not been deleted ')		! V02
	END
