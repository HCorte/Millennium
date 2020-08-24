C
C SUBROUTINE CHKPRV
C $Log:   GXAFXT:[GOLS]CHKPRV.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:33:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:50:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chkprv.for **
C
C CHKPRV.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 17-MAY-88 DSL  Initial release adds users to USER file
C
C              CALLING SEQUENCE
C                             INPUT - UID - user id performing act
C                                     PRV - level of user to change
C                            OUTPUT - IND - task not able to change
C                                   - STS - value -1 == cannot change
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
	 SUBROUTINE CHKPRV(UID,PRV,IND,STS)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:RECUSE.DEF'
C
	 INTEGER *4 UID,STS,REC,IND
	 INTEGER*4 USEFDB(7), I, NIBBLE, ST
	 INTEGER*4 PRV(24),PRV1(24)
	 STS=0
C
C If uid is equal to zero then first record added will be super
C user
C
	 IF(UID.LE.0) RETURN
C
C Open USER file
C
	 CALL OPENX(3,'GXTSK:USER.FIL',4,0,0,ST)
	 CALL IOINIT(USEFDB,3,USESEC*256)
	 IF (ST.NE.0) THEN
	    WRITE(5,899) ST
 899	    FORMAT(' user file open error  st - ',I4)
	    CALL USRCLOS1(     3)
	    STS=-1
	    RETURN
	 ENDIF
C
C Read USER file for privilege group of user changing record
C
	 REC=UID
	 CALL READW(USEFDB,REC,USEREC,ST)
	 IF (ST.NE.0) THEN
	    WRITE(5,900) REC,ST
 900	    FORMAT(' user file read error  rec -',I4,' st - ',I4)
	    CALL XWAIT(2,2,ST)
	    CALL USRCLOS1(     3)
	    STS=-1
	    RETURN
	 ENDIF
C
C Get levels for original record  because privilege group didn't
C change
C
	   DO 22 NIBBLE=0,23
C***	      CALL GETNIB(NIBBLE,USERSECL,PRV1(NIBBLE+1))
	      CALL GETNIBLE(PRV1(NIBBLE+1),USERSECL,NIBBLE+1)
 22	   CONTINUE
C
C Determine if user REC can  change record
C
	 DO 25 I=1,24
	    IF(PRV1(I).LT.PRV(I)) THEN
	       STS=-1
	       IND=I
	       CALL USRCLOS1(     3)
	       RETURN
	    ENDIF
 25	 CONTINUE
C
C Close files and return
C
	 CALL USRCLOS1(     3)
	 RETURN
	 END
