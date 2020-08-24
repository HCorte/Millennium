C
C SUBROUTINE GETCMB2
C $Log:   GXAFXT:[GOLS]GETCMB2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:24:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - getcmb2.for **
C
C GETCMB2.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C GETCMB.FTN
C
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C
C     GETCMB2.FTN
C     ----------
C
C     GET ALL COMBINATIONS OF (IND) ELEMENTS OUT OF BET ELEMENTS
C
C     GETCMB2(INDTAB,BET,IND)
C     OUT:
C     INDTAB - TABLE WITH INDEXES
C     E.G. FOR CALL GETCMB(INDTAB,4,3) RESULTING INDTAB WILL BE
C     1,2,3
C     1,2,4
C     1,3,4
C     2,3,4
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
	SUBROUTINE GETCMB2(INDTAB,BET,IND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 OFF2, OFF1, OFF, NEXT, IND, BET
	INTEGER*2 INDTAB(IND,*)    !TABLE WITH COMBINATIONS OF ELEMENTS
C
	INTEGER*2 INDEX(20)
C
	NEXT=1
C
C     INITIALISE TO FIRST SET OF ELEMENT 1,2,3,...
C
	INDEX(1)=1
	DO 100, OFF=2,IND
	   INDEX(OFF)=INDEX(OFF-1)+1
100	CONTINUE
C
110	CONTINUE
C
C     SET NEXT SET OF ELEMENTS
C
	DO  120, OFF=1,IND
	   INDTAB(OFF,NEXT)=INDEX(OFF)
120	CONTINUE
	NEXT=NEXT+1
C
C     CHOSE NEXT INDEXES
C
	 DO 140, OFF1=IND,1,-1
	    INDEX(OFF1)=INDEX(OFF1)+1
	    DO 130, OFF2=OFF1,IND-1
	       INDEX(OFF2+1)=INDEX(OFF2)+1
130	   CONTINUE
	   IF(INDEX(IND).LE.BET) GOTO 110
140	CONTINUE
	RETURN
	END
