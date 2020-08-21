C
C SUBROUTINE GNET
C $Log:   GXAFXT:[GOLS]GNET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:25:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   11 Jun 1993 17:40:58   HXK
C  ADDED AGTINF.DEF
C  
C     Rev 1.0   21 Jan 1993 16:30:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_gnet.for **
C
C GNET.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO ENCORPORATE THE GNET SIGN INTO THE SNAP
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
	SUBROUTINE GNET(ROW,COL)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4 TAB(8,14),TABL(8,7),TABR(8,7),LI(14,5)
C
	INTEGER*4 I, JJ, J, K, STEP, COL, ROW, COLX
C
	EQUIVALENCE (TAB(1,1),TABL(1,1)),(TAB(1,8),TABR(1,1))
	DATA TAB
     *	     /8*'*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      '*   ',6*'    ','*   ',
     *	      8*'*   '/
	DATA LI
     *	     /14*'G   ',
     *	     14*'T   ',
     *	     14*'N   ',
     *	     14*'E   ',
     *	     14*'T   '/
	DATA STEP/0/
C
C
	STEP=MOD(STEP,6)+1
	IF(STEP.EQ.1)THEN
	   DO 5 K=2,13
	   DO 5 J=2,7
	   TAB(J,K)='    '
5	   CONTINUE
	ENDIF
C
	IF(STEP.NE.6)THEN
	   DO 30 J=2,7
C**      TABL(J,STEP+2)=LI(J,STEP)
	   TABR(J,STEP+1)=LI(J,STEP)
30	   CONTINUE
	ELSE
	   DO 40 K=2,13
	   DO 40 J=2,7
	   TAB(J,K)='    '
40	   CONTINUE
	   DO 50 J=2,6
	   JJ=J
	   IF(JJ.GT.3)JJ=JJ+1
	   DO 50 K=2,13
	   TAB(JJ,K)=LI(K,J-1)
50	   CONTINUE
	ENDIF
	DO 60 K=1,14
	DO 60 I=1,8
	  COLX=COL+K-1
	  WRITE (XNEW(ROW+I-1)(COLX:80),900) TAB(I,K)
60	CONTINUE
C
C
900	FORMAT(A1)
C
	RETURN
	END
