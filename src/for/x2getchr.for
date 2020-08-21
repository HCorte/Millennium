C
C SUBROUTINE X2GETCHR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETCHR.FOV                                 $
C  $Date::   17 Apr 1996 16:19:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2comsnp.for;2 **
C
C
C     X2GETCHR(VALUE,UNIT,DISP_CHAR)
C     IN:
C     VALUE - I*4 VALUE TO BE DISPLAYED
C     UNIT  - 0 FOR SECS, 1 FOR COUNT
C     DISP_CHAR - CHARACTER*1 DISPLAY CHARACTER
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
	SUBROUTINE X2GETCHR(VALUE,UNIT,DISP_CHAR)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 VALUE,UNIT,LOC_VAL, OFF, MOD1
C
	CHARACTER*1 DISP_CHAR
	CHARACTER*1 ONES_TAB(0:9)/'0','1','2','3','4','5','6','7','8',
     *	                          '9'/
	CHARACTER*1 TEN_TAB(9)/'A','B','C','D','E','F','G','H','I'/
	CHARACTER*1 HUN_TAB(9)/'J','K','L','M','N','O','P','Q','R'/
	CHARACTER*1 THU_TAB(9)/'a','b','c','d','e','f','g','h','i'/
	CHARACTER*1 TTH_TAB(9)/'j','k','l','m','n','o','p','q','r'/
	INTEGER*4 MOD_CNT(4)/10,100,1000,10000/
	INTEGER*4 MOD_TIM(4)/10,60,600,3600/
	CHARACTER*1 LOG_TAB(9,4)
	EQUIVALENCE (LOG_TAB(1,1),TEN_TAB(1))
	EQUIVALENCE (LOG_TAB(1,2),HUN_TAB(1))
	EQUIVALENCE (LOG_TAB(1,3),THU_TAB(1))
	EQUIVALENCE (LOG_TAB(1,4),TTH_TAB(1))
C
	LOC_VAL=VALUE
	DO 10, OFF=4,1,-1
	MOD1=MOD_CNT(OFF)
	IF (UNIT.EQ.0) MOD1=MOD_TIM(OFF)
	IF (LOC_VAL/MOD1.GT.0) THEN
	   LOC_VAL=LOC_VAL/MOD1
	   IF (LOC_VAL.GT.9) LOC_VAL=9
	   DISP_CHAR=LOG_TAB(LOC_VAL,OFF)
	   RETURN
	ENDIF
10	CONTINUE
	DISP_CHAR=ONES_TAB(LOC_VAL)
	RETURN
	END
