C SUBROUTINE SYSDEL
C  
C V02 03-MAR-2000 OXK Vakio changes
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C SUBROUTINE TO DELETE A BET DESCRIPTION FROM SYSBET FILE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SYSDEL(SYS,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSSF.DEF'
C
	INTEGER*4 NEXT, LEN, I, BROWS, IND, STYP, FLAG, ST, SYS
C
	COMMON /SCOMON/SSFREC
C
C
	ST=-1
	TYPE*,'Deleteing system bet definition # ',SYS
      CALL WIMG(5,'Are you sure you wamt to delete this system [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) THEN
	  TYPE*,'System defintion ',SYS,' not deleted'
	  CALL XWAIT(2,2,ST)
	  RETURN
	ENDIF
C
C
	STYP=SSFATR(SYS)
	IND=SSFPTR(SYS)
	BROWS=SPGNBR
	SSFATR(SYS)=0
	DO 10 I=1,5
	SSFNUM(I,SYS)=0
10	CONTINUE
	SSFPTR(SYS)=0
	IF(STYP.EQ.FULSYS) GOTO 60
C
C
	LEN=SSFTAB(IND)*BROWS+1
	NEXT=IND+LEN
	DO 20 I=IND,IND+LEN-1
	SSFTAB(I)=0
20	CONTINUE
C
	DO 30 I=1,40
	IF(SSFPTR(I).GT.IND) SSFPTR(I)=SSFPTR(I)-LEN
30	CONTINUE
	SSFFPT=SSFFPT-LEN
C
C
	DO 40 I=NEXT,SFTABMAX
	SSFTAB(IND)=SSFTAB(I)
	IND=IND+1
40	CONTINUE
C
C
	DO 50 I=SSFFPT,SFTABMAX
	SSFTAB(I)=0
50	CONTINUE
60	CONTINUE
	TYPE*,'System bet ',SYS,' deleted'
	ST=0
	RETURN
	END
