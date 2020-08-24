C
C SUBROUTINE TCFSNP
C $Log:   GXAFXT:[GOLS]TCFSNP.FOV  $
C  
C V05 17-MAY-1996 HXK Update from Wojtek, Siew Mun
C V04 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                      and Comm 1/93 updateDEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-MAR-1990 TDM INITIAL RELEASE FOR DENMARK
C
C
C VIS_TCFSNP.FOR
C
C CARRYOVER SNAPSHOT FOR VISION
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
	SUBROUTINE TCFSNP(CDC,SER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INTEGER*4 LBUF(LREC*3),KEY(2)
	INTEGER*4 OFFSET, INDEX, BLOCK, ST1, K, ST, BLANK, SER, CDC
	DATA BLANK/'    '/
C
C
	SMODE=.TRUE.
	CALL FASTSET(BLANK,NEW(1,1),480)
C
C TRY TO READ THE TRANSACTION FROM THE CARRYOVER FILE
C
	IF(CDC.LT.0) CDC=DAYCDC
	IF(SER.LE.0) SER=1
	KEY(1)=CDC
	KEY(2)=SER
	CALL IOPEN(SFNAMES(1,TCF),1,LREC*2,LCDC,LSER*2-1,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,800) (SFNAMES(K,TCF),K=1,5),ST
	  RETURN
	ENDIF
	CALL IREAD(KEY,LBUF,1,ST)
	CALL ICLOSE(1,BIGBUF,ST1)
C
C TRY TO READ FROM DCF
C
	IF(ST.NE.0) THEN
          CALL IOPEN(SFNAMES(1,DCF),1,LREC*2,LCDC,LSER*2-1,ST)
          IF(ST.NE.0) THEN
            WRITE(CLIN23,800) (SFNAMES(K,DCF),K=1,5),ST
            RETURN
          ENDIF
          CALL IREAD(KEY,LBUF,1,ST)
          CALL ICLOSE(1,BIGBUF,ST1)
	  IF(ST.NE.0) THEN
	    WRITE(CLIN23,810) CDC,SER
	    RETURN
	  ENDIF
	ENDIF
C
C
	CALL GETBI(SER,BLOCK,INDEX,OFFSET)
	CALL LOGTRA(TRABUF,LBUF)
	CALL TRNSNP1(TRABUF,BLOCK,INDEX)
	RETURN
C
C
C
800	FORMAT(5A4,' open error> ',I4)
810	FORMAT(' Record not found for cdc - ',I4,' serial - ',I9)
	END
