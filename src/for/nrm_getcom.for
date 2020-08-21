C
C SUBROUTINE GETCOM
C $Log:   GXAFXT:[GOLS]GETCOM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   29 Aug 1995 11:15:24   PXB
C  Commission on ravi v5 on track terminals.
C  
C     Rev 1.1   24 Apr 1995 17:01:44   HXK
C  Merge of V5 development with March 10th 1995 bible
C  
C     Rev 1.1   02 Mar 1995 12:34:36   HXK
C  Changed commision calculation for Ontrack agents
C  
C     Rev 1.0   21 Jan 1993 16:24:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getcom.for **
C
C GETCOM.FOR
C
C V04 01-JAN-2010 FJG ePASSIVE
C V03 13-FEB-92 GCAN CHANGED ROUNDING ROULES (ROUND UP ON LAST DECIMAL)
C V02 12-NOV-91 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO CALCULATE COMMISSION
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETCOM(AMOUNT,TYPE,GAME,COMAMT,TOTCOM,GTYP,GIND,TER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4 GAME, AMOUNT, TYPE, TEMP, UNIT
	INTEGER*4 COMAMT(2),TOTCOM(2)
        INTEGER*4 GTYP,GIND,TER
C
C SALES COMMISSION (BY GAME)
C
	IF(TYPE.EQ.TWAG) THEN
          IF(GTYP.NE.0.AND.GIND.NE.0.AND.TER.NE.0) THEN
             TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_BETUNIT)*
     *                      CALPER(COMGAM(GAME)))
             COMAMT(1)=TEMP/DYN_BETUNIT
             COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
             TOTCOM(1)=TOTCOM(1)+COMAMT(1)
             TOTCOM(2)=TOTCOM(2)+COMAMT(2)
             TEMP=TOTCOM(2)/DYN_BETUNIT
             TOTCOM(1)=TOTCOM(1)+TEMP
             TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
             RETURN
          ELSE
	     TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_BETUNIT)*
     *                   CALPER(COMGAM(GAME)))
	     COMAMT(1)=TEMP/DYN_BETUNIT
	     COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
	     TOTCOM(1)=TOTCOM(1)+COMAMT(1)
	     TOTCOM(2)=TOTCOM(2)+COMAMT(2)
	     TEMP=TOTCOM(2)/DYN_BETUNIT
	     TOTCOM(1)=TOTCOM(1)+TEMP
	     TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
	     RETURN
           ENDIF
	ENDIF
C
C VALIDATION/REFUND COMMISSION
C
C
        IF(TYPE.EQ.TVAL.OR.TYPE.EQ.TREF) THEN
	  UNIT=DYN_VALUNIT
	  IF(TYPE.EQ.TREF) UNIT=DYN_BETUNIT
          TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(UNIT)*
     *                CALPER(COMTYP(TVAL)))
          COMAMT(1)=TEMP/DYN_BETUNIT
          COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
          TOTCOM(1)=TOTCOM(1)+COMAMT(1)
          TOTCOM(2)=TOTCOM(2)+COMAMT(2)
          TEMP=TOTCOM(2)/DYN_BETUNIT
          TOTCOM(1)=TOTCOM(1)+TEMP
          TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
          RETURN
        ENDIF
C
C CLAIM COMMISSION
C
	IF(TYPE.EQ.TRET) THEN
	  COMAMT(1)=AMOUNT*COMTYP(TRET)
	  COMAMT(2)=0
	  TOTCOM(1)=TOTCOM(1)+COMAMT(1)
	  RETURN
	ENDIF
C
C HIGH TIER WINNERS COMMISSION
C
        IF(TYPE.EQ.-1) THEN
          TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_VALUNIT)*
     *                CALPER(HVCRAT))
          COMAMT(1)=TEMP/DYN_BETUNIT
          COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
          TOTCOM(1)=TOTCOM(1)+COMAMT(1)
          TOTCOM(2)=TOTCOM(2)+COMAMT(2)
          TEMP=TOTCOM(2)/DYN_BETUNIT
          TOTCOM(1)=TOTCOM(1)+TEMP
          TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
          RETURN
        ENDIF
	RETURN
	END
