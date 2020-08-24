C
C V07 16-MAR-2011 GPW NUMAGT=12288
C V06 13-NOV-2002 CPH PUT *** ON CHECK DIGITS PLACE
C V05 22-FEB-2001 EPH INCLUDE OP DATA
C V04 16-JAN-2001 EPH REMOVE VK2PAMT
C V03 24-MAR-2000 UXN CMONY changed to CSMONY
C V02 25-NOV-1999 UXN WAP added for TEBE.
C V01 19-NOV-1999 OXK Moved from VALDIS.FOR
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE VALDI(VALREC,LU)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'             
C
        INTEGER*4 J, S, D, U, I, TOTPAY, K, R
        INTEGER*4 PRIZES(7,VMAX), BLANK, B, UNIT
        INTEGER*4 GTYP, TOTTAX, LU, SG, DR
        INTEGER*4  YESNO(2)                 !
        INTEGER*4  YNJOK(4)                 !
        INTEGER*4  SUBG(2)                  !
C
        INTEGER*4 GIND
        INTEGER*4 FPN
        LOGICAL   FROZEN                                            !V06
C
	CHARACTER OP*2                      !'OP'=IS PART OF AN OP / '  '=ISN'T    !V05
        INTEGER*2 DATE(12)
        INTEGER*4 ASA,SCDC,SSER,SCHK
C
        CHARACTER*28 CPRIZES(VMAX)
	BYTE	     BPRIZES(28,VMAX)
        EQUIVALENCE (PRIZES,CPRIZES,BPRIZES)
        DATA      YESNO/' no ',' ys '/
        DATA      BLANK/'    '/
        DATA      YNJOK/'  no',' 1st',' 2nd',' 1+2'/
        DATA      SUBG /' bAB',' bFH'/  
	CHARACTER*1 EXYES(0:1) /' ','y'/
	INTEGER*4   EX
C	CHARACTER*7 TEBE(0:3)
C	DATA	    TEBE/'       ','    VRU','    WWW','    WAP'/

C	INTEGER*4   IND
C
C
        CALL DLOGVAL(VALREC,VDETAIL)
        TOTPAY=VALREC(VPAMT)+VALREC(VKPAMT)+VALREC(VRAMT)
        TOTTAX=VALREC(VTAMT)+VALREC(VKTAMT)
        GTYP=VALREC(VGTYP)
        GIND=VALREC(VGIND)                                      !V06
        FROZEN=.FALSE.                                          !V06
        FPN=0                                                   !V06
C
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
        DATE(VCDC)=VALREC(VSCDC)
        CALL CDATE(DATE)
        SCDC=DATE(VJUL)
C
C ENCODE DETAIL PRIZE DATA
C
        CALL FASTSET(BLANK,PRIZES(1,1),7*VMAX)
        DO 200 I=1,VALREC(VPZOFF)
          K = 1
          IF (VDETAIL(VKIK,I) .EQ.1)K=2
          IF (VDETAIL(VKI2,I) .EQ.1)K=3
          IF (VDETAIL(VKIK,I) .EQ.1 .AND. VDETAIL(VKI2,I) .EQ.1)K=4

          U=VDETAIL(VUPD,I)+1
          D=VDETAIL(VDIV,I)
          S=VDETAIL(VSHR,I)
          B=VDETAIL(VBDR,I)+1
          R=VDETAIL(VREF,I)+1
          SG = VDETAIL(VSUB,I)	  
          DR = VDETAIL(VDRW,I)	  
          IF (VDETAIL(VOP,I).EQ.1) THEN        !V05
             OP = 'OP'   ! This detail is part of an OP 
          ELSE
             OP = '  '
          ENDIF
	  IF(GTYP.EQ.TLTO) EX = IAND(SG,'01'X)
          IF(GTYP.NE.TSCR .AND. GTYP.NE.TWIT .AND. GTYP.NE.TTSL .AND.
     *       GTYP.NE.TBNG .AND. GTYP.NE.TDBL .AND. GTYP.NE.TCPL .AND.
     *       GTYP.NE.TSSC .AND. GTYP.NE.TTRP .AND. GTYP.NE.TSTR)THEN
              WRITE (CPRIZES(I),8201)DR,YESNO(U),YNJOK(K),YESNO(B),D,S,EXYES(EX)
          ELSEIF(GTYP.EQ.TBNG) THEN
              WRITE (CPRIZES(I),820) DR,YESNO(U),YNJOK(K),SUBG(SG),D,S
          ELSE
            UNIT=VALUNIT
            IF(R.EQ.2) UNIT=BETUNIT
            WRITE (CPRIZES(I),830) DR,YESNO(U),YNJOK(K),YESNO(R),
     *                             CSMONY(S,10,UNIT), OP       !V05
          ENDIF
200     CONTINUE
C
        IF(LU.EQ.5) THEN
          ASA = ' '
          CALL CLRSCR(5)
        ELSE
          ASA = '1'
        ENDIF
C	IND = VALREC(VFTBSF)
C        WRITE(LU,901) ASA,SCDC,SSER,SCHK,TEBE(IND)
C        WRITE(LU,901) ASA,SCDC,SSER,SCHK    V06
	 WRITE(LU,901) ASA,SCDC,SSER        !VO6
       WRITE(LU,902) VALST(VALREC(VSTAT)),VALREC(VGAM),
     *               GTNAMES(VALREC(VGTYP)),
     *               CSMONY(VALREC(VPAMT),11,VALUNIT)
        WRITE(LU,903) VALREC(VFRAC),VALREC(VPZOFF),
     *                   VALREC(VGIND),
     *                   CSMONY(VALREC(VKPAMT),11,VALUNIT)     !-VALREC(VK2PAMT),11,VALUNIT)  !V04
        WRITE(LU,904) VALREC(VSCDC),VALREC(VSTER),    
     *                   VALREC(VSSER),CSMONY(0,11,VALUNIT)    !VALREC(VK2PAMT),11,VALUNIT)   !V04
        WRITE(LU,905) VALREC(VCCDC),VALREC(VCTER),
     *                   VALREC(VCSER),CSMONY(TOTPAY,11,VALUNIT)
        WRITE(LU,906) VALREC(VLCDC),VALREC(VLTER),
     *                    VALREC(VLSER),CSMONY(TOTTAX,11,VALUNIT)
        WRITE(LU,907) VALREC(VEXP),VALREC(VKEXP),
     *                   VALREC(VWCDC), CSMONY(VALREC(VRAMT),11,BETUNIT)
        WRITE(LU,908)  VALREC(VBNKID),VALREC(VBNKNUM),VALREC(VKGME)
        WRITE(LU,915)  VALREC(VOPSCNT), VALREC(VOPSAMT),VALREC(VKOPSAMT)        !V05

        IF(GTYP.EQ.TNBR) THEN
          WRITE(LU,911)
        ELSEIF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TTSL.OR.
     *         GTYP.EQ.TDBL .OR. GTYP.EQ.TCPL.OR. GTYP.EQ.TSSC.OR.
     *         GTYP.EQ.TTRP .OR. GTYP.EQ.TSTR) THEN
          WRITE(LU,912)
        ELSEIF(GTYP.EQ.TBNG) THEN
          WRITE(LU,913)
        ELSE
          WRITE(LU,909)
        ENDIF
C
        DO I = 1, VALREC(VPZOFF), 3
            WRITE(LU,910) ((BPRIZES(J,K),J=1,25),K=I,I+2)
        END DO
C
        WRITE(LU,'()')
C                                                                   !V06...
        WRITE(LU,'()')
C                                                                   !...V06

        RETURN
C
820     FORMAT(I4,A3,A4,A4,1X,I3,I4)
8201    FORMAT(I4,A3,A4,A4,1X,I3,I4,1X,A1)
830     FORMAT(I4,A3,A4,A4,A10,1X,A2)        !V05
C901     FORMAT(A1,79('-'),/,
C     *         16X,'### Ticket serial number: ',I3.3,'-',I8.8,'-',I3.3,
C     *         ' ###',9X,A7,/,1X,79('-'))
C901     FORMAT(A1,79('-'),/,                                             V06
C     *         16X,'### Ticket serial number: ',I3.3,'-',I8.8,'-',I3.3,  V06
C     *         ' ###',9X,/,1X,79('-'))                                   V06
901     FORMAT(A1,79('-'),/,                                              !V06
     *         16X,'### Ticket serial number: ',I3.3,'-',I8.8,'-','***',  !V06
     *         ' ###',9X,/,1X,79('-'))                                    !V06
902     FORMAT(1X,'Status     ',A4,5X,'Game       ',I4,5X,
     *            'Gtype  ',A8,5X,'Regpay ',A11)
903     FORMAT(1X,'Fractions  ',I4,5X,'# wins     ',I4,5X,
     *            'Gindx   ',I8,4X,'Kikpay1',A11)
904     FORMAT(1X,'Scdc       ',I4,5X,'Ster      ',I5,5X,
     *            'Sser   ',I9,4X,'Kikpay2',A11)
905     FORMAT(1X,'Ccdc       ',I4,5X,'Cter      ',I5,5X,
     *            'Cser   ',I9,4X,'Totpay ',A11)
906     FORMAT(1X,'Lcdc       ',I4,5X,'Lter      ',I5,5X,
     *            'Lser   ',I9,4X,'Wintax ',A11)
907     FORMAT(1X,'Expdrw     ',I4,5X,'Kexpdrw    ',I4,5X,
     *            'Winsel cdc  ',I4,4X,'Refund ',A11)
908     FORMAT(1X,'Bnk ID ',I8,5X,'Bank # ',I8,5X,'Kgame       ',I4)
909     FORMAT(1X,2('Drw Upd Kik Bns Div Shr X  '),
     *              'Drw Upd Kik Bns Div Shr X')
910     FORMAT(1X,2(25A1,2X),25A1)
911     FORMAT(1X,2('Drw Upd Kik Bns Pol Shr    '),
     *              'Drw Upd Kik Bns Pol Shr')
912     FORMAT(1X,2('Drw Upd Kik Ref    Amount  '),
     *              'Drw Upd Kik Ref    Amount')
913     FORMAT(1X,2('Drw Upd Kik Sub Div Shr    '),
     *              'Drw Upd Kik Sub Div Shr')
915     FORMAT(1X,'OPs count', 4X, I2, 5X,  'OPsAmt', X, I8, 5X, 
     *            'OPsAmKik', I8)            ! V05

        END
