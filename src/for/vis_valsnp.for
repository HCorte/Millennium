C
C SUBROUTINE VALSNP
C
C VALSNP.FOR
C
C V12 16-MAR-2011 GPW NUMAGT=12288
C V11 16-JAN-2001 EPH REMOVE VK2PAMT
C V10 17-MAY-1999 UXN Super Triple added.
C V09 17-MAY-1996 HXK Update from Wojtek, Siew Mun
C V08 15-DEC-1995 HXK Fix for Double, Couple games
C V07 18-NOV-1994 HXK Added Bingo subgames
C V06 03-OCT-1993 HXK Change TOTPAY to include refunds.
C V05 08-JUL-1993 SXH Released for Finland
C V04 13-JUN-1993 HXK added AGTINF.DEF
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C VALIDATION SNAPSHOT FOR VISION
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE VALSNP(CDC,SER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
        INTEGER*4  CDC                      !
        INTEGER*4  SER                      !
C
        ! variables
	INTEGER*4  KEY(2)                   !
	INTEGER*4  YESNO(2)                 !
        INTEGER*4  YNJOK(4)                 !
        INTEGER*4  SUBG(2)                  !
	INTEGER*4  J                        !
	INTEGER*4  IND                      !
	INTEGER*4  S                        !
	INTEGER*4  D                        !
	INTEGER*4  U                        !
	INTEGER*4  I                        !
	INTEGER*4  TOTPAY                   !
	INTEGER*4  ST1                      !
	INTEGER*4  K                        !
	INTEGER*4  PRIZES(7,VMAX)           !
	BYTE       BPRIZES(28,VMAX)           !
	INTEGER*4  ST                       !
	INTEGER*4  BLANK                    !
	INTEGER*4  B                        !
	INTEGER*4  UNIT                     !
	INTEGER*4  R                        !
	INTEGER*4  GTYP                     !
	INTEGER*4  TOTTAX                   !
        INTEGER*4  SG                       !
C

	CHARACTER*28 CPRIZES(VMAX)          !

	EQUIVALENCE (PRIZES,CPRIZES,BPRIZES)
	DATA      YESNO/'  no',' yes'/
        DATA      YNJOK/'  no',' 1st',' 2nd',' 1+2'/
        DATA      SUBG /' bAB',' bFH'/  
	DATA      BLANK/'    '/
	LOGICAL	  EXTRA_WINNER
C
C
	SMODE=.TRUE.
	EXTRA_WINNER = .FALSE.
	CALL FASTSET(BLANK,PRIZES(1,1),7*VMAX)
C
C TRY TO READ THE TRANSACTION FROM THE VALIDATION FILE
C
	IF(CDC.LT.0) CDC=DAYCDC
	IF(SER.LE.0) SER=1

	KEY(1)=CDC
	KEY(2)=SER
	CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,800) (SFNAMES(K,VLF),K=1,5),ST
	    RETURN
	ENDIF

	CALL IREAD(KEY,V4BUF,1,ST)
	CALL ICLOSE(1,BIGBUF,ST1)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,810) CDC,SER
	    RETURN
	ENDIF
C
C
	CALL LOGVAL(VALREC,V4BUF)
	CALL DLOGVAL(VALREC,VDETAIL)
	TOTPAY = VALREC(VPAMT) + VALREC(VKPAMT) + VALREC(VRAMT)
	TOTTAX = VALREC(VTAMT) + VALREC(VKTAMT)
	GTYP   = VALREC(VGTYP)
C
C ENCODE DETAIL PRIZE DATA
C
	DO I = 1, VALREC(VPZOFF)
            K = 1
            IF (VDETAIL(VKIK,I) .EQ.1)K=2
            IF (VDETAIL(VKI2,I) .EQ.1)K=3
            IF (VDETAIL(VKIK,I) .EQ.1 .AND. VDETAIL(VKI2,I) .EQ.1)K=4
	    U  = VDETAIL(VUPD,I) + 1
	    D  = VDETAIL(VDIV,I)
	    S  = VDETAIL(VSHR,I)
	    B  = VDETAIL(VBDR,I)+1
	    R  = VDETAIL(VREF,I)+1
            SG = VDETAIL(VSUB,I)
	    IF(GTYP.EQ.1.AND.SG.EQ.1) EXTRA_WINNER = .TRUE.
	    IF(GTYP.NE.TSCR .AND. GTYP.NE.TWIT .AND. GTYP.NE.TTSL .AND.
     *         GTYP.NE.TBNG .AND. GTYP.NE.TDBL .AND. GTYP.NE.TCPL .AND.
     *         GTYP.NE.TSSC .AND. GTYP.NE.TTRP .AND. GTYP.NE.TSTR) THEN
	        WRITE (CPRIZES(I),820) I,YESNO(U),YNJOK(K),YESNO(B),D,S
            ELSEIF(GTYP.EQ.TBNG) THEN
                WRITE (CPRIZES(I),820) I,YESNO(U),YNJOK(K),SUBG(SG),D,S
	    ELSE
	        UNIT=VALUNIT
	        IF(R.EQ.2) UNIT=BETUNIT
	        WRITE (CPRIZES(I),830) I,YESNO(U),YNJOK(K),YESNO(R),
     *                                 CMONY(S,11,UNIT)
	    ENDIF
        END DO
C
C
	WRITE(CLIN1,901)
	WRITE(CLIN2,902) VALST(VALREC(VSTAT)),VALREC(VGAM),
     *	                 GTNAMES(VALREC(VGTYP)),
     *                   CMONY(VALREC(VPAMT),11,VALUNIT)
	WRITE(CLIN3,903) VALREC(VFRAC),VALREC(VPZOFF),
     *	                 VALREC(VGIND),
     *                   CMONY(VALREC(VKPAMT),11,VALUNIT)    !-VALREC(VK2PAMT),11,VALUNIT)  !V11
	WRITE(CLIN4,904) VALREC(VSCDC),VALREC(VSTER),   
     *	                 VALREC(VSSER),CMONY(0,11,VALUNIT)   !VALREC(VK2PAMT),11,VALUNIT)   !V11
	WRITE(CLIN5,905) VALREC(VCCDC),VALREC(VCTER),
     *	                 VALREC(VCSER),CMONY(TOTPAY,11,VALUNIT)
	WRITE(CLIN6,906) VALREC(VLCDC),VALREC(VLTER),
     *	                  VALREC(VLSER),CMONY(TOTTAX,11,VALUNIT)
	WRITE(CLIN7,907) VALREC(VEXP),VALREC(VKEXP),
     *	                 VALREC(VWCDC), CMONY(VALREC(VRAMT),11,BETUNIT)
        WRITE(CLIN8,908) VALREC(VBNKID),VALREC(VBNKNUM),VALREC(VKGME)
	IF(EXTRA_WINNER) THEN
	    WRITE(CLIN9,'(A)') '***This ticket is an extra draw winner***'
	ENDIF
	IF(GTYP.EQ.TNBR) THEN
	  WRITE(CLIN10,911)
	ELSEIF(GTYP.EQ.TSCR .OR. GTYP.EQ.TWIT .OR. GTYP.EQ.TTSL .OR.
     *         GTYP.EQ.TDBL .OR. GTYP.EQ.TCPL .OR.
     *         GTYP.EQ.TSSC .OR. GTYP.EQ.TTRP .OR.
     *         GTYP.EQ.TSTR) THEN
	  WRITE(CLIN10,912)
        ELSEIF(GTYP.EQ.TBNG) THEN
          WRITE(CLIN10,913)
	ELSE
	  WRITE(CLIN10,909)
	ENDIF

	IND=11
	DO I = 1, 21, 3
	    WRITE(XNEW(IND),910) ((BPRIZES(J,K),J=1,26),K=I,I+2)
	    IND=IND+1
        END DO

	RETURN
C
C     ==================== FORMAT STATEMENTS ===================
C
800	FORMAT(5A4,' open error> ',I4)
810	FORMAT(' Record not found for cdc - ',I4,' serial - ',I9)
820	FORMAT(I3,A4,A4,A4,1X,I3,I4)
830	FORMAT(I3,A4,A4,A4,A11)
901	FORMAT('Validation snapshot ')
902	FORMAT('Status     ',A4,5X,'Game       ',I4,5X,
     *	       'Gtype   ',A8,4X,'Regpay ',A11)
903	FORMAT('Fractions  ',I4,5X,'# wins     ',I4,5X,
     *	       'Gindx   ',I8,4X,'Kikpay1',A11)
904	FORMAT('Scdc       ',I4,5X,'Ster      ',I5,5X,
     *	       'Sser   ',I9,4X,'Kikpay2',A11)
905	FORMAT('Ccdc       ',I4,5X,'Cter      ',I5,5X,
     *	       'Cser   ',I9,4X,'Totpay ',A11)
906	FORMAT('Lcdc       ',I4,5X,'Lter      ',I5,5X,
     *	       'Lser   ',I9,4X,'Wintax ',A11)
907	FORMAT('Expdrw     ',I4,5X,'Kexpdrw    ',I4,5X,
     *	       'Winsel cdc  ',I4,4X,'Refund ',A11)
908	FORMAT('Bnk ID ',I8,5X,'Bank # ',I8,5X,'Kgame       ',I4)
909	FORMAT('Prz Upd Kik Bns Div Shr    ','Prz Upd Kik Bns Div Shr    ',
     *         'Prz Upd Kik Bns Div Shr ')
910	FORMAT(26A1,1X,26A1,1X,26A1)
911     FORMAT(2('Prz Upd Kik Bns Pol Shr   '),
     *           'Prz Upd Kik Bns Pol Shr ')
912     FORMAT('Prz Upd Kik Ref     Amount ','Prz Upd Kik Ref     Amount ',
     *         'Prz Upd Kik Ref     Amount')
913     FORMAT('Prz Upd Kik Sub Div Shr    ','Prz Upd Kik Sub Div Shr    ',
     *         'Prz Upd Kik Sub Div Shr ')

	END
