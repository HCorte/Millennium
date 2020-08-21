C
C PROGRAM VALSTS
C $Log:   GXAFXT:[GOLS]VALSTS.FOV  $
C  
C     Rev 1.1   17 May 1996 11:44:08   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.0   21 Jan 1993 18:00:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - valsts.for **
C
C VALSTS.FOR
C
C V01 18-AUG-92 WLM INITIAL RELEASE FOR NETHERLANDS
C
C PROGRAM TO CHANGE STATUS OF A VALIDATION FILE RECORD
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
	PROGRAM VALSTS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 INPVER
	INTEGER*4 KEY(2),YESNO(2),J, S, D, U, I, TOTPAY, ST1, K
	CHARACTER*24 CPRIZES(VMAX)
	INTEGER*4 PRIZES(6,VMAX), ST, BLANK, SER, CDC, B, UNIT, R
	INTEGER*4 SCRAM, CHECK, CHKERR, STS
	INTEGER*4 GTYP, TOTTAX, EXT, BIGBUF(2500)
	EQUIVALENCE (PRIZES,CPRIZES)
	DATA      YESNO/'  no',' yes'/
	DATA      BLANK/'    '/

        CHARACTER*8 PASPAS
        CHARACTER*20 PASENT
        EQUIVALENCE (PASPAS,PASENT)
 
C
        CALL COPYRITE
C
        CALL PASSWORD(5,PASENT)
        IF(PASPAS.NE.'ZQYAWE1C') THEN
          TYPE*,' ********** ACCESS DENIED SORRY **********'
          TYPE*,' YOU MUST HAVE CORRECT PASSWORD FOR ACCESS'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C	
	CALL FASTSET(BLANK,PRIZES(1,1),6*VMAX)
C
C GET DATE OF THE WAGER
C
	WRITE(5,700) IAM()
	CALL INPDAT(CDC,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
C
C GET SERIAL NUMBER
C
	CALL INPNUM('Enter External serial number: ',
     *	             SCRAM,1,99999999,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter Check digits          : ',
     *	             CHECK,0,999,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CHKERR=INPVER(CDC,SCRAM,SER,CHECK)
	IF(CHKERR.NE.0) THEN
	  WRITE(5,710) IAM()
	  CALL GSTOP(GEXIT_OPABORT)
	ENDIF
C
C TRY TO READ THE TRANSACTION FROM THE VALIDATION FILE
C
	IF(CDC.LT.0) CDC=DAYCDC
	IF(SER.LE.0) SER=1
	KEY(1)=CDC
	KEY(2)=SER
	CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,800) (SFNAMES(K,VLF),K=1,5),ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	CALL IREAD(KEY,V4BUF,1,ST)
	IF(ST.NE.0) THEN
	  WRITE(5,810) CDC,SER
	  CALL ICLOSE(1,BIGBUF,ST1)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C
	CALL LOGVAL(VALREC,V4BUF)
	CALL DLOGVAL(VALREC,VDETAIL)
	TOTPAY=VALREC(VPAMT)+VALREC(VKPAMT)
	TOTTAX=VALREC(VTAMT)+VALREC(VKTAMT)
	GTYP=VALREC(VGTYP)
C
C ENCODE DETAIL PRIZE DATA
C
	DO 100 I=1,VALREC(VPZOFF)
	K=VDETAIL(VKIK,I)+1
	U=VDETAIL(VUPD,I)+1
	D=VDETAIL(VDIV,I)
	S=VDETAIL(VSHR,I)
	B=VDETAIL(VBDR,I)+1
	R=VDETAIL(VREF,I)+1
	IF(GTYP.NE.TSCR.AND.GTYP.NE.TWIT.AND.GTYP.NE.TTSL) THEN
	  WRITE (CPRIZES(I),820) I,YESNO(U),YESNO(K),YESNO(B),D,S
	ELSE
	  UNIT=VALUNIT
	  IF(R.EQ.2) UNIT=BETUNIT
	  WRITE (CPRIZES(I),830) I,YESNO(U),YESNO(K),YESNO(R),
     *          CMONY(S,9,UNIT)
	ENDIF
100	CONTINUE
C
C
	CALL CLRSCR(5)
	WRITE(5,901)
	WRITE(5,902) VALST(VALREC(VSTAT)),VALREC(VGAM),
     *	                 GTNAMES(VALREC(VGTYP)),
     *                   CMONY(VALREC(VPAMT),11,VALUNIT)
	WRITE(5,903) VALREC(VPZOFF),
     *	                 VALREC(VGIND),
     *                   CMONY(VALREC(VKPAMT),11,VALUNIT)
	WRITE(5,904) VALREC(VSCDC),VALREC(VSTER),
     *	                 VALREC(VSSER),CMONY(TOTPAY,11,VALUNIT)
	WRITE(5,905) VALREC(VCCDC),VALREC(VCTER),
     *	                 VALREC(VCSER),CMONY(TOTTAX,11,VALUNIT)
	WRITE(5,906) VALREC(VLCDC),VALREC(VLTER),
     *	                  VALREC(VLSER),CMONY(VALREC(VRAMT),11,BETUNIT)
	WRITE(5,907) VALREC(VEXP),VALREC(VKEXP),
     *	                 VALREC(VWCDC),VALREC(VKGME)
	IF(GTYP.EQ.TNBR) THEN
	  WRITE(5,911)
	ELSEIF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TTSL) THEN
	  WRITE(5,912)
	ELSE
	  WRITE(5,909)
	ENDIF
	DO 200 I=1,24,3
	WRITE(5,910) ((PRIZES(J,K),J=1,6),K=I,I+2)
200	CONTINUE
	CALL PRMYESNO('Do you want this record? [Y/N]',EXT)
	IF(EXT.NE.1) CALL GSTOP(GEXIT_OPABORT)
	CALL CLRSCR(5)
	WRITE(5,720)
	CALL INPNUM('Enter new status code > ',STS,
     *		     VNOWIN,VPRPOST,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	VALREC(VSTAT) = STS
	CALL DVALLOG(VALREC,VDETAIL)
	CALL VALLOG(VALREC,V4BUF)
	CALL IWRITE(V4BUF,1,ST)
	CALL ICLOSE(1,BIGBUF,ST1)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
	CALL CLRSCR(5)
	WRITE(5,721)
	WRITE(5,902) VALST(VALREC(VSTAT)),VALREC(VGAM),
     *	                 GTNAMES(VALREC(VGTYP)),
     *                   CMONY(VALREC(VPAMT),11,VALUNIT)
	WRITE(5,903) VALREC(VPZOFF),
     *	                 VALREC(VGIND),
     *                   CMONY(VALREC(VKPAMT),11,VALUNIT)
	WRITE(5,904) VALREC(VSCDC),VALREC(VSTER),
     *	                 VALREC(VSSER),CMONY(TOTPAY,11,VALUNIT)
	WRITE(5,905) VALREC(VCCDC),VALREC(VCTER),
     *	                 VALREC(VCSER),CMONY(TOTTAX,11,VALUNIT)
	WRITE(5,906) VALREC(VLCDC),VALREC(VLTER),
     *	                  VALREC(VLSER),CMONY(VALREC(VRAMT),11,BETUNIT)
	WRITE(5,907) VALREC(VEXP),VALREC(VKEXP),
     *	                 VALREC(VWCDC),VALREC(VKGME)
	IF(GTYP.EQ.TNBR) THEN
	  WRITE(5,911)
	ELSEIF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TTSL) THEN
	  WRITE(5,912)
	ELSE
	  WRITE(5,909)
	ENDIF
	DO 300 I=1,24,3
	WRITE(5,910) ((PRIZES(J,K),J=1,6),K=I,I+2)
300	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
C     ==================== FORMAT STATEMENTS ===================
C
700	FORMAT(1X,A,'Specify wager date.')
710	FORMAT(1X,A,'Wrong Check digits.')
720	FORMAT(1X,'Validation status codes:',//,5X
     *         '0 - VNOWIN     5 - VCXL      10 - VCLAM  ',/,5X,
     *         '1 - VUNCSH     6 - VHOLD     11 - VCLAMX ',/,5X,
     *         '2 - VCASH      7 - VNOPAY    12 - VPRPAY ',/,5X,
     *         '3 - VCASHX     8 - VNOPRZ    13 - VPPNPZ ',/,5X,
     *         '4 - VDEL       9 - VPOST     14 - VPRPOST',/)
721	FORMAT(1X,'Validation record after change')
800	FORMAT(1X,5A4,' open error> ',I4)
810	FORMAT(1X,' Record not found for cdc - ',I4,' serial - ',I8)
820	FORMAT(1X,I3,A4,A4,A4,1X,I3,I4)
830	FORMAT(I3,A4,A4,A4,A9)
901	FORMAT(1X,'Validation record before change')
902	FORMAT(1X,'Status     ',A4,5X,'Game       ',I4,5X,
     *	       'Gtype  ',A8,5X,'Regpay ',A11)
903	FORMAT(1X,'            ',8X,'# wins     ',I4,5X,
     *	       'Gindx  ',I8,5X,'Kikpay ',A11)
904	FORMAT(1X,'Scdc       ',I4,5X,'Ster       ',I4,5X,
     *	       'Sser  ',I9,5X,'Totpay ',A11)
905	FORMAT(1X,'Ccdc       ',I4,5X,'Cter       ',I4,5X,
     *	       'Cser  ',I9,5X,'Wintax ',A11)
906	FORMAT(1X,'Lcdc       ',I4,5X,'Lter       ',I4,5X,
     *	       'Lser  ',I9,5X,'Refund ',A11)
907	FORMAT(1X,'Expdrw     ',I4,5X,'Kexpdrw    ',I4,5X,
     *	       'Winsel cdc ',I4,5X,'Kgame  ',I10)
909	FORMAT(1X,2('Prz Upd Kik Bns Div Shr   '),
     *           'Prz Upd Kik Bns Div Shr ')
910	FORMAT(2(6A4,2X),6A4,2X)
911     FORMAT(1X,2('Prz Upd Kik Bns Pol Shr   '),
     *           'Prz Upd Kik Bns Pol Shr ')
912     FORMAT(1X,2('Prz Upd Kik Ref Amount    '),
     *           'Prz Upd Kik Ref Amount  ')
	END
