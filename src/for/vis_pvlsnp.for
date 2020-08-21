C VIS_PVLSNP.FOR
C
C V01 08-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C PASSIVE VALIDATION SNAPSHOT FOR VISION
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
C Copyright 1990,1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PVLSNP(gind,emis,tnum)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
C PARAMETERS
C
	integer*4     emis
        integer*4     gind
        integer*4     tnum
C
        integer*4     xemis
        integer*4     xgind
        integer*4     xtnum
C
        integer*4     mnum
        integer*4     mser
        integer*4     mfra        
C
        integer*4     vlun  
        integer*4     erro          
        integer*4     junk    
        integer*4     mlin
        integer*4     tmp1
        integer*4     tmp2
        integer*4     vkey(2)
        integer*4     vbuf(I4BUCSIZ)
C
C LOCAL VARIABLES
C
	INTEGER*4 PRZS
	INTEGER*4 DRAW
	INTEGER*4 SHRS
	INTEGER*4 MDIV
	INTEGER*4 PCDC
	INTEGER*4 GNUM
	INTEGER*4 BLNK
	INTEGER*4 PTAB(6,VMAX)
	INTEGER*4 PAMT
C
	CHARACTER*24 CPTAB(VMAX)
	EQUIVALENCE (PTAB,CPTAB)

	DATA      BLNK/'    '/

	CALL FASTSET(BLNK,PTAB(1,1),6*VMAX)
C
        if(tnum.gt.0.and.tnum.lt.999991010) then
          write(xnew(01),9000) tnum
        else
          write(xnew(01),9001)
        endif
C        
        if(xemis.ne.emis.or.gind.ne.xgind.or.xtnum.ne.tnum) then
          if(emis.le.0.or.emis.gt.pagemi) then
            write(xnew(03),9100) 'Invalid emission index value  ',emis
            return
          endif
          if(gind.le.0.or.gind.gt.numpas) then
            write(xnew(03),9100) 'Invalid game index for Passive',gind
            return
          endif
          if(passts(emis,gind).ne.GFINAL) then
            write(xnew(03),9100) 'Invalid draw status           ',passts(emis,gind)
            return
          endif
          mfra = mod(tnum,100)
          if(mfra.le.0.or.mfra.gt.pasnoffra(emis,gind)) then
            write(xnew(03),9100) 'Invalid fraction for emission ',mfra
            return
          endif
          mser = mod((tnum/100),100)
          if(mser.le.0.or.mser.gt.pasnumser(emis,gind)) then
            write(xnew(03),9100) 'Invalid serie for emission    ',mser
            return
          endif
          mnum = tnum/10000
          if(mnum.lt.0.or.mnum.ge.pasnumtck(emis,gind)) then
            write(xnew(03),9100) 'Invalid number for emission   ',mnum
            return
          endif        
          if(.not.filexist(cpasvpffil(emis,gind))) then
            write(xnew(03),9100) 'TPF file does not exist       ',pasdraw(emis,gind)
            return
          endif        
          call find_available_lun(vlun,erro)
          if(erro.ne.0) then
            write(xnew(03),9100) 'Error getting logical unit    ',erro
            return
          endif        
          call iopen(pasvpffil(1,emis,gind),vlun,VPFLEN*2,VFSCDC,VFSSER*2-1,erro)
          if(erro.ne.0) then
            write(xnew(03),9100) 'Error opening VPF file        ',erro
            return
          endif                    
          vkey(1) = mfra
          vkey(2) = ishft(mser,24) + mnum
          call iread(vkey,V4BUF_PAS,vlun,erro)
          call iclose(vlun,vbuf,junk)
          if(erro.ne.0) then
            if(erro.eq.ERRRNF) then
              write(xnew(03),9100) 'RECORD NOT FOUND. Not a winner',tnum
            else
              write(xnew(03),9100) 'Error reading VPF file        ',erro
            endif
            return
          endif                    
          xemis = emis
          xgind = gind
          xtnum = tnum
        endif
C
C DECODE RECORD TO VALREC
C
	CALL LOGPAS(VALREC,V4BUF_PAS)
	CALL DLOGPAS(VALREC,VDETAIL)

	PAMT = VALREC(VPAMT)
C
C ENCODE DETAIL PRIZE DATA
C
	DO PRZS = 1, VALREC(VPZOFF)
	  DRAW = VDETAIL(VDRW,PRZS)
	  MDIV  = VDETAIL(VDIV,PRZS)
	  SHRS = VDETAIL(VSHR,PRZS)
	  WRITE (CPTAB(PRZS),9008) PRZS, DRAW, MDIV, SHRS
	ENDDO
C********
C IF PRIZE IS ALREADY PAID,
C SHOW CORRECT PURGE CDC
C********
	IF (VALREC(VSTAT).EQ.VCASH) THEN
	    GNUM = GTNTAB(VALREC(VGTYP),VALREC(VGIND))
	    PCDC = VALREC(VCCDC) + PRGDAY(GNUM)
	ELSE
	    PCDC = VALREC(VPRGCDC)
	ENDIF

        WRITE(CLIN3,9002) DRAW,mnum,mser,mfra
	WRITE(CLIN4,9003) VALST(VALREC(VSTAT)),
     *			  VALREC(VGAM),
     *			  GTNAMES(VALREC(VGTYP)),
     *			  CMONY(VALREC(VPAMT),11,VALUNIT)

	WRITE(CLIN5,9004) VALREC(VPZOFF),
     *			  VALREC(VGIND),
     *			  VALREC(VEXP),
     *			  VALREC(VWCDC)

	WRITE(CLIN6,9005) VALREC(VCCDC),
     *			  VALREC(VCTER),
     *			  VALREC(VCSER),PCDC
        IF(VALREC(VPASTYP).EQ.VPASOFF) THEN
	  WRITE(CLIN7,90061) 99999-VALREC(VVALN),VALREC(VSTER)
	ELSE
	  WRITE(CLIN7,90062) VALREC(VSCDC),
     *	                     VALREC(VSTER),
     *	                     VALREC(VSSER)
	ENDIF

	WRITE(CLIN9,9007)
C
C PRINT DETAILED PTAB
C
	mlin = 10

	DO PRZS = 1,23,3    !TOP=VMAX-2
	  WRITE(XNEW(mlin),9009) ((PTAB(tmp1,tmp2),tmp1=1,6),tmp2=PRZS,PRZS+2)
	  mlin = mlin + 1
	ENDDO

	RETURN
C
C ==================== format STATEMENTS ===================
C
9000    format('Passive Validation snapshot: ',I9.9)
9001    format('Passive Validation snapshot')
9002    format(3X,'Emission',I6,     3X,'TickNum ',I6,3X,'Serie ',I8,  3X,'Fraction',I11)
9003    format(3X,'Status  ',2X,A4,  3X,'Game    ',I6,3X,'Gtype ',A8,  3X,'Regpay  ',A11)
9004    format(3X,'#prizes ',I6,     3X,'Gind    ',I6,3X,'EmsExp  ',I6,3X,'WinCDC  ',I11)
9005    format(3X,'Ccdc    ',I6,     3X,'Cter    ',I6,3X,'Cser  ',I8,  3X,'Prgcdc  ',I11)
90061   format(3X,'Valn Off',1X,I5.5,3X,'Ster    ',I6,3X,17X,'Ticket:     OFFLINE')
90062   format(3X,'Scdc    ',I6,     3X,'Ster    ',I6,3X,'Sser  ',I8,  3X,'Ticket:      ONLINE')
9007    format(3X,3('Prize Emision DIV Share  '))             
9008    format(2X, I3, 2X, I6, X, I3, 2X, I4)
9009    format(3X,3(6A4,X))
9100    format('ERROR: ',A30,I10)
	END
