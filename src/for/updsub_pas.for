C UPDSUB_PAS.FOR
C
C V02 01-JAN-2010 FJG ePassive
C V01 14-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C INPUT
C     VALREC - VALIDATION INTERNAL FORMAT.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPDSUB_PAS(VALREC)
	IMPLICIT NONE
!
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C
C LOCAL VARIABLES
C
	integer*4 gind
	integer*4 emis
	integer*4 div
	integer*4 shr
	integer*4 przoff
	integer*4 divoff
	integer*4 poff
!-------Function	
	integer*4 getpagemi
!	
	call dlogpas(valrec,vdetail)

	gind = valrec(vgind)

	do przoff = 1,valrec(vpzoff)
          emis = vdetail(vdrw,przoff) 
          div  = vdetail(vdiv,przoff)
          shr  = vdetail(vshr,przoff)
          poff = getpagemi(emis,gind)
          if(poff.gt.0.and.poff.lt.PAGEMI) paspad(div,poff,gind) = paspad(div,poff,gind) + shr 
	enddo
!-------NOW UPDATE PLANO REAL
        do divoff = 1,pagdiv+pagedv
          if(pasdivpln(1,divoff,poff,gind).eq.valrec(vpamt)) then
            pasdivpln(2,divoff,poff,gind) = pasdivpln(2,divoff,poff,gind) + 1
            exit
          endif
        enddo
!
	return
 	end
