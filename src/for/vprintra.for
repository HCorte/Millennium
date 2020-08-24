C VPRINTRA.FOR
C
C V04 28-NOV-2013 SCML Updating data for payment orders
C V03 07-OCT-2013 SCML Net pay amount added
C V02 01-JAN-2010 FJG  ePassive
C V01 08-DEC-2000 ANG  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO PRINT TRANSACTIONS IN VMIR FORMAT
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VPRINTRA(VALREC, PUNIT, VDETAIL, VERSION)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:NAMCMD.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
C  LOCAL VARIABLES
C
	INTEGER*2   DAT(12)
	INTEGER*2   AUX

        INTEGER*4   VERSION
	INTEGER*4   I
	INTEGER*4   PAGE
	INTEGER*4   LINCNT
        INTEGER*4   PUNIT
	INTEGER*4   DIV
	INTEGER*4   SHR
	INTEGER*4   GTYP
	INTEGER*4   GIND
	INTEGER*4   DRW
	INTEGER*4   TSHARES(40)
	INTEGER*4   SSER
	INTEGER*4   SCHK
	INTEGER*4   NUMDIVS(MAXGAM)
	INTEGER*4   BIGDIVS(10,MAXGAM)
	INTEGER*4   KIK

	INTEGER*4   PAYAMT
	INTEGER*4   NETPAYAMT !V03

CCC        LOGICAL       PRINT

	CHARACTER*16  STATAUX
        CHARACTER*123 CPRIZES(VMAX)
        CHARACTER*2  HAS_OP
        INTEGER*4    NET_AMT
        INTEGER*4    KNET_AMT

	INTEGER*4 MAXPRTLN
	PARAMETER (MAXPRTLN = 59)
C
C  COMMON AREA
C
	COMMON /VMIR/ LINCNT
C
C********************************************************************
C  CODE STARTS HERE
C
CCC        PRINT = .FALSE.
        AUX   = 1
	CALL FASTSET(0,TSHARES,40)

	GTYP = VALREC(VGTYP)
	GIND = VALREC(VGIND)

	PAYAMT = VALREC(VPAMT)
	NETPAYAMT = VALREC(VOPSAMT) ! V03
C
C PRINT VLF INFORMATION 
C

	DO 100 I=1,VALREC(VPZOFF)
	    IF (GTYP.EQ.TPAS) THEN
	       DRW = VDETAIL(VDRW,I)
	       DIV = VDETAIL(VDIV,I)
	       SHR = VDETAIL(VSHR,I)
	       TSHARES(DIV) = TSHARES(DIV) + SHR

	       WRITE (CPRIZES(AUX),804) DRW,DIV,SHR                

	    ELSE
	       DRW = VDETAIL(VDRW,I)
	       DIV = VDETAIL(VDIV,I)
	       SHR = VDETAIL(VSHR,I)
	       KIK = VDETAIL(VKIK,I)
	       TSHARES(DIV) = TSHARES(DIV) + SHR

	       IF(I.EQ.1) THEN
		  WRITE (CPRIZES(AUX),805) DRW,DIV,SHR,                 
     *				           VALREC(VEXP),VALREC(VKEXP),KIK
	       ELSE
		  WRITE (CPRIZES(AUX),800) DRW,DIV,SHR,KIK                  
	       ENDIF
	    ENDIF
	    AUX = AUX + 1
100	CONTINUE

CCC	IF (.NOT.PRINT) RETURN


	DAT(VCDC) = VALREC(VSCDC)
	CALL CDATE(DAT)
	CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
C
C  CHECK IF NEW PAGE IS NEEDED
C
	IF (LINCNT.GT.((MAXPRTLN-4)-(AUX-1))) THEN
                IF(PAGE .GE. 9999) PAGE = 0 
		CALL TITLE('VALIDATION FILE REPORT','    VMIR',
     *                    VERSION,PUNIT,PAGE,DAYCDC,0,0)

		! IF IT IS A PASSIVE LOTTERY GAME TYPE, PRINT OTHER LAY-OUT
		IF (GTYP .EQ. TPAS) THEN
		  WRITE(PUNIT,905)
		ELSE
		  WRITE(PUNIT,900)
		ENDIF

	        LINCNT = 7
	ENDIF
C
C  PRINT VLF LINES
C
	WRITE(STATAUX,903) VALST(VALREC(VSTAT))

        IF(VALREC(VOPSCNT) .NE. 0) THEN
            HAS_OP=' Y'
        ELSE
            HAS_OP=' N'
        ENDIF
        
	! IF IT IS A PASSIVE LOTTERY GAME TYPE, PRINT OTHER LAY-OUT
	IF (GTYP .EQ. TPAS) THEN
	  WRITE(PUNIT,906) VALREC(VEXTR),		      ! EMISSION NUMBER
     *			   VALREC(VTCKT),		      ! TICKET NUMBER
     *		           VALREC(VSERN),		      ! SERIE NUMBER
     *		           VALREC(VPFRAC),		      ! TEN
     *                     VALREC(VGAM),		      ! GAME NUMBER
     *		           GSNAMES(VALREC(VGAM)),	      ! GAME NAME 
     *		           VALREC(VCCDC),		      ! CASHING CDC
     *		           STATAUX,			      ! STATUS
     *                     VALREC(VCTER),		      ! CASHING TER
     *                     CMONY(PAYAMT,13,VALUNIT),	      ! PAY AMOUNT
     *                     CMONY(NETPAYAMT,13,VALUNIT),	    ! NET PAY AMOUNT !V03
     *                     99999-VALREC(VVALN),               ! ALGORITMO
     *		           VALREC(VPRGCDC),		      ! PURGE CDC
     *		           VALREC(VOFFTER),		      ! OFFLINE TERMINAL
     *		           VALREC(VPASTYP),     	      ! TICKET TYPE: ON/OFFLINE    
     *		           VALREC(VSTER),       	      ! SELLING TERMINAL
     *		           VALREC(VSCDC),       	      ! SELLING CDC
     *		           VALREC(VSSER)		      ! SELLING SERIAL
	ELSE
	  IF(VALREC(VOPSAMT) .EQ. 0) THEN
	     NET_AMT = PAYAMT
	  ELSE
	     NET_AMT = VALREC(VOPSAMT) 
	  ENDIF
          IF(VALREC(VKOPSAMT) .EQ. 0) THEN
             KNET_AMT = VALREC(VKPAMT)
          ELSE
             KNET_AMT = VALREC(VKOPSAMT) 
          ENDIF
          
	  WRITE(PUNIT,901) VALREC(VSCDC),		  ! SELLING CDC
     *			   VALREC(VSSER),		  ! SELLING SERIAL
     *		           VALREC(VSTER),		  ! SELLING TERMINAL 
C**     *		           IAGT_NO(VALREC(VSAGT)),	  ! SELLING AGENT
     *			   DAT(VJUL),			  ! 
     *			   SSER,			  ! EXTERNAL SERIAL
     *			   SCHK,			  ! 
     *                     VALREC(VGAM),		  ! GAME NUMBER
     *		           GSNAMES(VALREC(VGAM)),	  ! GAME NAME 
     *		           VALREC(VCCDC),		  ! CASHING CDC
     *		           STATAUX,			  ! STATUS
     *			   DRW,				  ! DRAW NUMBER
     *                     VALREC(VCTER),		  ! CASHING TER
     *                     CMONY(PAYAMT,13,VALUNIT),	  !PAY AMOUNT
     *                     CMONY(NET_AMT,11,VALUNIT),     !NET AMOUNT
     *                     HAS_OP,	                  !OP ?
     *                     CMONY(VALREC(VKPAMT),13,VALUNIT),	  !KIKER AMT
     *                     CMONY(KNET_AMT,11,VALUNIT),	  !OPS AMOUNT
     *		           VALREC(VPRGCDC)		  ! PURGE CDC
	ENDIF

        LINCNT = LINCNT + 1

	DO 1000 I=1,AUX-1
	    WRITE(PUNIT,810) CPRIZES(I)
	    LINCNT = LINCNT + 1
1000	CONTINUE
	WRITE(PUNIT,FMT='(X)')
C
C  PRINT DIVISIONS 
C
	IF (VALREC(VGTYP).EQ.TSPT) 
     *   CALL SPTHDR2(PUNIT,GIND,LINCNT,BIGDIVS,TSHARES)

C**	IF (VALREC(VGTYP).EQ.TKNO) THEN
C**	    WRITE(PUNIT,*)
C**	    LINCNT = LINCNT + 1
C**	ENDIF

C**********************************************************************
C***********************************************************************
C***********************************************************************
C  FORMAT STATEMENTS
C
C**800 	FORMAT('DRAW: ',I6,'  DIVISION: ',I3,'  NUMBER OF SHARES: ',I6,
C**     *	       ' AMOUNT:',I8,' CDC (ONLY KENO):',I5)

800 	FORMAT('DRAW: ',I6,'  DIVISION: ',I3,'  NUMBER OF SHARES: ',I6,' VKIK: ',I1)
804 	FORMAT('EXTR: ',I6,'  DIVISION: ',I3,'  NUMBER OF SHARES: ',I6)
   
C**805 	FORMAT('DRAW: ',I6,'  DIVISION: ',I3,'  NUMBER OF SHARES: ',I6,
C**     *	       ' AMOUNT:',I8,' CDC (ONLY KENO):',I5, ' PV:',I5.5, ' VEXP:',I4)

805 	FORMAT('DRAW: ',I6,'  DIVISION: ',I3,'  NUMBER OF SHARES: ',I6,
     *	       ' VEXP:',I4,' VKEXP:',I4,' VKIK: ',I1)
!810	FORMAT(9X,A)
810	FORMAT(6X,A) !V03
C----+------------------------------------------------------------------
C V04| Updating data for payment orders
C----+------------------------------------------------------------------
C900     FORMAT(/,' SCDC    SSER   STER   SAGT',8X,
C     *         'EXT.SERIAL      GAME  CCDC STATUS  DRW#',
C     *         '   CTER        PRIZE  OP AMT  KIK PRZ   KOP AMT PRGCDC',
C     *         /,1X,127('='),/)
900     FORMAT(/
     *          ,' '
     *          ,'SCDC'
     *          ,' '
     *          ,'     SSER'
     *          ,' '
     *          ,' STER'
     *          ,' '
     *          ,'      EXT.SERIAL'
     *          ,' '
     *          ,'  GAME'
     *          ,' '
     *          ,'CCDC'
     *          ,' '
     *          ,'STAT'
     *          ,' '
     *          ,' DRAW#'
     *          ,' '
     *          ,' CTER'
     *          ,' '
     *          ,'        PRIZE'
     *          ,' '
     *          ,'  NET PRIZE'
     *          ,' '
     *          ,'OP'
     *          ,' '
     *          ,'    KIK PRIZE'
     *          ,' '
     *          ,' KNET PRIZE'
     *          ,' '
     *          ,'PRGCDC',
     *         /,1X,129('='),/)

C**901	FORMAT(1X,I4,1X,I9,1X,I5,1X,A11,1X,I3.3,'-',I8.8,'-',I3.3,1X,I2,A4,
C**     *		  1X,I4,1X,A4,1X,I6,1X,I5,1X,A13,1X,A13,1X,A11,1X,I4)

C901     FORMAT(1X,I4,1X,I9,1X,I5,1X,I3.3,'-',I8.8,'-',I3.3,1X,I2,A4,
C     *            1X,I4,1X,A4,1X,I6,1X,I5,1X,A13,1X,A11,1X,A13,1X,A11,1X,I4)
901     FORMAT(
     *           1X                            !
     *          ,I4                            !SCDC
     *          ,1X                            !
     *          ,I9                            !SSER
     *          ,1X                            !
     *          ,I5                            !STER
     *          ,1X                            !
     *          ,I3.3,'-',I8.8,'-',I3.3        !EXT.SERIAL
     *          ,1X                            !
     *          ,I2                            !GAME
     *          ,A4                            !GAME
     *          ,1X                            !
     *          ,I4                            !CCDC
     *          ,1X                            !
     *          ,A4                            !STATUS
     *          ,1X                            !
     *          ,I6                            !DRW#
     *          ,1X                            !
     *          ,I5                            !CTER
     *          ,1X                            !
     *          ,A13                           !PRIZE
     *          ,1X                            !
     *          ,A11                           !NET AMT
     *          ,1X                            !
     *          ,A2                            !OP
     *          ,1X                            !
     *          ,A13                           !KIK PRZ
     *          ,1X                            !
     *          ,A11                           !KOP AMT
     *          ,1X                            !
     *          ,I6                            !PRGCDC
     *  )

C----+------------------------------------------------------------------
C V04| Updating data for payment orders
C----+------------------------------------------------------------------


903	FORMAT(A4)

904	FORMAT(8X,<NUMDIVS(VALREC(VGAM))>(I7))

C-------->>V03 --------------------------
!905	FORMAT(/,' EMISSION   TICKET   SER   FRA    GAME    CCDC   STATUS   ',
!     *         ' CTER         PRIZE  VALNUM   PRGCDC  VOFFTER  T   STER  SCDC   SSER',
!     *	       /,1X,127('='),/)
!906	FORMAT(1X,I8,3X,I5,4X,I2,4X,I2,3X,I2,1X,A4,3X,I4,4X,A4,5X,I5,1X,
!     *		  A13,2X,I5,4X,I4,5X,I5,2X,I1,2X,I5,1X,I5,1X,I8)

905	FORMAT(/,'  EMIS  TICKT SER FRA    GAME   CCDC STATUS   CTER         PRIZE',
     *       '     NET PRIZE  VALNUM  PRGCDC  VOFFTER  T   STER  SCDC     SSER',
     *	       /,1X,127('='),/) !V03

906	FORMAT(1X,I5,  ! EMISSION      !V03
     *     2X,I5,  ! TICKET NUMBER
     *     2X,I2,  ! SERIE
     *     2X,I2,  ! FRACTION
     *     2X,I2,  ! GAME NUMBER
     *     1X,A4,  ! GAME SHORT NAME
     *     2X,I4,  ! CASHING CDC
     *     3X,A4,  ! STATUS 
     *     2X,I5,  ! CASHING TERMINAL
     *     1X,A13, ! PRIZE AMOUNT
     *     1X,A13, ! NET PRIZE AMOUNT
     *     3X,I5,  ! VALIDATION NUMBER
     *     4X,I4,  ! PURGE CDC
     *     4X,I5,  ! OFFLINE TERMINAL
     *     2X,I1,  ! TICKET TYPE: 1 - ONLINE / 0 - OFFLINE
     *     2X,I5,  ! SELLING TERMINAL (ONLINE)
     *     1X,I5,  ! SELLING CDC (ONLINE)
     *     1X,I8)  ! SELLLING SERIAL (ONLINE)
C-------- V03<<--------------------------

907	FORMAT(1X,I8,1X,I2,1X,I10,1X,I2,1X,A4,1X,I4,2X,A4,2X,I5,1X,
     *		  A13,1X,A11,1X,A11,3X,I4)

908     FORMAT(A11)

909     FORMAT('           ')
	END

C
C=========================================================================
C
C                                                             SPTHDR
C
        SUBROUTINE  SPTHDR2(PUNIT,GIND,LINCNT,BIGDIVS,TSHARES)

        IMPLICIT NONE
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'

        INTEGER*4     PUNIT,GIND,LINCNT,BIGDIVS(10,MAXGAM)

        INTEGER*4     MAXSPTDIV
        PARAMETER     (MAXSPTDIV = 2)
        CHARACTER*6   HEAD(10)
        INTEGER*4     I,K
        INTEGER*4     TSHARES(*)
        INTEGER*4     NUMDIVS
        INTEGER*4     GNUM

C..........................................................................

        GNUM = GTNTAB(TSPT,GIND)
        IF (GNUM.EQ.0) RETURN

        DO 100 I=1, 10
           HEAD(I) = '      '
100     CONTINUE

        NUMDIVS = 0
        DO 1000 I = SPTMAX(GIND),(SPTMAX(GIND)-SPTDIV(GIND)+1),-1
	   NUMDIVS = NUMDIVS + 1
           BIGDIVS(NUMDIVS,GNUM) = 1
           WRITE(HEAD(NUMDIVS),9008) I,SPTMAX(GIND)
           IF (NUMDIVS.EQ.MAXSPTDIV) GOTO 1999
1000    CONTINUE
1999    CONTINUE
	WRITE(PUNIT, 9001) ((HEAD(K),TSHARES(K)), K=1, NUMDIVS)           
        LINCNT = LINCNT + 2
        RETURN

9001	FORMAT(8X,<NUMDIVS>(A6,'=',I5,6X)/)
9008	FORMAT(I2.2,'/',I2.2,' ')
        END

