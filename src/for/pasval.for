C PASVAL.FOR
C
C V05 16-MAY-2011 RXK Remember serial for validation inquiries
C V04 16-MAY-2011 FJG RETY erroneous logic
C V03 01-JAN-2010 FJG ePassive
C V02 12-FEB-2001 UXN WAIT_APUQUE added. TYPE* replaced with OPSTXT()
C V01 15-DEC-2000 CS  INITIAL RELEASE FOR PORTUGAL
C
C PASSIVE VALIDATION PROCESSING TASK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM PASVAL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C PARAMETER
C
	INTEGER*4 DAY_SUSPENDED
	PARAMETER (DAY_SUSPENDED = 1)
C
C LOCAL VARIABLES
C
	LOGICAL	  OKOPN
C
	INTEGER*2 I2CHKSM
C
	INTEGER*4 TASK,MESS(EDLEN),LSTSUP,ST
	INTEGER*4 LBUF(LREC*3),DUMMY,STAT
	INTEGER*4 TER,TYPE,BUF,I
	INTEGER*4 GNUM,GIND
	INTEGER*4 LSTSER,LASTRA(TRALEN),STATUS,GAME
	
	DATA LSTSUP/1/

	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C IDENTIFY THIS TASK
C
	TASK    = PSV
	MESS(1) = PSV
5	CONTINUE
	BASECHKSUM=IAND(DAYCDC,'FFFF'X)
C
10      CONTINUE
        IF(DAYSTS.EQ.DSCLOS) THEN
          CALL LISTTOP(DUMMY,REPQUEPAS(1,RQPASVAL),ST)
          IF(ST.NE.0) CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
        IF(DAYSTS.EQ.DSSUSP) THEN
           CALL CLSVAL		    !CLOSE VALIDATION FILES
           LSTSUP = DAY_SUSPENDED
           DO WHILE(1)
              CALL HOLD(0,ST)
              IF(DAYSTS.EQ.DSOPEN) GOTO 5
	      IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
	   ENDDO
        ENDIF
C
        CALL HOLD(0,ST)
30      CONTINUE
	IF(P(SUPVAL).NE.LSTSUP) THEN
           IF(P(SUPVAL).EQ.0) THEN
	      LSTSUP = P(SUPVAL)
	      CALL OPNVAL
     	   ELSE
	      CALL LISTTOP(DUMMY,REPQUEPAS(1,RQPASVAL),ST)
  	      IF(ST.NE.GLIST_STAT_GOOD) THEN
                 LSTSUP=P(SUPVAL)
	         CALL CLSVAL
              ENDIF
           ENDIF
        ENDIF
C
	CALL TOPQUE(TASK,BUF)
	CALL REPVAL_PAS(STAT,LSTSUP)
        IF(BUF.LE.0.AND.STAT.NE.0) GOTO 10
        IF(BUF.LE.0) GOTO 30
C
C BUILD TRANSACTION
C
	CALL FASTSET(0,TRABUF,TRALEN)

	TER  = HPRO(TERNUM,BUF)
	TYPE = HPRO(TRCODE,BUF)

	TRABUF(TSTAT) = GOOD
	TRABUF(TERR)  = NOER
        TRABUF(TTYP)  = TVAL
	TRABUF(TTER)  = TER
	TRABUF(TAGT)  = AGTTAB(AGTNUM,TER)
	TRABUF(TCDC)  = DAYCDC
	TRABUF(TSER)  = PRO(SERIAL,BUF)
	TRABUF(TTIM)  = PRO(TSTAMP,BUF)
	TRABUF(TSIZE) = HPRO(NUMLRC,BUF)

	CALL DVAL_PAS(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
C
C CHECK POSSIBLE SYNTAX ERRORS
C
	IF(P(SUPSYN).EQ.0.AND.SYNTERRCOD.NE.0) THEN
	  MESS(2) = TEGEN
	  MESS(3) = 10
	  MESS(4) = SYNTERRCOD
	  MESS(5) = TER
	  MESS(6) = TRABUF(TGAMTYP)
	  MESS(7) = TRABUF(TGAMIND)
	  MESS(8) = TRABUF(TSER)
	  CALL QUEMES(MESS)
	ENDIF
C
C CHECK AGENT AND SYSTEM STATUS
C
	IF(P(SUPVAL).NE.0)                     TRABUF(TERR)=SUPR

	IF(AGTHTB(AOPSTS,TER).NE.SIGNON)       TRABUF(TERR)=NOTON

        IF( TRABUF(TSTAT).EQ.GOOD .AND.
     *      (BTEST(AGTTAB(AGTTYP,TER),AGTTRN)) ) TRABUF(TERR)=TRIN

	IF(P(SYSSTS).EQ.SYSDRW)                TRABUF(TERR) = SDRW
	IF(P(SYSSTS).EQ.SYSDOR)                TRABUF(TERR) = SDOR

	IF(TRABUF(TERR).NE.NOER .AND. 
     *     TRABUF(TERR).NE.VINQ)               TRABUF(TSTAT)=REJT

	IF(TRABUF(TSTAT).EQ.GOOD) THEN 
C
C RETRY PROCESSING
C
	    IF(TRABUF(TTRN).EQ.AGTHTB(ATRNUM,TER).AND.
     *	       TRABUF(TSTAT).EQ.GOOD .AND. TRABUF(TERR).NE.VINQ) THEN

		LSTSER=AGTTAB(ALSTRA,TER)
		CALL RLOG(LSTSER,LBUF,TASK,STATUS)
CV02
		IF(STATUS.NE.0) THEN
		   CALL WAIT_APUQUE
		   CALL RLOG(LSTSER,LBUF,TASK,STATUS)
	        ENDIF
CEV02
		IF(STATUS.EQ.0) THEN
		  CALL LOGTRA(LASTRA,LBUF)

		  I2CHKSM = TRABUF(TCHK)
C                 IF( (AGTHTB(ACHKSM,TER) .EQ. I2CHKSM)      .OR.
C    *                (LASTRA(TTYP) .EQ. TVAL          .AND.
C    *		       LASTRA(TTRN) .EQ. TRABUF(TTRN)  .AND.
C    * 		       LASTRA(TSTAT).EQ. TRABUF(TSTAT) .AND.
C    * 		       LASTRA(TTER) .EQ. TRABUF(TTER)       )    ) THEN
                  IF( AGTHTB(ACHKSM,TER) .EQ. I2CHKSM        .AND.
     *                LASTRA(TTYP)       .EQ. TVAL           .AND.
     *                LASTRA(TCHK)       .EQ. TRABUF(TCHK)   .AND.
     *                LASTRA(TTRN)       .EQ. TRABUF(TTRN)   .AND.
     *                LASTRA(TSTAT)      .EQ. TRABUF(TSTAT)  .AND.
     *                LASTRA(TTER)       .EQ. TRABUF(TTER)   .AND.
     *                LASTRA(TGAM)       .EQ. TRABUF(TGAM) ) THEN

		      TRABUF(TSTAT) = REJT
		      TRABUF(TERR)  = RETY

		      DO I=TPTCK,TRALEN
		         TRABUF(I)=LASTRA(I)
		      ENDDO

		      CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
		      CALL OUTVAL_PAS(LASTRA,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
		      GOTO 100
		  ENDIF
	        ENDIF
	    ENDIF
C
C END OF RETRY PROCESSING
C

C
C PASOPNVAL TELL ME IF I NEED TO OPEN ANOTHER VPF FILE
C IT OCOURS WHEN WINPAS RUNS 
C
	    IF (PASOPNVAL.NE.0) THEN
C
C IF ALL PASSIVES ARE ENABLE TO VALIDATE, MEANS WINPAS HAS FINISHED IN ALL BOXES, SO I CAN OPEN THE NEW VPF FILE 
C
	        OKOPN = .TRUE.
		DO GIND=1, NUMPAS
		    GNUM = GTNTAB(TPAS,GIND)
		    IF (GNUM.GT.0) THEN
			IF (TSBIT(P(SUPGVA),GNUM)) OKOPN = .FALSE.
		    ENDIF
		ENDDO	
	
	        IF (OKOPN) THEN
		   CALL CLSVAL
		   CALL XWAIT(50,1,ST)
	   	   CALL OPNVAL
		   PASOPNVAL = 0  
	        ENDIF
	    ENDIF

	    CALL VALUPD_PAS(TRABUF)
	ENDIF
C
C UPDATE FINANCIAL INFORMATION
C
	GAME = TRABUF(TGAM)
	IF(TRABUF(TSTAT).EQ.GOOD) THEN                               !V05
     	   CALL UPDSUB(TRABUF)
           PERFRM(1,GAME)=PERFRM(1,GAME)+1
	ENDIF

	CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
C
C SEND MESSAGE BACK TO TERMINAL
C
	CALL OUTVAL_PAS(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
C
C SEND BUFFER TO APULOG( MEMORY UPDATES AND REQUEUEING TO LOGGER )
C AND TAKE IT OUT OF THIS TASK'S QUEUE
C
100	CONTINUE
	AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
	IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.RETY) THEN
	    AGTHTB(ACHKSM,TER)=-1
	ELSE
	  AGTHTB(ACHKSM,TER)=TRABUF(TCHK)
	ENDIF
	CALL QUETRA(APU,BUF)
	CALL DQUTRA(TASK,BUF)
C
C GET TRANSACTIONS ON TASK QUEUE
C
	GOTO 30
	END
C
C===============================================================================
C       SUBROUTINE TO OPEN FILES FOR PASVAL
C===============================================================================
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE OPNVAL
	IMPLICIT   NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	include 'inclib:pascom.def'	
	include 'inclib:prmvpf.def'	
	include 'inclib:pasiosubs.def'
C
C LOCAL VARIABLES
C
	integer*4 erro
	integer*4 poff
	integer*4 gind
!-------------------------------------------------------------------------------
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES VPFS
!-------------------------------------------------------------------------------
        record /stpaslun/ paslun(pagemi,numpas)
!        
        common /shpaslun/ paslun
!-------------------------------------------------------------------------------   
C
C FILL STRUCTURES WITH ACTIVE EMISSIONS
C
!       type*,iam(),'Opening VPF files'
        do gind=1,numpas
          do poff=1,pagemi
            if(passubsts(poff,gind).eq.pdrwval.or.passubsts(poff,gind).eq.pdrwpur) then
              if(paslun(poff,gind).emis.eq.0) then  
                if(filexist(cpasvpffil(poff,gind))) then
                  call find_available_lun(paslun(poff,gind).plun, erro)
                  if(erro.ne.0) then
                    call opstxt('Deactivating draw. Error in LUN ' // cpasvpffil(poff,gind))
                    passubsts(poff,gind) = pdrwerr
                  else
                    call iopen(pasvpffil(1,poff,gind),paslun(poff,gind).plun, vpflen*2,vfscdc,vfsser*2-1,erro)
                    if (erro.ne.0) then
                      call opstxt('Deactivating draw. Error opening file ' // cpasvpffil(poff,gind))
                      passubsts(poff,gind) = pdrwerr
                    else
                      paslun(poff,gind).emis = pasemis(poff,gind)
!                     type*,iam(),'Opened: ',cpasvpffil(poff,gind),paslun(poff,gind).plun,paslun(poff,gind).emis
                    endif
                  endif
                else
                  call opstxt('Deactivating draw. File not found ' // cpasvpffil(poff,gind))
                  passubsts(poff,gind) = pdrwerr
                endif
              endif
            endif
          enddo
        enddo
!        
	return
	end
C===============================================================================
C       SUBROUTINE TO READ FILES FOR PASVAL
C===============================================================================
C=======OPTIONS    /check=nooverflow
	subroutine reaval(poff,gind,emis,xnum,xser,xfra,v4buf_pas,erro)
	implicit   none
!
	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
	include 'inclib:global.def'
	include 'inclib:concom.def'
	include 'inclib:pasiosubs.def'
        include 'inclib:valpasfil.def'
        include 'inclib:prmhsh.def'
!-------INPUT-------------------------------------------------------------------
        integer*4     poff       ! Offset to paslun    
        integer*4     gind       ! Game index of NUMPAS
        integer*4     emis       ! For check draw number
        integer*4     xnum       ! Number
        integer*4     xser       ! Serie
        integer*4     xfra       ! Fraction
!                     v4buf_pas  ! Already defined in        
!-------OUTPUT------------------------------------------------------------------
        integer*4     erro       ! No error = 0 
!-------LOCAL VARIABLES---------------------------------------------------------
        integer*4     vkey(2)
!-------------------------------------------------------------------------------
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES VPFS
!-------------------------------------------------------------------------------
        record /stpaslun/ paslun(pagemi,numpas)
!        
        common /shpaslun/ paslun
!-------------------------------------------------------------------------------         
        erro = 0
        if(emis.le.0) then
          erro = 101
        elseif(emis.eq.paslun(poff,gind).emis) then
          if(paslun(poff,gind).plun.gt.0) then
            vkey(1) = xfra
            vkey(2) = ishft(xser,24) + xnum
            call iread(vkey,V4BUF_PAS,paslun(poff,gind).plun,erro)
          else
            erro = 102
          endif
          
        else
          erro = 101
        endif
!
   	return
   	end
C===============================================================================
C       SUBROUTINE TO READ FILES FOR PASVAL
C===============================================================================
C=======OPTIONS    /check=nooverflow
	subroutine wrival(poff,gind,emis,v4buf_pas,erro)
	implicit   none
!
	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
	include 'inclib:global.def'
	include 'inclib:concom.def'
	include 'inclib:pasiosubs.def'
        include 'inclib:valpasfil.def'
        include 'inclib:prmhsh.def'
!-------INPUT-------------------------------------------------------------------
        integer*4     poff       ! Offset to paslun    
        integer*4     gind       ! Game index of NUMPAS
        integer*4     emis       ! For check draw number
!                     v4buf_pas  ! Already defined in        
!-------OUTPUT------------------------------------------------------------------
        integer*4     erro       ! No error = 0 
!-------LOCAL VARIABLES---------------------------------------------------------
!-------------------------------------------------------------------------------
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES VPFS
!-------------------------------------------------------------------------------
        record /stpaslun/ paslun(pagemi,numpas)
!        
        common /shpaslun/ paslun
!-------------------------------------------------------------------------------         
        erro = 0
        if(emis.le.0) then
          erro = 201
        elseif(emis.eq.paslun(poff,gind).emis) then
          if(paslun(poff,gind).plun.gt.0) then
            call iwrite(V4BUF_PAS,paslun(poff,gind).plun,erro)
          else
            erro = 202
          endif
          
        else
          erro = 201
        endif
!
   	return
   	end   	
C===============================================================================
C       SUBROUTINE TO CLOSE FILES FOR PASVAL
C===============================================================================
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CLSVAL
	IMPLICIT   NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	include 'inclib:prmvpf.def'	
        include 'inclib:prmhsh.def'	
	INCLUDE 'INCLIB:CONCOM.DEF'
	include 'inclib:pascom.def'	
	include 'inclib:pasiosubs.def'
C
C LOCAL VARIABLES
C
	integer*4 erro
	integer*4 poff
	integer*4 gind
!
        integer*4 xbuf(I4BUCSIZ)
!-------------------------------------------------------------------------------
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES VPFS
!-------------------------------------------------------------------------------
        record /stpaslun/ paslun(pagemi,numpas)
!        
        common /shpaslun/ paslun
!------------------------------------------------------------------------------- 
C
C CLOSE PASSIVE LOTTERY VALIDATION FILE
C
        type*,iam(),'Closing VPF files'
        do gind=1,numpas
          do poff=1,pagemi
            if(paslun(poff,gind).emis.gt.0) then
              call iclose(paslun(poff,gind).plun, xbuf, erro)
              paslun(poff,gind).emis = 0
            endif
            paslun(poff,gind).plun = 0
          enddo
        enddo
!        
	return
	end
C****************************************
C VALIDATION REPROCESS SUBROUTINE
C****************************************
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE REPVAL_PAS(STAT,OPENST)
	IMPLICIT   NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C
C ROUTINE PARAMETERS
C
	INTEGER*4 STAT, OPENST
C
C LOCAL VARIABLES
C
	INTEGER*4 BUF, MESS(EDLEN), STATUS, ST
C
C
        CALL LISTTOP(BUF,REPQUEPAS(1,RQPASVAL),STAT)
        IF(STAT.EQ.GLIST_STAT_EMPTY) RETURN
C
	IF(OPENST.NE.0) THEN
	    CALL OPNVAL
	    OPENST=0
	ELSEIF (PASOPNVAL.NE.0) THEN  !WINPAS RAN...WE MUST OPEN ONE MORE VPF
	    CALL CLSVAL
	    CALL XWAIT(50,1,STAT)
	    CALL OPNVAL
	    PASOPNVAL = 0  
	ENDIF
C
        CALL LOGTRA(TRABUF,PRO(WRKTAB,BUF))
	CALL RVALUPD_PAS(TRABUF,STATUS)
C
	IF  (STATUS.NE.VWINNER) THEN	!READ ERROR OR NOT WINNER
	    MESS(1)=PSV
	    MESS(2)=TEGEN
	    MESS(3)=9
	    MESS(4)=STATUS
	    MESS(5)=TRABUF(TSER)
	    CALL QUEMES(MESS)
	ENDIF
C
        CALL RELBUF(BUF)
        CALL RTL(BUF,REPQUEPAS(1,RQPASVAL),ST)
C
	RETURN
	END
