C VALUPD_PAS.FOR
C
C V07 11-APR-16 SCML Do not allow non-privileged terminals in the retailer network to 
C                    validate the physical tickets with prizes >= €1.000,00
C                    satisfying the following conditions:
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 15/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 16/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 17/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 18/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 19/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 20/2017
C                     - pTickets >= 50.000 of 3rd and 4th series of LP 21/2017
C V06 07-FEB-14 SCML Batch validation: 
C                     - do not set ticket status to NPAYBAT when agent sap is 007456 (Portal SAP).
C                     - privileged agents can pay prizes greater than € 4.9999,99.
C V05 25-SEP-13 SCML Privileged agents now can make bank transfers up to € 4.999,99
C                    Non-privileged agents can´t pay in batch mid-tier prizes in cash
C V04 11-NOV-10 FJG TPOFFTER terminal gets all privileges
C               FJG Add addtional controls for OutOfBounds
C V03 01-Jan-10 FJG ePassive
C     06-Aug-10 FJG Last minute SCML change of mind about INQUIRIES
C V02 09-SEP-05 FRP RFSS #26 (Change Control Order to Always First Check the Code Validity).
C V01 12-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO RETRIEVE AND UPDATE TRANSACTIONS FROM
C THE VALIDATION PASSIVE FILES.
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
C PROJECT LOTARIA CLASSICA EM SERIES (PLCS)
C ADD NEW FIELD ON FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VALUPD_PAS(TRABUF)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
C
C FUNCTIONS
C
        integer*4 getpagemi
C
C LOCAL VARIABLES
C
	INTEGER*4 GIND
	INTEGER*4 GTYP
	INTEGER*4 GAM
	INTEGER*4 EMIS
	INTEGER*4 ST
	integer*4 TCKSFOUND
	INTEGER*4 TER
	integer*4 TCKS
	INTEGER*4 OFFSET_TRA
	INTEGER*4 VST
	integer*4 vnum
	integer*4 vser
	integer*4 vfra
	integer*4 tsta
	integer*4 eoff
	integer*4 NUMTCKS
	integer*4 SYNTERR
C
	NUMTCKS = TRABUF(TPTCK)
C
C INITIALIZE NUMBER OF TICKETS ON VALIDATION FILE
C
	TCKSFOUND = 0	
C
C GET TERMINAL NUMBER
C
	TER = TRABUF(TTER)
C
C LOOP THRU ALL TICKETS TO BE VALIDATED
C
	DO TCKS = 1,NUMTCKS
C
C PREPARE VALIDATION RECORD TO BE USED,
C CLEARING IT
C
	    CALL FASTSET(0,VALREC,SIZEOF(VALREC)/4)
C
C TRY TO READ THE TRANSACTION FROM THE CORRECT VALIDATION FILE.
C
	    OFFSET_TRA = (TCKS-1)*OFFTRA
C
C VERIFY IF WE HAVE ALREADY DONE WINNER SELECTION
C FOR THIS PARTICULAR EMISSION AND IF HAS NOT BEEN PURGED (PARTIAL OR TOTAL)
C
	    IF (TRABUF(TPSTS1 + OFFSET_TRA) .EQ. NWIN) THEN
              gind = TRABUF(TGAMIND)
              emis = TRABUF(TPEMIS1 + offset_tra)
              eoff = getpagemi(emis,gind)
              vnum = trabuf(TPNUM1+offset_tra)
              vser = trabuf(TPSER1+offset_tra)
              vfra = trabuf(TPTEN1+offset_tra)              
              call reaval(eoff,gind,emis,vnum,vser,vfra,v4buf_pas,st)
              if(st.eq.0) then
                tcksfound = tcksfound + 1
		call logpas(valrec,v4buf_pas)                
		vst  = valrec(VSTAT)
		gam  = valrec(VGAM)
		gtyp = valrec(VGTYP)
		gind = valrec(VGIND)	
                if(trabuf(tgamtyp).ne.gtyp.or.
     *             trabuf(tgamind).ne.gind.or.
     *             trabuf(tgam).ne.gam) then
		   trabuf(tstat) = rejt		
		   trabuf(terr)  = synt
		   synterr       = 20
		   call valerr(st,vpf,trabuf(tser),10,tpas,trabuf(tgamind),synterr,ter)
                   return
                endif
!+++++++++++++++INI CHECK STATUSES++++++++++++++++++++++++++++++++++++++++++++++
                if(vst.eq.VCASH.or.vst.eq.VBANK) then
		  trabuf(TPSTS1+offset_tra) = apad
		elseif(vst.eq.vdel) then
		  trabuf(TPSTS1+offset_tra) = utkt
		elseif(vst.eq.vcxl) then
		  trabuf(TPSTS1+offset_tra) = rtkt
      		elseif(vst.eq.VUNCSH .or. vst.eq.VPRPAY) then
!                 If we have the chance to check RETAFTDRW great 
       		  if(passaltab(eoff,gind).gt.0) then ! Check in memory bitmap
                    if(gind.eq.PSBCLA) then
                      tsta = pasnumcla(vnum,passaltab(eoff,gind)).billet(vser,vfra)
                    else
                      tsta = pasnumpop(vnum,passaltab(eoff,gind)).billet(vser,vfra)                      
                    endif
                  else ! If not, during the night they will be updated. Little risk.
                    tsta = pbilwon  
      		  endif
!
                  select case(tsta)      		  
!+++++++++++++++++If draw date, statuses are not updated to pbilwon/pbilwof. So winners and sold
                  case ( pbilwon, pbilwof, pbilson, pbilsof )
                    if(trabuf(TVEPTYP).ne.valrec(VPASTYP)) then
                      trabuf(TPSTS1+offset_tra) = BSERL
                    else
!=====================Old layout as it was======================================
                      if(trabuf(TVEPTYP).eq.0) then
                        if(trabuf(TPKEY1+offset_tra).ne.valrec(VVALN)) then
                          trabuf(TPSTS1+offset_tra) = BSERL
                        else
                          trabuf(TPSTS1+offset_tra) = VWINNER
			  call chkred_pas(trabuf,valrec,offset_tra)
                        endif
!=====================New layout================================================
                      else
                        if(trabuf(TVSER).ne.valrec(VSSER).or.trabuf(TVCDC).ne.valrec(VSCDC)) then
                          trabuf(TPSTS1+offset_tra) = BSERL
                        else
                          trabuf(TPSTS1+offset_tra) = VWINNER    
                          call chkred_pas(trabuf,valrec,offset_tra)
                        endif                  
                      endif    
!===============================================================================
                      if(trabuf(TPSTS1+offset_tra).eq.VWINNER) then
			if(trabuf(tstat).eq.good.and.trabuf(terr).ne.vinq) then
                	  valrec(VCCDC)   = trabuf(TCDC)
                	  valrec(VCTER)   = trabuf(TTER)
                          valrec(VCSER)   = trabuf(TSER)
                	  valrec(VOFFTER) = trabuf(TPOFFTER)   !OFFLINE PAYMENTS ON PRIV. TERMS.
                	  if(trabuf(TVTYPE).eq.VPNBNK) then
                            valrec(VSTAT)   = VBANK                          	  
                          else                	    
                            valrec(VSTAT)   = VCASH                          	  
                          endif
                          call paslog(valrec,v4buf_pas)
                          call wrival(eoff,gind,emis,v4buf_pas,st)
                          if(st.ne.0) then
                            call valerr(st,vpf,trabuf(tser),10,tpas,trabuf(tgamind),0,ter)
                            trabuf(tpsts1+offset_tra) = nwsel ! Better this than NO WINNER
                          else
                            call updsub_pas(valrec)
                          endif
                        endif
                      endif
                    endif
      		  case ( pbilxof, pbilrof, pbilkof, pbilcof )
		    trabuf(TPSTS1+offset_tra) = rtkt
		  case( pbilnot, pbilonl, pbilcon, pbilxon, pbiloff )
		    trabuf(TPSTS1+offset_tra) = utkt		    
      		  end select
      		endif
!+++++++++++++++FIN CHECK STATUSES++++++++++++++++++++++++++++++++++++++++++++++
              elseif(st.eq.ERRRNF) then
		TRABUF(TPPAY1+OFFSET_TRA)   = 0  
                TRABUF(TPSTS1 + OFFSET_TRA) = NWIN		              
              else
		call valerr(st,vpf,trabuf(tser),10,tpas,trabuf(tgamind),synterr,ter)                
                TRABUF(TSTAT)  = REJT
                TRABUF(TERR)   = SUPR
                return		
              endif
            ENDIF
C-------------------------------------------------------------------------------
	ENDDO
!
	RETURN
	END
!=============================================================================
!       SUBROUTINE TO DETERMINE IF A TICKET CAN BE CASHED.
!=============================================================================
C=======OPTIONS    /check=nooverflow
	subroutine chkred_pas(trabuf,valrec,offset_tra)
	implicit   none

	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
	include 'inclib:global.def'
	include 'inclib:concom.def'
	include 'inclib:agtcom.def'
	include 'inclib:scrcom.def'
	include 'inclib:desval.def'
	include 'inclib:destra.def'
!
	integer*4 offset_tra
!
	integer*4 game
	integer*4 gtyp
	integer*4 gind
	integer*8 regamt
	logical   isprv
!
	logical   isoffter !V06
	integer*4 portalsap !V06
	parameter (portalsap=007456)	
!
	game   = trabuf(tgam)
	gtyp   = gnttab(gamtyp,game)
	gind   = gnttab(gamidx,game)

	regamt = valrec(vpamt)
	trabuf(tppay1+offset_tra) = regamt
	trabuf(tvoppay) = trabuf(tvoppay) + valrec(vopsamt) !V05
	
	isprv = .FALSE.
	isoffter = .FALSE. !V06
        if(tsbit(agttab(AGTTYP,trabuf(TTER)),AGTPRV)) then
          if(trabuf(TPOFFTER).gt.0) then
            if(trabuf(TPOFFTER).le.NUMAGT) then
            	isoffter = .TRUE.
              if(tsbit(agttab(AGTTYP,trabuf(TPOFFTER)),AGTPRV)) isprv = .TRUE.
              if(tsbit(agttab(AGTTYP,trabuf(TPOFFTER)),AGTBNK)) isprv = .FALSE.  ! BANK Terminal is not PRIVILEGED anymore
            else
              TYPE*,'========== VALUPD_PAS =========='
              TYPE*,'STATUS:   ',TRABUF(TSTAT)
              TYPE*,'ERROR:    ',TRABUF(TERR) 
              TYPE*,'CDC:      ',TRABUF(TCDC)
              TYPE*,'SERIAL:   ',TRABUF(TSER)
              TYPE*,'TERMINAL: ',TRABUF(TTER)
              TYPE*,'AGENT:    ',TRABUF(TAGT)                       
              TYPE*,'TRNTYPE:  ',TRABUF(TTYP)
              TYPE*,'GAME:     ',TRABUF(TGAM)
              TYPE*,'GTYPE:    ',TRABUF(TGAMTYP)
              TYPE*,'GINDEX:   ',TRABUF(TGAMIND)
              TYPE*,'TPOFFTER: ',TRABUF(TPOFFTER)     
            endif
          else
            isprv = .TRUE.
          endif
        endif
!-------IF PRIVILEGED-----------------------------------------------------------
        if(isprv) then
!---------PRIVILEGED NOW CAN MAKE BANK TRANSFERS--------------------------------	
          if(trabuf(tvtype).eq.VPNREG) then
            trabuf(terr) = VINQ
!-------->>V05 -----------------------------------------------------------------
!          else if(trabuf(tvtype).eq.VPNBNK) then
!            if(regamt.lt.P(VALORDER).or.regamt.gt.redmax(game)) then
!      	      trabuf(tpsts1+offset_tra) = ovrred
!      	    endif
!    	  endif
          elseif(trabuf(tvtype).eq.VPNBNK) then
            if(redmax(game).gt.0.and.regamt.gt.redmax(game)) then
              trabuf(tpsts1+offset_tra) = ovrred
            endif
          elseif(trabuf(tvtype).eq.VPPINQ) then
            if(regamt.ge.P(VALORDER) .and. regamt.le.redmax(game)) then
              ! € 150,01 <= regamt <= € 4.999,99
!-------->>V06 -----------------------------------------------------------------
!              trabuf(tpsts1+offset_tra) = npaybat
              if(isoffter) then
                if(agtsap(trabuf(tpoffter)) .ne. portalsap) then
                  trabuf(tpsts1+offset_tra) = npaybat
                endif
              else
                if(agtsap(trabuf(tter)) .ne. portalsap) then
                  trabuf(tpsts1+offset_tra) = npaybat
                endif
              endif
!-------- V06<<-----------------------------------------------------------------
            elseif(redmax(game).gt.0.and.regamt.gt.redmax(game)) then
              ! regamt > € 4.999,99
!-------->>V06 -----------------------------------------------------------------
!              trabuf(tpsts1+offset_tra) = ovrred
              if(isoffter) then
                if(agtsap(trabuf(tpoffter)) .ne. portalsap) then
                  trabuf(tpsts1+offset_tra) = ovrred
                endif
              else
                if(agtsap(trabuf(tter)) .ne. portalsap) then
                  trabuf(tpsts1+offset_tra) = ovrred
                endif
              endif
!-------- V06<<-----------------------------------------------------------------
            endif
          endif
!-------- V05<<-----------------------------------------------------------------
        else
!-------IF NOT PRIVILEGED-------------------------------------------------------
          if(agttab(agtrmx,trabuf(tter)).gt.0.and.regamt.gt.agttab(agtrmx,trabuf(tter))) then
!-------->>V07 -----------------------------------------------------------------
            if(agtsap(trabuf(tter)).ne.portalsap) then                          !Terminal must belong to retailer network
              if(trabuf(tgam).eq.9) then                                        !Lotaria Popular only
                if(trabuf(tpemis1+offset_tra).ge.819 .and.                      !Emissions 15/2017 thru 21/2017
     *            trabuf(tpemis1+offset_tra).le.825) then                       
                  if(trabuf(tpten1+offset_tra).eq.3 .or.                        !3rd series
     *               trabuf(tpten1+offset_tra).eq.4) then                       !4th series
                     if(trabuf(tpnum1+offset_tra).ge.50000) then                !Ticket # greater or equal than 50.000
                       if(trabuf(tppay1+offset_tra).ge.100000) then             !Prizes greater or equal than €1.000,00
                         if(trabuf(tveptyp).eq.0) then                          !pTickets only
                           trabuf(tpsts1+offset_tra) = bserl                    !Invalidate prize validation with error code "Bad Validation #"
                           return
                         endif
                       endif
                     endif
                  endif
                endif
              endif
            endif
!-------- V07<<-----------------------------------------------------------------
            trabuf(tpsts1+offset_tra) = ovrred
          else
            if(redmax(game).gt.0.and.regamt.gt.redmax(game)) then
!-------->>V07 -----------------------------------------------------------------
              if(agtsap(trabuf(tter)).ne.portalsap) then                        !Terminal must belong to retailer network
                if(trabuf(tgam).eq.9) then                                      !Lotaria Popular only
                  if(trabuf(tpemis1+offset_tra).ge.819 .and.                    !Emissions 15/2017 thru 21/2017
     *              trabuf(tpemis1+offset_tra).le.825) then                     
                    if(trabuf(tpten1+offset_tra).eq.3 .or.                      !3rd series
     *                 trabuf(tpten1+offset_tra).eq.4) then                     !4th series
                       if(trabuf(tpnum1+offset_tra).ge.50000) then              !Ticket # greater or equal than 50.000
                         if(trabuf(tppay1+offset_tra).ge.100000) then           !Prizes greater or equal than €1.000,00
                           if(trabuf(tveptyp).eq.0) then                        !pTickets only
                             trabuf(tpsts1+offset_tra) = bserl                  !Invalidate prize validation with error code "Bad Validation #"
                             return
                           endif
                         endif
                       endif
                    endif
                  endif
                endif
              endif
!-------- V07<<-----------------------------------------------------------------
              trabuf(tpsts1+offset_tra) = ovrred
            else
              if(valrec(vstat).eq.vprpay) then
!-------->>V07 -----------------------------------------------------------------
                if(agtsap(trabuf(tter)).ne.portalsap) then                      !Terminal must belong to retailer network
                  if(trabuf(tgam).eq.9) then                                    !Lotaria Popular only
                    if(trabuf(tpemis1+offset_tra).ge.819 .and.                  !Emissions 15/2017 thru 21/2017
     *                trabuf(tpemis1+offset_tra).le.825) then                     
                      if(trabuf(tpten1+offset_tra).eq.3 .or.                    !3rd series
     *                   trabuf(tpten1+offset_tra).eq.4) then                   !4th series
                         if(trabuf(tpnum1+offset_tra).ge.50000) then            !Ticket # greater or equal than 50.000
                           if(trabuf(tppay1+offset_tra).ge.100000) then         !Prizes greater or equal than €1.000,00
                             if(trabuf(tveptyp).eq.0) then                      !pTickets only
                               trabuf(tpsts1+offset_tra) = bserl                !Invalidate prize validation with error code "Bad Validation #"
                               return
                             endif
                           endif
                         endif
                      endif
                    endif
                  endif
                endif
!-------- V07<<-----------------------------------------------------------------
                trabuf(tpsts1+offset_tra) = ovrred
              else
!-------->>V05 -----------------------------------------------------------------
C      	        if(trabuf(tvtype).eq.VPNREG) then
C      	          trabuf(terr) = VINQ
C      	        endif
                if(trabuf(tvtype).eq.VPPINQ) then
                  if(regamt.ge.P(VALORDER) .and. regamt.le.redmax(game)) then
                    ! € 150,01 <= regamt <= € 4.999,99
!-------->>V06 -----------------------------------------------------------------
!                    trabuf(tpsts1+offset_tra) = npaybat !CAN'T PAY IN BATCH
                    if(isoffter) then
                      if(agtsap(trabuf(tpoffter)) .ne. portalsap) then
                        trabuf(tpsts1+offset_tra) = npaybat !CAN'T PAY IN BATCH
                      endif
                    else
                      if(agtsap(trabuf(tter)) .ne. portalsap) then
                        trabuf(tpsts1+offset_tra) = npaybat !CAN'T PAY IN BATCH
                      endif
                    endif
!-------- V06<<-----------------------------------------------------------------
                  endif
                elseif(trabuf(tvtype).eq.VPNREG) then
!-------->>V07 -----------------------------------------------------------------
                  if(agtsap(trabuf(tter)).ne.portalsap) then                    !Terminal must belong to retailer network
                    if(trabuf(tgam).eq.9) then                                  !Lotaria Popular only
                      if(trabuf(tpemis1+offset_tra).ge.819 .and.                !Emissions 15/2017 thru 21/2017
     *                  trabuf(tpemis1+offset_tra).le.825) then
                        if(trabuf(tpten1+offset_tra).eq.3 .or.                  !3rd series
     *                     trabuf(tpten1+offset_tra).eq.4) then                 !4th series
                           if(trabuf(tpnum1+offset_tra).ge.50000) then          !Ticket # greater or equal than 50.000
                             if(trabuf(tppay1+offset_tra).ge.100000) then       !Prizes greater or equal than €1.000,00
                               if(trabuf(tveptyp).eq.0) then                    !pTickets only
                                 trabuf(tpsts1+offset_tra) = bserl              !Invalidate prize validation with error code "Bad Validation #"
                                 return
                               endif
                             endif
                           endif
                        endif
                      endif
                    endif
                  endif
!-------- V07<<-----------------------------------------------------------------
                  trabuf(terr) = VINQ
                endif
!-------- V05<<-----------------------------------------------------------------
              endif
            endif
          endif
        endif
!-------------------------------------------------------------------------------
        return
        end
C==========================================================================
C       SUBROUTINE TO QUEUE ERROR MESSAGES TO THE ERRLOG TASK.
C==========================================================================
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VALERR(STATUS,FILE,SERIAL,MSGNO,GTYP,GIND,SYNTERR,TER)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'

	INTEGER*4 MESS(EDLEN)
	INTEGER*4 SERIAL
	INTEGER*4 FILE
	INTEGER*4 STATUS,MSGNO,GTYP,GIND,SYNTERR,TER


	MESS(1) = PSV
	MESS(2) = TEGEN
	MESS(3) = MSGNO

	IF(MSGNO.EQ.10) THEN
      	    MESS(4) = SYNTERR
      	    MESS(5) = TER
      	    MESS(6) = GTYP
      	    MESS(7) = GIND
      	    MESS(8) = SERIAL
	ELSEIF(MSGNO.EQ.16) THEN
	    IF(FILE.EQ.-1) THEN
      	      MESS(4)='DRAW'
      	      MESS(5)=' FIL'
      	      MESS(6)='ES  '
      	      MESS(7)='    '
      	      MESS(8)='    '
	    ELSE
	      CALL FASTMOV(SFNAMES(1,FILE),MESS(4),5)
	    ENDIF
	    MESS(9)  = STATUS
	    MESS(10) = SERIAL
	ENDIF

	CALL QUEMES(MESS)
	RETURN
	END	
