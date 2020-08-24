C PROUPD_PAS.FOR
C
C V05 18-AUG-2011 FJG EVO Project bug with PASRETAFTAMT solved
C     24-AUG-2011 FJG Incorrect counting
C V04 08-OCT-2010 FJG ePassive check all before return
C V03 07-OCT-2010 FJG HARDCODE FOR ALLOW RETURNS LATER ON CLOSING DRAW
C V02 01-JAN-2010 FJG ePassive
C V01 12-FEB-01 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE TO RETRIEVE AND UPDATE TRANSACTIONS FROM
C THE RETURN IDX PASSIVE FILES.
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
C       COMPLETE REWRITE 
C
C=======OPTIONS /check=nooverflow
	subroutine proupd_pas(trabuf)
	implicit none
!
	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
	include 'inclib:global.def'
	include 'inclib:concom.def'
	include 'inclib:pascom.def'
	include 'inclib:destra.def'
	include 'inclib:taskid.def'
	include 'inclib:pasiosubs.def'
!	
        integer*4 gnum
        integer*4 gtyp
        integer*4 gind
        integer*4 ntic   
        integer*4 tcks     
        integer*4 offs
        integer*4 emis
        integer*4 xnum
        integer*4 mnum
        integer*4 snum        
        integer*4 xser
        integer*4 xfra
        integer*4 mfra        
        integer*4 ifra
        integer*4 ffra
        integer*4 cfra
        integer*4 temp
        integer*4 tots
        integer*4 tamt     
        integer*4 wamt   
        integer*4 tcom        
        integer*4 serr
        integer*4 verr        
        integer*4 mess(edlen)
!        
        integer*4 imax
        integer*4 tmax
        integer*4 pmax
        parameter (pmax = 20)
!        
        integer*4 anum(pmax)
        integer*4 aser(pmax)
        integer*4 afra(pmax)    
        integer*4 vamt(pmax)            
!
        logical   rsec
        logical   aftd
        logical   updt
!+++++++FUNCTIONS
        integer*4 getpagemi
        integer*4 chkretwin
!===============================================================================
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES TPFS ONLINE
!===============================================================================
        record /stpasfdb/  pasfdb(pagemi,numpas)
        record /stpasrec/  pasrec(pmax)
!
        common /pastruct/ pasfdb        
!=============================================================================== 	
	ntic = trabuf(TPTCK)
	gnum = trabuf(TGAM)
	gind = trabuf(TGAMIND)
	gtyp = trabuf(TGAMTYP)
	serr = 0
!+++++++INI LOOP
	do  tcks = 1,ntic
	  aftd = .false.
	  rsec = .false.
          snum = -1	  	  
	  tots = 0
	  offs = (tcks-1)*OFFTRA
	  if(trabuf(TPSTS1+offs).eq.RETURND) then
	    emis = getpagemi(trabuf(TPEMIS1+offs),gind) ! Range should be checked in DPAS
	    if(passubsts(emis,gind).eq.pdrwnot) then
	      trabuf(TPSTS1+offs) = WEMIS	    
	    elseif(passubsts(emis,gind).eq.pdrwpur) then	      
	      trabuf(TPSTS1+offs) = PURGED
	    elseif(passubsts(emis,gind).eq.pdrwclo) then	      	      
	      trabuf(TPSTS1+offs) = ENDEMIS	  	    
	    elseif(passubsts(emis,gind).eq.pdrwerr) then	      	      
	      trabuf(TPSTS1+offs) = EMINOPN	  	      
	    else
	      if(passts(emis,gind).gt.GAMBFD) then	        
	        trabuf(TPSTS1+offs) = RETAFDR
	        aftd = .true.
	      endif
!=============================================================================== 	
              xnum = trabuf(TPNUM1+offs)   
              mnum = xnum
              xser = trabuf(TPSER1+offs)
              mfra = trabuf(TPTEN1+offs)   ! Remember it in the loop for control
              updt = .true.
!-------------See how many tickets could be returned----------------------------            
              if(trabuf(TPRETYP).eq.ALLTCK) then
                ifra = 1
                ffra = pasnoffra(emis,gind)
!---------------If popular Full Billet we need the second number----------------                            
                if(gind.eq.PSBPOP) then
                  temp = pasnumtck(emis,gind)/2
                  if(trabuf(TPNUM1+offs).ge.temp) then
                    snum = trabuf(TPNUM1+offs) - temp
                  else
                    snum = trabuf(TPNUM1+offs) + temp                
                  endif
                  rsec = .true.                  
                endif
              elseif(trabuf(TPRETYP).eq.HALFTCK.or.trabuf(TPRETYP).eq.QUARTCK) then      
                if(gind.eq.PSBPOP) then
                  ifra = 1
                  ffra = pasnoffra(emis,gind)
                else
                  if(trabuf(TPRETYP).eq.HALFTCK) then
                    cfra = pasnoffra(emis,gind)/2
                  else
                    cfra = pasnoffra(emis,gind)/4
                  endif
                  temp = 0
                  do while(trabuf(TPTEN1+offs).GT.cfra*temp)
                    temp = temp + 1
                  enddo
                  ffra = cfra * temp
                  ifra = ffra - cfra + 1
                endif
              else ! SHOULD BE BYFRAC
                ifra = trabuf(TPTEN1+offs)
                ffra = ifra
              endif
!-------------Clean up working variables for this barcode-----------------------                            
              imax = 0  
              do tmax = 1,pmax
                anum(tmax) = -1
                aser(tmax) = -1
                afra(tmax) = -1   
                vamt(tmax) = 0             
                pasrec(tmax).stat    = 0
                pasrec(tmax).cdc     = 0
                pasrec(tmax).agt     = 0
                pasrec(tmax).serial  = 0
                pasrec(tmax).control = 0
              enddo
!-------------If second number for popular come back here-----------------------                                          
10            continue
!+++++++++++++++++INI LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++
              do xfra = ifra,ffra
                imax = imax + 1
                anum(imax) = xnum
                aser(imax) = xser
                afra(imax) = xfra
                call pasio_read(pasfdb(emis,gind),anum(imax),aser(imax),afra(imax),pasrec(imax))  
                if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                  trabuf(TPSTS1+offs) = HLDRTCK
                  pasioerrs(ioreano,emis,gind) = pasioerrs(ioreano,emis,gind) + 1
                  serr = serr + 1
                  updt = .false.
                  exit
                else
!-----------------If it is the read fraction, check security control------------                              
                  pasioerrs(ioreaok,emis,gind) = pasioerrs(ioreaok,emis,gind) + 1                    
                  if(afra(imax).eq.mfra.and.anum(imax).eq.mnum) then
                    if(trabuf(TPKEY1+offs).ne.pasrec(imax).control) then
                      trabuf(TPSTS1+offs) = BTKT
                      updt = .false.
                      exit
                    endif
                  endif
!-----------------Now check each ticket status----------------------------------
                  if(pasrec(imax).stat.eq.pbiloff) then      ! < Available for offsale
                    trabuf(TPSTS1+offs) = BTKT              
                    updt = .false.
                    exit
                  elseif(pasrec(imax).stat.eq.pbilsof) then  ! < Sold offline
                    if(aftd) then
                      pasrec(imax).stat   = pbilrof    ! > Returned offline after draw
                    else
                      pasrec(imax).stat   = pbilcof    ! > Returned offline              
                    endif
                    pasrec(imax).cdc    = daycdc
                    pasrec(imax).serial = trabuf(TSER)
                    pasrec(imax).agt    = trabuf(TTER)
!-------------------------------------------------------------------------------                        
                  elseif(pasrec(imax).stat.eq.pbilcof) then  ! < Returned offline
                    trabuf(TPSTS1+offs) = RTKT
                    updt = .false.
                    exit                    
                  elseif(pasrec(imax).stat.eq.pbilwof) then  ! < Sold offline and winner
                    verr = chkretwin(gind,emis,anum(imax),aser(imax),afra(imax),wamt,serr)
                    trabuf(TPSTS1+offs) = verr
!+++++++++++++++++++Here we have to check if the ticket was already paid++++++++
                    if(verr.eq.RETAFDR) then
                      pasrec(imax).stat   = pbilkof    ! > Returned offline after draw and winner
                      pasrec(imax).cdc    = daycdc
                      pasrec(imax).serial = trabuf(TSER)
                      pasrec(imax).agt    = trabuf(TTER)                   
                      vamt(imax) = wamt
                    else
                      updt = .false.
                      exit                                     
                    endif
!------------------------------------------------------------------------------- 
                  elseif(pasrec(imax).stat.eq.pbilxof) then  ! Returned offline and winner
                    trabuf(TPSTS1+offs) = RTKT     
                    updt = .false.
                    exit                                     
                  elseif(pasrec(imax).stat.eq.pbilrof) then  ! Returned offline after draw
                    trabuf(TPSTS1+offs) = RTKT                      
                    updt = .false.
                    exit                                     
                  elseif(pasrec(imax).stat.eq.pbilkof) then  ! Returned offline after draw and winner
                    trabuf(TPSTS1+offs) = RTKT                
                    updt = .false.
                    exit                                                               
                  else
                    trabuf(TPSTS1+offs) = BTKT         ! Not defined or Online
                    updt = .false.
                    exit                                                         
                  endif
                endif  
              enddo
!+++++++++++++++++FIN LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++    
              if(updt.and.rsec) then
                rsec = .false.
                xnum = snum
                goto 10
              endif
!=============================================================================== 
              if(updt) then
                tots = 0 
!---------------Now write all records back to TPF file--------------------------                                             
                do tmax = 1,imax 
                  call pasio_swrite(pasfdb(emis,gind),anum(tmax),aser(tmax),afra(tmax),pasrec(tmax)) 
                  if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                    trabuf(TPSTS1+offs) = HLDRTCK
                    pasioerrs(iowrino,emis,gind) = pasioerrs(iowrino,emis,gind) + 1
                    serr = serr + 1                         
                  else
                    pasioerrs(iowriok,emis,gind) = pasioerrs(iowriok,emis,gind) + 1
                    tots = tots + 1        
!==>                +++++++++++++ Update Memory if possible ++++++++++++++
                    if(passaltab(emis,gind).gt.0) then 
                      if(gind.eq.PSBCLA) then
                        pasnumcla(anum(tmax),passaltab(emis,gind)).billet(aser(tmax),afra(tmax)) = pasrec(tmax).stat
                      else
                        pasnumpop(anum(tmax),passaltab(emis,gind)).billet(aser(tmax),afra(tmax)) = pasrec(tmax).stat        
                      endif
                    endif    
!==>                ++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   pasretaftamt(emis,gind) = pasretaftamt(emis,gind) + vamt(tmax) ! V5
                    if (pasrec(imax).stat.eq.pbilkof) pasretaftamt(emis,gind) = pasretaftamt(emis,gind) + 1          ! V5
!                   Because of reprocessing now here we just count the TICKETS to be sure
!                   Later on UPDVPF will update the real amounts about payments returned after draw.
                  endif                  
                enddo  
!---------------Discount the pTickets sales comission for real value------------                             
                tamt = pasprc(emis,gind)
                tcom = idnint(dfloat(tamt)*calper(retcom(gind)))  
                trabuf(TPPAY1+offs) = (tamt - tcom) * tots
                trabuf(TPTEN1+offs) = ifra
                trabuf(TPFRCNT) = trabuf(TPFRCNT) + tots   
              endif
!=============================================================================== 
            endif
          endif
        enddo  	              
!+++++++FIN LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
        if(serr.gt.0) then
          mess(1) = PST
          mess(2) = TEGEN
          mess(3) = 10
          mess(4) = 10
          mess(5) = trabuf(TTER)
          mess(6) = gtyp
          mess(7) = gind
          mess(8) = trabuf(TSER)
          call quemes(mess)
        endif        
	return
	end
C==================================================
C       RETURN VPF Fraction Status
C==================================================
C=======OPTIONS   /check=nooverflow
        integer*4 function chkretwin(gind,emis,xnum,xser,xfra,wamt,serr)
        implicit  none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
        include 'inclib:desval.def'
        include 'inclib:valpasfil.def'
        include 'inclib:prmhsh.def'
        include 'inclib:pascom.def'
!
        integer*4 gind
        integer*4 emis
        integer*4 xnum
        integer*4 xser
        integer*4 xfra
        integer*4 wamt
        integer*4 serr
!
        integer*4 vlun
        integer*4 erro       
        integer*4 junk         
        integer*4 vkey(2)
        integer*4 vbuf(I4BUCSIZ)        
!
        call find_available_lun(vlun,erro)
        if(erro.ne.0) then
          chkretwin = HLDRTCK
          serr = serr + 1      
          type*,iam(),'Error finding lun: ',erro    
        else
          call iopen(pasvpffil(1,emis,gind),vlun,VPFLEN*2,VFSCDC,VFSSER*2-1,erro)
          if(erro.ne.0) then
            chkretwin = HLDRTCK
            serr = serr + 1
            type*,iam(),'Error opening VPF file: ',cpasvpffil(emis,gind),erro
          else
            vkey(1) = xfra
            vkey(2) = ishft(xser,24) + xnum
            call iread(vkey,V4BUF_PAS,vlun,erro)
            call iclose(vlun,vbuf,junk)
            if(erro.ne.0) then
              chkretwin = HLDRTCK
              serr = serr + 1
              type*,iam(),'Error reading VPF file: ',erro
            else
              call logpas(valrec,v4buf_pas)
              if(valrec(vstat).eq.vcash) then
                chkretwin = apad
              elseif(valrec(vstat).eq.vcxl) then
                chkretwin = rtkt
              elseif(valrec(vstat).eq.vdel) then
                chkretwin = utkt
              elseif(valrec(vstat).eq.vuncsh.or.valrec(vstat).eq.vprpay) then
                chkretwin = retafdr
                wamt = valrec(vpamt)
              else
                chkretwin = HLDRTCK
              endif
            endif
          endif
        endif
!
        return
        end
