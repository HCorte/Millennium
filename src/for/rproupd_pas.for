C RPROUPD_PAS.FOR
C
C V04 18-AUG-2011 FJG EVO Project bug with PASRETAFTAMT solved
C V03 08-OCT-2010 FJG RAFTD boolean
C V02 01-JAN-2010 FJG ePassive
C V01 13-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C
C SUBROUTINE TO REAPPLY RETURNED TICKETS FOR REPROCESSING.
C
C CALLING SEQUENCE:
C      CALL RPROUPD_PAS(TRABUF,STATUS)
C
C INPUT
C       TRABUF - INTERNAL TRANSACTION BUFFER
C OUTPUT
C       STATUS - 0 NO ERROR
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C       COMPLETE REWRITE 
C
C=======OPTIONS /check=nooverflow
	subroutine rproupd_pas(trabuf,erro,elist)
	implicit none
!
	include 'inclib:sysparam.def'
	include 'inclib:sysextrn.def'
	include 'inclib:global.def'
	include 'inclib:concom.def'
	include 'inclib:pascom.def'
	include 'inclib:destra.def'
	include 'inclib:pasiosubs.def'
!
        integer*4 erro	
        integer*4 elist(10)
!
        integer*4 gnum
        integer*4 gtyp
        integer*4 gind
        integer*4 ntic   
        integer*4 tcks     
        integer*4 offs
        integer*4 emis
        integer*4 xnum
        integer*4 snum        
        integer*4 xser
        integer*4 xfra
        integer*4 ifra
        integer*4 ffra
        integer*4 cfra
        integer*4 temp
        integer*4 tots
        integer*4 iocnt
        logical   rsec
        logical   aftd
!+++++++FUNCTIONS
        integer*4 getpagemi
!===============================================================================
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES TPFS ONLINE
!===============================================================================
        record /stpasfdb/  pasfdb(pagemi,numpas)
        record /stpasrec/  pasrec
!
        common /pastruct/ pasfdb        
!=============================================================================== 	
	ntic = trabuf(TPTCK)
	gnum = trabuf(TGAM)
	gind = trabuf(TGAMIND)
	gtyp = trabuf(TGAMTYP)
	rsec = .false.
	tots = 0
	erro = 0
        iocnt = 0
!+++++++INI LOOP
	do  tcks = 1,ntic
	  offs = (tcks-1)*OFFTRA
	  if(trabuf(TPSTS1+offs).eq.RETURND.or.trabuf(TPSTS1+offs).eq.RETAFDR) then
	    if(trabuf(TPSTS1+offs).eq.RETAFDR) then
	      aftd = .true.
	    else
	      aftd = .false.	      
	    endif
	    emis = getpagemi(trabuf(TPEMIS1+offs),gind) ! Range should be checked in DPAS
!=============================================================================== 	
! IF POPULAR AND RETURNING ALL TICKET, GET SECOND TICKET NUMBER TO RETURN
!            
            if(gind.eq.PSBPOP.and.trabuf(TPRETYP).eq.ALLTCK) then
              temp = pasnumtck(emis,gind)/2
              if(trabuf(TPNUM1+offs).ge.temp) then
                snum = trabuf(TPNUM1+offs) - temp
              else
                snum = trabuf(TPNUM1+offs) + temp                
              endif
              rsec = .true.
            endif
!-----------INI IF 1--------------------------------------------------------
            if(trabuf(TPRETYP).eq.ALLTCK) then
              ifra = 1
              ffra = pasnoffra(emis,gind)
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
!-----------FIN IF 1--------------------------------------------------------                
            xnum = trabuf(TPNUM1+offs)   
            xser = trabuf(TPSER1+offs)
            xfra = trabuf(TPTEN1+offs)     
10          continue           ! FOR POPULAR CAN BE DONE TWICE
!+++++++++++INI LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            do xfra = ifra,ffra
              call pasio_read(pasfdb(emis,gind),xnum,xser,xfra,pasrec)  
              if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                iocnt = iocnt + 1
                elist(iocnt) = emis
                TRABUF(TERR) = EPIO
                pasioerrs(ioreano,emis,gind) = pasioerrs(ioreano,emis,gind) + 1
                erro = erro + 1
              else
                pasioerrs(ioreaok,emis,gind) = pasioerrs(ioreaok,emis,gind) + 1         
!---------------INI SELECT -----------------------------------------------------                
                select case (pasrec.stat)
                  case ( pbilsof , pbilcof , pbilrof )
                    if(aftd) then
                      pasrec.stat   = pbilrof          ! > Returned offline after draw
                    else
                      pasrec.stat   = pbilcof          ! > Returned offline              
                    endif
                    pasrec.cdc    = daycdc
                    pasrec.serial = trabuf(TSER)
                    pasrec.agt    = trabuf(TTER)     
!-----------------UPDATE----------------------------------------------------                   
                    call pasio_swrite(pasfdb(emis,gind),xnum,xser,xfra,pasrec)  
                    if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                      pasioerrs(iowrino,emis,gind) = pasioerrs(iowrino,emis,gind) + 1
                      erro = erro + 1                         
                    else
                      pasioerrs(iowriok,emis,gind) = pasioerrs(iowriok,emis,gind) + 1
                      tots = tots + 1                          
                    endif
!---------------------------------------------------------------------------  
                  case ( pbilwof , pbilkof , pbilxof )                               
                    if(aftd) then
                      pasrec.stat   = pbilkof          ! > Returned offline after draw and winner
!                      
                      pasretaftamt(emis,gind) = pasretaftamt(emis,gind) + 1  ! V4 Being consistent with PROUPD_PAS
                    else                      
                      pasrec.stat   = pbilxof          ! > Returned offline and winner
                    endif
                    pasrec.cdc    = daycdc
                    pasrec.serial = trabuf(TSER)
                    pasrec.agt    = trabuf(TTER)
!-----------------UPDATE----------------------------------------------------                   
                    call pasio_swrite(pasfdb(emis,gind),xnum,xser,xfra,pasrec)  
                    if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                      iocnt = iocnt + 1
                      elist(iocnt) = emis
                      TRABUF(TERR) = EPIO
                      pasioerrs(iowrino,emis,gind) = pasioerrs(iowrino,emis,gind) + 1   
                      erro = erro + 1                         
                    else
                      pasioerrs(iowriok,emis,gind) = pasioerrs(iowriok,emis,gind) + 1
                      tots = tots + 1                                                    
                    endif
!---------------------------------------------------------------------------
                  case default
                    erro = erro + 1
                end select
!---------------FIN SELECT -----------------------------------------------------                
              endif  
            enddo
!+++++++++++FIN LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
            if(rsec) then
              rsec = .false.
              xnum = snum
              goto 10
            endif
!=============================================================================== 
          endif
        enddo  	              
!+++++++FIN LOOP++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
        if(trabuf(TPFRCNT).ne.tots) erro = erro + 1
	return
	end
