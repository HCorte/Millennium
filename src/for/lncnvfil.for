C
C V01 01-JAN-2010 FJG ePassive
C
C PROGRAM TO CONVERT OLD Passive FILES TO NEW ePassive files
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /check=nooverflow
	program lncnvfil
	implicit none
!
 	include 'inclib:sysextrn.def'
	include 'inclib:sysdefine.def'
!
	include 'inclib:global.def'
	include 'inclib:dparec.def'
	include 'inclib:concom.def'
	include 'inclib:pascom.def'
	include 'inclib:standard.def'
!
        integer*4 emi	
        integer*4 ind
!
        call copyrite
        call intmem
        call syscon
        call lnintpas
!
!       Converting TPFs
!
        do ind=1,numpas
          do emi=1,pagemi
            if(passts(emi,ind).ne.gamnul) then
              if(cpastpffil(emi,ind).ne.'                    ') then
                call chgtpf(cpastpffil(emi,ind),ind,pasemis(emi,ind),pasnumtck(emi,ind)-1,pasnumser(emi,ind),pasnoffra(emi,ind))
              endif
            endif
          enddo
        enddo
!
	call gstop(gexit_success)
	end
!===============================================================================
!       Subroutine to convert one fil
!===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine chgtpf(wrkfil,wrkind,wrkdrw,wrknum,wrkser,wrkfra)
!
 	include 'inclib:sysextrn.def'
	include 'inclib:sysdefine.def'
!
	include 'inclib:global.def'
	include 'inclib:dparec.def'
	include 'inclib:concom.def'
	include 'inclib:pascom.def'
	include 'inclib:standard.def'
	include 'inclib:idxprm.def'
        include 'inclib:pasiosubs.def' 	
!        
        character*20 wrkfil      !passive file
        integer*4    wrkind      !passive game index
        integer*4    wrkdrw      !emision draw number
        integer*4    wrknum      !max num of emission
        integer*4    wrkser      !max ser of emission
        integer*4    wrkfra      !max fra of emission
!
        integer*4    filold(5)   !source file
        character*20 cfilold     !source file in char
        integer*4    filnew(5)   !destination file
        character*20 cfilnew     !destination in char
        character*20 cfilwrk     !original renamed
!
        logical      newmax      !over maxser                
!        
        record /newstcidx/ newtpfrec
        record /stcidx/    oldtpfrec   
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec
        record /stpasrec/  pasbuk(pasbukrec) !32 of stpasrec          
!        
        integer*4    fdb(7)
        integer*4    tmpvar
        integer*4    tmperr
        integer*4    tmpkey
        integer*4    tmpsta
        integer*4    tmpser
        integer*4    tmpfra
        integer*4    tmpmax
        integer*4    tmprea
        integer*4    tmpnum        
        integer*4    tmpwri
        integer*4    tmpbuk        
        integer*4    offrec     
        integer*4    totrea
        integer*4    totwri        
        integer*4    inptab(0:15)
        character*12 inpnam(0:15)/'Returned    ',3*'Error       ','Sold Ticket ','Returned AD ',
     *                            'Error       ','Winner      ','Error       ','Unsold      ',6*'Error       '/
        integer*4    outtab(pbilmin:pbilmax)
        character*12 outnam(pbilmin:pbilmax)/'RetAftWinner',
     *                                       'RetAfterDraw',
     *                                       'RetWinner   ',
     *                                       'SoldOffWin  ',
     *                                       'ReturnedOff ',
     *                                       'SoldOffline ',
     *                                       'AvailableOff',
     *                                       'NOT DEFINED ',
     *                                       'AvailableOnl',     
     *                                       'SoldOnline  ',
     *                                       'CancelledOnl',     
     *                                       'SoldOnlWin  ',
     *                                       'CanOnlineWin'/
!
        equivalence(filold,cfilold)        
        equivalence(filnew,cfilnew)      
!
        call fastset(0,inptab,16)
        call fastset(0,outtab,pbilmax-pbilmin+1)
        cfilold = wrkfil
        tmpvar = index(cfilold,'.')-1
        cfilnew = cfilold(1:tmpvar) // '.NEW'
        cfilwrk = cfilold(1:tmpvar) // '.OLD'
!
        totrea = wrknum + 1
        totwri = totrea * wrkser * wrkfra
!        
        type*,iam()
        type*,iam(),'Source:      ',cfilold
        type*,iam(),'Destination: ',cfilnew        
        type*,iam()
        type*,iam(),'Passive:     ',wrkind
        type*,iam(),'Emission:    ',wrkdrw
        type*,iam(),'Max Num:     ',wrknum
        type*,iam(),'Series:      ',wrkser
        type*,iam(),'Fractions:   ',wrkfra     
        type*,iam()
        type*,iam(),'Est.Read:    ',totrea
        type*,iam(),'Est.Write:   ',totwri
        type*,iam()
!
        if(wrkser.gt.maxser) then
          newmax = .true.
        else
          newmax = .false.
        endif
!        
        if(newmax) then
          tmpmax = newmaxser
          call fidx_open(fdb,cfilold,sizeof(newtpfrec),'OLD',tmperr)
        else
          tmpmax = maxser
          call fidx_open(fdb,cfilold,sizeof(oldtpfrec),'OLD',tmperr)          
        endif
        if(tmperr.ne.0) then
          call filerr(filold,open_error,tmperr,0)
          call gstop(gexit_fatal)
        else
          type*,iam(),'Starting conversion ...'          
          call pasio_init(pasfdb,wrkind,wrkdrw,wrknum,wrkser,wrkfra,cfilnew)
          call pasio_create(pasfdb)
          if(pasfdb.err.eq.ioe_noerr) then   
            type*,iam(),'TPF file created: ',pasfdb.filnam  
            type*,iam()      
            call pasio_open(pasfdb) 
            if(pasfdb.err.eq.ioe_noerr) then   
              tmprea = 0
              tmpwri = 0   
              tmpbuk = 0    
              offrec = 0      
              call fastset(0,pasbuk,pasbuklen/4)
              do while (.true.)
                if(newmax) then
                  call fidx_read(fdb,tmprea,sizeof(newtpfrec),newtpfrec,tmperr)
                else
                  call fidx_read(fdb,tmprea,sizeof(oldtpfrec),oldtpfrec,tmperr)              
                endif
                tmprea = tmprea + 1                 
                if(tmperr.eq.0) then
                  tmpnum = tmprea - 1 !Because number starts in zero is one less than record
                  if(mod(tmprea,10000).eq.0) type*,iam(),tmprea,' records read ...'                
                  do tmpser=1,tmpmax
                    do tmpfra=1,noffra
                      if(newmax) then                        
                        call get_tick_status(newtpfrec.algoritmo(tmpfra,tmpser),tmpkey,tmpsta)
                      else
                        call get_tick_status(oldtpfrec.algoritmo(tmpfra,tmpser),tmpkey,tmpsta)                          
                      endif
                      if(tmpfra.le.wrkfra.and.tmpser.le.wrkser) then
                        pasrec.cdc = 0                  
                        pasrec.serial = 0                  
                        pasrec.agt = 0                  
                        pasrec.control = 0  
                        inptab(tmpsta) = inptab(tmpsta) + 1
                        select case (tmpsta)
                          case (0)   ! RETURND: Values hardcoded in case they are deleted in global.def
                            pasrec.stat = pbilcof 
                          case (4)   ! SOLDTCK: Values hardcoded in case they are deleted in global.def
                            pasrec.stat = pbilsof                        
                          case (5)   ! RETAFDR: Values hardcoded in case they are deleted in global.def
                            pasrec.stat = pbilrof  
                          case (7)   ! VWINNER: Values hardcoded in case they are deleted in global.def
                            pasrec.stat = pbilwof  
                          case (9)   ! UTKT:    Values hardcoded in case they are deleted in global.def
                            pasrec.stat = pbiloff                                                                           
                          case default
                            type*,iam(),'Incorrect status: ',tmpsta,' (',inpnam(tmpsta),')'
                            call gpause                          
                        end select
                        pasrec.control = tmpkey
                        pasrec.key = pasio_key(tmpnum,tmpser,tmpfra)
                        offrec = offrec + 1
                        pasbuk(offrec) = pasrec
                        if(offrec.eq.pasbukrec) then
                          offrec = 0
                          tmpbuk = tmpbuk + 1
!                         call pasio_write(pasfdb,tmpnum,tmpser,tmpfra,pasrec)
                          call writew(pasfdb.fdb,tmpbuk,pasbuk,pasfdb.err)    
                          if(pasfdb.err.ne.ioe_noerr) then
                            type*,iam(),'Error: ',pasfdb.err,' writing bucket: ',tmpbuk
                            call gpause
                          endif
                          call fastset(0,pasbuk,pasbuklen/4)
                        endif
                        tmpwri = tmpwri + 1                          
                        outtab(pasrec.stat) = outtab(pasrec.stat) + 1                        
                      else
                        if(tmpsta.ne.0) then
                          type*,iam(),'Incongruence reading in record: ',tmprea,tmpser,tmpfra,tmpsta
                          call gpause
                        endif
                      endif
                    enddo
                  enddo
                else if(tmperr.eq.fidx_rnf) then
                  type*,iam(),tmprea,' record not found. Assuming end of file.'                                  
                  call fidx_close(fdb,tmperr)
                  if(offrec.gt.0) then
                    tmpbuk = tmpbuk + 1
                    call writew(pasfdb.fdb,tmpbuk,pasbuk,pasfdb.err)    
                    if(pasfdb.err.ne.ioe_noerr) then
                      type*,iam(),'Error: ',pasfdb.err,' writing bucket: ',tmpbuk
                      call gpause
                    endif                    
                  endif
                  call pasio_close(pasfdb)
                  tmpvar = 0
                  type*,iam()
                  type*,iam(),'===== INPUT  SUMMARY ====='                  
                  do tmpsta = 0,15
                    tmpvar = tmpvar + inptab(tmpsta)
                    if(inptab(tmpsta).gt.0) type*,iam(),inpnam(tmpsta),': ',inptab(tmpsta)
                  enddo
                  type*,iam(),'=========================='                  
                  type*,iam(),'Total:        ',tmpvar
                  type*,iam()     
                  tmpvar = 0
                  type*,iam(),'===== OUTPUT SUMMARY =====' 
                  do tmpsta = pbilmax,pbilmin,-1
                    tmpvar = tmpvar + outtab(tmpsta)
                    if(outtab(tmpsta).gt.0) type*,iam(),outnam(tmpsta),': ',outtab(tmpsta)
                  enddo
                  type*,iam(),'=========================='                  
                  type*,iam(),'Total:        ',tmpvar                               
                  type*,iam()
                  type*,iam(),'=============== BALANCE =============='
                  type*,iam(),'Total Read:  ',tmprea-1,'/',totrea
                  type*,iam(),'Total Write: ',tmpwri,'/',totwri                  
                  type*,iam(),'Total Bucks: ',tmpbuk
                  type*,iam()
                  type*,iam(),'Renaming ',cfilold,' >> ',cfilwrk
                  call lib$rename_file(cfilold,cfilwrk)
                  type*,iam(),'Renaming ',cfilnew,' >> ',cfilold                  
                  call lib$rename_file(cfilnew,cfilold)                  
                  exit                    
                else
                  type*,iam(),'Error ',tmperr,' reading ',filold,' record: ',tmprea
                  call gpause
                endif
              enddo 
            else
              type*,iam(),'Error: ',pasfdb.err,' opening ',pasfdb.filnam  
            endif
          else
            type*,iam(),'Error: ',pasfdb.err,' creating ',pasfdb.filnam
          endif
        endif
!
        return
        end
!===============================================================================
!       Subroutine to load memory table. Modification of INTPAS for RESET
!===============================================================================        
C=======OPTIONS /check=nooverflow/ext
        subroutine lnintpas
        implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
!
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:datbuf.def'
        include 'inclib:gtnames.def'
        include 'inclib:dparec.def'
        include 'inclib:recdaf.def'        
        include 'inclib:standard.def'
!
        integer*4    game
        integer*4    gtyp
        integer*4    gind
        integer*4    draw
        integer*4    drai
        integer*4    draf
        integer*4    drac
        integer*4    fdb(7)
        integer*4    erro
        integer*4    emis
        integer*4    temp
        character*20 ctpf
        character*20 cvpf
        character*4  mtip  
!       
        daycdc = p(nxtcdc)
        write(5,940) iam(),p(nxtcdc)
        call openw(3,sfnames(1,daf),4,0,0,erro)
        call ioinit(fdb,3,dafsec*256)
        if(erro.ne.0) then
          call filerr(gfnames(1,game),open_error,erro,0)
          call gstop(gexit_fatal)
        endif
        call readw(fdb,p(nxtcdc),dafrec,erro)
        if(erro.ne.0) then
          call filerr(gfnames(1,game),read_error,erro,p(nxtcdc))
          call gstop(gexit_fatal)
        endif        
        call closefil(fdb)
!
        do gind=1,numpas
          gtyp = tpas
          game = gtntab(tpas,gind)
          write(mtip,'(A4)') gsnames(game)
          draw = dafdrw(game)
          write(5,930) iam(),mtip, draw
          drai = draw-50
          draf = draw+40
          write(ctpf,'(5A4)') (sfnames(temp,tpf),temp=1,5)
          write(cvpf,'(5A4)') (sfnames(temp,vpf),temp=1,5)	
!
          call openw(3,gfnames(1,game),4,0,0,erro)
          call ioinit(fdb,3,dpasec*256)
          if(erro.ne.0) then
            call filerr(gfnames(1,game),open_error,erro,0)
            call gstop(gexit_fatal)
          endif
!
          emis = 0       
          do drac=draf,drai,-1
            call readw(fdb,drac,dparec,erro)
            if(erro.ne.0) then
              call filerr(gfnames(1,game),read_error,erro,drac)
              call gstop(gexit_fatal)
            endif
!
            if(dpasts.gt.gaminf) then
              if(emis.eq.pagemi) then
                write(5,900) iam(), drac
              else
                if(dpasts.eq.gfinal) then
                  if(dpaprgcdc.ge.p(nxtcdc)) then              ! Needed both files
                    emis = emis + 1  
                    passubsts(emis,gind) = pdrwval
                    call loggampas(emis,gind,dparec,pasblk)                    
                    write(cpastpffil(emis,gind),'(A8,I2.2,I4.4,A6)') ctpf(1:8),gind,pasemis(emis,gind),ctpf(15:20)
                    write(cpasvpffil(emis,gind),'(A8,I2.2,I4.4,A6)') cvpf(1:8),gind,pasemis(emis,gind),cvpf(15:20)
                    if(.not.filexist(cpasvpffil(emis,gind))) then 
                      write(5,920) iam(),emis,drac,cpasvpffil(emis,gind)  
                      cpasvpffil(emis,gind) = '****                '                 
                    endif
                    if(.not.filexist(cpastpffil(emis,gind))) then                
                      write(5,920) iam(),emis,drac,cpastpffil(emis,gind)
                      cpastpffil(emis,gind) = '****                '
                    endif
                  endif
                else if(dpasts.eq.gamopn) then             ! Just TPF
                  emis = emis + 1                                    
                  passubsts(emis,gind) = pdrwret  
                  call loggampas(emis,gind,dparec,pasblk)                    
                  write(cpastpffil(emis,gind),'(A8,I2.2,I4.4,A6)') ctpf(1:8),gind,pasemis(emis,gind),ctpf(15:20)
                  cpasvpffil(emis,gind) = '                    '
                  if(.not.filexist(cpastpffil(emis,gind))) then
                    write(5,920) iam(),emis,drac,cpastpffil(emis,gind)
                    cpasvpffil(emis,gind) = '****                '
                  endif
                else
                  write(5,910) iam(),emis,drac,dpasts                  
                endif
              endif
            endif
  !
          enddo
          call closefil(fdb)
        enddo   
!
        return
!===============================================================================
!       Format statements        
!===============================================================================
900     format(1X,A,'Skipping draw ',I4,': no space enough table')
910     format(1X,A,'Entry: ',I2,' Skipping draw ',I4,': status incorrect: ',I2)
920     format(1X,A,'Entry: ',I2,' ERROR draw ',I4,' FILE NOT FOUND: ',A20)
930     format(1X,A,'Passive ',A4,' open emission: ',I4)
940     format(1X,A,'Tomorrows CDC: ',I4)
        end
