C
C GENPAS.FOR
C
C V04 12-AUG-2011 RXK "Millennium" replaced with "ES Evolution"
C V03 15-OCT-2010 FJG Automate loading process
C     03-NOV-2010 FJG Include .TST processing
C V02 01-JAN-2010 FJG ePassive
C V01 17-JAN-2001 CS  INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO SELECT AND WRITE ON A FILE THE PASSIVE LOTTERY FILE.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! COMPLETELY REWRITE
!
C=======OPTIONS /check=nooverflow
        program genpas
        implicit none
!       
        include '(lib$routines)'
        include 'inclib:sysextrn.def'
        include 'inclib:sysdefine.def'
!       
        include 'inclib:global.def'
        include 'inclib:dparec.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def' 		
!   
        integer*4    erro
        integer*4    nrec
        character*20 cfil
        character*19 crec
        character*24 dlit
        logical      updt
        logical      ovrd
        integer*4    gind
        integer*4    gnum
        integer*4    scml
        integer*4    temp
        integer*4    draw  
        integer*4    flun
        integer*4    xlun        
        integer*4    xnum
        integer*4    xser
        integer*4    xfra
        integer*4    mnum
        integer*4    mser
        integer*4    mfra        
        integer*4    inum
        integer*4    iser
        integer*4    ifra           
        integer*4    xsec
        integer*4    netk
        integer*4    nptk 
        integer*4    rlun   
        integer*4    page
        integer*4    ffdb(7)    
        integer*4    stab(pbilmin:pbilmax+2)                
        character*15 ctab(pbilmin:pbilmax+2)/'RetAftWinner:  ',
     *                                       'RetAfterDraw:  ',
     *                                       'RetWinner:     ',
     *                                       'SoldOffWin:    ',
     *                                       'ReturnedOff:   ',
     *                                       'SoldOffline:   ',
     *                                       'AvailableOff:  ',
     *                                       'NOT DEFINED:   ',
     *                                       'AvailableOnl:  ',     
     *                                       'SoldOnline:    ',
     *                                       'CancelledOnl:  ',     
     *                                       'SoldOnlWin:    ',
     *                                       'CanOnlineWin:  ',
     *                                       'Total Tickets: ',
     *                                       'Erroneous:     '/               
        integer*4    itpf(5)
        character*20 ctpf
        equivalence  (itpf,ctpf)
!       
        integer*4    rlen
        parameter    (rlen = 19)
!       
        integer*4    tind
        integer*4    aind
        integer*4    amax
        parameter    (amax = 10)
        character*20 afil(amax)
!       
        integer*4 alls(0:99999,10,6) ! Maximum number, series and fractions
!       
        character*255   look
        character*255   long 
!        
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec        	
!===============================================================================
!       START OF PROGRAM
!===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<<<<<<<< GENPAS - Generation of TPF files >>>>>>>>>>>>'
        type*,iam()
!       
        if (daysts.eq.dsopen) then
          type*,iam()
          type*,iam(),' System still ACTIVE'
          type*,iam()
          call gpause
        endif
!
        tind = 0
        aind = 0
        page = 1
!+++++++Let's look for files. First .DAT and later on .0xx++++++++++++++++++++++        
        look = 'cod*.dat;*'
        do while (lib$find_file (look, long, erro))
          aind = aind + 1
          afil(aind) = long(index(long,']')+1:index(long,';')-1)
          if(aind.eq.amax) then
            type*,iam(),'WARNING: Maximum number of ',amax,' files to load reached'
            exit
          endif
        end do        
!        
        look = 'cod*.001;*'
        do while (lib$find_file (look, long, erro))
          aind = aind + 1
          afil(aind) = long(index(long,']')+1:index(long,';')-1)
          if(aind.eq.amax) then
            type*,iam(),'WARNING: Maximum number of ',amax,' files to load reached'
            exit
          endif
        end do
!        
        look = 'cod*.tst;*'
        do while (lib$find_file (look, long, erro))
          aind = aind + 1
          afil(aind) = long(index(long,']')+1:index(long,';')-1)
          if(aind.eq.amax) then
            type*,iam(),'WARNING: Maximum number of ',amax,' files to load reached'
            exit
          endif
        end do        
!               
        call find_available_lun(rlun,erro)
        if(erro.ne.0) then          
          type*,iam(),'Not available logical units for report'
          aind = 0   ! STOP Processing
        endif
!
        call ropen('genpas.rep',rlun,erro)    
        if (erro.ne.0) then
          type*,iam(),'Report file open error > ',erro
          aind = 0   ! STOP Processing
        end if              
!===============================================================================
!       Process File
!===============================================================================
10      continue
        if(tind.eq.aind) then
          type*,iam()
          call gstop(gexit_success)
        endif
!        
        tind = tind + 1
        cfil = afil(tind)
!        
        type*,iam()
        type*,iam(),'=========================================================='
        type*,iam()
        type*,iam(),'Processing file: ',cfil
        type*,iam()
        call fastset(0,alls,100000*10*6)
!       call  inptext('Enter name of COD file to load:', cfil, erro)
!       if(cfil.eq.'E '.or.cfil.eq.'e ') call gstop(GEXIT_OPABORT)
!       
        if(.not.filexist(cfil)) then
          type*,iam(),'File not found: ',cfil
          goto 90
        endif
!                  
        if(cfil(1:3).ne.'COD'.and.cfil(1:3).ne.'cod') then
          type*,iam(),'Incorrect type of file: ',cfil(1:3)
          goto 90
        endif            
!           
        if(cfil(4:4).ne.'P'.and.cfil(4:4).ne.'p'.and.cfil(4:4).ne.'C'.and.cfil(4:4).ne.'c') then              
          type*,iam(),'Incorrect type of game: ',cfil(4:4)
          goto 90           
        endif
!       
        if(cfil(4:4).eq.'C'.or.cfil(4:4).eq.'c') then
          gind = PSBCLA
        else
          gind = PSBPOP                
        endif 
!       
        gnum = gtntab(tpas,gind)              
        scml = ctoi(cfil(5:8),temp)    ! SCML external draw number
        if(temp.ne.4) then
          type*,iam(),'Incorrect emission number: ',cfil(5:8)
          goto 90
        endif
!        
        draw = ctoi(cfil(10:13),temp) ! Millennium internal draw number                
        if(temp.ne.4) then
          type*,iam(),'Incorrect internal draw number: ',cfil(10:13)
          goto 90          
        endif
!       
        if(cfil(14:14).ne.'.') then
          type*,iam(),'Incorrect extension: ',cfil(14:17)
          goto 90          
        endif
!       
        write(ctpf,'(5A4)') (sfnames(temp,tpf),temp=1,5) 
        write(ctpf,'(A8,I2.2,I4.4,A6)') ctpf(1:8),gind,draw,ctpf(15:20)
        
        if(cfil(15:17).eq.'dat'.or.cfil(15:17).eq.'DAT') then                    
          if(filexist(ctpf)) then
            type*,iam(),'File already exist for CREATE mode: ',ctpf     
            goto 90             
          else
            updt = .false.                          
          endif
        else
          if(cfil(15:17).eq.'tst'.or.cfil(15:17).eq.'TST') then                              
            ovrd = .true.
            if(filexist(ctpf)) then
              updt = .true.                        
            else
              type*,iam(),'File does not exist for UPDATE mode: ',ctpf 
              goto 90           
            endif
          else
            ovrd = .false.
            if(filexist(ctpf)) then
              updt = .true.                        
            else
              type*,iam(),'File does not exist for UPDATE mode: ',ctpf 
              goto 90           
            endif
          endif
        endif
!===============================================================================
!       ACCESS GAME FILE
!===============================================================================
        call find_available_lun(flun,erro)
        if(erro.ne.0) then          
          type*,iam(),'Not available logical units for file'
          goto 90
        endif
!        
        call openw(flun,gfnames(1,gnum),4,0,0,erro)
        if(erro.ne.0) then
          call filerr(gfnames(1,gnum),open_error,erro,0)
          goto 90
        endif
        call ioinit(ffdb,flun,dpasec*256)        
!       
        call readw(ffdb,draw,dparec,erro)
        if(erro.ne.0) then
          call filerr(gfnames(1,gnum),read_error,erro,draw)             
          call closefil(ffdb)
          goto 90
        endif
!       
        if(dpaemis.ne.draw) then
          type*,iam(),'Inconsistent draw number: ',draw,dpaemis
          goto 90
        endif
!       
        temp = dpadraw/10000
        temp = (temp*100) + mod(dpadraw,100)
        if(temp.ne.scml) then
          type*,iam(),'Inconsistent external draw number: ',temp,scml
          goto 90
        endif        
!       
        if(updt) then
          if(dpasts.ne.GAMOPN) then
            type*,iam(),'Incorrect draw status for update: ',dpasts
            goto 90
          endif
          if(dpabsd.le.daycdc) then
            type*,iam(),'WARNING: Sales are already opened in CDC: ',dpabsd
          endif
        else
          if(dpasts.ne.GAMINF) then
            type*,iam(),'Incorrect draw status for create: ',dpasts
            goto 90
          endif
        endif        
!       
        mnum = dpanumtck-1
        mser = dpanumser
        mfra = dpanoffra
        write(dlit,'(6A4)') (dpalitdrw(temp),temp=1,6)
!===============================================================================
!       SHOW DRAW INFO
!===============================================================================
        type*,iam()
        type*,iam(),'Passive index:     ',gind
        type*,iam(),'SCML draw:         ',scml
        type*,iam(),'ES Evolution draw: ',draw
        type*,iam(),'File:      ',ctpf
        if(updt) then
          type*,iam(),'Mode of load:            UPDATE'
        else
          type*,iam(),'Mode of load:            CREATE'            
        endif
        type*,iam(),'Max number:        ',mnum
        type*,iam(),'Max serie:         ',mser
        type*,iam(),'Max fraction:      ',mfra
        type*,iam(),'Draw name: ',dlit
        type*,iam()        
!===============================================================================
!       READ COD FILE
!===============================================================================
        call find_available_lun(xlun,erro)
        if(erro.ne.0) then
          type*,iam(),' Not available logical units'
          goto 90
        endif
!          
        open(unit         = xlun,
     *       file         = cfil,
     *       form         = 'FORMATTED',
     *       organization = 'SEQUENTIAL',
     *       access       = 'SEQUENTIAL',
     *       recl         = rlen,
     *       status       = 'OLD',
     *       recordtype   = 'VARIABLE',
     *       iostat       = erro)
!       
        if(erro.ne.0) then
          type*,iam(),'Error opening file: ',cfil,erro
          goto 90
        else
          type*,iam(),'Loading tickets in memory...'
          type*,iam()
          nrec = 0
          netk = 0
          nptk = 0
          do while (erro.eq.0)
            if(nrec.gt.0.and.mod(nrec,100000).eq.0) type*,iam(),nrec,' Records loaded...'
            read(xlun,'(A<rlen>)',iostat=erro,end=99) crec
            nrec = nrec + 1                
            if(erro.ne.0) then
              type*,iam(),'Error reading record: ',nrec,erro
              erro = erro + 1
            else
              xnum = ctoi(crec(1:5),temp)
              if(temp.ne.5) then
                type*,iam(),'Error in record format number: ',crec(1:5),temp
                erro = erro + 1
              else
                if(xnum.lt.0.or.xnum.gt.mnum) then
                  type*,iam(),'Error in number limits: ',xnum,mnum
                  erro = erro + 1
                endif
              endif
!       
              xser = ctoi(crec(7:8),temp)                    
              if(temp.ne.2) then
                type*,iam(),'Error in record format serie: ',crec(7:8),temp
                erro = erro + 1
              else
                if(xser.lt.0.or.xser.gt.mser) then
                  type*,iam(),'Error in serie limits: ',xser,mser
                  erro = erro + 1
                endif
              endif
!                  
              xfra = ctoi(crec(10:11),temp) 
              xfra = xfra + 1                 ! HARDCODE BECAUSE OF BARCODE                
              if(temp.ne.2) then
                type*,iam(),'Error in record format fraction: ',crec(10:11),temp
                erro = erro + 1
              else
                if(xfra.lt.0.or.xfra.gt.mfra) then
                  type*,iam(),'Error in fraction limits: ',xfra,mfra
                  erro = erro + 1
                endif
              endif
!                  
              xsec = ctoi(crec(13:17),temp)                    
              if(temp.ne.5) then
                type*,iam(),'Error in record format security code: ',crec(13:17),temp
                erro = erro + 1
              endif
!       
              if(crec(19:19).ne.'V'.and.crec(19:19).ne.'v'.and.crec(19:19).ne.'I'.and.crec(19:19).ne.'i') then
                type*,iam(),'Error in record format type: ',crec(19:19),temp                              
                erro = erro + 1
              endif
!                   
              if(erro.eq.0) then
                if(crec(19:19).eq.'V'.or.crec(19:19).eq.'v') then
                  alls(xnum,xser,xfra) = '0E000000'X
                  netk = netk + 1
                else
                  alls(xnum,xser,xfra) = '0A000000'X + xsec
                  nptk = nptk + 1                              
                endif
              else
                type*,iam(),'Error(s) ',erro,' in record: ',nrec
                type*,iam(),'File NOT loaded... skipping'               
              endif
            endif
          enddo
        endif
        goto 90
!===============================================================================
!       EOF
!===============================================================================
99      continue  
        close(unit=xlun)  
!            
        type*,iam()
        type*,iam(),'Records read:    ',nrec
        type*,iam()        
        type*,iam(),'eTickets:        ',netk
        type*,iam(),'pTickets:        ',nptk
        type*,iam(),'Total tickets:   ',netk+nptk 
!===============================================================================
!       MANAGE TPF FILE
!===============================================================================
        call pasio_init(pasfdb,gind,draw,mnum,mser,mfra,ctpf)
        if(.not.updt) then
          call pasio_create(pasfdb)
          if(pasfdb.err.eq.ioe_noerr) then   
            type*,iam(),'TPF file created: ',pasfdb.filnam  
            type*,iam()            
          else
            type*,iam(),'Error creating file: ',pasfdb.filnam,pasfdb.err
            goto 90
          endif
        endif
!       
        call pasio_open(pasfdb) 
        if(pasfdb.err.ne.ioe_noerr) then   
          type*,iam(),'Error opening file: ',pasfdb.filnam,pasfdb.err
          goto 90
        endif
!       
        call title(pasfdb.filnam,'GENPAS  ',1,rlun,page,daycdc)
        erro = 0
        do inum = 0,mnum
          do iser = 1,mser
            do ifra = 1,mfra
!+++++++++++++IF UPDATE MUST BE READ BEFORE+++++++++++++++++++++++++++++++++++++              
              if(updt) then     
                if(alls(inum,iser,ifra).ge.'0A000000'X.and.alls(inum,iser,ifra).le.'0A01869F'X) then
                  call pasio_read(pasfdb,inum,iser,ifra,pasrec)
                  if(pasfdb.err.ne.ioe_noerr) then
                    type*,iam(),'Error: ',pasfdb.err,' reading record: ',pasrec.key
                    goto 90
                  endif    
                  if(pasrec.stat.lt.pbilnot) then                        ! IF OFFLINE, DO IT... ORACLE ORDERS
                    pasrec.control = alls(inum,iser,ifra) - '0A000000'X  ! UPDATE JUST THE SECURITY CONTROL   
                    call pasio_write(pasfdb,inum,iser,ifra,pasrec)
                    if(pasfdb.err.ne.ioe_noerr) then
                      type*,iam(),'Error: ',pasfdb.err,' writing record: ',pasrec.key
                      goto 90
                    endif                                          
                  else
                    write(rlun,1000) inum,iser,ifra,alls(inum,iser,ifra),'Not pticket. Impossible to update'
                    erro = erro + 1
                  endif
                else
                  if(ovrd.and.alls(inum,iser,ifra).eq.'0E000000'X) then  ! LET CHANGE THE CHANNEL
                    call pasio_read(pasfdb,inum,iser,ifra,pasrec)
                    if(pasfdb.err.ne.ioe_noerr) then
                      type*,iam(),'Error: ',pasfdb.err,' reading record: ',pasrec.key
                      goto 90
                    endif    
                    if(pasrec.stat.eq.pbiloff.or.pasrec.stat.eq.pbilnot) then ! IF ONLINE
                      pasrec.stat    = pbilonl
                      pasrec.control = 0   
                      call pasio_write(pasfdb,inum,iser,ifra,pasrec)
                      if(pasfdb.err.ne.ioe_noerr) then
                        type*,iam(),'Error: ',pasfdb.err,' writing record: ',pasrec.key
                        goto 90
                      endif                                          
                    else
                      write(rlun,1000) inum,iser,ifra,alls(inum,iser,ifra),'Not eticket. Impossible to update'
                      erro = erro + 1                      
                    endif                    
                  else
                    if(alls(inum,iser,ifra).ne.0) then
                      write(rlun,1000) inum,iser,ifra,alls(inum,iser,ifra),'Impossible to change the channel'
                      erro = erro + 1                      
                    endif
                  endif
                endif
              else     
!+++++++++++++IF CREATE ALL TICKETS MUST BE WRITE+++++++++++++++++++++++++++++++ 
                pasrec.stat    = pbilnot
!               pasrec.tics    = 0
                pasrec.cdc     = 0                  
                pasrec.serial  = 0                  
                pasrec.agt     = 0                  
                pasrec.control = 0  
                pasrec.key = pasio_key(inum,iser,ifra)                
                if(alls(inum,iser,ifra).eq.'0E000000'X) then ! eTicket
                  pasrec.stat    = pbilonl
                  pasrec.control = 0
                else
                  if(alls(inum,iser,ifra).ge.'0A000000'X.and.alls(inum,iser,ifra).le.'0A01869F'X) then ! pTickets 5 digits + OFFSET
                    pasrec.stat    = pbiloff
                    pasrec.control = alls(inum,iser,ifra) - '0A000000'X
                  else
                    if (alls(inum,iser,ifra).ne.0) type*,iam(),'Inconsistency info: ',inum,iser,ifra,alls(inum,iser,ifra)
                    pasrec.stat    = pbilnot
                    pasrec.control = 0                    
                  endif
                endif
                call pasio_fwrite(pasfdb,inum,iser,ifra,pasrec)
                if(pasfdb.err.ne.ioe_noerr) then
                  type*,iam(),'Error: ',pasfdb.err,' writing record: ',inum*10000+iser*100+ifra
                  goto 90
                endif 
              endif                             
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++              
            enddo
          enddo
        enddo        
        call pasio_close(pasfdb)
        if(erro.ne.0) then
          type*,iam()
          type*,iam(),'Logical Errors found: ',erro,' Check GENPAS.REP'
        endif
!===============================================================================
!       UPDATE DRAW FILE
!===============================================================================
        if(.not.updt) dpasts = gamopn
        dpachgcdc = daycdc
        call writew(ffdb,draw,dparec,erro)
        if(erro.ne.0) call filerr(gfnames(1,gnum),write_error,erro,draw)             
        call closefil(ffdb)
!===============================================================================
!      ANALYZING TPF FILE
!===============================================================================
        type*,iam()
        type*,iam(),'TPF File updated. Analyzing file integrity...'
        type*,iam()
        call pasio_init(pasfdb,gind,draw,mnum,mser,mfra,ctpf,sizeof(pasrec))
        call pasio_open(pasfdb) 
        if(pasfdb.err.ne.ioe_noerr) then   
          type*,iam(),'Error opening file: ',pasfdb.filnam
          goto 90
        endif        
        do inum = 0,mnum
          do iser = 1,mser
            do ifra = 1,mfra
              call pasio_read(pasfdb,inum,iser,ifra,pasrec)
              if(pasfdb.err.ne.ioe_noerr) then
                stab(pbilmax+2) = stab(pbilmax+2) + 1
                type*,iam(),'Error: ',pasfdb.err,' reading record: ',pasrec.key
              else
                stab(pasrec.stat) = stab(pasrec.stat) + 1
                stab(pbilmax+1)   = stab(pbilmax+1) + 1
              endif
            enddo
          enddo
        enddo
        call pasio_close(pasfdb)            
!       
        do temp=pbilmin,pbilmax+2      
          type*,iam(),ctab(temp),stab(temp)
        enddo
!      
        cfil = cfil(1:14) // 'OLD'
        type*,iam()
        type*,iam(),'File processed. Renaming to: ',cfil
        call lib$rename_file(afil(tind),cfil)        
!===============================================================================
!       END LOOP/CYCLE
!===============================================================================   
90      continue
        goto 10
!===============================================================================
!       FORMAT
!===============================================================================   
1000    format(X,'Number: ',I5.5,' Serie: ',I2.2,' Fraction: ',I2.2,' Status: ',Z8,' Error: ',A)        
        end
