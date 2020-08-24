C SUBROUTINE INTPAS
C
C V01 25-JAN-2011 FJG Load draw even if the status is incorrect
C V00 01-JAN-2010 FJG ePassive
C
C SUBROUTINE TO INITIALIZE PASSIVE GAME COMMONS FOR RESET
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /check=nooverflow/ext
        subroutine intpas(game)
        implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'

        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:datbuf.def'
        include 'inclib:gtnames.def'
        include 'inclib:dparec.def'
        include 'inclib:pasiosubs.def' 	        
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
        integer*4    year
        integer*4    week
        integer*4    asal(2,pagemi)
        integer*4    amax
        integer*4    acan        
        integer*4    tsal(2) 
        integer*4    mnum
        integer*4    mser
        integer*4    mfra
        logical      load
        character*8  cdrw(6) /'PURGED  ','PAYMENTS','SALES   ','RETURNS ','CLOSED  ','ERROR   '/
        character*9  ckey
        character*20 ctpf
        character*20 cvpf        
!
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec          
!
        gtyp = gnttab(gamtyp,game)
        gind = gnttab(gamidx,game)
	draw = daydrw(game)
	drai = draw-50
	draf = draw+40
!
        if(gtyp.ne.tpas) then
          write(5,900) iam(),gtyp
          return
        else
          type*,iam()
        endif
!	
	write(ctpf,'(5A4)') (sfnames(temp,tpf),temp=1,5)
	write(cvpf,'(5A4)') (sfnames(temp,vpf),temp=1,5)	
!	
!       Scan file 
!
        call openw(3,gfnames(1,game),4,0,0,erro)
        call ioinit(fdb,3,dpasec*256)
        if(erro.ne.0) then
          call filerr(gfnames(1,game),open_error,erro,0)
          call gpause()
          return
        endif
!
        emis = 0       
!=====> INI LOOP        
        do drac=draf,drai,-1
          call readw(fdb,drac,dparec,erro)
          if(erro.ne.0) then
            call filerr(gfnames(1,game),read_error,erro,drac)
            call gpause()
            return
          endif
!
          if(dpasts.gt.gaminf) then
            load = .false.
            if(emis.lt.pagemi) then
!+++++++++++++V01+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++              
!             if(dpasts.ge.gamopn.and.dpasts.le.gfinal) then
!               load = .true.
!             endif
!             if(load) then
              if(dpasts.ge.gamopn.and.dpasts.le.gfinal) then  ! V01
                emis = emis + 1    
                if(draw.eq.drac) pascurdrw(gind) = emis       ! SAVE THE DINAMIC CURDRW
                call loggampas(emis,gind,dparec,pasblk)
                write(cpastpffil(emis,gind),'(A8,I2.2,I4.4,A6)') ctpf(1:8),gind,pasemis(emis,gind),ctpf(15:20)
                write(cpasvpffil(emis,gind),'(A8,I2.2,I4.4,A6)') cvpf(1:8),gind,pasemis(emis,gind),cvpf(15:20)                     
!
!               This next lines came from the beginning
!
                if(pastim(emis,gind).ge.'40000000'X) pastim(emis,gind)=pastim(emis,gind)-'40000000'X           
                if(passts(emis,gind).ne.gamopn.or.pastim(emis,gind).eq.0.or.pasesd(emis,gind).ne.daycdc)
     *             pastim(emis,gind) = pastim(emis,gind)+'40000000'X
!
                pasretaftamt(emis,gind) = 0
                pastopayamt(emis,gind) = 0
                pastodpay(emis,gind) = 0
              endif
            endif
          endif
!
        enddo
!=====> FIN LOOP                
        call closefil(fdb)
!
        amax = 0
!=====> INI LOOP                
        do emis = 1,pagemi
          call getpasdrw(pasdraw(emis,gind),week,year)
          year = mod(year,10)
          if(passts(emis,gind).eq.gfinal) then
            pasextdrw(week,year,gind) = emis
            if(pasprgcdc(emis,gind).lt.daycdc) then ! Overpurged
              passubsts(emis,gind) = pdrwclo          
            else
              if(filexist(cpasvpffil(emis,gind))) then
                if(pasesd(emis,gind)+pasmaxdaypay(emis,gind).ge.daycdc) then
                  passubsts(emis,gind) = pdrwval
                else
                  passubsts(emis,gind) = pdrwpur                                  
                endif
              else
                write(5,930) iam(),gsnames(game),cpasvpffil(emis,gind)
                passubsts(emis,gind) = pdrwerr                
              endif
            endif
          else
            if(passts(emis,gind).eq.gamopn) then 
              pasextdrw(week,year,gind) = emis                                                  
              if(filexist(cpastpffil(emis,gind))) then
                passubsts(emis,gind) = pdrwret   
                if(pasbsd(emis,gind).le.daycdc) then
                  amax = amax + 1
                  asal(1,amax) = emis
                  asal(2,amax) = pasbsd(emis,gind)
                endif                   
              else
                write(5,930) iam(),gsnames(game),cpastpffil(emis,gind)
                passubsts(emis,gind) = pdrwerr
              endif                             
            else
              write(5,950) iam(),gsnames(game)
              passubsts(emis,gind) = pdrwerr                 
            endif
          endif
        enddo  
!=====> FIN LOOP                
!
!       LETS DO A BUBBLE TO ORDER THE BDS JUST IN CASE MORE THAN 4 DRAW ARE OPEN FOR SALES
!
        load = .true.
        temp = 1
!=====> INI LOOP                
        do while(load)
          load = .false.
!=======> INI LOOP                  
          do emis = 1,amax-temp
            if(asal(2,emis).gt.asal(2,emis+1)) then
              tsal(1) = asal(1,emis)
              tsal(2) = asal(2,emis)
              asal(1,emis) = asal(1,emis+1)
              asal(2,emis) = asal(2,emis+1)       
              asal(1,emis+1) = tsal(1) 
              asal(2,emis+1) = tsal(2)        
              load = .true.       
            endif
          enddo   
!=======> FIN LOOP                            
          temp = temp + 1                           
        enddo
!=====> FIN LOOP                
!
!       DRAWS FOR SELLING: A MAXIMUM OF FOUR        
!
!=====> INI LOOP  
        do emis = 1,min(amax,pmaxsal)
          passubsts(asal(1,emis),gind) = pdrwwag        
          passaltab(asal(1,emis),gind) = emis  
        enddo
!=====> FIN LOOP          
!
!      LETS DISPLAY THE RESULTS
!
!=====> INI LOOP 
        do emis = 1,pagemi
          if(passubsts(emis,gind).gt.pdrwnot) then
            write(5,920) iam(),gsnames(game),emis,pasemis(emis,gind),
     *                   pasdraw(emis,gind),cdrw(passubsts(emis,gind)),passaltab(emis,gind)
          endif
        enddo   
!=====> FIN LOOP         
        type*,iam()             
!
!      Lets load the bitmaps
!
!=====> INI LOOP 
        do emis = 1,pagemi
          if(passubsts(emis,gind).eq.pdrwwag) then
            if(passaltab(emis,gind).ge.1.and.passaltab(emis,gind).le.pmaxsal) then
!===============================================================================
!             LOAD BITMAP 
!===============================================================================              
              amax = 0
              acan = 0
              call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                        pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_openro(pasfdb)  
              if(pasfdb.err.ne.ioe_noerr) then
                type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam  
                call pasio_dump(pasfdb)
                call gstop(gexit_fatal)              
              else
                type*,iam(),'Loading BITMAP: ',pasfdb.filnam                 
!=============> INI LOOPS
                do mnum = 0,pasnumtck(emis,gind)-1
                  do mser = 1,pasnumser(emis,gind)
                    do mfra = 1,pasnoffra(emis,gind)
                      call pasio_read(pasfdb,mnum,mser,mfra,pasrec)
                      if(pasfdb.err.ne.ioe_noerr) then
                        write(ckey,'(I9.9)') pasrec.key                        
                        type*,iam(),'Error: ',pasfdb.err,' reading record: ',ckey
                        pasioerrs(ioreano,emis,gind) = pasioerrs(ioreano,emis,gind) + 1
                      else
                        pasioerrs(ioreaok,emis,gind) = pasioerrs(ioreaok,emis,gind) + 1
!===============================================================================
!                       IF CANCELLED AND DAYS TO RESELL ARE OVER... AVAILABLE 
!===============================================================================                                     
                        if(pasrec.stat.eq.pbilcon.and.daycdc.ge.(pasrec.cdc+p(pdayrsl))) then
                          pasrec.stat = pbilonl
                          acan = acan + 1
                        endif
!===============================================================================
!                       BASED ON THE TYPE, UPDATE STATUS
!===============================================================================                                                             
                        if(gind.eq.psbcla) then
                          pasnumcla(mnum,passaltab(emis,gind)).billet(mser,mfra) = pasrec.stat
                          if(pasrec.stat.eq.pbilonl) then
                            pasnumcla(mnum,passaltab(emis,gind)).forsal = pasnumcla(mnum,passaltab(emis,gind)).forsal + 1
                            amax = amax + 1
                          endif
                        else
                          pasnumpop(mnum,passaltab(emis,gind)).billet(mser,mfra) = pasrec.stat
                          if(pasrec.stat.eq.pbilonl) then
                            pasnumpop(mnum,passaltab(emis,gind)).forsal = pasnumpop(mnum,passaltab(emis,gind)).forsal + 1
                            amax = amax + 1
                          endif
                        endif
                      endif
                    enddo
                  enddo
                enddo
!=============> END LOOPS                
                call pasio_close(pasfdb)  
                type*,iam(),'BITMAP loaded: ',amax,' fractions available for Sale'
                type*,iam(),'BITMAP loaded: ',acan,' fractions recovered for Sale'
                type*,iam()
              endif              
!===============================================================================
!             END LOADING
!===============================================================================              
            else
              write(5,940) pasemis(emis,gind),passubsts(emis,gind),passaltab(emis,gind)
            endif
          endif
        enddo   
!=====> FIN LOOP         
        return
!===============================================================================
!       Format statements        
!===============================================================================
900     format(1X,A,' ERROR: Bad Game Type (',I2,') not Passive: ')
920     format(1X,A,X,A4,' loaded ',I2,' draw: ',I4,' (',I6.6,') for: ',A,' (',I1,')')
930     format(1X,A,X,A4,' ERROR: Deactivating draw. File not found: ',A20)
940     format(1X,A,X,A4,' ERROR: Loading bitmap for draw: ',I4,' [',I2,'/',I2,']')
950     format(1X,A,X,A4,' ERROR: Incorrect status, deactivating draw')
        end
