C UPDJACKPOT.FOR
C
C V01 12-DEC-2010 FJG LOTTO2 BATCH: INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO MOVE JACKPOTS BETWEEN GAMES
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        options /check=nooverflow
        program updjackpot
        implicit none
c
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:dltrec.def'
        include 'inclib:dkkrec.def'
        include 'inclib:dsprec.def'
        include 'inclib:dtgrec.def'
        include 'inclib:standard.def'
!
        integer*4 gtyp
        integer*4 gind
        integer*4 gnum        
        integer*4 gdrw
        integer*4 glun
        integer*4 gpol
        integer*4 npol
!
        integer*4 ffdb(7)
!
        integer*4 opts           
        integer*4 erro
        integer*4 temp
        integer*4 offs
        integer*4 pool
!===============================================================================
!       INIT PROCESS
!===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<< UPDJACKPOT -  Update Draws Jackpots - V01 >>>>>'
        type*,iam()
        pool = 0
!        
10      continue
        if(pool.eq.0) then
          type*,iam()          
          type*,iam(),'1.- DISCHARGE a Jackpot pool'
          type*,iam(),'2.- INCREASE a Jackpot pool'
          type*,iam()          
    	  call inpnum('Enter option (E-Exit): ',opts,1,2,erro)
    	  if(erro.ne.0) call gstop(GEXIT_OPABORT)          
        else
          opts = 2          
        endif
!
        type*,iam()
        if(opts.eq.1) then
          type*,iam(),'Select the game to DISCHARGE the Jackpot pool'
        else
          type*,iam(),'Select the game to INCREASE the Jackpot pool'
          type*,iam()
          if(pool.ne.0) write(5,9000) iam(),csmony(pool,14,VALUNIT)
        endif
!          
        type*,iam()
        do temp=1,MAXGAM	
          if(gnttab(GAMTYP,temp).gt.0) then
            write(5,'(1X,A,1X,I2,1X,4A4)') iam(),temp,(glnames(offs,temp),offs=1,4)
          endif
        enddo
        type*,iam()
!
        call inpnum('Enter game number: ',gnum,1,maxgam,erro)
        if (erro.ne.0) goto 10
!
        call inpnum('Enter draw number: ',gdrw,1,99999,erro)
        if (erro.ne.0) goto 10
!
        gtyp = gnttab(gamtyp,gnum)
        gind = gnttab(gamidx,gnum)
!
        call find_available_lun(glun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error getting logical unit for game: ',gnum
          if (erro.ne.0) goto 10
        endif
!        
        call openw(glun,gfnames(1,gnum),4,0,0,erro)
        if(erro.ne.0) then
          call filerr(gfnames(1,gnum),1,erro,0)
          goto 10
        endif
!        
        if (gtyp.eq.TLTO) then
          call ioinit(ffdb,glun,dltsec*256)
        elseif (gtyp.eq.TSPT) then
          call ioinit(ffdb,glun,dspsec*256)
        elseif (gtyp.eq.TKIK) then
          call ioinit(ffdb,glun,dkksec*256)
        elseif (gtyp.eq.TTGL) then
          call ioinit(ffdb,glun,dtgsec*256)
        else
          type*,iam(),'Invalid game type ',gtyp
          if (erro.ne.0) goto 10
        endif
!
        if (gtyp.eq.TLTO) then
          call readw(ffdb,gdrw,dltrec,erro)
        elseif (gtyp.eq.TSPT) then
          call readw(ffdb,gdrw,dsprec,erro)
        elseif (gtyp.eq.TKIK) then
          call readw(ffdb,gdrw,dkkrec,erro)
        elseif (gtyp.eq.TTGL) then
          call readw(ffdb,gdrw,dtgrec,erro)
        endif
!
        if(erro.ne.0) then
          call filerr(gfnames(1,gnum),2,erro,gdrw)
          call closefil(ffdb)
          goto 10
        endif
!
        if(opts.eq.1) then !------------- Get JACKPOT
          if (gtyp.eq.TLTO) then
            gpol = dltpol(1)
          elseif (gtyp.eq.TSPT) then
            gpol = dsppol(1)
          elseif (gtyp.eq.TKIK) then
            gpol = dkkpol(1,1)
          elseif (gtyp.eq.TTGL) then
            gpol = dtgpol(1)
          endif     
          write(5,9010) iam(),gdrw,csmony(gpol,14,VALUNIT)
          call inpmony('Enter total jackpot fund to be substracted ',npol,valunit,erro)
          if (erro.ne.0) then
            type*,iam(),'Exitting...'
            call closefil(ffdb)
            goto 10            
          endif
          gpol = gpol - npol
          write(5,9020) iam(),gdrw,csmony(gpol,14,VALUNIT)       
          call inpyesno('Is this correct [Y/N] ?',erro)
          if(erro.eq.1) then
            pool = npol  
            if (gtyp.eq.TLTO) then
              dltpol(1) = gpol
              call writew(ffdb,gdrw,dltrec,erro)
            elseif (gtyp.eq.TSPT) then
              dsppol(1) = gpol
              call writew(ffdb,gdrw,dsprec,erro)
            elseif (gtyp.eq.TKIK) then
              dkkpol(1,1) = gpol
              call writew(ffdb,gdrw,dkkrec,erro)              
            elseif (gtyp.eq.TTGL) then
              dtgpol(1) = gpol
              call writew(ffdb,gdrw,dtgrec,erro)              
            endif               
            if(erro.ne.0) call filerr(gfnames(1,gnum),3,erro,gdrw)   
          endif
        else               !------------- Put JACKPOT
          if (gtyp.eq.TLTO) then
            gpol = dltpol(1)
          elseif (gtyp.eq.TSPT) then
            gpol = dsppol(1)
          elseif (gtyp.eq.TKIK) then
            gpol = dkkpol(1,1)
          elseif (gtyp.eq.TTGL) then
            gpol = dtgpol(1)
          endif     
          write(5,9030) iam(),gdrw,csmony(gpol,14,VALUNIT)
          if(pool.gt.0) then
            npol = pool            
          else
            call inpmony('Enter total jackpot fund to be added',npol,valunit,erro)
            if (erro.ne.0) then
              type*,iam(),'Exitting...'
              type*,iam()
              call closefil(ffdb)
              goto 10            
            endif
          endif
          gpol = gpol + npol
          write(5,9020) iam(),gdrw,csmony(gpol,14,VALUNIT)       
          call inpyesno('Is this correct [Y/N] ?',erro)
          if(erro.eq.1) then
            pool = 0  
            if (gtyp.eq.TLTO) then
              dltpol(1) = gpol
              call writew(ffdb,gdrw,dltrec,erro)
            elseif (gtyp.eq.TSPT) then
              dsppol(1) = gpol
              call writew(ffdb,gdrw,dsprec,erro)
            elseif (gtyp.eq.TKIK) then
              dkkpol(1,1) = gpol
              call writew(ffdb,gdrw,dkkrec,erro)              
            elseif (gtyp.eq.TTGL) then
              dtgpol(1) = gpol
              call writew(ffdb,gdrw,dtgrec,erro)              
            endif               
            if(erro.ne.0) call filerr(gfnames(1,gnum),3,erro,gdrw)   
          endif
        endif       
!
        call closefil(ffdb)        
        goto 10
!===============================================================================
!       FORMAT STATEMENTS
!===============================================================================
9000    format(1X,A,'JACKPOT previously discharged available: ',A14)
9010    format(1X,A,'Dischargable JACKPOT for draw ',I4,': ',A14)
9020    format(1X,A,'The new JACKPOT for draw ',I4,' will be: ',A14)
9030    format(1X,A,'Actual JACKPOT for draw ',I4,': ',A14)
9040    format(1X,A,'Reamining discharger JACKPOT available: ',A14)
        end
