C
C PROGRAM GNINVALL
C
C GNINVALL.FOR
C
C V06 05-SEP-2013 SCML New Billing Platform (NBP):
C                      Adapting GNINVALL to run every day, not just at
C                      invoice days; writing data with lotteries (which 
C                      are passive games) to AM_WAG file. 
C V05 30-JAN-2013 SCML Fix: don't accumulate prize amounts which have OP.
C                      Since prizes began to be taxed (01.01.2013), the
C                      difference between prize amount and OP prize amount
C                      is no longer zero for prizes taxed, which mean
C                      the difference began to be accumulated, erroneously,
C                      in the accounting of the total amount of prizes.
C V04 01-OCT-2011 FJG Don't show not defined games
C V03 04-APR-2011 FJG Incorrect Concurso ID
C V02 23-MAR-2011 FJG OPs with CASH prize counted
C V01 13-JAN-2010 FJG Lotto2 Batch: New invoice interface
C     02-FEB-2011 FJG Increase length
C     21-FEB-2011 FJG Joker counting twice
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
C
C====== options /check=nooverflow
        program gninvall
        implicit none
c
        include '(LIB$ROUTINES)'
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
c        
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:agtcom.def'        
        include 'inclib:prmlog.def'
        include 'inclib:destra.def'
        include 'inclib:deslog.def'
        include 'inclib:datbuf.def'        
        include 'inclib:tnames.def'                
        include 'inclib:dltrec.def'
        include 'inclib:dsprec.def'
        include 'inclib:dtgrec.def'
        include 'inclib:dkkrec.def'        
        include 'inclib:prmvlf.def'              
        include 'inclib:desval.def'            
        include 'inclib:vdetail.def'             
C===============================================================================
        integer*4    gnum
        integer*4    gtyp
        integer*4    gcdc
        integer*4    icdc
        integer*4    fcdc                
        integer*4    offs
        character*20 gfil(7,maxgam)
        character*20 cfil
        logical      xfil
        integer*4    erro
        integer*4    nagt
        integer*4    xdrw
        integer*4    kdrw        
        integer*4    odrw        
        integer*4    xday 
        integer*4    xwek
        integer*4    xano   
        integer*4    ffdb(7)        
        integer*4    tbuf(8192) 
        integer*4    mdrw
        parameter   (MDRW = 20)
        integer*4    flun
        parameter   (FLUN = 10)        
        integer*4    blck
        integer*4    teof
        integer*4    bind
        integer*4    tamt(3,NUMAGT,MDRW,MAXGAM)  ! Amounts
        integer*2    tcnt(3,NUMAGT,MDRW,MAXGAM)  ! Counter
        real*8       camt(3,MAXGAM+1) 
        integer*4    ccnt(3,MAXGAM+1) 
        character*7  csor(MDRW,MAXGAM)
        character*8  dsor(MDRW,MAXGAM)        
        integer*4    tots
        integer*2    idat(LDATE_LEN) /LDATE_LEN*0/    
        integer*2    fdat(LDATE_LEN) /LDATE_LEN*0/    
        integer*4    rtyp
        integer*4    rlen
!
        integer*4    mrec
        parameter   (mrec=2048/vflen)
        integer*4    vbuf(32*64)
        integer*4    xbuf(vflen,mrec)
        equivalence (vbuf,xbuf)
!        
        integer*4    temp
        byte         i1temp(4)
        equivalence (temp,i1temp)        
C===============================================================================
C       START PROCESSING
C===============================================================================
        type*,iam()
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<< GNINVALL - Generate INVOICE information >>>>>>'
        type*,iam()
        call fastset(0,tamt,3*MDRW*NUMAGT*MAXGAM)
        call fastset(0,tcnt,(3*MDRW*NUMAGT*MAXGAM)/2)        
C=======LOAD DRAW INFORMATION===================================================
        type*,iam(),'Loading draw dates...'        
        do gnum = 1,MAXGAM
          gtyp = gnttab(GAMTYP,gnum)
          if(gtyp.ne.TPAS) then
            if(dayhdr(gnum).gt.0) then
              call openw(1,gfnames(1,gnum),0,0,0,erro)
              if(erro.ne.0) then
                type*,iam(),'Error#',erro,' opening file game#',gnum
                call gstop(GEXIT_FATAL)
              endif
!              
              if(gtyp.eq.tlto) then
                call ioinit(ffdb,1,dltsec*256)
              elseif(gtyp.eq.tspt) then
                call ioinit(ffdb,1,dspsec*256)
              elseif(gtyp.eq.ttgl) then
                call ioinit(ffdb,1,dtgsec*256)
              elseif(gtyp.eq.tkik) then
                call ioinit(ffdb,1,dkksec*256)
              else
                type*,iam(),'Game Type ',gtyp,' not supported'
              endif
!
              do xdrw = dayhdr(gnum) + 4,max(1,dayhdr(gnum) - 15),-1
                odrw = dayhdr(gnum) + 5 - xdrw

                if(odrw.gt.0.and.odrw.le.MDRW) then
                  if(gtyp.eq.tlto) then
                    call readw(ffdb,xdrw,dltrec,erro)
                    idat(VCDC) = dltdat(1)
                    call lcdate(idat)
                    if(dltccc.gt.0) then
                      xwek = dltccc
                      xano = idat(VYEAR2)
                    else
                      call figwek(dltbsd,xwek,xano) ! V03 Instead of DLTDAT(1)
                    endif
                    write(csor(odrw,gnum),9020) xwek,xano
                    write(dsor(odrw,gnum),9030) idat(VYEAR2),idat(VMON),idat(VDAY)                  
                  elseif(gtyp.eq.tspt) then
                    call readw(ffdb,xdrw,dsprec,erro)
                    idat(VCDC) = dspdat(1)
                    call lcdate(idat)
                    call figwek(dspbsd,xwek,xano) ! V03 Instead of DSPDAT(1)
                    write(csor(odrw,gnum),9020) xwek,xano
                    write(dsor(odrw,gnum),9030) idat(VYEAR2),idat(VMON),idat(VDAY)  
                  elseif(gtyp.eq.ttgl) then
                    call readw(ffdb,xdrw,dtgrec,erro)
                    idat(VCDC) = dtgdat(1)
                    call lcdate(idat)
                    call figwek(dtgbsd,xwek,xano) ! V03 Instead of DSPDAT(1)
                    write(csor(odrw,gnum),9020) xwek,xano
                    write(dsor(odrw,gnum),9030) idat(VYEAR2),idat(VMON),idat(VDAY)                      
                  elseif(gtyp.eq.tkik) then
                    call readw(ffdb,xdrw,dkkrec,erro)
                    idat(VCDC) = dkkdat(1)
                    call lcdate(idat)
                    call figwek(dkkesd,xwek,xano) ! V03 Instead of DKKDAT(1)
                    write(csor(odrw,gnum),9020) xwek,xano
                    write(dsor(odrw,gnum),9030) idat(VYEAR2),idat(VMON),idat(VDAY)                 
                  else
                    type*,iam(),'Game Type ',gtyp,' not supported'                         
                  endif
                  if(erro.ne.0) then
                    type*,iam(),'Error#',erro,' reading file game#',gnum,' record#',xdrw
                    call gstop(GEXIT_FATAL)                  
                  endif
                else
                  type*,iam(),'Incorrect Draw offset in Game ',odrw,gnum
                endif
              enddo
            endif            
          endif
        enddo
C=======LOOK FOR FILES==========================================================
        type*,iam(),'Loading game files...'
        type*,iam()
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------
C        icdc = daycdc-7+1
        icdc = daycdc
        idat(VCDC) = icdc
        call lcdate(idat) ! Init date
C        fcdc = daycdc
        fcdc = icdc
        fdat(VCDC) = fcdc ! End date
        call lcdate(fdat)        
C        write(5,9100) iam(),(idat(temp),temp=7,13),icdc,(fdat(temp),temp=7,13),fcdc
        write(5,9100) iam(),(idat(temp),temp=7,13),icdc
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------        
        type*,iam()                
!        
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C       initializing gfil array with default values
C-----------------------------------------------------------------------
        do gnum = 1,MAXGAM
            do xday = 1,7
                gfil(xday,gnum) = 'NONE'
            enddo
        enddo
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------
        do gnum = 1,MAXGAM
          gtyp = gnttab(GAMTYP,gnum)
          if(gtyp.gt.0.and.gtyp.ne.TPAS) then   ! NO PASSIVE, ONLY ACTIVE GAMES
            temp = 0
            do gcdc = icdc,fcdc
              write(cfil,9000) P(REG_DRWPCK),GSNAMES(gnum),gcdc
              inquire(file=cfil,exist=xfil)
              if(xfil)then
                gfil(fcdc-gcdc+1,gnum) = cfil
                temp = temp + 1
              else
                gfil(fcdc-gcdc+1,gnum) = 'NONE'
              endif
            enddo
            write(cfil,'(4A4)') (glnames(offs,gnum),offs=1,4)
            if(temp.eq.0) then
              if(daydrw(gnum).gt.0) then
                type*,iam(),'>>> ERROR: ',cfil,' ACTIVE but no files were FOUND'
              else
                type*,iam(),cfil,' NOT ACTIVE'
              endif
            elseif(temp.eq.7) then
              if(daydrw(gnum).gt.0) then
                type*,iam(),cfil,' ACTIVE'              
              else
                type*,iam(),'>>> WARNING: ',cfil,' ACTIVE but no draws are configured'
              endif
            else
              type*,iam(),'>>> WARNING: ',cfil,' ACTIVE ',temp,' days <<<'              
            endif
          else
            do gcdc = icdc,fcdc
              gfil(fcdc-gcdc+1,gnum) = 'NONE'
            enddo
          endif
        enddo
        type*,iam()
C=======PROCESSING LOOP 1=======================================================
        type*,iam(),'Processing game files for sales and cancellations...'
        type*,iam()
        do gnum=1,MAXGAM
          do xday=1,7      
            if(gfil(xday,gnum).ne.'NONE') then
              call openx(1,gfil(xday,gnum),0,0,0,erro)
              call ioinit(ffdb,1,128*256)
              if(erro.eq.0) then
                tots = 0
                blck = 0
                teof = 0
                bind = 8192
                do while (teof.lt.1000)
                  if(bind.ge.8157) then
                    blck = blck+1
                    bind = 1
                    call readw(ffdb,blck,tbuf,erro)
                    if(erro.ne.0) then
                      type*,iam(),'Reading Error#',erro,' Record: ',blck
                      call gpause
                    endif
                  endif     
                  if(tbuf(bind).EQ.0) then
                    teof = teof + 1
                    bind = bind + LREC
                  else
                    teof = 0
                    temp = tbuf(bind+LREC-1)
                    rtyp = i1temp(4)
                    if(rtyp.ne.lone.and.rtyp.ne.lreg) then
                      type*,iam(),' Bad record Type ',rtyp,' Index > ',bind
                      bind = bind + LREC
                    else
                      call logtra(trabuf,tbuf(bind))
                      if(trabuf(ttyp).eq.TWAG) then
                        if(trabuf(tstat).eq.GOOD) then
                          offs = 1
                        elseif(trabuf(tstat).eq.VOID) then
                          offs = 2
                        else
                          type*,iam(),'Incorrect Transaction Status ',trabuf(TSER)                          
                          offs = 2                          
                        endif
                        nagt = trabuf(tter)!                        
                        do xdrw = trabuf(TWBEG),trabuf(TWEND)
                          odrw = dayhdr(gnum) + 5 - xdrw
                          if(odrw.gt.0.and.odrw.le.MDRW) then
                            tcnt(offs,nagt,odrw,gnum) = tcnt(offs,nagt,odrw,gnum) + 1
                            tamt(offs,nagt,odrw,gnum) = tamt(offs,nagt,odrw,gnum) + trabuf(TWAMT)
                          else
                            type*,iam(),'Incorrect Draw offset ',odrw,trabuf(TSER)                            
                          endif
                        enddo
                        if(trabuf(twkgme).ne.0.and.trabuf(twkgme).ne.gnum.and.(trabuf(twkflg).ne.0.or.trabuf(twkflg2).ne.0)) then
                          do xdrw = trabuf(TWKBEG),trabuf(TWKEND)
                            odrw = dayhdr(trabuf(twkgme)) + 5 - xdrw
                            if(odrw.gt.0.and.odrw.le.MDRW) then
                              tcnt(offs,nagt,odrw,trabuf(twkgme)) = tcnt(offs,nagt,odrw,trabuf(twkgme)) + 1
                              tamt(offs,nagt,odrw,trabuf(twkgme)) = tamt(offs,nagt,odrw,trabuf(twkgme)) + trabuf(TWKAMT)
                            else
                              type*,iam(),'Incorrect Joker draw offset ',odrw,trabuf(TSER)
                            endif
                          enddo                          
                        endif 
                      else
                        type*,iam(),'Incorrect Transaction Type ',trabuf(TSER)
                      endif
                      tots = tots + 1
!                      
                      rlen = LREC
                      if(rtyp.eq.LONE) then
                        temp = tbuf(bind + LREC*2-1)
                        rtyp = i1temp(4)
                        if(rtyp.eq.LEND) rlen = LREC*2
                        if(rtyp.eq.LTWO) rlen = LREC*3
                      endif
                      bind = bind + rlen
                    endif
                  endif
                enddo
                type*,iam(),'Processed file ',gfil(xday,gnum),tots,' records'
                call closefil(ffdb)
              else
                type*,iam(),'Error#',erro,' opening file ',gfil(xday,gnum)
                call gpause
              endif
            endif
          enddo
        enddo         
        type*,iam()        
C=======PROCESSING LOOP 2=======================================================
        type*,iam(),'Processing Validation files for payments...'
        type*,iam()
        blck = 0     
        teof = 0   
        call openw(1,sfnames(1,vlf),0,0,0,erro)
        if (erro.ne.0)then
          type*,iam(),'Error#',erro,' opening VLF file '
          call gstop(GEXIT_FATAL)
        endif
        call ioinit(ffdb,1,32*256)
        do while (.not.teof)
          blck = blck+1
          call readw(ffdb,blck,vbuf,erro)
          if(erro.ne.0) then
            if(erro.ne.144) then
              type*,iam(),'Error#',erro,' reading VLF file in block#',blck
              call gstop(GEXIT_FATAL)              
            else
              teof = 1
            endif
          else
            rlen = 0
            do bind = 1,mrec
              if(rlen.gt.0) then
                rlen = rlen - 1
                cycle
              endif
              if(xbuf(1,bind).ne.0) then
                rlen = ishft(xbuf(1,bind),-30)
                call logval(valrec,xbuf(1,bind))
!===============================================================================                
                if(valrec(VSTAT).eq.VCASH) then
C V02             if(valrec(VOPSCNT).eq.0) then
C V02               if(valrec(VOPSAMT)+valrec(VKOPSAMT).ne.0) then
C V02                 type*,iam(),'=================================================='
C V02                 type*,iam(),'Payment Order amount not zero: ',valrec(VOPSAMT)+valrec(VKOPSAMT)
C V02                 type*,iam(),'Validation Serial#',valrec(VSCDC),'/',valrec(VSSER)
C V02                 type*,iam(),'=================================================='                            
C V02               endif
                  if(valrec(VCCDC).ge.icdc.and.valrec(VCCDC).le.fcdc) then
                    call dlogval(valrec,vdetail)                    
                    nagt = valrec(VCTER)
                    xdrw = 0
                    kdrw = 0     
                    gnum = 0               
!+++++++++++++++++++CHECKING MULTIDRAW VERY COMPLEX TO CONTROL++++++++++++++++++                    
                    do temp = 1,min(valrec(VPZOFF),VMAX)
                      if(vdetail(VKIK,temp).ne.0) then ! This is a Kicker division
                        if(kdrw.eq.0) then
                          kdrw = vdetail(VDRW,temp)
                        else
                          if(kdrw.ne.vdetail(VDRW,temp)) then
                            type*,iam(),'=================================================='
                            type*,iam(),'MDRW  ',valrec(VGAM),' draw#',kdrw,vdetail(VDRW,temp)
                            type*,iam(),'Validation Serial#',valrec(VSCDC),'/',valrec(VSSER)
                            type*,iam(),'=================================================='                            
!>>>>>>>>>>>>>>>>>>>>>>>>>>>IF ERROR APPEARS CONTINUE AND ADD TO THE FIRST DRAW
                          endif
                        endif                        
                      else
                        if(xdrw.eq.0) then
                          xdrw = vdetail(VDRW,temp)
                        else
                          if(xdrw.ne.vdetail(VDRW,temp)) then
                            type*,iam(),'=================================================='
                            type*,iam(),'MKDRW ',valrec(VGAM),' draw#',xdrw,vdetail(VDRW,temp)
                            type*,iam(),'Validation Serial#',valrec(VSCDC),'/',valrec(VSSER)
                            type*,iam(),'=================================================='    
!>>>>>>>>>>>>>>>>>>>>>>>>>>>IF ERROR APPEARS CONTINUE AND ADD TO THE FIRST DRAW                            
                          endif
                        endif
                      endif 
                    enddo
!-------------------LETS GO FOR ACTIVE GAMES------------------------------------                    
                    if(valrec(VGAM).ne.valrec(VKGME)) then
                      gnum = valrec(VGAM)
                      if(gnum.gt.0.and.xdrw.gt.0) then
                        odrw = dayhdr(gnum) + 5 - xdrw
                        if(odrw.gt.0.and.odrw.le.MDRW) then
                          nagt = valrec(VCTER)
                          tcnt(3,nagt,odrw,gnum) = tcnt(3,nagt,odrw,gnum) + 1
!------------------ V05 BEGIN --------------------------------------------------
!                         tamt(3,nagt,odrw,gnum) = tamt(3,nagt,odrw,gnum) + valrec(VPAMT) - valrec(VOPSAMT) ! V02
C----+------------------------------------------------------------------
C V06| Allowing daily generations of AM_WAG file
C----+------------------------------------------------------------------
C                         if(valrec(VOPSAMT) .eq. 0) then
                          if(valrec(VPAMT) .lt. P(VALORDER)) then
C----+------------------------------------------------------------------
C V06| Allowing daily generations of AM_WAG file
C----+------------------------------------------------------------------
                            ! only accumulate prize amounts which don't have OP
                            tamt(3,nagt,odrw,gnum) = tamt(3,nagt,odrw,gnum) + valrec(VPAMT)
                          endif
!------------------ V05 END   --------------------------------------------------
                        else
                          type*,iam(),'Incorrect Draw offset ',odrw,'#',valrec(VSCDC),valrec(VSSER)                           
                        endif                                         
                      endif
                    endif
!-------------------LETS GO FOR ADDON JOKER GAME--------------------------------
                    if(kdrw.gt.0) then
                      gnum = valrec(VKGME)
                      if(gnum.le.0) then
                        type*,iam(),'================== E R R O R ====================='
                        type*,iam(),'Inconsistent data addon game#',gnum,' draw#',kdrw
                        type*,iam(),'Validation Serial#',valrec(VSCDC),valrec(VSSER)
                        type*,iam(),'=================================================='
                      else
                        odrw = dayhdr(gnum) + 5 - kdrw
                        if(odrw.gt.0.and.odrw.le.MDRW) then
                          nagt = valrec(VCTER)
                          tcnt(3,nagt,odrw,gnum) = tcnt(3,nagt,odrw,gnum) + 1
!------------------ V05 BEGIN --------------------------------------------------
!                         tamt(3,nagt,odrw,gnum) = tamt(3,nagt,odrw,gnum) + valrec(VKPAMT) - valrec(VKOPSAMT) ! V02
C----+------------------------------------------------------------------
C V06| Allowing daily generations of AM_WAG file
C----+------------------------------------------------------------------
C                         if(valrec(VKOPSAMT) .eq. 0) then
                          if(valrec(VKPAMT) .lt. P(VALORDER)) then
C----+------------------------------------------------------------------
C V06| Allowing daily generations of AM_WAG file
C----+------------------------------------------------------------------
                          	! only accumulate prize amounts which don't have OP
                            tamt(3,nagt,odrw,gnum) = tamt(3,nagt,odrw,gnum) + valrec(VKPAMT)
                          endif
!------------------ V05 END   --------------------------------------------------
                        else
                          type*,iam(),'Incorrect Draw offset ',odrw,'#',valrec(VSCDC),valrec(VSSER)                           
                        endif                                         
                      endif
                    else
                      if(valrec(VGAM).eq.valrec(VKGME)) then
                        type*,iam(),'================== E R R O R ====================='
                        type*,iam(),'Inconsistent data addon game#',gnum,' draw#',kdrw
                        type*,iam(),'Validation Serial#',valrec(VSCDC),'/',valrec(VSSER)
                        type*,iam(),'=================================================='
                      endif
                    endif
!-------------------------------------------------------------------------------                    
                  endif                  
C V02             endif
                endif
!===============================================================================                
              endif
            enddo
          endif
        enddo
        call closefile(ffdb)
C=======CREATING THE FILE=======================================================
        write(cfil,9010) fdat(VYEAR2),fdat(VMON),fdat(VDAY)
        open(UNIT            = FLUN,
     *       FILE            = CFIL,
     *       STATUS          = 'NEW',
     *       CARRIAGECONTROL = 'LIST',
     *       ACCESS          = 'SEQUENTIAL',
     *       IOSTAT          = erro)
        if(erro.ne.0) then
          type*,iam(),'Error#',erro,' opening file ',cfil
          call gstop(GEXIT_FATAL)
        else
          type*,iam()
          type*,iam(),'Creating interface file ',cfil
          type*,iam()
        endif
        write(FLUN,9900) fdat(VYEAR2),fdat(VMON),fdat(VDAY),idat(VYEAR2),idat(VMON),idat(VDAY),fdat(VYEAR2),fdat(VMON),fdat(VDAY)
        tots = 2 ! Header + Footer
C=======WRITING RECORDS=========================================================
        do gnum=1,MAXGAM
          do xdrw=1,MDRW
            do nagt=1,NUMAGT
              if(tcnt(1,nagt,xdrw,gnum).gt.0.or.
     *           tcnt(2,nagt,xdrw,gnum).gt.0.or.
     *           tcnt(3,nagt,xdrw,gnum).gt.0) then
                tots = tots + 1
                write(FLUN,9910) agttab(AGTNUM,nagt),
     *                           tcnt(1,nagt,xdrw,gnum),tamt(1,nagt,xdrw,gnum),
     *                           tcnt(2,nagt,xdrw,gnum),tamt(2,nagt,xdrw,gnum),      
     *                           tcnt(3,nagt,xdrw,gnum),tamt(3,nagt,xdrw,gnum),
     *                           gnum,csor(xdrw,gnum),dsor(xdrw,gnum)
                ccnt(1,gnum) = ccnt(1,gnum) + tcnt(1,nagt,xdrw,gnum)
                camt(1,gnum) = camt(1,gnum) + tamt(1,nagt,xdrw,gnum)
                ccnt(2,gnum) = ccnt(2,gnum) + tcnt(2,nagt,xdrw,gnum)
                camt(2,gnum) = camt(2,gnum) + tamt(2,nagt,xdrw,gnum) 
                ccnt(3,gnum) = ccnt(3,gnum) + tcnt(3,nagt,xdrw,gnum)
                camt(3,gnum) = camt(3,gnum) + tamt(3,nagt,xdrw,gnum) 
!
                ccnt(1,MAXGAM+1) = ccnt(1,MAXGAM+1) + tcnt(1,nagt,xdrw,gnum)
                camt(1,MAXGAM+1) = camt(1,MAXGAM+1) + tamt(1,nagt,xdrw,gnum)
                ccnt(2,MAXGAM+1) = ccnt(2,MAXGAM+1) + tcnt(2,nagt,xdrw,gnum)
                camt(2,MAXGAM+1) = camt(2,MAXGAM+1) + tamt(2,nagt,xdrw,gnum)
                ccnt(3,MAXGAM+1) = ccnt(3,MAXGAM+1) + tcnt(3,nagt,xdrw,gnum)
                camt(3,MAXGAM+1) = camt(3,MAXGAM+1) + tamt(3,nagt,xdrw,gnum)
              else
                if(tamt(1,nagt,xdrw,gnum).gt.0.or.
     *             tamt(2,nagt,xdrw,gnum).gt.0.or.
     *             tamt(3,nagt,xdrw,gnum).gt.0) then ! No counter No amount
                  type*,iam()
                  type*,iam(),'<<<<<< Data inconsistency >>>>>>'
                  type*,iam(),'Game:        ',gnum
                  type*,iam(),'Draw:        ',dayhdr(gnum)-xdrw+1,'(',xdrw,')'
                  type*,iam(),'Terminal:    ',nagt
                  type*,iam(),'Wagers:      ',tcnt(1,nagt,xdrw,gnum),tamt(1,nagt,xdrw,gnum)
                  type*,iam(),'Cancels:     ',tcnt(2,nagt,xdrw,gnum),tamt(2,nagt,xdrw,gnum)
                  type*,iam(),'Validations: ',tcnt(3,nagt,xdrw,gnum),tamt(3,nagt,xdrw,gnum)                                    
                  type*,iam()
                  call gpause
                endif
              endif
            enddo
          enddo
        enddo
C===============================================================================        
        write(FLUN,9920) tots
        close(FLUN) 
        type*,iam(),tots,' Records written'
        type*,iam()
        do gnum = 1,MAXGAM
          if(ccnt(1,gnum).gt.0.or.ccnt(2,gnum).gt.0.or.ccnt(3,gnum).gt.0) then
            write(5,9040) iam(),(glnames(offs,gnum),offs=1,4),
     *                    iam(),ccnt(1,gnum),camt(1,gnum)/100,
     *                    iam(),ccnt(2,gnum),camt(2,gnum)/100,
     *                    iam(),ccnt(3,gnum),camt(3,gnum)/100,
     *                    iam(),iam()
          endif
        enddo
        write(5,9040) iam(),'ALL ','GAME','S   ','    ',
     *                iam(),ccnt(1,gnum),camt(1,gnum)/100,
     *                iam(),ccnt(2,gnum),camt(2,gnum)/100,
     *                iam(),ccnt(3,gnum),camt(3,gnum)/100,
     *                iam(),iam()        
!        
        call gstop(GEXIT_SUCCESS)
C=======FORMAT STATEMENTS=======================================================
102     FORMAT(I7)
9000    format(A4,':',A4,I4.4,'.FIL')     
9010    format('AM_WAG',I4.4,I2.2,I2.2,'.ASC')
9020    format(I3.3,I4.4)
9030    format(I4.4,I2.2,I2.2)
9040    format(1X,A,'------------- ',4A4,' ------------',/,
     *         1X,A,'TOTAL SALES ...... ',I10,2X,F12.2,/,
     *         1X,A,'TOTAL CANCELS .... ',I10,2X,F12.2,/,
     *         1X,A,'TOTAL PAYMENTS ... ',I10,2X,F12.2,/,
     *         1X,A,'-------------------------------------------',/,
     *         1X,A)
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------
C9100    format(1X,A,'Invoice from',7A2,' (',I4,') to',7A2,' (',I4,')')
9100    format(1X,A,'Invoice of',7A2,' (',I4,')')
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------
9900    format('HW1',I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,I4.4,I2.2,I2.2,73(' '))      
9910    format('HW2',I7.7,I6.6,I13.13,I6.6,I13.13,I6.6,I13.13,I2.2,A7,A8,16(' '))
9920    format('HW9',I10.10,87(' '))
        end
