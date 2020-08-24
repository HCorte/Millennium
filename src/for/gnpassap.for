C GNPASSAP.FOR
C
C V01 19-OCT-2010 FJG ePassive
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        options /check=nooverflow/ext
	program gnpassap
        implicit none
!
        include 'inclib:sysextrn.def'
        include 'inclib:sysdefine.def'
!
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def' 	
	include 'inclib:desval.def'        
        include 'inclib:valpasfil.def'
        include 'inclib:prmhsh.def'    
	include 'inclib:datbuf.def'            
!
        integer*4    vlun
        parameter    (vlun = 9)
!        
        integer*4    erro
        integer*4    gind
        integer*4    emis
        integer*4    mnum
        integer*4    mser
        integer*4    mfra
        integer*4    terr
        integer*4    mcdc
        integer*4    ocdc
        integer*4    icdc
        integer*4    fcdc
        integer*4    ttyp
	integer*2    date(DATLEN)           
        character*11 ckey        
        integer*4    week
        integer*4    year 
        integer*4    xcdc
        parameter    (xcdc = 3416)        
!
        integer*4    rmax
        parameter   (rmax=i4bucsiz/vpflen)
!                        
        integer*4    mbuf(VPFLEN,rmax)
        integer*4    nrec
        integer*4    nbuk
        integer*4    obuk
        integer*4    vkey(2)
!        
        integer*4    vbuf(I4BUCSIZ)
        equivalence (vbuf,mbuf)     
!       
        integer*4    tcnt
        integer*4    tamt 
        integer*4    ffdb(7)
        integer*4    offs
        integer*4    cnts
        integer*4    blks
        integer*4    mulr
        integer*4    nulr
        integer*4    mulc(1:4)          
        integer*4    rbuk(0:rmax)
!
        integer*4    smax
        parameter    (smax = 18)        
        integer*4    cmax
        parameter    (cmax = 90) 
!                       
        integer*4    statab(numtot,smax,pagemi,numpas)           
        integer*4    cdctab(numtot,cmax,pagemi,numpas)             
!        
        character*6  vsta(smax) /'VUNCSH','VCASH ','VCASHX','VDEL  ','VCXL  ','VHOLD ',
     *                           'VNOPAY','VNOPRZ','VPOST ','VCLAM ','VCLAMX','VPRPAY',
     *                           'VPPNPZ','VPRPST','VBANK ','VSBNK ','VSBNKM','TOTAL '/    
!
        record /stpasfdb/  pasfdb        
        record /stpasrec/  pasrec        
        record /stpasrec/  pasbuk(pasbukrec) !32 of stpasrec            
C===============================================================================
C       Start process 
C===============================================================================
        call copyrite
        type*,iam()
        type*,iam(),'<<<<<<<<<<< GNPASSAP - Check Passive integrity >>>>>>>>>>>'
        type*,iam()     
!
        if (daysts.eq.dsopen) then
          type*,iam()
          type*,iam(),'************************* WARNING *************************'
          type*,iam(),'*                                                         *'          
          type*,iam(),'* THIS PROGRAM SHOULD NOT BE RUN DURING ONLINE PROCESSING *'
          type*,iam(),'*     IF YOU CONTINUE YOU WILL DO IT BY YOUR OWN RISK     *'          
          type*,iam(),'*                                                         *'                    
          type*,iam(),'***********************************************************'          
          type*,iam()
          call gpause        
        endif
        call fastset(0,statab,numtot*smax*pagemi*numpas)                
        call fastset(0,cdctab,numtot*cmax*pagemi*numpas)
        type*,iam()
        call inpnum('Enter initial CDC to process ',icdc,xcdc,xcdc+cmax,erro)
        if(erro.ne.0) call gstop(gexit_opabort)
        call inpnum('Enter final CDC to process   ',fcdc,icdc,xcdc+cmax,erro)
        if(erro.ne.0) call gstop(gexit_opabort)
        type*,iam()
        type*,iam(),'**** Processing from ',icdc,' to ',fcdc,' ****'                
        type*,iam()        
C===============================================================================
C       Main Loop 
C===============================================================================        
        do gind=1,numpas
          do emis=1,pagemi
            if(passts(emis,gind).eq.GFINAL.and.passubsts(emis,gind).lt.pdrwclo) then
              type*,iam(),'====================================================='
              type*,iam(),'Draw: ',pasemis(emis,gind),' SCML: ',pasdraw(emis,gind),' Date: ',pasesd(emis,gind)
              type*,iam()                  
!+++++++++++++Get stats from VPF++++++++++++++++++++++++++++++++++++++++++++++++
              blks = 0
              call fastset(0,rbuk,rmax)
              call fastset(0,mulc,4)          
!           
              call openw(vlun,pasvpffil(1,emis,gind),0,0,0,erro)
              if(erro.ne.0) then
                type*,iam(),'Error: ',erro,' opening file: ',cpasvpffil(emis,gind)                
                cycle
              endif
              call ioinit(ffdb,vlun,bucsec*256) 
              type*,iam(),'Scanning file: ',cpasvpffil(emis,gind)                
C=============================================================================
C             Loop blks
C=============================================================================
!===========> INI LOOP
              do while (.true.)
                blks = blks + 1
                call readw(ffdb,blks,vbuf,erro)
                if(erro.eq.144) exit
                if(erro.ne.0) then
                  type*,iam(),'Error: ',erro,' reading file: ',cpasvpffil(emis,gind),blks
                  cycle
                endif
!
                mulr = 0
                cnts = 0
                nulr = 0
!=============> INI LOOP
                do offs = 1,rmax
                  if(mulr.gt.1) then
                    mulr = mulr - 1
                  else
                    if(mbuf(1,offs).ne.0) then
                      mulr = ishft(mbuf(1,offs),-30) + 1
                      mulc(mulr) = mulc(mulr) + 1
                      cnts = cnts + mulr
                      call logpas(valrec,mbuf(1,offs))
!                     Per status
                      statab(tracnt,valrec(vstat),emis,gind) = 
     *                statab(tracnt,valrec(vstat),emis,gind) + 1
                      statab(dolamt,valrec(vstat),emis,gind) = 
     *                statab(dolamt,valrec(vstat),emis,gind) + valrec(vpamt)
!                     Per Total
                      statab(tracnt,smax,emis,gind) = 
     *                statab(tracnt,smax,emis,gind) + 1
                      statab(dolamt,smax,emis,gind) = 
     *                statab(dolamt,smax,emis,gind) + valrec(vpamt) 
                    endif
                  endif
                enddo   
!=============> END LOOP             
  	      enddo
!===========> FIN LOOP  	        
              call closefil(ffdb)              
!+++++++++++++Check integrity with TPF++++++++++++++++++++++++++++++++++++++++++
              call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                        pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_openro(pasfdb)  
              if(pasfdb.err.ne.ioe_noerr) then
                type*,iam(),'Error: ',pasfdb.err,' opening file: ',pasfdb.filnam
                cycle              
              else 
                call iopen(pasvpffil(1,emis,gind),vlun,VPFLEN*2,VFSCDC,VFSSER*2-1,erro)
                if(erro.ne.0) then
                  type*,iam(),'Error: ',erro,' opening file: ',cpasvpffil(emis,gind)
                  call pasio_close(pasfdb) 
                  cycle    
                else
                  type*,iam(),'Scanning file: ',pasfdb.filnam                 
                  terr = 0
                  obuk = 0
!===============================================================================
                  do mnum = 0,pasnumtck(emis,gind)-1
                    do mser = 1,pasnumser(emis,gind)
                      do mfra = 1,pasnoffra(emis,gind)
!===============================================================================
!                       This is copied from PASIOSUBS to avoid the sucesive reads
!                       Please keep in mind that should be the same               
!===============================================================================       
                        nrec = (mnum+1-1)*pasfdb.maxser*pasfdb.maxfra !step 1: pasnum-1+1 because starts at zero
                        nrec = nrec + ((mser-1)*pasfdb.maxfra)        !step 2: the previous sers * # of fras
                        nrec = nrec + mfra                            !step 3: add the searched fraction
                        nbuk = ((nrec - 1)/pasbukrec)+1               !step 4: calculate bucket record
                        nrec = nrec-((nbuk-1)*pasbukrec)              !step 5: offset within the bucket                      
!===============================================================================                      
                        if(nbuk.ne.obuk) then
                          call readw(pasfdb.fdb,nbuk,pasbuk,pasfdb.err)
                          obuk = nbuk
                        else
                          pasfdb.err = ioe_noerr 
                        endif
                        pasrec = pasbuk(nrec)
                        if(pasfdb.err.eq.ioe_noerr.and.pasrec.key.ne.pasio_key(mnum,mser,mfra)) pasfdb.err = ioe_keyerr
!                      
                        if(pasfdb.err.ne.ioe_noerr) then
                          write(ckey,900) mnum,mser,mfra                        
                          type*,iam(),'Error: ',pasfdb.err,' reading record: ',ckey
                          terr = terr + 1
                          if(terr.ge.20) then
                            terr = 0
                            call gpause
                            type*,iam(),'Skipping file: ',pasfdb.filnam
                            goto 10
                          endif
                        else
!+++++++++++++++++++++++FRACTION FOUND++++++++++++++++++++++++++++++++++++++++++
                          if(pasrec.stat.eq.pbilkof) then
                            vkey(1) = mfra
                            vkey(2) = ishft(mser,24) + mnum                            
                            call iread(vkey,v4buf_pas,vlun,erro)
                            if(erro.ne.0) then
                              type*,iam(),'Error: ',erro,' reading VPF record'
                              type*,iam(),'Num: ',mnum,' Ser: ',mser,' Fra: ',mfra
                              call gpause
                              type*,iam(),'Skipping file...'
                              call iclose(vlun,vbuf,erro)   
                              goto 10
                            else
	                      call logpas(valrec,v4buf_pas)                      
	                      if(valrec(VSTAT).eq.VNOPAY) then     ! RAFTD
	                        ocdc = (pasrec.cdc - xcdc) + 1
	                        if(ocdc.gt.0) then
                                  cdctab(tracnt,ocdc,emis,gind) = cdctab(tracnt,ocdc,emis,gind) + 1
                                  cdctab(dolamt,ocdc,emis,gind) = cdctab(dolamt,ocdc,emis,gind) + valrec(vpamt)
                                  if(ocdc.gt.1.and.valrec(vstat).eq.VCXL) then
                                    type*,iam(),'Later'
                                    type*,iam(),mnum,mser,mfra
                                    type*,iam(),'CDC: ',pasrec.cdc
                                  endif
                                else
                                  type*,iam(),'Resta',ocdc
                                endif
                              else
                                if(valrec(VSTAT).ne.VCXL) type*,iam(),vsta(valrec(VSTAT)),pasrec.cdc
                                ocdc = (pasrec.cdc - pasesd(emis,gind)) + 1
                                if(valrec(VSTAT).eq.VCXL) then
                                  if(ocdc.gt.1) type*,iam(),vsta(valrec(VSTAT)),pasrec.cdc,ocdc
                                endif
                              endif
                            endif
                          endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        endif
                      enddo
                    enddo
                  enddo
                  call iclose(vlun,vbuf,erro)
                endif
10              continue                
                call pasio_close(pasfdb)   
              endif
!+++++++++++++Printout totals
              type*,iam()                
              do offs = 1,smax
                if(statab(tracnt,offs,emis,gind).gt.0) then
                  write(5,910) iam(),vsta(offs),statab(tracnt,offs,emis,gind),cmony(statab(dolamt,offs,emis,gind),12,valunit)
                endif
              enddo
              type*,iam()     
              tcnt = 0
              tamt = 0        
              do offs = 1,cmax
                if(cdctab(tracnt,offs,emis,gind).gt.0) then
                  tcnt = tcnt + cdctab(tracnt,offs,emis,gind)
                  tamt = tamt + cdctab(dolamt,offs,emis,gind)
                  write(5,920) iam(),xcdc+offs-1,cdctab(tracnt,offs,emis,gind),cmony(cdctab(dolamt,offs,emis,gind),12,valunit)
                endif                
              enddo   
              if(tcnt.gt.0.or.tamt.gt.0) then           
                type*,iam(),'         ------ ------------'              
                write(5,930) iam(),tcnt,cmony(tamt,12,valunit)
                type*,iam()              
              endif
            endif
          enddo
        enddo
!===============================================================================
        type*,iam()
        do mcdc = icdc,fcdc
          call opensap(vlun,mcdc)
          nrec = 1
          do gind=1,numpas
            do emis=1,pagemi
              if(passts(emis,gind).eq.GFINAL) then   
                if(gind.eq.PSBPOP) then
                  ttyp = 5
                else
                  ttyp = 0
                endif      
                week = pasdraw(emis,gind)/10000
                year = mod(pasdraw(emis,gind),10000)
                
                date(vcdc) = pasesd(emis,gind)
                call cdate(date)  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                
                if(pasesd(emis,gind).eq.mcdc) then              ! Today is draw date RC 03
                  tamt = statab(dolamt,VUNCSH,emis,gind) + 
     *                   statab(dolamt,VCASH,emis,gind)  +
     *                   statab(dolamt,VNOPAY,emis,gind) +
     *                   statab(dolamt,VBANK,emis,gind)  +
     *                   statab(dolamt,VPRPAY,emis,gind)
                  tamt = tamt - cdctab(dolamt,mcdc-xcdc+1,emis,gind) !Less RAFTD
                  nrec = nrec + 1
                  write(vlun,950) '03',ttyp,year,date(vmon),week,tamt,' '     
                else
                  if(pasprgcdc(emis,gind).eq.mcdc) then           ! Today is purge date RC 34
                    tamt = statab(dolamt,VUNCSH,emis,gind) +
     *                     statab(dolamt,VPRPAY,emis,gind)
                    nrec = nrec + 1
                    write(vlun,950) '34',ttyp,year,date(vmon),week,tamt,' '          
                  else
                    if(pasprgcdc(emis,gind).gt.mcdc) then
                      tamt = cdctab(dolamt,mcdc-xcdc+1,emis,gind)   ! RETAFTD RC 33
                      nrec = nrec + 1
                      write(vlun,950) '33',ttyp,year,date(vmon),week,tamt,' '                              
                    endif
                  endif
                endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                
              endif
            enddo
          enddo      
          nrec = nrec + 1
	  write(vlun,940) nrec,' '          
          close(vlun)
        enddo
!===============================================================================
        call gstop(gexit_success)
!===============================================================================
!       Formats
!===============================================================================
900     format(I5.5,'s',I2.2,'f',I2.2)
910     format(1X,A,1X,A7,1X,I6,1X,A12)                
920     format(1X,A,1X,I7,1X,I6,1X,A12)                
930     format(1X,A,1X,'Total: ',1X,I6,1X,A12)     
940     format('TP',I6.6,34(A1))         
950 	FORMAT(A2,I1,I4,I2.2,I2.2,I13.13,18(A1))  
        end
!===============================================================================
!       SUBROUTINE
!===============================================================================        
	options    /check=nooverflow
	subroutine opensap(vlun,mcdc)
	implicit   none
!
        include 'inclib:sysextrn.def'
        include 'inclib:sysdefine.def'
	include 'inclib:global.def'        
	include 'inclib:datbuf.def'
!	
        integer*4    vlun
        integer*4    mcdc
        integer*4    erro
	integer*2    date(DATLEN)        
	character*20 fnam
!	        
	write(fnam,900) mcdc
!	
	open ( unit   = vlun,
     *         file   = fnam,
     *         status = 'new',
     *         access = 'sequential',
     *         iostat = erro)
!
	if(erro.ne.0) then
	  type*,iam(),'Error oppening file ',fnam,erro
	  call gpause()
	  return
	endif
!
        date(VCDC) = mcdc
        call cdate(date)
        write(vlun,910) 2000+date(vyear),date(vmon),date(vday),' '
            	
	type*,iam(),'Created file: ',fnam
!
	return
!===============================================================================
!       Formats
!===============================================================================
900     format('FILE:PASSAP_',I4.4,'.ASC')
910     format('HP',I4.4,I2.2,I2.2,32(A1))
	end        
