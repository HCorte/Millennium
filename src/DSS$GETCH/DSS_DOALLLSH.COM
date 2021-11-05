$!
$! v01 13-feb-95 wxm initial release
$!
$ if "''p1'" .eqs. "" then p1 = "ACTIVE"
$ if "''p1'" .eqs. "*"      then goto START
$ if "''p1'" .eqs. "ACTIVE" then goto START
$ if "''p1'" .eqs. "LIVE"   then goto START
$ if "''p1'" .eqs. "TOOL"   then goto START
$ if "''p1'" .eqs. "JUNK"   then goto START
$ if "''p1'" .eqs. "UPDATE" then goto START
$ write sys$output "Invalid group parameter."
$ goto END
$ START:
$ if f$search("gxsrc:alllsh.lis") .nes. "" then del/nolog gxsrc:alllsh.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:alllsh.lis *.LSH
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:alllsh.lis 'p1'_code
$ endif
$ if f$search("gxsrc:alllsh.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:alllsh.lis
$ if f$search("alllsh.com") .nes. "" then del/nolog gxsrc:alllsh.com;*
$ open/write/error=erropout outfil gxsrc:alllsh.com
$ write outfil "$set nover"
$ write outfil "$set noon"
$ write outfil "$link = ""link/nomap"""
$ cnt = 0
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if f$parse("''file'",,,"TYPE") .nes. ".LSH" then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  write/err=errwrout outfil "$@dss$gtech:dss_dolsh ''name'"
$  cnt = cnt+1
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:ALLLSH.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:ALLLSH.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:ALLLSH.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:ALLLSH.COM"
$ close infil
$ goto END
$ ENDIN:
$ write/err=errwrout outfil "$del/symbol link"
$ close infil
$ close outfil
$ del/nolog gxsrc:alllsh.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No .LSH files found in ''p1' group"
$  del/nolog gxsrc:alllsh.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No .LSH files found in ''p1' group"
$ END:
$ exit
