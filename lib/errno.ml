(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t =
  | E2BIG
  | EACCES
  | EADDRINUSE
  | EADDRNOTAVAIL
  | EAFNOSUPPORT
  | EAGAIN
  | EALREADY
  | EBADF
  | EBADMSG
  | EBUSY
  | ECANCELED
  | ECHILD
  | ECONNABORTED
  | ECONNREFUSED
  | ECONNRESET
  | EDEADLK
  | EDESTADDRREQ
  | EDOM
  | EDQUOT
  | EEXIST
  | EFAULT
  | EFBIG
  | EHOSTDOWN (* Linux: Host is down *)
  | EHOSTUNREACH
  | EIDRM
  | EILSEQ
  | EINPROGRESS
  | EINTR
  | EINVAL
  | EIO
  | EISCONN
  | EISDIR
  | ELOOP
  | EMFILE
  | EMLINK
  | EMSGSIZE
  | EMULTIHOP
  | ENAMETOOLONG
  | ENETDOWN
  | ENETRESET
  | ENETUNREACH
  | ENFILE
  | ENOBUFS
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOLINK
  | ENOMEM
  | ENOMSG
  | ENOPROTOOPT
  | ENOSPC
  | ENOSYS
  | ENOTBLK
  | ENOTCONN
  | ENOTDIR
  | ENOTEMPTY
  | ENOTRECOVERABLE
  | ENOTSOCK
  | ENOTSUP
  | ENOTTY
  | ENXIO
  | EOPNOTSUPP
  | EOVERFLOW
  | EOWNERDEAD
  | EPERM
  | EPFNOSUPPORT (* Linux: Protocol family not supported *)
  | EPIPE
  | EPROTO
  | EPROTONOSUPPORT
  | EPROTOTYPE
  | ERANGE
  | EREMOTE
  | EROFS
  | ESHUTDOWN (* Linux: Cannot send after transport endpoint shutdown *)
  | ESOCKTNOSUPPORT (* Linux: Socket type not supported *)
  | ESPIPE
  | ESRCH
  | ESTALE
  | ETIMEDOUT
  | ETOOMANYREFS (* Linux: Too many references: cannot splice *)
  | ETXTBSY
  | EUSERS
  | EWOULDBLOCK
  | EXDEV
  | ECHRNG
  | EL2NSYNC
  | EL3HLT
  | EL3RST
  | ELNRNG
  | EUNATCH
  | ENOCSI
  | EL2HLT
  | EBADE
  | EBADR
  | EXFULL
  | ENOANO
  | EBADRQC
  | EBADSLT
  | EBFONT
  | ENONET
  | ENOPKG
  | EADV
  | ESRMNT
  | ECOMM
  | EDOTDOT
  | ENOTUNIQ
  | EBADFD
  | EREMCHG
  | ELIBACC
  | ELIBBAD
  | ELIBSCN
  | ELIBMAX
  | ELIBEXEC
  | ERESTART
  | ESTRPIPE
  | EUCLEAN
  | ENOTNAM
  | ENAVAIL
  | EISNAM
  | EREMOTEIO
  | ENOMEDIUM
  | EMEDIUMTYPE
  | ENOKEY
  | EKEYEXPIRED
  | EKEYREVOKED
  | EKEYREJECTED
  | ERFKILL
  | EHWPOISON
  | EPWROFF
  | EDEVERR
  | EBADEXEC
  | EBADARCH
  | ESHLIBVERS
  | EBADMACHO
  | ENOPOLICY
  | EQFULL
  | EDOOFUS
  | ENOTCAPABLE
  | ECAPMODE
  | EPROCLIM
  | EBADRPC
  | ERPCMISMATCH
  | EPROGUNAVAIL
  | EPROGMISMATCH
  | EPROCUNAVAIL
  | EFTYPE
  | EAUTH
  | ENEEDAUTH
  | ENOATTR
  | ENOSTR
  | ENODATA
  | ETIME
  | ENOSR
  | EUNKNOWNERR of Signed.sint

type error = {
  errno : t list;
  call  : string;
  label : string;
}

exception Error of error

type defns = {
  e2big : Signed.sint option;
  eacces : Signed.sint option;
  eaddrinuse : Signed.sint option;
  eaddrnotavail : Signed.sint option;
  eafnosupport : Signed.sint option;
  eagain : Signed.sint option;
  ealready : Signed.sint option;
  ebadf : Signed.sint option;
  ebadmsg : Signed.sint option;
  ebusy : Signed.sint option;
  ecanceled : Signed.sint option;
  echild : Signed.sint option;
  econnaborted : Signed.sint option;
  econnrefused : Signed.sint option;
  econnreset : Signed.sint option;
  edeadlk : Signed.sint option;
  edestaddrreq : Signed.sint option;
  edom : Signed.sint option;
  edquot : Signed.sint option;
  eexist : Signed.sint option;
  efault : Signed.sint option;
  efbig : Signed.sint option;
  ehostdown : Signed.sint option;
  ehostunreach : Signed.sint option;
  eidrm : Signed.sint option;
  eilseq : Signed.sint option;
  einprogress : Signed.sint option;
  eintr : Signed.sint option;
  einval : Signed.sint option;
  eio : Signed.sint option;
  eisconn : Signed.sint option;
  eisdir : Signed.sint option;
  eloop : Signed.sint option;
  emfile : Signed.sint option;
  emlink : Signed.sint option;
  emsgsize : Signed.sint option;
  emultihop : Signed.sint option;
  enametoolong : Signed.sint option;
  enetdown : Signed.sint option;
  enetreset : Signed.sint option;
  enetunreach : Signed.sint option;
  enfile : Signed.sint option;
  enobufs : Signed.sint option;
  enodev : Signed.sint option;
  enoent : Signed.sint option;
  enoexec : Signed.sint option;
  enolck : Signed.sint option;
  enolink : Signed.sint option;
  enomem : Signed.sint option;
  enomsg : Signed.sint option;
  enoprotoopt : Signed.sint option;
  enospc : Signed.sint option;
  enosys : Signed.sint option;
  enotblk : Signed.sint option;
  enotconn : Signed.sint option;
  enotdir : Signed.sint option;
  enotempty : Signed.sint option;
  enotrecoverable : Signed.sint option;
  enotsock : Signed.sint option;
  enotsup : Signed.sint option;
  enotty : Signed.sint option;
  enxio : Signed.sint option;
  eopnotsupp : Signed.sint option;
  eoverflow : Signed.sint option;
  eownerdead : Signed.sint option;
  eperm : Signed.sint option;
  epfnosupport : Signed.sint option;
  epipe : Signed.sint option;
  eproto : Signed.sint option;
  eprotonosupport : Signed.sint option;
  eprototype : Signed.sint option;
  erange : Signed.sint option;
  eremote : Signed.sint option;
  erofs : Signed.sint option;
  eshutdown : Signed.sint option;
  esocktnosupport : Signed.sint option;
  espipe : Signed.sint option;
  esrch : Signed.sint option;
  estale : Signed.sint option;
  etimedout : Signed.sint option;
  etoomanyrefs : Signed.sint option;
  etxtbsy : Signed.sint option;
  eusers : Signed.sint option;
  ewouldblock : Signed.sint option;
  exdev : Signed.sint option;
  echrng : Signed.sint option;
  el2nsync : Signed.sint option;
  el3hlt : Signed.sint option;
  el3rst : Signed.sint option;
  elnrng : Signed.sint option;
  eunatch : Signed.sint option;
  enocsi : Signed.sint option;
  el2hlt : Signed.sint option;
  ebade : Signed.sint option;
  ebadr : Signed.sint option;
  exfull : Signed.sint option;
  enoano : Signed.sint option;
  ebadrqc : Signed.sint option;
  ebadslt : Signed.sint option;
  ebfont : Signed.sint option;
  enonet : Signed.sint option;
  enopkg : Signed.sint option;
  eadv : Signed.sint option;
  esrmnt : Signed.sint option;
  ecomm : Signed.sint option;
  edotdot : Signed.sint option;
  enotuniq : Signed.sint option;
  ebadfd : Signed.sint option;
  eremchg : Signed.sint option;
  elibacc : Signed.sint option;
  elibbad : Signed.sint option;
  elibscn : Signed.sint option;
  elibmax : Signed.sint option;
  elibexec : Signed.sint option;
  erestart : Signed.sint option;
  estrpipe : Signed.sint option;
  euclean : Signed.sint option;
  enotnam : Signed.sint option;
  enavail : Signed.sint option;
  eisnam : Signed.sint option;
  eremoteio : Signed.sint option;
  enomedium : Signed.sint option;
  emediumtype : Signed.sint option;
  enokey : Signed.sint option;
  ekeyexpired : Signed.sint option;
  ekeyrevoked : Signed.sint option;
  ekeyrejected : Signed.sint option;
  erfkill : Signed.sint option;
  ehwpoison : Signed.sint option;
  epwroff : Signed.sint option;
  edeverr : Signed.sint option;
  ebadexec : Signed.sint option;
  ebadarch : Signed.sint option;
  eshlibvers : Signed.sint option;
  ebadmacho : Signed.sint option;
  enopolicy : Signed.sint option;
  eqfull : Signed.sint option;
  edoofus : Signed.sint option;
  enotcapable : Signed.sint option;
  ecapmode : Signed.sint option;
  eproclim : Signed.sint option;
  ebadrpc : Signed.sint option;
  erpcmismatch : Signed.sint option;
  eprogunavail : Signed.sint option;
  eprogmismatch : Signed.sint option;
  eprocunavail : Signed.sint option;
  eftype : Signed.sint option;
  eauth : Signed.sint option;
  eneedauth : Signed.sint option;
  enoattr : Signed.sint option;
  enostr : Signed.sint option;
  enodata : Signed.sint option;
  etime : Signed.sint option;
  enosr : Signed.sint option;
}

type index = (Signed.sint, t) Hashtbl.t

let empty_defns = {
  e2big = None;
  eacces = None;
  eaddrinuse = None;
  eaddrnotavail = None;
  eafnosupport = None;
  eagain = None;
  ealready = None;
  ebadf = None;
  ebadmsg = None;
  ebusy = None;
  ecanceled = None;
  echild = None;
  econnaborted = None;
  econnrefused = None;
  econnreset = None;
  edeadlk = None;
  edestaddrreq = None;
  edom = None;
  edquot = None;
  eexist = None;
  efault = None;
  efbig = None;
  ehostdown = None;
  ehostunreach = None;
  eidrm = None;
  eilseq = None;
  einprogress = None;
  eintr = None;
  einval = None;
  eio = None;
  eisconn = None;
  eisdir = None;
  eloop = None;
  emfile = None;
  emlink = None;
  emsgsize = None;
  emultihop = None;
  enametoolong = None;
  enetdown = None;
  enetreset = None;
  enetunreach = None;
  enfile = None;
  enobufs = None;
  enodev = None;
  enoent = None;
  enoexec = None;
  enolck = None;
  enolink = None;
  enomem = None;
  enomsg = None;
  enoprotoopt = None;
  enospc = None;
  enosys = None;
  enotblk = None;
  enotconn = None;
  enotdir = None;
  enotempty = None;
  enotrecoverable = None;
  enotsock = None;
  enotsup = None;
  enotty = None;
  enxio = None;
  eopnotsupp = None;
  eoverflow = None;
  eownerdead = None;
  eperm = None;
  epfnosupport = None;
  epipe = None;
  eproto = None;
  eprotonosupport = None;
  eprototype = None;
  erange = None;
  eremote = None;
  erofs = None;
  eshutdown = None;
  esocktnosupport = None;
  espipe = None;
  esrch = None;
  estale = None;
  etimedout = None;
  etoomanyrefs = None;
  etxtbsy = None;
  eusers = None;
  ewouldblock = None;
  exdev = None;
  echrng = None;
  el2nsync = None;
  el3hlt = None;
  el3rst = None;
  elnrng = None;
  eunatch = None;
  enocsi = None;
  el2hlt = None;
  ebade = None;
  ebadr = None;
  exfull = None;
  enoano = None;
  ebadrqc = None;
  ebadslt = None;
  ebfont = None;
  enonet = None;
  enopkg = None;
  eadv = None;
  esrmnt = None;
  ecomm = None;
  edotdot = None;
  enotuniq = None;
  ebadfd = None;
  eremchg = None;
  elibacc = None;
  elibbad = None;
  elibscn = None;
  elibmax = None;
  elibexec = None;
  erestart = None;
  estrpipe = None;
  euclean = None;
  enotnam = None;
  enavail = None;
  eisnam = None;
  eremoteio = None;
  enomedium = None;
  emediumtype = None;
  enokey = None;
  ekeyexpired = None;
  ekeyrevoked = None;
  ekeyrejected = None;
  erfkill = None;
  ehwpoison = None;
  epwroff = None;
  edeverr = None;
  ebadexec = None;
  ebadarch = None;
  eshlibvers = None;
  ebadmacho = None;
  enopolicy = None;
  eqfull = None;
  edoofus = None;
  enotcapable = None;
  ecapmode = None;
  eproclim = None;
  ebadrpc = None;
  erpcmismatch = None;
  eprogunavail = None;
  eprogmismatch = None;
  eprocunavail = None;
  eftype = None;
  eauth = None;
  eneedauth = None;
  enoattr = None;
  enostr = None;
  enodata = None;
  etime = None;
  enosr = None;
}

let to_code ~host = let (defns,_) = host in function
  | x when x = E2BIG -> defns.e2big
  | x when x = EACCES -> defns.eacces
  | x when x = EADDRINUSE -> defns.eaddrinuse
  | x when x = EADDRNOTAVAIL -> defns.eaddrnotavail
  | x when x = EAFNOSUPPORT -> defns.eafnosupport
  | x when x = EAGAIN -> defns.eagain
  | x when x = EALREADY -> defns.ealready
  | x when x = EBADF -> defns.ebadf
  | x when x = EBADMSG -> defns.ebadmsg
  | x when x = EBUSY -> defns.ebusy
  | x when x = ECANCELED -> defns.ecanceled
  | x when x = ECHILD -> defns.echild
  | x when x = ECONNABORTED -> defns.econnaborted
  | x when x = ECONNREFUSED -> defns.econnrefused
  | x when x = ECONNRESET -> defns.econnreset
  | x when x = EDEADLK -> defns.edeadlk
  | x when x = EDESTADDRREQ -> defns.edestaddrreq
  | x when x = EDOM -> defns.edom
  | x when x = EDQUOT -> defns.edquot
  | x when x = EEXIST -> defns.eexist
  | x when x = EFAULT -> defns.efault
  | x when x = EFBIG -> defns.efbig
  | x when x = EHOSTDOWN -> defns.ehostdown
  | x when x = EHOSTUNREACH -> defns.ehostunreach
  | x when x = EIDRM -> defns.eidrm
  | x when x = EILSEQ -> defns.eilseq
  | x when x = EINPROGRESS -> defns.einprogress
  | x when x = EINTR -> defns.eintr
  | x when x = EINVAL -> defns.einval
  | x when x = EIO -> defns.eio
  | x when x = EISCONN -> defns.eisconn
  | x when x = EISDIR -> defns.eisdir
  | x when x = ELOOP -> defns.eloop
  | x when x = EMFILE -> defns.emfile
  | x when x = EMLINK -> defns.emlink
  | x when x = EMSGSIZE -> defns.emsgsize
  | x when x = EMULTIHOP -> defns.emultihop
  | x when x = ENAMETOOLONG -> defns.enametoolong
  | x when x = ENETDOWN -> defns.enetdown
  | x when x = ENETRESET -> defns.enetreset
  | x when x = ENETUNREACH -> defns.enetunreach
  | x when x = ENFILE -> defns.enfile
  | x when x = ENOBUFS -> defns.enobufs
  | x when x = ENODEV -> defns.enodev
  | x when x = ENOENT -> defns.enoent
  | x when x = ENOEXEC -> defns.enoexec
  | x when x = ENOLCK -> defns.enolck
  | x when x = ENOLINK -> defns.enolink
  | x when x = ENOMEM -> defns.enomem
  | x when x = ENOMSG -> defns.enomsg
  | x when x = ENOPROTOOPT -> defns.enoprotoopt
  | x when x = ENOSPC -> defns.enospc
  | x when x = ENOSYS -> defns.enosys
  | x when x = ENOTBLK -> defns.enotblk
  | x when x = ENOTCONN -> defns.enotconn
  | x when x = ENOTDIR -> defns.enotdir
  | x when x = ENOTEMPTY -> defns.enotempty
  | x when x = ENOTRECOVERABLE -> defns.enotrecoverable
  | x when x = ENOTSOCK -> defns.enotsock
  | x when x = ENOTSUP -> defns.enotsup
  | x when x = ENOTTY -> defns.enotty
  | x when x = ENXIO -> defns.enxio
  | x when x = EOPNOTSUPP -> defns.eopnotsupp
  | x when x = EOVERFLOW -> defns.eoverflow
  | x when x = EOWNERDEAD -> defns.eownerdead
  | x when x = EPERM -> defns.eperm
  | x when x = EPFNOSUPPORT -> defns.epfnosupport
  | x when x = EPIPE -> defns.epipe
  | x when x = EPROTO -> defns.eproto
  | x when x = EPROTONOSUPPORT -> defns.eprotonosupport
  | x when x = EPROTOTYPE -> defns.eprototype
  | x when x = ERANGE -> defns.erange
  | x when x = EREMOTE -> defns.eremote
  | x when x = EROFS -> defns.erofs
  | x when x = ESHUTDOWN -> defns.eshutdown
  | x when x = ESOCKTNOSUPPORT -> defns.esocktnosupport
  | x when x = ESPIPE -> defns.espipe
  | x when x = ESRCH -> defns.esrch
  | x when x = ESTALE -> defns.estale
  | x when x = ETIMEDOUT -> defns.etimedout
  | x when x = ETOOMANYREFS -> defns.etoomanyrefs
  | x when x = ETXTBSY -> defns.etxtbsy
  | x when x = EUSERS -> defns.eusers
  | x when x = EWOULDBLOCK -> defns.ewouldblock
  | x when x = EXDEV -> defns.exdev
  | x when x = ECHRNG -> defns.echrng
  | x when x = EL2NSYNC -> defns.el2nsync
  | x when x = EL3HLT -> defns.el3hlt
  | x when x = EL3RST -> defns.el3rst
  | x when x = ELNRNG -> defns.elnrng
  | x when x = EUNATCH -> defns.eunatch
  | x when x = ENOCSI -> defns.enocsi
  | x when x = EL2HLT -> defns.el2hlt
  | x when x = EBADE -> defns.ebade
  | x when x = EBADR -> defns.ebadr
  | x when x = EXFULL -> defns.exfull
  | x when x = ENOANO -> defns.enoano
  | x when x = EBADRQC -> defns.ebadrqc
  | x when x = EBADSLT -> defns.ebadslt
  | x when x = EBFONT -> defns.ebfont
  | x when x = ENONET -> defns.enonet
  | x when x = ENOPKG -> defns.enopkg
  | x when x = EADV -> defns.eadv
  | x when x = ESRMNT -> defns.esrmnt
  | x when x = ECOMM -> defns.ecomm
  | x when x = EDOTDOT -> defns.edotdot
  | x when x = ENOTUNIQ -> defns.enotuniq
  | x when x = EBADFD -> defns.ebadfd
  | x when x = EREMCHG -> defns.eremchg
  | x when x = ELIBACC -> defns.elibacc
  | x when x = ELIBBAD -> defns.elibbad
  | x when x = ELIBSCN -> defns.elibscn
  | x when x = ELIBMAX -> defns.elibmax
  | x when x = ELIBEXEC -> defns.elibexec
  | x when x = ERESTART -> defns.erestart
  | x when x = ESTRPIPE -> defns.estrpipe
  | x when x = EUCLEAN -> defns.euclean
  | x when x = ENOTNAM -> defns.enotnam
  | x when x = ENAVAIL -> defns.enavail
  | x when x = EISNAM -> defns.eisnam
  | x when x = EREMOTEIO -> defns.eremoteio
  | x when x = ENOMEDIUM -> defns.enomedium
  | x when x = EMEDIUMTYPE -> defns.emediumtype
  | x when x = ENOKEY -> defns.enokey
  | x when x = EKEYEXPIRED -> defns.ekeyexpired
  | x when x = EKEYREVOKED -> defns.ekeyrevoked
  | x when x = EKEYREJECTED -> defns.ekeyrejected
  | x when x = ERFKILL -> defns.erfkill
  | x when x = EHWPOISON -> defns.ehwpoison
  | x when x = EPWROFF -> defns.epwroff
  | x when x = EDEVERR -> defns.edeverr
  | x when x = EBADEXEC -> defns.ebadexec
  | x when x = EBADARCH -> defns.ebadarch
  | x when x = ESHLIBVERS -> defns.eshlibvers
  | x when x = EBADMACHO -> defns.ebadmacho
  | x when x = ENOPOLICY -> defns.enopolicy
  | x when x = EQFULL -> defns.eqfull
  | x when x = EDOOFUS -> defns.edoofus
  | x when x = ENOTCAPABLE -> defns.enotcapable
  | x when x = ECAPMODE -> defns.ecapmode
  | x when x = EPROCLIM -> defns.eproclim
  | x when x = EBADRPC -> defns.ebadrpc
  | x when x = ERPCMISMATCH -> defns.erpcmismatch
  | x when x = EPROGUNAVAIL -> defns.eprogunavail
  | x when x = EPROGMISMATCH -> defns.eprogmismatch
  | x when x = EPROCUNAVAIL -> defns.eprocunavail
  | x when x = EFTYPE -> defns.eftype
  | x when x = EAUTH -> defns.eauth
  | x when x = ENEEDAUTH -> defns.eneedauth
  | x when x = ENOATTR -> defns.enoattr
  | x when x = ENOSTR -> defns.enostr
  | x when x = ENODATA -> defns.enodata
  | x when x = ETIME -> defns.etime
  | x when x = ENOSR -> defns.enosr
  | EUNKNOWNERR x   -> Some x
  | _ -> assert false

let with_code defns symbol code = match symbol with
  | x when x = E2BIG -> { defns with e2big = code }
  | x when x = EACCES -> { defns with eacces = code }
  | x when x = EADDRINUSE -> { defns with eaddrinuse = code }
  | x when x = EADDRNOTAVAIL -> { defns with eaddrnotavail = code }
  | x when x = EAFNOSUPPORT -> { defns with eafnosupport = code }
  | x when x = EAGAIN -> { defns with eagain = code }
  | x when x = EALREADY -> { defns with ealready = code }
  | x when x = EBADF -> { defns with ebadf = code }
  | x when x = EBADMSG -> { defns with ebadmsg = code }
  | x when x = EBUSY -> { defns with ebusy = code }
  | x when x = ECANCELED -> { defns with ecanceled = code }
  | x when x = ECHILD -> { defns with echild = code }
  | x when x = ECONNABORTED -> { defns with econnaborted = code }
  | x when x = ECONNREFUSED -> { defns with econnrefused = code }
  | x when x = ECONNRESET -> { defns with econnreset = code }
  | x when x = EDEADLK -> { defns with edeadlk = code }
  | x when x = EDESTADDRREQ -> { defns with edestaddrreq = code }
  | x when x = EDOM -> { defns with edom = code }
  | x when x = EDQUOT -> { defns with edquot = code }
  | x when x = EEXIST -> { defns with eexist = code }
  | x when x = EFAULT -> { defns with efault = code }
  | x when x = EFBIG -> { defns with efbig = code }
  | x when x = EHOSTDOWN -> { defns with ehostdown = code }
  | x when x = EHOSTUNREACH -> { defns with ehostunreach = code }
  | x when x = EIDRM -> { defns with eidrm = code }
  | x when x = EILSEQ -> { defns with eilseq = code }
  | x when x = EINPROGRESS -> { defns with einprogress = code }
  | x when x = EINTR -> { defns with eintr = code }
  | x when x = EINVAL -> { defns with einval = code }
  | x when x = EIO -> { defns with eio = code }
  | x when x = EISCONN -> { defns with eisconn = code }
  | x when x = EISDIR -> { defns with eisdir = code }
  | x when x = ELOOP -> { defns with eloop = code }
  | x when x = EMFILE -> { defns with emfile = code }
  | x when x = EMLINK -> { defns with emlink = code }
  | x when x = EMSGSIZE -> { defns with emsgsize = code }
  | x when x = EMULTIHOP -> { defns with emultihop = code }
  | x when x = ENAMETOOLONG -> { defns with enametoolong = code }
  | x when x = ENETDOWN -> { defns with enetdown = code }
  | x when x = ENETRESET -> { defns with enetreset = code }
  | x when x = ENETUNREACH -> { defns with enetunreach = code }
  | x when x = ENFILE -> { defns with enfile = code }
  | x when x = ENOBUFS -> { defns with enobufs = code }
  | x when x = ENODEV -> { defns with enodev = code }
  | x when x = ENOENT -> { defns with enoent = code }
  | x when x = ENOEXEC -> { defns with enoexec = code }
  | x when x = ENOLCK -> { defns with enolck = code }
  | x when x = ENOLINK -> { defns with enolink = code }
  | x when x = ENOMEM -> { defns with enomem = code }
  | x when x = ENOMSG -> { defns with enomsg = code }
  | x when x = ENOPROTOOPT -> { defns with enoprotoopt = code }
  | x when x = ENOSPC -> { defns with enospc = code }
  | x when x = ENOSYS -> { defns with enosys = code }
  | x when x = ENOTBLK -> { defns with enotblk = code }
  | x when x = ENOTCONN -> { defns with enotconn = code }
  | x when x = ENOTDIR -> { defns with enotdir = code }
  | x when x = ENOTEMPTY -> { defns with enotempty = code }
  | x when x = ENOTRECOVERABLE -> { defns with enotrecoverable = code }
  | x when x = ENOTSOCK -> { defns with enotsock = code }
  | x when x = ENOTSUP -> { defns with enotsup = code }
  | x when x = ENOTTY -> { defns with enotty = code }
  | x when x = ENXIO -> { defns with enxio = code }
  | x when x = EOPNOTSUPP -> { defns with eopnotsupp = code }
  | x when x = EOVERFLOW -> { defns with eoverflow = code }
  | x when x = EOWNERDEAD -> { defns with eownerdead = code }
  | x when x = EPERM -> { defns with eperm = code }
  | x when x = EPFNOSUPPORT -> { defns with epfnosupport = code }
  | x when x = EPIPE -> { defns with epipe = code }
  | x when x = EPROTO -> { defns with eproto = code }
  | x when x = EPROTONOSUPPORT -> { defns with eprotonosupport = code }
  | x when x = EPROTOTYPE -> { defns with eprototype = code }
  | x when x = ERANGE -> { defns with erange = code }
  | x when x = EREMOTE -> { defns with eremote = code }
  | x when x = EROFS -> { defns with erofs = code }
  | x when x = ESHUTDOWN -> { defns with eshutdown = code }
  | x when x = ESOCKTNOSUPPORT -> { defns with esocktnosupport = code }
  | x when x = ESPIPE -> { defns with espipe = code }
  | x when x = ESRCH -> { defns with esrch = code }
  | x when x = ESTALE -> { defns with estale = code }
  | x when x = ETIMEDOUT -> { defns with etimedout = code }
  | x when x = ETOOMANYREFS -> { defns with etoomanyrefs = code }
  | x when x = ETXTBSY -> { defns with etxtbsy = code }
  | x when x = EUSERS -> { defns with eusers = code }
  | x when x = EWOULDBLOCK -> { defns with ewouldblock = code }
  | x when x = EXDEV -> { defns with exdev = code }
  | x when x = ECHRNG -> { defns with echrng = code }
  | x when x = EL2NSYNC -> { defns with el2nsync = code }
  | x when x = EL3HLT -> { defns with el3hlt = code }
  | x when x = EL3RST -> { defns with el3rst = code }
  | x when x = ELNRNG -> { defns with elnrng = code }
  | x when x = EUNATCH -> { defns with eunatch = code }
  | x when x = ENOCSI -> { defns with enocsi = code }
  | x when x = EL2HLT -> { defns with el2hlt = code }
  | x when x = EBADE -> { defns with ebade = code }
  | x when x = EBADR -> { defns with ebadr = code }
  | x when x = EXFULL -> { defns with exfull = code }
  | x when x = ENOANO -> { defns with enoano = code }
  | x when x = EBADRQC -> { defns with ebadrqc = code }
  | x when x = EBADSLT -> { defns with ebadslt = code }
  | x when x = EBFONT -> { defns with ebfont = code }
  | x when x = ENONET -> { defns with enonet = code }
  | x when x = ENOPKG -> { defns with enopkg = code }
  | x when x = EADV -> { defns with eadv = code }
  | x when x = ESRMNT -> { defns with esrmnt = code }
  | x when x = ECOMM -> { defns with ecomm = code }
  | x when x = EDOTDOT -> { defns with edotdot = code }
  | x when x = ENOTUNIQ -> { defns with enotuniq = code }
  | x when x = EBADFD -> { defns with ebadfd = code }
  | x when x = EREMCHG -> { defns with eremchg = code }
  | x when x = ELIBACC -> { defns with elibacc = code }
  | x when x = ELIBBAD -> { defns with elibbad = code }
  | x when x = ELIBSCN -> { defns with elibscn = code }
  | x when x = ELIBMAX -> { defns with elibmax = code }
  | x when x = ELIBEXEC -> { defns with elibexec = code }
  | x when x = ERESTART -> { defns with erestart = code }
  | x when x = ESTRPIPE -> { defns with estrpipe = code }
  | x when x = EUCLEAN -> { defns with euclean = code }
  | x when x = ENOTNAM -> { defns with enotnam = code }
  | x when x = ENAVAIL -> { defns with enavail = code }
  | x when x = EISNAM -> { defns with eisnam = code }
  | x when x = EREMOTEIO -> { defns with eremoteio = code }
  | x when x = ENOMEDIUM -> { defns with enomedium = code }
  | x when x = EMEDIUMTYPE -> { defns with emediumtype = code }
  | x when x = ENOKEY -> { defns with enokey = code }
  | x when x = EKEYEXPIRED -> { defns with ekeyexpired = code }
  | x when x = EKEYREVOKED -> { defns with ekeyrevoked = code }
  | x when x = EKEYREJECTED -> { defns with ekeyrejected = code }
  | x when x = ERFKILL -> { defns with erfkill = code }
  | x when x = EHWPOISON -> { defns with ehwpoison = code }
  | x when x = EPWROFF -> { defns with epwroff = code }
  | x when x = EDEVERR -> { defns with edeverr = code }
  | x when x = EBADEXEC -> { defns with ebadexec = code }
  | x when x = EBADARCH -> { defns with ebadarch = code }
  | x when x = ESHLIBVERS -> { defns with eshlibvers = code }
  | x when x = EBADMACHO -> { defns with ebadmacho = code }
  | x when x = ENOPOLICY -> { defns with enopolicy = code }
  | x when x = EQFULL -> { defns with eqfull = code }
  | x when x = EDOOFUS -> { defns with edoofus = code }
  | x when x = ENOTCAPABLE -> { defns with enotcapable = code }
  | x when x = ECAPMODE -> { defns with ecapmode = code }
  | x when x = EPROCLIM -> { defns with eproclim = code }
  | x when x = EBADRPC -> { defns with ebadrpc = code }
  | x when x = ERPCMISMATCH -> { defns with erpcmismatch = code }
  | x when x = EPROGUNAVAIL -> { defns with eprogunavail = code }
  | x when x = EPROGMISMATCH -> { defns with eprogmismatch = code }
  | x when x = EPROCUNAVAIL -> { defns with eprocunavail = code }
  | x when x = EFTYPE -> { defns with eftype = code }
  | x when x = EAUTH -> { defns with eauth = code }
  | x when x = ENEEDAUTH -> { defns with eneedauth = code }
  | x when x = ENOATTR -> { defns with enoattr = code }
  | x when x = ENOSTR -> { defns with enostr = code }
  | x when x = ENODATA -> { defns with enodata = code }
  | x when x = ETIME -> { defns with etime = code }
  | x when x = ENOSR -> { defns with enosr = code }
  | EUNKNOWNERR _ -> defns
  | _ -> assert false

let of_code ~host code =
  let (_,index) = host in
  match Hashtbl.find_all index code with
  | [] -> [EUNKNOWNERR code]
  | errnos -> errnos

let to_string = function
  | x when x = E2BIG -> "E2BIG"
  | x when x = EACCES -> "EACCES"
  | x when x = EADDRINUSE -> "EADDRINUSE"
  | x when x = EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | x when x = EAFNOSUPPORT -> "EAFNOSUPPORT"
  | x when x = EAGAIN -> "EAGAIN"
  | x when x = EALREADY -> "EALREADY"
  | x when x = EBADF -> "EBADF"
  | x when x = EBADMSG -> "EBADMSG"
  | x when x = EBUSY -> "EBUSY"
  | x when x = ECANCELED -> "ECANCELED"
  | x when x = ECHILD -> "ECHILD"
  | x when x = ECONNABORTED -> "ECONNABORTED"
  | x when x = ECONNREFUSED -> "ECONNREFUSED"
  | x when x = ECONNRESET -> "ECONNRESET"
  | x when x = EDEADLK -> "EDEADLK"
  | x when x = EDESTADDRREQ -> "EDESTADDRREQ"
  | x when x = EDOM -> "EDOM"
  | x when x = EDQUOT -> "EDQUOT"
  | x when x = EEXIST -> "EEXIST"
  | x when x = EFAULT -> "EFAULT"
  | x when x = EFBIG -> "EFBIG"
  | x when x = EHOSTDOWN -> "EHOSTDOWN"
  | x when x = EHOSTUNREACH -> "EHOSTUNREACH"
  | x when x = EIDRM -> "EIDRM"
  | x when x = EILSEQ -> "EILSEQ"
  | x when x = EINPROGRESS -> "EINPROGRESS"
  | x when x = EINTR -> "EINTR"
  | x when x = EINVAL -> "EINVAL"
  | x when x = EIO -> "EIO"
  | x when x = EISCONN -> "EISCONN"
  | x when x = EISDIR -> "EISDIR"
  | x when x = ELOOP -> "ELOOP"
  | x when x = EMFILE -> "EMFILE"
  | x when x = EMLINK -> "EMLINK"
  | x when x = EMSGSIZE -> "EMSGSIZE"
  | x when x = EMULTIHOP -> "EMULTIHOP"
  | x when x = ENAMETOOLONG -> "ENAMETOOLONG"
  | x when x = ENETDOWN -> "ENETDOWN"
  | x when x = ENETRESET -> "ENETRESET"
  | x when x = ENETUNREACH -> "ENETUNREACH"
  | x when x = ENFILE -> "ENFILE"
  | x when x = ENOBUFS -> "ENOBUFS"
  | x when x = ENODEV -> "ENODEV"
  | x when x = ENOENT -> "ENOENT"
  | x when x = ENOEXEC -> "ENOEXEC"
  | x when x = ENOLCK -> "ENOLCK"
  | x when x = ENOLINK -> "ENOLINK"
  | x when x = ENOMEM -> "ENOMEM"
  | x when x = ENOMSG -> "ENOMSG"
  | x when x = ENOPROTOOPT -> "ENOPROTOOPT"
  | x when x = ENOSPC -> "ENOSPC"
  | x when x = ENOSYS -> "ENOSYS"
  | x when x = ENOTBLK -> "ENOTBLK"
  | x when x = ENOTCONN -> "ENOTCONN"
  | x when x = ENOTDIR -> "ENOTDIR"
  | x when x = ENOTEMPTY -> "ENOTEMPTY"
  | x when x = ENOTRECOVERABLE -> "ENOTRECOVERABLE"
  | x when x = ENOTSOCK -> "ENOTSOCK"
  | x when x = ENOTSUP -> "ENOTSUP"
  | x when x = ENOTTY -> "ENOTTY"
  | x when x = ENXIO -> "ENXIO"
  | x when x = EOPNOTSUPP -> "EOPNOTSUPP"
  | x when x = EOVERFLOW -> "EOVERFLOW"
  | x when x = EOWNERDEAD -> "EOWNERDEAD"
  | x when x = EPERM -> "EPERM"
  | x when x = EPFNOSUPPORT -> "EPFNOSUPPORT"
  | x when x = EPIPE -> "EPIPE"
  | x when x = EPROTO -> "EPROTO"
  | x when x = EPROTONOSUPPORT -> "EPROTONOSUPPORT"
  | x when x = EPROTOTYPE -> "EPROTOTYPE"
  | x when x = ERANGE -> "ERANGE"
  | x when x = EREMOTE -> "EREMOTE"
  | x when x = EROFS -> "EROFS"
  | x when x = ESHUTDOWN -> "ESHUTDOWN"
  | x when x = ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
  | x when x = ESPIPE -> "ESPIPE"
  | x when x = ESRCH -> "ESRCH"
  | x when x = ESTALE -> "ESTALE"
  | x when x = ETIMEDOUT -> "ETIMEDOUT"
  | x when x = ETOOMANYREFS -> "ETOOMANYREFS"
  | x when x = ETXTBSY -> "ETXTBSY"
  | x when x = EUSERS -> "EUSERS"
  | x when x = EWOULDBLOCK -> "EWOULDBLOCK"
  | x when x = EXDEV -> "EXDEV"
  | x when x = ECHRNG -> "ECHRNG"
  | x when x = EL2NSYNC -> "EL2NSYNC"
  | x when x = EL3HLT -> "EL3HLT"
  | x when x = EL3RST -> "EL3RST"
  | x when x = ELNRNG -> "ELNRNG"
  | x when x = EUNATCH -> "EUNATCH"
  | x when x = ENOCSI -> "ENOCSI"
  | x when x = EL2HLT -> "EL2HLT"
  | x when x = EBADE -> "EBADE"
  | x when x = EBADR -> "EBADR"
  | x when x = EXFULL -> "EXFULL"
  | x when x = ENOANO -> "ENOANO"
  | x when x = EBADRQC -> "EBADRQC"
  | x when x = EBADSLT -> "EBADSLT"
  | x when x = EBFONT -> "EBFONT"
  | x when x = ENONET -> "ENONET"
  | x when x = ENOPKG -> "ENOPKG"
  | x when x = EADV -> "EADV"
  | x when x = ESRMNT -> "ESRMNT"
  | x when x = ECOMM -> "ECOMM"
  | x when x = EDOTDOT -> "EDOTDOT"
  | x when x = ENOTUNIQ -> "ENOTUNIQ"
  | x when x = EBADFD -> "EBADFD"
  | x when x = EREMCHG -> "EREMCHG"
  | x when x = ELIBACC -> "ELIBACC"
  | x when x = ELIBBAD -> "ELIBBAD"
  | x when x = ELIBSCN -> "ELIBSCN"
  | x when x = ELIBMAX -> "ELIBMAX"
  | x when x = ELIBEXEC -> "ELIBEXEC"
  | x when x = ERESTART -> "ERESTART"
  | x when x = ESTRPIPE -> "ESTRPIPE"
  | x when x = EUCLEAN -> "EUCLEAN"
  | x when x = ENOTNAM -> "ENOTNAM"
  | x when x = ENAVAIL -> "ENAVAIL"
  | x when x = EISNAM -> "EISNAM"
  | x when x = EREMOTEIO -> "EREMOTEIO"
  | x when x = ENOMEDIUM -> "ENOMEDIUM"
  | x when x = EMEDIUMTYPE -> "EMEDIUMTYPE"
  | x when x = ENOKEY -> "ENOKEY"
  | x when x = EKEYEXPIRED -> "EKEYEXPIRED"
  | x when x = EKEYREVOKED -> "EKEYREVOKED"
  | x when x = EKEYREJECTED -> "EKEYREJECTED"
  | x when x = ERFKILL -> "ERFKILL"
  | x when x = EHWPOISON -> "EHWPOISON"
  | x when x = EPWROFF -> "EPWROFF"
  | x when x = EDEVERR -> "EDEVERR"
  | x when x = EBADEXEC -> "EBADEXEC"
  | x when x = EBADARCH -> "EBADARCH"
  | x when x = ESHLIBVERS -> "ESHLIBVERS"
  | x when x = EBADMACHO -> "EBADMACHO"
  | x when x = ENOPOLICY -> "ENOPOLICY"
  | x when x = EQFULL -> "EQFULL"
  | x when x = EDOOFUS -> "EDOOFUS"
  | x when x = ENOTCAPABLE -> "ENOTCAPABLE"
  | x when x = ECAPMODE -> "ECAPMODE"
  | x when x = EPROCLIM -> "EPROCLIM"
  | x when x = EBADRPC -> "EBADRPC"
  | x when x = ERPCMISMATCH -> "ERPCMISMATCH"
  | x when x = EPROGUNAVAIL -> "EPROGUNAVAIL"
  | x when x = EPROGMISMATCH -> "EPROGMISMATCH"
  | x when x = EPROCUNAVAIL -> "EPROCUNAVAIL"
  | x when x = EFTYPE -> "EFTYPE"
  | x when x = EAUTH -> "EAUTH"
  | x when x = ENEEDAUTH -> "ENEEDAUTH"
  | x when x = ENOATTR -> "ENOATTR"
  | x when x = ENOSTR -> "ENOSTR"
  | x when x = ENODATA -> "ENODATA"
  | x when x = ETIME -> "ETIME"
  | x when x = ENOSR -> "ENOSR"
  | EUNKNOWNERR x   -> "EUNKNOWNERR_"^(Signed.SInt.to_string x)
  | _ -> assert false

let of_string = function
  | "E2BIG" -> Some E2BIG
  | "EACCES" -> Some EACCES
  | "EADDRINUSE" -> Some EADDRINUSE
  | "EADDRNOTAVAIL" -> Some EADDRNOTAVAIL
  | "EAFNOSUPPORT" -> Some EAFNOSUPPORT
  | "EAGAIN" -> Some EAGAIN
  | "EALREADY" -> Some EALREADY
  | "EBADF" -> Some EBADF
  | "EBADMSG" -> Some EBADMSG
  | "EBUSY" -> Some EBUSY
  | "ECANCELED" -> Some ECANCELED
  | "ECHILD" -> Some ECHILD
  | "ECONNABORTED" -> Some ECONNABORTED
  | "ECONNREFUSED" -> Some ECONNREFUSED
  | "ECONNRESET" -> Some ECONNRESET
  | "EDEADLK" -> Some EDEADLK
  | "EDESTADDRREQ" -> Some EDESTADDRREQ
  | "EDOM" -> Some EDOM
  | "EDQUOT" -> Some EDQUOT
  | "EEXIST" -> Some EEXIST
  | "EFAULT" -> Some EFAULT
  | "EFBIG" -> Some EFBIG
  | "EHOSTDOWN" -> Some EHOSTDOWN
  | "EHOSTUNREACH" -> Some EHOSTUNREACH
  | "EIDRM" -> Some EIDRM
  | "EILSEQ" -> Some EILSEQ
  | "EINPROGRESS" -> Some EINPROGRESS
  | "EINTR" -> Some EINTR
  | "EINVAL" -> Some EINVAL
  | "EIO" -> Some EIO
  | "EISCONN" -> Some EISCONN
  | "EISDIR" -> Some EISDIR
  | "ELOOP" -> Some ELOOP
  | "EMFILE" -> Some EMFILE
  | "EMLINK" -> Some EMLINK
  | "EMSGSIZE" -> Some EMSGSIZE
  | "EMULTIHOP" -> Some EMULTIHOP
  | "ENAMETOOLONG" -> Some ENAMETOOLONG
  | "ENETDOWN" -> Some ENETDOWN
  | "ENETRESET" -> Some ENETRESET
  | "ENETUNREACH" -> Some ENETUNREACH
  | "ENFILE" -> Some ENFILE
  | "ENOBUFS" -> Some ENOBUFS
  | "ENODEV" -> Some ENODEV
  | "ENOENT" -> Some ENOENT
  | "ENOEXEC" -> Some ENOEXEC
  | "ENOLCK" -> Some ENOLCK
  | "ENOLINK" -> Some ENOLINK
  | "ENOMEM" -> Some ENOMEM
  | "ENOMSG" -> Some ENOMSG
  | "ENOPROTOOPT" -> Some ENOPROTOOPT
  | "ENOSPC" -> Some ENOSPC
  | "ENOSYS" -> Some ENOSYS
  | "ENOTBLK" -> Some ENOTBLK
  | "ENOTCONN" -> Some ENOTCONN
  | "ENOTDIR" -> Some ENOTDIR
  | "ENOTEMPTY" -> Some ENOTEMPTY
  | "ENOTRECOVERABLE" -> Some ENOTRECOVERABLE
  | "ENOTSOCK" -> Some ENOTSOCK
  | "ENOTSUP" -> Some ENOTSUP
  | "ENOTTY" -> Some ENOTTY
  | "ENXIO" -> Some ENXIO
  | "EOPNOTSUPP" -> Some EOPNOTSUPP
  | "EOVERFLOW" -> Some EOVERFLOW
  | "EOWNERDEAD" -> Some EOWNERDEAD
  | "EPERM" -> Some EPERM
  | "EPFNOSUPPORT" -> Some EPFNOSUPPORT
  | "EPIPE" -> Some EPIPE
  | "EPROTO" -> Some EPROTO
  | "EPROTONOSUPPORT" -> Some EPROTONOSUPPORT
  | "EPROTOTYPE" -> Some EPROTOTYPE
  | "ERANGE" -> Some ERANGE
  | "EREMOTE" -> Some EREMOTE
  | "EROFS" -> Some EROFS
  | "ESHUTDOWN" -> Some ESHUTDOWN
  | "ESOCKTNOSUPPORT" -> Some ESOCKTNOSUPPORT
  | "ESPIPE" -> Some ESPIPE
  | "ESRCH" -> Some ESRCH
  | "ESTALE" -> Some ESTALE
  | "ETIMEDOUT" -> Some ETIMEDOUT
  | "ETOOMANYREFS" -> Some ETOOMANYREFS
  | "ETXTBSY" -> Some ETXTBSY
  | "EUSERS" -> Some EUSERS
  | "EWOULDBLOCK" -> Some EWOULDBLOCK
  | "EXDEV" -> Some EXDEV
  | "ECHRNG" -> Some ECHRNG
  | "EL2NSYNC" -> Some EL2NSYNC
  | "EL3HLT" -> Some EL3HLT
  | "EL3RST" -> Some EL3RST
  | "ELNRNG" -> Some ELNRNG
  | "EUNATCH" -> Some EUNATCH
  | "ENOCSI" -> Some ENOCSI
  | "EL2HLT" -> Some EL2HLT
  | "EBADE" -> Some EBADE
  | "EBADR" -> Some EBADR
  | "EXFULL" -> Some EXFULL
  | "ENOANO" -> Some ENOANO
  | "EBADRQC" -> Some EBADRQC
  | "EBADSLT" -> Some EBADSLT
  | "EBFONT" -> Some EBFONT
  | "ENONET" -> Some ENONET
  | "ENOPKG" -> Some ENOPKG
  | "EADV" -> Some EADV
  | "ESRMNT" -> Some ESRMNT
  | "ECOMM" -> Some ECOMM
  | "EDOTDOT" -> Some EDOTDOT
  | "ENOTUNIQ" -> Some ENOTUNIQ
  | "EBADFD" -> Some EBADFD
  | "EREMCHG" -> Some EREMCHG
  | "ELIBACC" -> Some ELIBACC
  | "ELIBBAD" -> Some ELIBBAD
  | "ELIBSCN" -> Some ELIBSCN
  | "ELIBMAX" -> Some ELIBMAX
  | "ELIBEXEC" -> Some ELIBEXEC
  | "ERESTART" -> Some ERESTART
  | "ESTRPIPE" -> Some ESTRPIPE
  | "EUCLEAN" -> Some EUCLEAN
  | "ENOTNAM" -> Some ENOTNAM
  | "ENAVAIL" -> Some ENAVAIL
  | "EISNAM" -> Some EISNAM
  | "EREMOTEIO" -> Some EREMOTEIO
  | "ENOMEDIUM" -> Some ENOMEDIUM
  | "EMEDIUMTYPE" -> Some EMEDIUMTYPE
  | "ENOKEY" -> Some ENOKEY
  | "EKEYEXPIRED" -> Some EKEYEXPIRED
  | "EKEYREVOKED" -> Some EKEYREVOKED
  | "EKEYREJECTED" -> Some EKEYREJECTED
  | "ERFKILL" -> Some ERFKILL
  | "EHWPOISON" -> Some EHWPOISON
  | "EPWROFF" -> Some EPWROFF
  | "EDEVERR" -> Some EDEVERR
  | "EBADEXEC" -> Some EBADEXEC
  | "EBADARCH" -> Some EBADARCH
  | "ESHLIBVERS" -> Some ESHLIBVERS
  | "EBADMACHO" -> Some EBADMACHO
  | "ENOPOLICY" -> Some ENOPOLICY
  | "EQFULL" -> Some EQFULL
  | "EDOOFUS" -> Some EDOOFUS
  | "ENOTCAPABLE" -> Some ENOTCAPABLE
  | "ECAPMODE" -> Some ECAPMODE
  | "EPROCLIM" -> Some EPROCLIM
  | "EBADRPC" -> Some EBADRPC
  | "ERPCMISMATCH" -> Some ERPCMISMATCH
  | "EPROGUNAVAIL" -> Some EPROGUNAVAIL
  | "EPROGMISMATCH" -> Some EPROGMISMATCH
  | "EPROCUNAVAIL" -> Some EPROCUNAVAIL
  | "EFTYPE" -> Some EFTYPE
  | "EAUTH" -> Some EAUTH
  | "ENEEDAUTH" -> Some ENEEDAUTH
  | "ENOATTR" -> Some ENOATTR
  | "ENOSTR" -> Some ENOSTR
  | "ENODATA" -> Some ENODATA
  | "ETIME" -> Some ETIME
  | "ENOSR" -> Some ENOSR
  | _ -> None

let iter_defns defns f_exist f_missing =
  (match defns.e2big with
   | Some x -> f_exist x E2BIG | None -> f_missing E2BIG);
  (match defns.eacces with
   | Some x -> f_exist x EACCES | None -> f_missing EACCES);
  (match defns.eaddrinuse with
   | Some x -> f_exist x EADDRINUSE | None -> f_missing EADDRINUSE);
  (match defns.eaddrnotavail with
   | Some x -> f_exist x EADDRNOTAVAIL | None -> f_missing EADDRNOTAVAIL);
  (match defns.eafnosupport with
   | Some x -> f_exist x EAFNOSUPPORT | None -> f_missing EAFNOSUPPORT);
  (match defns.eagain with
   | Some x -> f_exist x EAGAIN | None -> f_missing EAGAIN);
  (match defns.ealready with
   | Some x -> f_exist x EALREADY | None -> f_missing EALREADY);
  (match defns.ebadf with
   | Some x -> f_exist x EBADF | None -> f_missing EBADF);
  (match defns.ebadmsg with
   | Some x -> f_exist x EBADMSG | None -> f_missing EBADMSG);
  (match defns.ebusy with
   | Some x -> f_exist x EBUSY | None -> f_missing EBUSY);
  (match defns.ecanceled with
   | Some x -> f_exist x ECANCELED | None -> f_missing ECANCELED);
  (match defns.echild with
   | Some x -> f_exist x ECHILD | None -> f_missing ECHILD);
  (match defns.econnaborted with
   | Some x -> f_exist x ECONNABORTED | None -> f_missing ECONNABORTED);
  (match defns.econnrefused with
   | Some x -> f_exist x ECONNREFUSED | None -> f_missing ECONNREFUSED);
  (match defns.econnreset with
   | Some x -> f_exist x ECONNRESET | None -> f_missing ECONNRESET);
  (match defns.edeadlk with
   | Some x -> f_exist x EDEADLK | None -> f_missing EDEADLK);
  (match defns.edestaddrreq with
   | Some x -> f_exist x EDESTADDRREQ | None -> f_missing EDESTADDRREQ);
  (match defns.edom with
   | Some x -> f_exist x EDOM | None -> f_missing EDOM);
  (match defns.edquot with
   | Some x -> f_exist x EDQUOT | None -> f_missing EDQUOT);
  (match defns.eexist with
   | Some x -> f_exist x EEXIST | None -> f_missing EEXIST);
  (match defns.efault with
   | Some x -> f_exist x EFAULT | None -> f_missing EFAULT);
  (match defns.efbig with
   | Some x -> f_exist x EFBIG | None -> f_missing EFBIG);
  (match defns.ehostdown with
   | Some x -> f_exist x EHOSTDOWN | None -> f_missing EHOSTDOWN);
  (match defns.ehostunreach with
   | Some x -> f_exist x EHOSTUNREACH | None -> f_missing EHOSTUNREACH);
  (match defns.eidrm with
   | Some x -> f_exist x EIDRM | None -> f_missing EIDRM);
  (match defns.eilseq with
   | Some x -> f_exist x EILSEQ | None -> f_missing EILSEQ);
  (match defns.einprogress with
   | Some x -> f_exist x EINPROGRESS | None -> f_missing EINPROGRESS);
  (match defns.eintr with
   | Some x -> f_exist x EINTR | None -> f_missing EINTR);
  (match defns.einval with
   | Some x -> f_exist x EINVAL | None -> f_missing EINVAL);
  (match defns.eio with
   | Some x -> f_exist x EIO | None -> f_missing EIO);
  (match defns.eisconn with
   | Some x -> f_exist x EISCONN | None -> f_missing EISCONN);
  (match defns.eisdir with
   | Some x -> f_exist x EISDIR | None -> f_missing EISDIR);
  (match defns.eloop with
   | Some x -> f_exist x ELOOP | None -> f_missing ELOOP);
  (match defns.emfile with
   | Some x -> f_exist x EMFILE | None -> f_missing EMFILE);
  (match defns.emlink with
   | Some x -> f_exist x EMLINK | None -> f_missing EMLINK);
  (match defns.emsgsize with
   | Some x -> f_exist x EMSGSIZE | None -> f_missing EMSGSIZE);
  (match defns.emultihop with
   | Some x -> f_exist x EMULTIHOP | None -> f_missing EMULTIHOP);
  (match defns.enametoolong with
   | Some x -> f_exist x ENAMETOOLONG | None -> f_missing ENAMETOOLONG);
  (match defns.enetdown with
   | Some x -> f_exist x ENETDOWN | None -> f_missing ENETDOWN);
  (match defns.enetreset with
   | Some x -> f_exist x ENETRESET | None -> f_missing ENETRESET);
  (match defns.enetunreach with
   | Some x -> f_exist x ENETUNREACH | None -> f_missing ENETUNREACH);
  (match defns.enfile with
   | Some x -> f_exist x ENFILE | None -> f_missing ENFILE);
  (match defns.enobufs with
   | Some x -> f_exist x ENOBUFS | None -> f_missing ENOBUFS);
  (match defns.enodev with
   | Some x -> f_exist x ENODEV | None -> f_missing ENODEV);
  (match defns.enoent with
   | Some x -> f_exist x ENOENT | None -> f_missing ENOENT);
  (match defns.enoexec with
   | Some x -> f_exist x ENOEXEC | None -> f_missing ENOEXEC);
  (match defns.enolck with
   | Some x -> f_exist x ENOLCK | None -> f_missing ENOLCK);
  (match defns.enolink with
   | Some x -> f_exist x ENOLINK | None -> f_missing ENOLINK);
  (match defns.enomem with
   | Some x -> f_exist x ENOMEM | None -> f_missing ENOMEM);
  (match defns.enomsg with
   | Some x -> f_exist x ENOMSG | None -> f_missing ENOMSG);
  (match defns.enoprotoopt with
   | Some x -> f_exist x ENOPROTOOPT | None -> f_missing ENOPROTOOPT);
  (match defns.enospc with
   | Some x -> f_exist x ENOSPC | None -> f_missing ENOSPC);
  (match defns.enosys with
   | Some x -> f_exist x ENOSYS | None -> f_missing ENOSYS);
  (match defns.enotblk with
   | Some x -> f_exist x ENOTBLK | None -> f_missing ENOTBLK);
  (match defns.enotconn with
   | Some x -> f_exist x ENOTCONN | None -> f_missing ENOTCONN);
  (match defns.enotdir with
   | Some x -> f_exist x ENOTDIR | None -> f_missing ENOTDIR);
  (match defns.enotempty with
   | Some x -> f_exist x ENOTEMPTY | None -> f_missing ENOTEMPTY);
  (match defns.enotrecoverable with
   | Some x -> f_exist x ENOTRECOVERABLE | None -> f_missing ENOTRECOVERABLE);
  (match defns.enotsock with
   | Some x -> f_exist x ENOTSOCK | None -> f_missing ENOTSOCK);
  (match defns.enotsup with
   | Some x -> f_exist x ENOTSUP | None -> f_missing ENOTSUP);
  (match defns.enotty with
   | Some x -> f_exist x ENOTTY | None -> f_missing ENOTTY);
  (match defns.enxio with
   | Some x -> f_exist x ENXIO | None -> f_missing ENXIO);
  (match defns.eopnotsupp with
   | Some x -> f_exist x EOPNOTSUPP | None -> f_missing EOPNOTSUPP);
  (match defns.eoverflow with
   | Some x -> f_exist x EOVERFLOW | None -> f_missing EOVERFLOW);
  (match defns.eownerdead with
   | Some x -> f_exist x EOWNERDEAD | None -> f_missing EOWNERDEAD);
  (match defns.eperm with
   | Some x -> f_exist x EPERM | None -> f_missing EPERM);
  (match defns.epfnosupport with
   | Some x -> f_exist x EPFNOSUPPORT | None -> f_missing EPFNOSUPPORT);
  (match defns.epipe with
   | Some x -> f_exist x EPIPE | None -> f_missing EPIPE);
  (match defns.eproto with
   | Some x -> f_exist x EPROTO | None -> f_missing EPROTO);
  (match defns.eprotonosupport with
   | Some x -> f_exist x EPROTONOSUPPORT | None -> f_missing EPROTONOSUPPORT);
  (match defns.eprototype with
   | Some x -> f_exist x EPROTOTYPE | None -> f_missing EPROTOTYPE);
  (match defns.erange with
   | Some x -> f_exist x ERANGE | None -> f_missing ERANGE);
  (match defns.eremote with
   | Some x -> f_exist x EREMOTE | None -> f_missing EREMOTE);
  (match defns.erofs with
   | Some x -> f_exist x EROFS | None -> f_missing EROFS);
  (match defns.eshutdown with
   | Some x -> f_exist x ESHUTDOWN | None -> f_missing ESHUTDOWN);
  (match defns.esocktnosupport with
   | Some x -> f_exist x ESOCKTNOSUPPORT | None -> f_missing ESOCKTNOSUPPORT);
  (match defns.espipe with
   | Some x -> f_exist x ESPIPE | None -> f_missing ESPIPE);
  (match defns.esrch with
   | Some x -> f_exist x ESRCH | None -> f_missing ESRCH);
  (match defns.estale with
   | Some x -> f_exist x ESTALE | None -> f_missing ESTALE);
  (match defns.etimedout with
   | Some x -> f_exist x ETIMEDOUT | None -> f_missing ETIMEDOUT);
  (match defns.etoomanyrefs with
   | Some x -> f_exist x ETOOMANYREFS | None -> f_missing ETOOMANYREFS);
  (match defns.etxtbsy with
   | Some x -> f_exist x ETXTBSY | None -> f_missing ETXTBSY);
  (match defns.eusers with
   | Some x -> f_exist x EUSERS | None -> f_missing EUSERS);
  (match defns.ewouldblock with
   | Some x -> f_exist x EWOULDBLOCK | None -> f_missing EWOULDBLOCK);
  (match defns.exdev with
   | Some x -> f_exist x EXDEV | None -> f_missing EXDEV);
  (match defns.echrng with
   | Some x -> f_exist x ECHRNG | None -> f_missing ECHRNG);
  (match defns.el2nsync with
   | Some x -> f_exist x EL2NSYNC | None -> f_missing EL2NSYNC);
  (match defns.el3hlt with
   | Some x -> f_exist x EL3HLT | None -> f_missing EL3HLT);
  (match defns.el3rst with
   | Some x -> f_exist x EL3RST | None -> f_missing EL3RST);
  (match defns.elnrng with
   | Some x -> f_exist x ELNRNG | None -> f_missing ELNRNG);
  (match defns.eunatch with
   | Some x -> f_exist x EUNATCH | None -> f_missing EUNATCH);
  (match defns.enocsi with
   | Some x -> f_exist x ENOCSI | None -> f_missing ENOCSI);
  (match defns.el2hlt with
   | Some x -> f_exist x EL2HLT | None -> f_missing EL2HLT);
  (match defns.ebade with
   | Some x -> f_exist x EBADE | None -> f_missing EBADE);
  (match defns.ebadr with
   | Some x -> f_exist x EBADR | None -> f_missing EBADR);
  (match defns.exfull with
   | Some x -> f_exist x EXFULL | None -> f_missing EXFULL);
  (match defns.enoano with
   | Some x -> f_exist x ENOANO | None -> f_missing ENOANO);
  (match defns.ebadrqc with
   | Some x -> f_exist x EBADRQC | None -> f_missing EBADRQC);
  (match defns.ebadslt with
   | Some x -> f_exist x EBADSLT | None -> f_missing EBADSLT);
  (match defns.ebfont with
   | Some x -> f_exist x EBFONT | None -> f_missing EBFONT);
  (match defns.enonet with
   | Some x -> f_exist x ENONET | None -> f_missing ENONET);
  (match defns.enopkg with
   | Some x -> f_exist x ENOPKG | None -> f_missing ENOPKG);
  (match defns.eadv with
   | Some x -> f_exist x EADV | None -> f_missing EADV);
  (match defns.esrmnt with
   | Some x -> f_exist x ESRMNT | None -> f_missing ESRMNT);
  (match defns.ecomm with
   | Some x -> f_exist x ECOMM | None -> f_missing ECOMM);
  (match defns.edotdot with
   | Some x -> f_exist x EDOTDOT | None -> f_missing EDOTDOT);
  (match defns.enotuniq with
   | Some x -> f_exist x ENOTUNIQ | None -> f_missing ENOTUNIQ);
  (match defns.ebadfd with
   | Some x -> f_exist x EBADFD | None -> f_missing EBADFD);
  (match defns.eremchg with
   | Some x -> f_exist x EREMCHG | None -> f_missing EREMCHG);
  (match defns.elibacc with
   | Some x -> f_exist x ELIBACC | None -> f_missing ELIBACC);
  (match defns.elibbad with
   | Some x -> f_exist x ELIBBAD | None -> f_missing ELIBBAD);
  (match defns.elibscn with
   | Some x -> f_exist x ELIBSCN | None -> f_missing ELIBSCN);
  (match defns.elibmax with
   | Some x -> f_exist x ELIBMAX | None -> f_missing ELIBMAX);
  (match defns.elibexec with
   | Some x -> f_exist x ELIBEXEC | None -> f_missing ELIBEXEC);
  (match defns.erestart with
   | Some x -> f_exist x ERESTART | None -> f_missing ERESTART);
  (match defns.estrpipe with
   | Some x -> f_exist x ESTRPIPE | None -> f_missing ESTRPIPE);
  (match defns.euclean with
   | Some x -> f_exist x EUCLEAN | None -> f_missing EUCLEAN);
  (match defns.enotnam with
   | Some x -> f_exist x ENOTNAM | None -> f_missing ENOTNAM);
  (match defns.enavail with
   | Some x -> f_exist x ENAVAIL | None -> f_missing ENAVAIL);
  (match defns.eisnam with
   | Some x -> f_exist x EISNAM | None -> f_missing EISNAM);
  (match defns.eremoteio with
   | Some x -> f_exist x EREMOTEIO | None -> f_missing EREMOTEIO);
  (match defns.enomedium with
   | Some x -> f_exist x ENOMEDIUM | None -> f_missing ENOMEDIUM);
  (match defns.emediumtype with
   | Some x -> f_exist x EMEDIUMTYPE | None -> f_missing EMEDIUMTYPE);
  (match defns.enokey with
   | Some x -> f_exist x ENOKEY | None -> f_missing ENOKEY);
  (match defns.ekeyexpired with
   | Some x -> f_exist x EKEYEXPIRED | None -> f_missing EKEYEXPIRED);
  (match defns.ekeyrevoked with
   | Some x -> f_exist x EKEYREVOKED | None -> f_missing EKEYREVOKED);
  (match defns.ekeyrejected with
   | Some x -> f_exist x EKEYREJECTED | None -> f_missing EKEYREJECTED);
  (match defns.erfkill with
   | Some x -> f_exist x ERFKILL | None -> f_missing ERFKILL);
  (match defns.ehwpoison with
   | Some x -> f_exist x EHWPOISON | None -> f_missing EHWPOISON);
  (match defns.epwroff with
   | Some x -> f_exist x EPWROFF | None -> f_missing EPWROFF);
  (match defns.edeverr with
   | Some x -> f_exist x EDEVERR | None -> f_missing EDEVERR);
  (match defns.ebadexec with
   | Some x -> f_exist x EBADEXEC | None -> f_missing EBADEXEC);
  (match defns.ebadarch with
   | Some x -> f_exist x EBADARCH | None -> f_missing EBADARCH);
  (match defns.eshlibvers with
   | Some x -> f_exist x ESHLIBVERS | None -> f_missing ESHLIBVERS);
  (match defns.ebadmacho with
   | Some x -> f_exist x EBADMACHO | None -> f_missing EBADMACHO);
  (match defns.enopolicy with
   | Some x -> f_exist x ENOPOLICY | None -> f_missing ENOPOLICY);
  (match defns.eqfull with
   | Some x -> f_exist x EQFULL | None -> f_missing EQFULL);
  (match defns.edoofus with
   | Some x -> f_exist x EDOOFUS | None -> f_missing EDOOFUS);
  (match defns.enotcapable with
   | Some x -> f_exist x ENOTCAPABLE | None -> f_missing ENOTCAPABLE);
  (match defns.ecapmode with
   | Some x -> f_exist x ECAPMODE | None -> f_missing ECAPMODE);
  (match defns.eproclim with
   | Some x -> f_exist x EPROCLIM | None -> f_missing EPROCLIM);
  (match defns.ebadrpc with
   | Some x -> f_exist x EBADRPC | None -> f_missing EBADRPC);
  (match defns.erpcmismatch with
   | Some x -> f_exist x ERPCMISMATCH | None -> f_missing ERPCMISMATCH);
  (match defns.eprogunavail with
   | Some x -> f_exist x EPROGUNAVAIL | None -> f_missing EPROGUNAVAIL);
  (match defns.eprogmismatch with
   | Some x -> f_exist x EPROGMISMATCH | None -> f_missing EPROGMISMATCH);
  (match defns.eprocunavail with
   | Some x -> f_exist x EPROCUNAVAIL | None -> f_missing EPROCUNAVAIL);
  (match defns.eftype with
   | Some x -> f_exist x EFTYPE | None -> f_missing EFTYPE);
  (match defns.eauth with
   | Some x -> f_exist x EAUTH | None -> f_missing EAUTH);
  (match defns.eneedauth with
   | Some x -> f_exist x ENEEDAUTH | None -> f_missing ENEEDAUTH);
  (match defns.enoattr with
   | Some x -> f_exist x ENOATTR | None -> f_missing ENOATTR);
  (match defns.enostr with
   | Some x -> f_exist x ENOSTR | None -> f_missing ENOSTR);
  (match defns.enodata with
   | Some x -> f_exist x ENODATA | None -> f_missing ENODATA);
  (match defns.etime with
   | Some x -> f_exist x ETIME | None -> f_missing ETIME);
  (match defns.enosr with
   | Some x -> f_exist x ENOSR | None -> f_missing ENOSR);
  ()

module Host = struct
  type t = defns * index

  let index_of_defns defns =
    let h = Hashtbl.create 100 in
    iter_defns defns (Hashtbl.add h) (fun _ -> ());
    h

  let of_defns defns = (defns, index_of_defns defns)

  let to_defns (defns, _) = defns

end

let string_of_defns defns =
  let buf = Buffer.create 1024 in
  iter_defns defns
    (fun code symbol ->
       Buffer.add_string buf (Printf.sprintf "%s\t%s\n" (to_string symbol)
                                (Signed.SInt.to_string code))
    )
    (fun symbol ->
       Buffer.add_string buf (Printf.sprintf "%s\t\n" (to_string symbol))
    );
  Buffer.contents buf

let defns_of_string s =
  let rec read_lines defns s =
    try
      let symbol, code, off = Scanf.sscanf s "%s\t%s\n" (fun symbol_s code_s ->
        of_string symbol_s,
        (if code_s = "" then None else Some (Signed.SInt.of_string code_s)),
        String.(length symbol_s + 1 + length code_s + 1)
      ) in
      let defns = match symbol with
        | Some symbol -> with_code defns symbol code
        | None -> defns
      in
      read_lines defns String.(sub s off (length s - off))
    with End_of_file -> defns
  in
  read_lines empty_defns s

let check_errno fn =
  try Ok (fn ())
  with Error e -> Error e

let string_of_error { errno; call; label } =
   Printf.sprintf "{ errno = [%s]; call = %s; label = %s }"
     (String.concat "; " (List.map to_string errno))
     call label

let () = Printexc.register_printer
   (function Error e -> Some (string_of_error e)
           | _ -> None)
