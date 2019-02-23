javascript-bridge allows Haskell to call JavaScript APIs, and
receive JavaScript events. 

Started Nov 2016 by Andy Gill while at the University of Kansas,
to support calling JavaScript from Haskell, via web sockets,
replacing the comet design pattern based kansas-comet.

 - Reworked in 2018 to
    - support Justin Dawson's Blank Canvas and Remote Monad work
    - direct remote monad / remote applicative support
 - Reworked in 2019 to
    - use Conal's Event implementation for JavaScript events
    - use STM as the underlying timing mechanism for Future
 
Authors, contributors, or sponsors:
  Andy Gill
  Ryan Scott, Indiana University
  Google Inc.
    - (Andy Gill works at Google, so his contributions to this library during
       his employment comply with Google's open source patching policy)
  Conal Elliott gave permission to use his Push-Pull code under BSD
  This work is supported by the National Science Foundation CAREER #1350901.

Other authors or contributors are welcome!
