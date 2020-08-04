# TODO: factor out params


.PHONY: examples
examples: assrt-tmp fluidsess-examples


.PHONY: assrt-tmp
assrt-tmp:
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch scribble-assertions/src/test/scrib/assrt/tmp/AssrtCoreTest2.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch scribble-assertions/src/test/scrib/assrt/tmp/AssrtCoreTest3.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch scribble-assertions/src/test/scrib/assrt/tmp/AssrtCoreTest4.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch scribble-assertions/src/test/scrib/assrt/tmp/AssrtCoreTest5.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch scribble-assertions/src/test/scrib/assrt/tmp/AssrtCoreTest6.scr


.PHONY: fluidsess-examples
fluidsess-examples:
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/Calculator/Calc.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/Fibonacci/Fib.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/HigherLower/HigherLower.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/HTTP/Http.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/Negotiation/SapNego.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/OnlineWallet/OnlineWallet.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/SH/SH.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/Ticket/Ticket.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/TravelAgency/TravelAgency.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact/examples/TwoBuyer/TwoBuyer.scr

	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Adder/Adder.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Counter/Counter.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/DbC/DbC.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/OAuth/OAuth.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Relay/Relay.scr


