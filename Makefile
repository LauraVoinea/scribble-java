
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
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Calculator/Calc.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Fibonacci/Fib.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/HigherLower/HigherLower.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/HTTP/Http.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Negotiation/SapNego.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/OnlineWallet/OnlineWallet.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/SH/SH.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Ticket/Ticket.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/TravelAgency/TravelAgency.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/TwoBuyer/TwoBuyer.scr

	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Adder/Adder.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Counter/Counter.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/DbC/DbC.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/OAuth/OAuth.scr
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/extra/Relay/Relay.scr


