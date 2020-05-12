
# TODO: factor out parameters

.PHONY: fluidsess-oopsla20
fluidsess-examples:
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Adder/Adder.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Calc/Calc.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Counter/Counter.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/DbC/DbC.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Fibonacci/Fib.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/HigherLower/HigherLower.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/OAuth/OAuth.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/OnlineWallet/OnlineWallet.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Relay/Relay.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi
	bin/scribblec-assrt.sh -fair -assrt -z3 -batch c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples/oopsla20/Ticket/Ticket.scr
	EXIT=$$?; if [ $$EXIT -ne 0 ]; then exit $$EXIT; fi


