
scribc = "bin/scribblec-assrt.sh"
scribjava = "."
oopslaart = "c:/Users/rhu/myroot/home/rhu1/code/misc/sessionstar/github.com/oopsla20-artifact"
fluidsessex = "c:/Users/rhu/myroot/home/rhu1/code/docker/fluidsess-dev/opam/scribbler/fluidsess/github.com/rhu1/fluidsess-examples"


.PHONY: help
help:
	@echo "Example usage:"
	@echo "        make examples scribc=\"...\" scribjava=\"...\" oopslaart=\"...\" fluidsess=\"...\""
	@echo "                scribc    -- path to scribblec-assrt.sh"
	@echo "                scribjava -- path to scribble-java (rhu1-assrt-artifact)"
	@echo "                oopslaart -- path to oopsla-artifact"
	@echo "                fluidsess -- path to fluidsess-examples"
	@echo ""


.PHONY: examples
examples: assrt-tmp oopsla20-examples fluidsess-extra


.PHONY: assrt-tmp
assrt-tmp:
	$(scribc) -fair -assrt -z3 -batch $(scribjava)/scribble-assertions/src/test/scrib/assrt/tmp/AssrtTest2.scr
	$(scribc) -fair -assrt -z3 -batch $(scribjava)/scribble-assertions/src/test/scrib/assrt/tmp/AssrtTest3.scr
	$(scribc) -fair -assrt -z3 -batch $(scribjava)/scribble-assertions/src/test/scrib/assrt/tmp/AssrtTest4.scr
	$(scribc) -fair -assrt -z3 -batch $(scribjava)/scribble-assertions/src/test/scrib/assrt/tmp/AssrtTest5.scr
	$(scribc) -fair -assrt -z3 -batch $(scribjava)/scribble-assertions/src/test/scrib/assrt/tmp/AssrtTest6.scr


.PHONY: oopsla20-examples
oopsla20-examples:
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/Calculator/Calc.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/Fibonacci/Fib.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/HigherLower/HigherLower.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/HTTP/Http.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/Negotiation/SapNego.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/OnlineWallet/OnlineWallet.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/SH/SH.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/Ticket/Ticket.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/TravelAgency/TravelAgency.scr
	$(scribc) -fair -assrt -z3 -batch $(oopslaart)/examples/TwoBuyer/TwoBuyer.scr


.PHONY: fluidsess-extra
fluidsess-extra:
	$(scribc) -fair -assrt -z3 -batch $(fluidsessex)/oopsla20/extra/Adder/Adder.scr
	$(scribc) -fair -assrt -z3 -batch $(fluidsessex)/oopsla20/extra/Counter/Counter.scr
	$(scribc) -fair -assrt -z3 -batch $(fluidsessex)/oopsla20/extra/DbC/DbC.scr
	$(scribc) -fair -assrt -z3 -batch $(fluidsessex)/oopsla20/extra/OAuth/OAuth.scr
	$(scribc) -fair -assrt -z3 -batch $(fluidsessex)/oopsla20/extra/Relay/Relay.scr

