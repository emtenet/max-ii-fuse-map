
ERL="c:\Program Files\Erlang OTP\bin\erl.exe"
ERLC="c:\Program Files\Erlang OTP\bin\erlc.exe"

.PHONY: run
run: ebin\quartus.beam ebin\quartus_compile.beam ebin\quartus_app.beam ebin\quartus_sup.beam
	${ERL} -setcookie michael -sname quartus -pa ebin -run quartus_app

ebin\quartus.beam: src\quartus.erl
	${ERLC} -o ebin src\quartus.erl

ebin\quartus_compile.beam: src\quartus_compile.erl
	${ERLC} -o ebin src\quartus_compile.erl

ebin\quartus_app.beam: src\quartus_app.erl
	${ERLC} -o ebin src\quartus_app.erl

ebin\quartus_sup.beam: src\quartus_sup.erl
	${ERLC} -o ebin src\quartus_sup.erl
