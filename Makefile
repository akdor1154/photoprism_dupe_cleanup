SHELL:=sh
CLAUDE:=claude-remote
.ONESHELL:

.PHONY: runstorage/Pictures/Photoprism/2018/01

_run_cp: bin/cleanup
	rsync -P bin/cleanup $(CLAUDE):/tmp/
_run_ssh:
	ssh $(CLAUDE) sh <<'EOSSH'
	set -ex
	cd ~/storage/Pictures/Photoprism
	/tmp/cleanup 2018/01 2018/10
	EOSSH

run: _run_cp _run_ssh


.PHONY: clean
clean:
	dotnet clean
	rm -r bin obj

.PHONY: build
build: bin/cleanup

bin/cleanup: Program.fs cleanup.fsproj
	dotnet publish \
		--self-contained \
		--configuration=Release \
		--runtime linux-x64
	cp bin/Release/net6.0/linux-x64/publish/cleanup bin/cleanup