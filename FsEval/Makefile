build:
	xbuild FsEval.fsproj
release:
	xbuild FsEval.fsproj /p:Configuration=Release
rebuild:
	xbuild FsEval.fsproj "/target:Clean;Compile"
clean:
	xbuild FsEval.fsproj /target:Clean /p:Configuration=Release
	xbuild FsEval.fsproj /target:Clean /p:Configuration=Debug
run:
	mono ./bin/Debug/FsEval.exe
