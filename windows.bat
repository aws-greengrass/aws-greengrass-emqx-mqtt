cd emqx
vcvarsall.bat x86_amd64 && ^
make && ^
cd _build\emqx\rel && ^
zip -r -q emqx-windows.zip emqx