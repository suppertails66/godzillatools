mkdir -p script/orig

make libsms && make godzilla_scriptdmp
./godzilla_scriptdmp godzilla.gg script/orig/
