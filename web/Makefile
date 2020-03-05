all: index.js.gz jspm.config.js.gz jspm_packages/system.js.gz jspm_packages/npm/datatables.net-dt@1.10.20/css/jquery.dataTables.css.gz style.css.gz

TS:=$(wildcard *.ts)

index.js style.css: node_modules/.bin/tsc node_modules/.bin/sass $(TS) scss/main.scss $(wildcard scss/*/*.scss)
	-node_modules/.bin/tsc
	LC_ALL=en_US.UTF-8 npm run-script build

node_modules/.bin/%:
	npm install

%.gz: %
	gzip -fk $< || gzip -c $< > $@
