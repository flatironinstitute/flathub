all: bundle.js.gz node_modules/datatables.net-dt/css/jquery.dataTables.css.gz style.css.gz

TS:=$(wildcard *.ts)

# Build sass
bundle.js style.css: node_modules/.bin/tsc node_modules/.bin/sass $(TS) scss/main.scss $(wildcard scss/*/*.scss)
	-node_modules/.bin/tsc
	LC_ALL=en_US.UTF-8 npm run-script build

# Run npm install
# node_modules/.bin/%:
# 	npm install

# Gzip everything
%.gz: %
	gzip -fk $< || gzip -c $< > $@
